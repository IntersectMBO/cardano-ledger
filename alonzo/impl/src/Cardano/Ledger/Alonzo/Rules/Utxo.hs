{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Rules.Utxo where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize)
import Cardano.Ledger.Alonzo.Data (dataHashSize)
import Cardano.Ledger.Alonzo.Rules.Utxos (UTXOS, UtxosPredicateFailure)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices, pointWiseExUnits)
import Cardano.Ledger.Alonzo.Tx
  ( ValidatedTx (..),
    isTwoPhaseScriptAddress,
    minfee,
    txbody,
    wits',
  )
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo (ValidatedTx, txins)
import Cardano.Ledger.Alonzo.TxBody
  ( TxOut (..),
    txnetworkid',
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxBody, TxOut)
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo (TxSeq)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (txrdmrs'), nullRedeemers)
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, TxInBlock, ValidateScript (..))
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.Mary.Value as Alonzo (Value)
import Cardano.Ledger.Shelley.Constraints
  ( UsesPParams,
  )
import Cardano.Ledger.ShelleyMA.Rules.Utxo (consumed)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..), inInterval)
import qualified Cardano.Ledger.Val as Val
import Cardano.Prelude (HeapWords (..))
import Cardano.Slotting.Slot (SlotNo)
import Control.Iterate.SetAlgebra (dom, eval, (⊆), (◁), (➖))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Coders
  ( Decode (..),
    Encode (..),
    Wrapped (Open),
    decode,
    decodeList,
    decodeSet,
    encode,
    encodeFoldable,
    (!>),
    (<!),
  )
import Data.Coerce (coerce)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address
  ( Addr (AddrBootstrap),
    RewardAcnt,
    bootstrapAddressAttrsSize,
    getNetwork,
    getRwdNetwork,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Network,
    ShelleyBase,
    StrictMaybe (..),
    networkId,
  )
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.STS.Utxo as Shelley
import Shelley.Spec.Ledger.Tx (TxIn)
import Shelley.Spec.Ledger.TxBody (unWdrl)
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    balance,
    txouts,
    unUTxO,
  )

-- | Compute an estimate of the size of storing one UTxO entry.
-- This function implements the UTxO entry size estimate done by scaledMinDeposit in the ShelleyMA era
utxoEntrySize :: Era era => TxOut era -> Integer
utxoEntrySize txout
  | Val.adaOnly v =
    -- no non-ada assets, no hash datum case
    case dh of
      SNothing -> adaOnlyUTxOSize
      _ -> adaOnlyUTxOSize + dataHashSize dh
  -- add the size of Value and the size of datum hash (if present) to base UTxO size
  -- max function is a safeguard (in case calculation returns a smaller size than an ada-only entry)
  | otherwise = max adaOnlyUTxOSize (utxoEntrySizeWithoutVal + Val.size v + dataHashSize dh)
  where
    v = getField @"value" txout
    dh = getField @"datahash" txout
    -- lengths obtained from tracing on HeapWords of inputs and outputs
    -- obtained experimentally, and number used here
    -- units are Word64s
    txoutLenNoVal = 14
    txinLen = 7

    -- unpacked CompactCoin Word64 size in Word64s
    coinSize :: Integer
    coinSize = fromIntegral $ heapWords (CompactCoin 0)

    -- size of UTxO entry excluding the Value part
    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = 6 + txoutLenNoVal + txinLen

    -- size of commont UTxO with only ada and no datum
    adaOnlyUTxOSize = utxoEntrySizeWithoutVal + coinSize

-- ============================================

-- | The uninhabited type that marks the Alonzo UTxO rule
data AlonzoUTXO era

-- ==========================================================

data UtxoPredicateFailure era
  = -- | The bad transaction inputs
    BadInputsUTxO
      !(Set (TxIn (Crypto era)))
  | OutsideValidityIntervalUTxO
      !ValidityInterval
      -- ^ transaction's validity interval
      !SlotNo
      -- ^ current slot
  | MaxTxSizeUTxO
      !Integer
      -- ^ the actual transaction size
      !Integer
      -- ^ the max transaction size
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      !Coin
      -- ^ the minimum fee for this transaction
      !Coin
      -- ^ the fee supplied in this transaction
  | ValueNotConservedUTxO
      !(Core.Value era)
      -- ^ the Coin consumed by this transaction
      !(Core.Value era)
      -- ^ the Coin produced by this transaction
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      !Network -- the expected network id
      !(Set (Addr (Crypto era)))
  | WrongNetworkWithdrawal
      !Network
      -- ^ the expected network id
      !(Set (RewardAcnt (Crypto era)))
      -- ^ the set of reward addresses with incorrect network IDs
  | -- | list of supplied transaction outputs that are too small
    OutputTooSmallUTxO
      ![Core.TxOut era]
  | -- | Subtransition Failures
    UtxosFailure (PredicateFailure (Core.EraRule "UTXOS" era))
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig
      ![Core.TxOut era]
  | TriesToForgeADA
  | -- | list of supplied bad transaction outputs
    OutputTooBigUTxO
      ![Core.TxOut era]
  | FeeNotBalancedUTxO
      !Coin
      -- ^ balance computed
      !Coin
      -- ^ the fee supplied in this transaction
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO
      !(UTxO era)
  | ExUnitsTooBigUTxO
      !ExUnits
      -- ^ Max EXUnits from the protocol parameters
      !ExUnits
      -- ^ EXUnits supplied
  | -- | The inputs marked for use as fees contain non-ADA tokens
    FeeContainsNonADA !(Core.Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody
      !Network -- Actual Network ID
      !Network -- Network ID in transaction body
  deriving (Generic)

deriving stock instance
  ( Era era,
    Show (Core.Value era),
    Show (Core.TxOut era),
    Show (Core.TxBody era),
    Show (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  Show (UtxoPredicateFailure era)

deriving stock instance
  ( Eq (Core.Value era),
    Eq (Core.TxOut era),
    Eq (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  Eq (UtxoPredicateFailure era)

instance
  ( Era era,
    Shelley.TransUTxOState NoThunks era,
    NoThunks (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  NoThunks (UtxoPredicateFailure era)

-- | feesOK is a predicate with several parts. Some parts only apply in special circumstances.
--   1) The fee paid is >= the minimum fee
--   2) If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked.
--   3) The fee inputs do not belong to non-native script addresses
--   4) The fee inputs are sufficient to cover the fee marked in the transaction
--   5) The fee inputs do not contain any non-ADA part
--   As a TransitionRule it will return (), and raise an error (rather than
--   return) if any of the required parts are False.
feesOK ::
  forall era.
  ( Era era,
    ValidateScript era, -- isTwoPhaseScriptAddress
    Core.TxOut era ~ Alonzo.TxOut era, -- balance requires this,
    Era.TxInBlock era ~ Alonzo.ValidatedTx era,
    HasField
      "txinputs_fee" -- to get inputs to pay the fees
      (Core.TxBody era)
      (Set (TxIn (Crypto era))),
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_prices" (Core.PParams era) Prices,
    HasField "address" (Alonzo.TxOut era) (Addr (Crypto era))
  ) =>
  Core.PParams era ->
  TxInBlock era ->
  UTxO era ->
  Rule (AlonzoUTXO era) 'Transition ()
feesOK pp tx (UTxO m) = do
  let txb = getField @"body" tx
      theFee = getField @"txfee" txb -- Coin supplied to pay fees
      fees = getField @"txinputs_fee" txb -- Inputs allocated to pay theFee
      utxoFees = eval (fees ◁ m) -- restrict Utxo to those inputs we use to pay fees.
      bal = balance @era (UTxO utxoFees)
      nonNative txout = isTwoPhaseScriptAddress @era tx (getField @"address" txout)
      minimumFee = minfee @era pp tx
  -- Part 1
  (minimumFee <= theFee) ?! FeeTooSmallUTxO minimumFee theFee
  -- Part 2
  if nullRedeemers . txrdmrs' . wits' $ tx
    then pure ()
    else do
      -- Part 3
      not (any nonNative utxoFees) ?! ScriptsNotPaidUTxO (UTxO (Map.filter nonNative utxoFees))
      -- Part 4
      (Val.coin bal >= theFee) ?! FeeNotBalancedUTxO (Val.coin bal) theFee
      -- Part 5
      Val.inject (Val.coin bal) == bal ?! FeeContainsNonADA bal
      pure ()

-- ================================================================

-- | The UTxO transition rule for the Alonzo eras.
utxoTransition ::
  forall era.
  ( Era era,
    ValidateScript era,
    -- instructions for calling UTXOS from AlonzoUTXO
    Embed (Core.EraRule "UTXOS" era) (AlonzoUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ TxInBlock era,
    -- We leave Core.PParams abstract
    UsesPParams era,
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "_prices" (Core.PParams era) Prices,
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_adaPerUTxOWord" (Core.PParams era) Coin,
    HasField "_maxValSize" (Core.PParams era) Natural,
    -- We fix Core.Tx, Core.Value, Core.TxBody, and Core.TxOut
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.Value era ~ Alonzo.Value (Crypto era),
    Core.TxBody era ~ Alonzo.TxBody era,
    TxInBlock era ~ Alonzo.ValidatedTx era,
    Era.TxSeq era ~ Alonzo.TxSeq era
  ) =>
  TransitionRule (AlonzoUTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp stakepools _genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo _deposits _fees _ppup = u

  let txb = txbody tx

  inInterval slot (getField @"vldt" txb)
    ?! OutsideValidityIntervalUTxO (getField @"vldt" txb) slot

  not (Set.null (Alonzo.txins @era txb)) ?! InputSetEmptyUTxO

  feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's
  eval (Alonzo.txins @era txb ⊆ dom utxo)
    ?! BadInputsUTxO (eval (Alonzo.txins @era txb ➖ dom utxo))

  let consumed_ = consumed @era pp utxo txb
      produced_ = Shelley.produced @era pp stakepools txb
  consumed_ == produced_ ?! ValueNotConservedUTxO consumed_ produced_

  -- Check that the mint field does not try to mint ADA. This is equivalent to
  -- the check `adaPolicy ∉ supp mint tx` in the spec.
  Val.coin (getField @"mint" txb) == Val.zero ?! TriesToForgeADA

  -- use serialized length of Value because this Value size is being limited inside a serialized Tx
  let outputs = Map.elems $ unUTxO (txouts @era txb)
      maxValSize = getField @"_maxValSize" pp
      outputsTooBig =
        filter
          ( \out ->
              let v = getField @"value" out
               in (fromIntegral . BSL.length . serialize) v > maxValSize
          )
          outputs
  null outputsTooBig ?! OutputTooBigUTxO outputsTooBig

  ni <- liftSTS $ asks networkId
  let addrsWrongNetwork =
        filter
          (\a -> getNetwork a /= ni)
          (fmap (getField @"address") $ toList $ getField @"outputs" txb)
  null addrsWrongNetwork ?! WrongNetwork ni (Set.fromList addrsWrongNetwork)
  let wdrlsWrongNetwork =
        filter
          (\a -> getRwdNetwork a /= ni)
          (Map.keys . unWdrl . getField @"wdrls" $ txb)
  null wdrlsWrongNetwork
    ?! WrongNetworkWithdrawal
      ni
      (Set.fromList wdrlsWrongNetwork)
  case txnetworkid' txb of
    SNothing -> pure ()
    SJust bid -> ni == bid ?! WrongNetworkInTxBody ni bid

  -- pointwise is used because non-ada amounts must be >= 0 too
  let (Coin adaPerUTxOWord) = getField @"_adaPerUTxOWord" pp
      outputsTooSmall =
        filter
          ( \out ->
              let v = getField @"value" out
               in not $
                    Val.pointwise
                      (>=)
                      v
                      (Val.inject $ Coin (utxoEntrySize out * adaPerUTxOWord))
          )
          outputs
  null outputsTooSmall ?! OutputTooSmallUTxO outputsTooSmall

  let maxTxSize_ = fromIntegral (getField @"_maxTxSize" pp)
      txSize_ = getField @"txsize" tx
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  let maxTxEx = getField @"_maxTxExUnits" pp
      totExunits = getField @"totExunits" tx
  pointWiseExUnits (<=) totExunits maxTxEx ?! ExUnitsTooBigUTxO maxTxEx totExunits

  -- This does not appear in the Alonzo specification. But the test should be in every Era.
  -- Bootstrap (i.e. Byron) addresses have variable sized attributes in them.
  -- It is important to limit their overall size.
  let outputsAttrsTooBig =
        filter
          ( \out -> case getField @"address" out of
              AddrBootstrap addr -> bootstrapAddressAttrsSize addr > 64
              _ -> False
          )
          outputs
  null outputsAttrsTooBig ?! OutputBootAddrAttrsTooBig outputsAttrsTooBig

  trans @(Core.EraRule "UTXOS" era) =<< coerce <$> judgmentContext

--------------------------------------------------------------------------------
-- AlonzoUTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( ValidateScript era,
    -- Instructions needed to call the UTXOS transition from this one.
    Embed (Core.EraRule "UTXOS" era) (AlonzoUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ ValidatedTx era,
    -- We leave Core.PParams abstract
    UsesPParams era,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_adaPerUTxOWord" (Core.PParams era) Coin,
    HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "_prices" (Core.PParams era) Prices,
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_adaPerUTxOWord" (Core.PParams era) Coin,
    HasField "_maxValSize" (Core.PParams era) Natural,
    -- We fix Core.Value, Core.TxBody, and Core.TxOut
    Core.Value era ~ Alonzo.Value (Crypto era),
    Core.TxBody era ~ Alonzo.TxBody era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Era.TxSeq era ~ Alonzo.TxSeq era,
    Era.TxInBlock era ~ Alonzo.ValidatedTx era
  ) =>
  STS (AlonzoUTXO era)
  where
  type State (AlonzoUTXO era) = Shelley.UTxOState era
  type Signal (AlonzoUTXO era) = ValidatedTx era
  type
    Environment (AlonzoUTXO era) =
      Shelley.UtxoEnv era
  type BaseM (AlonzoUTXO era) = ShelleyBase
  type
    PredicateFailure (AlonzoUTXO era) =
      UtxoPredicateFailure era

  initialRules = []
  transitionRules = [utxoTransition]

instance
  ( Era era,
    STS (UTXOS era),
    PredicateFailure (Core.EraRule "UTXOS" era) ~ UtxosPredicateFailure era
  ) =>
  Embed (UTXOS era) (AlonzoUTXO era)
  where
  wrapFailed = UtxosFailure

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Typeable era,
    Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  ToCBOR (UtxoPredicateFailure era)
  where
  toCBOR x = encode (encFail x)

encFail ::
  forall era.
  ( Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  UtxoPredicateFailure era ->
  Encode 'Open (UtxoPredicateFailure era)
encFail (BadInputsUTxO ins) =
  Sum (BadInputsUTxO @era) 0 !> E encodeFoldable ins
encFail (OutsideValidityIntervalUTxO a b) =
  Sum OutsideValidityIntervalUTxO 1 !> To a !> To b
encFail (MaxTxSizeUTxO a b) =
  Sum MaxTxSizeUTxO 2 !> To a !> To b
encFail (InputSetEmptyUTxO) =
  Sum InputSetEmptyUTxO 3
encFail (FeeTooSmallUTxO a b) =
  Sum FeeTooSmallUTxO 4 !> To a !> To b
encFail (ValueNotConservedUTxO a b) =
  Sum (ValueNotConservedUTxO @era) 5 !> To a !> To b
encFail (OutputTooSmallUTxO outs) =
  Sum (OutputTooSmallUTxO @era) 6 !> E encodeFoldable outs
encFail (UtxosFailure a) =
  Sum (UtxosFailure @era) 7 !> To a
encFail (WrongNetwork right wrongs) =
  Sum (WrongNetwork @era) 8 !> To right !> E encodeFoldable wrongs
encFail (WrongNetworkWithdrawal right wrongs) =
  Sum (WrongNetworkWithdrawal @era) 9 !> To right !> E encodeFoldable wrongs
encFail (OutputBootAddrAttrsTooBig outs) =
  Sum (OutputBootAddrAttrsTooBig @era) 10 !> E encodeFoldable outs
encFail (TriesToForgeADA) =
  Sum TriesToForgeADA 11
encFail (OutputTooBigUTxO outs) =
  Sum (OutputTooBigUTxO @era) 12 !> E encodeFoldable outs
encFail (FeeNotBalancedUTxO a b) =
  Sum FeeNotBalancedUTxO 13 !> To a !> To b
encFail (ScriptsNotPaidUTxO a) =
  Sum ScriptsNotPaidUTxO 14 !> To a
encFail (ExUnitsTooBigUTxO a b) =
  Sum ExUnitsTooBigUTxO 15 !> To a !> To b
encFail (FeeContainsNonADA a) =
  Sum FeeContainsNonADA 16 !> To a
encFail (WrongNetworkInTxBody a b) =
  Sum WrongNetworkInTxBody 17 !> To a !> To b

decFail ::
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  Word ->
  Decode 'Open (UtxoPredicateFailure era)
decFail 0 = SumD (BadInputsUTxO) <! D (decodeSet fromCBOR)
decFail 1 = SumD OutsideValidityIntervalUTxO <! From <! From
decFail 2 = SumD MaxTxSizeUTxO <! From <! From
decFail 3 = SumD InputSetEmptyUTxO
decFail 4 = SumD FeeTooSmallUTxO <! From <! From
decFail 5 = SumD (ValueNotConservedUTxO) <! From <! From
decFail 6 = SumD (OutputTooSmallUTxO) <! D (decodeList fromCBOR)
decFail 7 = SumD (UtxosFailure) <! From
decFail 8 = SumD (WrongNetwork) <! From <! D (decodeSet fromCBOR)
decFail 9 = SumD (WrongNetworkWithdrawal) <! From <! D (decodeSet fromCBOR)
decFail 10 = SumD (OutputBootAddrAttrsTooBig) <! D (decodeList fromCBOR)
decFail 11 = SumD TriesToForgeADA
decFail 12 = SumD (OutputTooBigUTxO) <! D (decodeList fromCBOR)
decFail 13 = SumD FeeNotBalancedUTxO <! From <! From
decFail 14 = SumD ScriptsNotPaidUTxO <! From
decFail 15 = SumD ExUnitsTooBigUTxO <! From <! From
decFail 16 = SumD FeeContainsNonADA <! From
decFail 17 = SumD WrongNetworkInTxBody <! From <! From
decFail n = Invalid n

instance
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  FromCBOR (UtxoPredicateFailure era)
  where
  fromCBOR = decode (Summands "UtxoPredicateFailure" decFail)
