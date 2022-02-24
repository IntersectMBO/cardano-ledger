{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA.Rules.Utxo where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, serialize)
import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.BaseTypes
  ( Network,
    ShelleyBase,
    StrictMaybe (..),
    networkId,
  )
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode
  ( Inject (..),
    InjectMaybe (..),
    Test,
    runTest,
  )
import Cardano.Ledger.Shelley.Constraints
  ( TransValue,
    UsesAuxiliary,
    UsesPParams,
    UsesScript,
    UsesTxBody,
    UsesTxOut,
    UsesValue,
  )
import Cardano.Ledger.Shelley.LedgerState (PPUPState)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.PParams (PParams, PParams' (..), Update)
import Cardano.Ledger.Shelley.Rules.Ppup (PPUP, PPUPEnv (..), PpupPredicateFailure)
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley
import Cardano.Ledger.Shelley.Tx (Tx (..), TxIn, TxOut)
import Cardano.Ledger.Shelley.TxBody (DCert, RewardAcnt, Wdrl)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), totalDeposits, txouts, txup)
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Ledger.ShelleyMA.TxBody (TxBody)
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Coders
  ( decodeList,
    decodeRecordSum,
    decodeSet,
    encodeFoldable,
    invalidKey,
  )
import qualified Data.Compact.SplitMap as SplitMap
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Validation

{- The scaledMinDeposit calculation uses the minUTxOValue protocol parameter
(passed to it as Coin mv) as a specification of "the cost of
making a Shelley-sized UTxO entry", calculated here by "utxoEntrySizeWithoutVal + uint",
using the constants in the "where" clause.
In the case when a UTxO entry contains coins only (and the Shelley
UTxO entry format is used - we will extend this to be correct for other
UTxO formats shortly), the deposit should be exactly the minUTxOValue.
This is the "inject (coin v) == v" case.
Otherwise, we calculate the per-byte deposit by multiplying the minimum deposit (which is
for the number of Shelley UTxO-entry bytes) by the size of a Shelley UTxO entry.
This is the "(mv * (utxoEntrySizeWithoutVal + uint))" calculation.
We then calculate the total deposit required for making a UTxO entry with a Val-class
member v by dividing "(mv * (utxoEntrySizeWithoutVal + uint))" by the
estimated total size of the UTxO entry containing v, ie by
"(utxoEntrySizeWithoutVal + size v)".
See the formal specification for details.
-}

-- This scaling function is right for UTxO, not EUTxO
--
scaledMinDeposit :: (Val.Val v) => v -> Coin -> Coin
scaledMinDeposit v (Coin mv)
  | Val.inject (Val.coin v) == v = Coin mv -- without non-Coin assets, scaled deposit should be exactly minUTxOValue
  -- The calculation should represent this equation
  -- minValueParameter / coinUTxOSize = actualMinValue / valueUTxOSize
  -- actualMinValue = (minValueParameter / coinUTxOSize) * valueUTxOSize
  | otherwise = Coin $ max mv (coinsPerUTxOWord * (utxoEntrySizeWithoutVal + Val.size v))
  where
    -- lengths obtained from tracing on HeapWords of inputs and outputs
    -- obtained experimentally, and number used here
    -- units are Word64s
    txoutLenNoVal = 14
    txinLen = 7

    -- unpacked CompactCoin Word64 size in Word64s
    coinSize :: Integer
    coinSize = 0

    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = 6 + txoutLenNoVal + txinLen

    -- how much ada does a Word64 of UTxO space cost, calculated from minAdaValue PP
    -- round down
    coinsPerUTxOWord :: Integer
    coinsPerUTxOWord = quot mv (utxoEntrySizeWithoutVal + coinSize)

-- ==========================================================

data UtxoPredicateFailure era
  = BadInputsUTxO
      !(Set (TxIn (Crypto era))) -- The bad transaction inputs
  | OutsideValidityIntervalUTxO
      !ValidityInterval -- transaction's validity interval
      !SlotNo -- current slot
  | MaxTxSizeUTxO
      !Integer -- the actual transaction size
      !Integer -- the max transaction size
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      !Coin -- the minimum fee for this transaction
      !Coin -- the fee supplied in this transaction
  | ValueNotConservedUTxO
      !(Core.Value era) -- the Coin consumed by this transaction
      !(Core.Value era) -- the Coin produced by this transaction
  | WrongNetwork
      !Network -- the expected network id
      !(Set (Addr (Crypto era))) -- the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      !Network -- the expected network id
      !(Set (RewardAcnt (Crypto era))) -- the set of reward addresses with incorrect network IDs
  | OutputTooSmallUTxO
      ![Core.TxOut era] -- list of supplied transaction outputs that are too small
  | UpdateFailure !(PredicateFailure (Core.EraRule "PPUP" era)) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      ![Core.TxOut era] -- list of supplied bad transaction outputs
  | TriesToForgeADA
  | OutputTooBigUTxO
      ![Core.TxOut era] -- list of supplied bad transaction outputs
  deriving (Generic)

deriving stock instance
  ( Shelley.TransUTxOState Show era,
    TransValue Show era,
    Show (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Show (UtxoPredicateFailure era)

deriving stock instance
  ( Shelley.TransUTxOState Eq era,
    TransValue Eq era,
    Eq (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Eq (UtxoPredicateFailure era)

instance
  ( Shelley.TransUTxOState NoThunks era,
    NoThunks (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  NoThunks (UtxoPredicateFailure era)

newtype UtxoEvent era
  = UpdateEvent (Event (Core.EraRule "PPUP" era))

-- | Calculate the value consumed by the transation.
--
--   This differs from the corresponding Shelley function @Shelley.consumed@
--   since it also considers the "mint" field which creates or destroys non-Ada
--   tokens.
--
--   Note that this is slightly confusing, since it also covers non-Ada assets
--   _created_ by the transaction, depending on the sign of the quantities in
--   the mint field.
consumed ::
  forall era.
  ( Era era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "mint" (Core.TxBody era) (Core.Value era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "_keyDeposit" (Core.PParams era) Coin
  ) =>
  Core.PParams era ->
  UTxO era ->
  Core.TxBody era ->
  Core.Value era
consumed pp u tx = Shelley.consumed pp u tx <> getField @"mint" tx

-- | The UTxO transition rule for the Shelley-MA (Mary and Allegra) eras.
utxoTransition ::
  forall era.
  ( UsesTxBody era,
    UsesValue era,
    STS (UTXO era),
    Core.Tx era ~ Tx era,
    Embed (Core.EraRule "PPUP" era) (UTXO era),
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "mint" (Core.TxBody era) (Core.Value era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_minUTxOValue" (Core.PParams era) Coin,
    HasField "_maxTxSize" (Core.PParams era) Natural
  ) =>
  TransitionRule (UTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp stakepools genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo _ _ ppup _ = u
  let txb = getField @"body" tx

  {- ininterval slot (txvld tx) -}
  runTest $ validateOutsideValidityIntervalUTxO slot txb

  {- txins txb ≠ ∅ -}
  -- runValidationTransMaybe fromShelleyFailure $ Shelley.validateInputSetEmptyUTxO txb
  runTest $ Shelley.validateInputSetEmptyUTxO txb

  {- minfee pp tx ≤ txfee txb -}
  runTest $ Shelley.validateFeeTooSmallUTxO pp tx

  {- txins txb ⊆ dom utxo -}
  runTest $ Shelley.validateBadInputsUTxO utxo $ getField @"inputs" txb

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runTest $ Shelley.validateWrongNetwork netId txb

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTest $ Shelley.validateWrongNetworkWithdrawal netId txb

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $ validateValueNotConservedUTxO pp utxo stakepools txb

  -- process Protocol Parameter Update Proposals
  ppup' <-
    trans @(Core.EraRule "PPUP" era) $ TRC (PPUPEnv slot pp genDelegs, ppup, txup tx)

  runTest $ validateTriesToForgeADA txb

  let outputs = txouts txb
  {- ∀ txout ∈ txouts txb, getValue txout ≥ inject (scaledMinDeposit v (minUTxOValue pp)) -}
  runTest $ validateOutputTooSmallUTxO pp outputs

  {- ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ MaxValSize -}
  -- MaxValSize = 4000
  runTest $ validateOutputTooBigUTxO outputs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTest $ Shelley.validateOutputBootAddrAttrsTooBig outputs

  {- txsize tx ≤ maxTxSize pp -}
  runTest $ Shelley.validateMaxTxSizeUTxO pp tx

  let refunded = Shelley.keyRefunds pp txb
  let txCerts = toList $ getField @"certs" txb
  let depositChange = totalDeposits pp (`Map.notMember` stakepools) txCerts Val.<-> refunded
  pure $! Shelley.updateUTxOState u txb depositChange ppup'

-- | Ensure the transaction is within the validity window.
--
-- > ininterval slot (txvld tx)
validateOutsideValidityIntervalUTxO ::
  HasField "vldt" (Core.TxBody era) ValidityInterval =>
  SlotNo ->
  Core.TxBody era ->
  Test (UtxoPredicateFailure era)
validateOutsideValidityIntervalUTxO slot txb =
  failureUnless (inInterval slot (txvldt txb)) $
    OutsideValidityIntervalUTxO (txvldt txb) slot
  where
    txvldt = getField @"vldt"

-- | Check that the mint field does not try to mint ADA. This is equivalent to
-- the check:
--
-- > adaPolicy ∉ supp mint tx
validateTriesToForgeADA ::
  (Val.Val (Core.Value era), HasField "mint" (Core.TxBody era) (Core.Value era)) =>
  Core.TxBody era ->
  Test (UtxoPredicateFailure era)
validateTriesToForgeADA txb =
  failureUnless (Val.coin (getField @"mint" txb) == Val.zero) TriesToForgeADA

-- | Ensure that there are no `Core.TxOut`s that have `Value` of size larger than @MaxValSize@
--
-- > ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ MaxValSize
validateOutputTooBigUTxO ::
  ( HasField "value" (Core.TxOut era) (Core.Value era),
    ToCBOR (Core.Value era)
  ) =>
  UTxO era ->
  Test (UtxoPredicateFailure era)
validateOutputTooBigUTxO (UTxO outputs) =
  failureUnless (null outputsTooBig) $ OutputTooBigUTxO outputsTooBig
  where
    maxValSize = 4000 :: Int64
    outputsTooBig =
      filter
        ( \out ->
            let v = getField @"value" out
             in BSL.length (serialize v) > maxValSize
        )
        (SplitMap.elems outputs)

-- | Ensure that there are no `Core.TxOut`s that have value less than the scaled @minUTxOValue@
--
-- > ∀ txout ∈ txouts txb, getValue txout ≥ inject (scaledMinDeposit v (minUTxOValue pp))
validateOutputTooSmallUTxO ::
  ( HasField "_minUTxOValue" (Core.PParams era) Coin,
    HasField "value" (Core.TxOut era) (Core.Value era),
    Val.Val (Core.Value era)
  ) =>
  Core.PParams era ->
  UTxO era ->
  Test (UtxoPredicateFailure era)
validateOutputTooSmallUTxO pp (UTxO outputs) =
  failureUnless (null outputsTooSmall) $ OutputTooSmallUTxO outputsTooSmall
  where
    minUTxOValue = getField @"_minUTxOValue" pp
    outputsTooSmall =
      filter
        ( \out ->
            let v = getField @"value" out
             in Val.pointwise (<) v (Val.inject $ scaledMinDeposit v minUTxOValue)
        )
        (SplitMap.elems outputs)

-- | Ensure that value consumed and produced matches up exactly. Note that this
-- is different from Shelley, since implementation of `consumed` has changed.
--
-- > consumed pp utxo txb = produced pp poolParams txb
validateValueNotConservedUTxO ::
  ( Era era,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "mint" (Core.TxBody era) (Core.Value era)
  ) =>
  Core.PParams era ->
  UTxO era ->
  Map.Map (KeyHash 'StakePool (Crypto era)) a ->
  Core.TxBody era ->
  Test (UtxoPredicateFailure era)
validateValueNotConservedUTxO pp utxo stakepools txb =
  failureUnless (consumedValue == producedValue) $
    ValueNotConservedUTxO consumedValue producedValue
  where
    consumedValue = consumed pp utxo txb
    producedValue = Shelley.produced pp (`Map.notMember` stakepools) txb

--------------------------------------------------------------------------------
-- UTXO STS
--------------------------------------------------------------------------------
data UTXO era

instance
  forall era.
  ( Era era,
    UsesAuxiliary era,
    UsesScript era,
    UsesTxOut era,
    UsesValue era,
    UsesPParams era,
    TransValue ToCBOR era,
    Core.PParams era ~ PParams era,
    Core.TxBody era ~ TxBody era,
    Core.TxOut era ~ TxOut era,
    Core.Tx era ~ Tx era,
    Embed (Core.EraRule "PPUP" era) (UTXO era),
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era)
  ) =>
  STS (UTXO era)
  where
  type State (UTXO era) = Shelley.UTxOState era
  type Signal (UTXO era) = Tx era
  type Environment (UTXO era) = Shelley.UtxoEnv era
  type BaseM (UTXO era) = ShelleyBase
  type PredicateFailure (UTXO era) = UtxoPredicateFailure era
  type Event (UTXO era) = UtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]

instance
  ( Era era,
    STS (PPUP era),
    PredicateFailure (Core.EraRule "PPUP" era) ~ PpupPredicateFailure era,
    Event (Core.EraRule "PPUP" era) ~ Event (PPUP era)
  ) =>
  Embed (PPUP era) (UTXO era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = UpdateEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------
instance
  ( TransValue ToCBOR era,
    Shelley.TransUTxOState ToCBOR era,
    ToCBOR (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  ToCBOR (UtxoPredicateFailure era)
  where
  toCBOR = \case
    BadInputsUTxO ins ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> encodeFoldable ins
    (OutsideValidityIntervalUTxO a b) ->
      encodeListLen 3 <> toCBOR (1 :: Word8)
        <> toCBOR a
        <> toCBOR b
    (MaxTxSizeUTxO a b) ->
      encodeListLen 3 <> toCBOR (2 :: Word8)
        <> toCBOR a
        <> toCBOR b
    InputSetEmptyUTxO -> encodeListLen 1 <> toCBOR (3 :: Word8)
    (FeeTooSmallUTxO a b) ->
      encodeListLen 3 <> toCBOR (4 :: Word8)
        <> toCBOR a
        <> toCBOR b
    (ValueNotConservedUTxO a b) ->
      encodeListLen 3 <> toCBOR (5 :: Word8)
        <> toCBOR a
        <> toCBOR b
    OutputTooSmallUTxO outs ->
      encodeListLen 2 <> toCBOR (6 :: Word8)
        <> encodeFoldable outs
    (UpdateFailure a) ->
      encodeListLen 2 <> toCBOR (7 :: Word8)
        <> toCBOR a
    (WrongNetwork right wrongs) ->
      encodeListLen 3 <> toCBOR (8 :: Word8)
        <> toCBOR right
        <> encodeFoldable wrongs
    (WrongNetworkWithdrawal right wrongs) ->
      encodeListLen 3 <> toCBOR (9 :: Word8)
        <> toCBOR right
        <> encodeFoldable wrongs
    OutputBootAddrAttrsTooBig outs ->
      encodeListLen 2 <> toCBOR (10 :: Word8)
        <> encodeFoldable outs
    TriesToForgeADA -> encodeListLen 1 <> toCBOR (11 :: Word8)
    OutputTooBigUTxO outs ->
      encodeListLen 2 <> toCBOR (12 :: Word8)
        <> encodeFoldable outs

instance
  ( TransValue FromCBOR era,
    Shelley.TransUTxO FromCBOR era,
    Val.DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  FromCBOR (UtxoPredicateFailure era)
  where
  fromCBOR =
    decodeRecordSum "PredicateFailureUTXO" $
      \case
        0 -> do
          ins <- decodeSet fromCBOR
          pure (2, BadInputsUTxO ins) -- The (2,..) indicates the number of things decoded, INCLUDING the tags, which are decoded by decodeRecordSumNamed
        1 -> do
          a <- fromCBOR
          b <- fromCBOR
          pure (3, OutsideValidityIntervalUTxO a b)
        2 -> do
          a <- fromCBOR
          b <- fromCBOR
          pure (3, MaxTxSizeUTxO a b)
        3 -> pure (1, InputSetEmptyUTxO)
        4 -> do
          a <- fromCBOR
          b <- fromCBOR
          pure (3, FeeTooSmallUTxO a b)
        5 -> do
          a <- fromCBOR
          b <- fromCBOR
          pure (3, ValueNotConservedUTxO a b)
        6 -> do
          outs <- decodeList fromCBOR
          pure (2, OutputTooSmallUTxO outs)
        7 -> do
          a <- fromCBOR
          pure (2, UpdateFailure a)
        8 -> do
          right <- fromCBOR
          wrongs <- decodeSet fromCBOR
          pure (3, WrongNetwork right wrongs)
        9 -> do
          right <- fromCBOR
          wrongs <- decodeSet fromCBOR
          pure (3, WrongNetworkWithdrawal right wrongs)
        10 -> do
          outs <- decodeList fromCBOR
          pure (2, OutputBootAddrAttrsTooBig outs)
        11 -> pure (1, TriesToForgeADA)
        12 -> do
          outs <- decodeList fromCBOR
          pure (2, OutputTooBigUTxO outs)
        k -> invalidKey k

-- ===============================================
-- Inject instances

fromShelleyFailure :: Shelley.UtxoPredicateFailure era -> Maybe (UtxoPredicateFailure era)
fromShelleyFailure = \case
  Shelley.BadInputsUTxO ins -> Just $ BadInputsUTxO ins
  Shelley.ExpiredUTxO {} -> Nothing -- Rule was replaced with `OutsideValidityIntervalUTxO`
  Shelley.MaxTxSizeUTxO a m -> Just $ MaxTxSizeUTxO a m
  Shelley.InputSetEmptyUTxO -> Just InputSetEmptyUTxO
  Shelley.FeeTooSmallUTxO mf af -> Just $ FeeTooSmallUTxO mf af
  Shelley.ValueNotConservedUTxO {} -> Nothing -- Rule was updated
  Shelley.WrongNetwork n as -> Just $ WrongNetwork n as
  Shelley.WrongNetworkWithdrawal n as -> Just $ WrongNetworkWithdrawal n as
  Shelley.OutputTooSmallUTxO {} -> Nothing -- Rule was updated
  Shelley.UpdateFailure ppf -> Just $ UpdateFailure ppf
  Shelley.OutputBootAddrAttrsTooBig outs -> Just $ OutputBootAddrAttrsTooBig outs

instance InjectMaybe (Shelley.UtxoPredicateFailure era) (UtxoPredicateFailure era) where
  injectMaybe = fromShelleyFailure

instance Inject (UtxoPredicateFailure era) (UtxoPredicateFailure era) where
  inject = id

instance Inject (Shelley.UtxoPredicateFailure era) (UtxoPredicateFailure era) where
  inject (Shelley.BadInputsUTxO ins) = BadInputsUTxO ins
  inject (Shelley.ExpiredUTxO ttl current) = OutsideValidityIntervalUTxO (ValidityInterval SNothing (SJust ttl)) current
  inject (Shelley.MaxTxSizeUTxO a m) = MaxTxSizeUTxO a m
  inject (Shelley.InputSetEmptyUTxO) = InputSetEmptyUTxO
  inject (Shelley.FeeTooSmallUTxO mf af) = FeeTooSmallUTxO mf af
  inject (Shelley.ValueNotConservedUTxO vc vp) = ValueNotConservedUTxO vc vp
  inject (Shelley.WrongNetwork n as) = WrongNetwork n as
  inject (Shelley.WrongNetworkWithdrawal n as) = WrongNetworkWithdrawal n as
  inject (Shelley.OutputTooSmallUTxO x) = OutputTooSmallUTxO x
  inject (Shelley.UpdateFailure x) = UpdateFailure x
  inject (Shelley.OutputBootAddrAttrsTooBig outs) = OutputTooBigUTxO outs
