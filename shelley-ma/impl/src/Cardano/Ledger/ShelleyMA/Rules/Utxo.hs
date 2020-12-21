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
-- The STS instance for UTXO is technically an orphan.
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.Rules.Utxo where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Ledger.Constraints
  ( TransValue,
    UsesAuxiliary,
    UsesScript,
    UsesTxBody,
    UsesValue,
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.ShelleyMA (MAValue, MaryOrAllegra, ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Ledger.ShelleyMA.TxBody (TxBody)
import Cardano.Ledger.Torsor (Torsor (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo)
import Control.Iterate.SetAlgebra (dom, eval, (∪), (⊆), (⋪), (◁))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.Coders
  ( decodeList,
    decodeRecordSum,
    decodeSet,
    encodeFoldable,
    invalidKey,
  )
import Data.Foldable (fold, toList)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Address
  ( Addr (AddrBootstrap),
    bootstrapAddressAttrsSize,
    getNetwork,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Network,
    ShelleyBase,
    StrictMaybe (..),
    networkId,
  )
import Shelley.Spec.Ledger.Coin
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), Update)
import Shelley.Spec.Ledger.STS.Ppup (PPUP, PPUPEnv (..))
import Shelley.Spec.Ledger.STS.Utxo (UTXO)
import qualified Shelley.Spec.Ledger.STS.Utxo as Shelley
import Shelley.Spec.Ledger.Tx (Tx (..), TxIn)
import Shelley.Spec.Ledger.TxBody
  ( DCert,
    RewardAcnt (getRwdNetwork),
    TxOut (TxOut),
    Wdrl,
    unWdrl,
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO,
    balance,
    totalDeposits,
    txins,
    txouts,
    txup,
    unUTxO,
  )

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
      !(Delta (Core.Value era)) -- the Coin consumed by this transaction
      !(Delta (Core.Value era)) -- the Coin produced by this transaction
  | WrongNetwork
      !Network -- the expected network id
      !(Set (Addr (Crypto era))) -- the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      !Network -- the expected network id
      !(Set (RewardAcnt (Crypto era))) -- the set of reward addresses with incorrect network IDs
  | OutputTooSmallUTxO
      ![TxOut era] -- list of supplied transaction outputs that are too small
  | UpdateFailure (PredicateFailure (PPUP era)) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      ![TxOut era] -- list of supplied bad transaction outputs
  | TriesToForgeADA
  deriving (Generic)

deriving stock instance
  TransValue Show era =>
  Show (UtxoPredicateFailure era)

deriving stock instance
  TransValue Eq era =>
  Eq (UtxoPredicateFailure era)

instance NoThunks (Delta (Core.Value era)) => NoThunks (UtxoPredicateFailure era)

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
  ( UsesValue era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "mint" (Core.TxBody era) (Core.Value era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  PParams era ->
  UTxO era ->
  Core.TxBody era ->
  Core.Value era
consumed pp u tx =
  balance (eval (txins @era tx ◁ u))
    <> getField @"mint" tx
    <> (Val.inject $ refunds <> withdrawals)
  where
    -- balance (UTxO (Map.restrictKeys v (txins tx))) + refunds + withdrawals
    refunds = Shelley.keyRefunds pp tx
    withdrawals = fold . unWdrl $ getField @"wdrls" tx

-- | The UTxO transition rule for the Shelley-MA (Mary and Allegra) eras.
utxoTransition ::
  forall era.
  ( UsesTxBody era,
    UsesAuxiliary era,
    UsesValue era,
    UsesScript era,
    STS (UTXO era),
    Embed (PPUP era) (UTXO era),
    BaseM (UTXO era) ~ ShelleyBase,
    Environment (UTXO era) ~ Shelley.UtxoEnv era,
    State (UTXO era) ~ Shelley.UTxOState era,
    Signal (UTXO era) ~ Tx era,
    PredicateFailure (UTXO era) ~ UtxoPredicateFailure era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "mint" (Core.TxBody era) (Core.Value era),
    HasField "outputs" (Core.TxBody era) (StrictSeq (TxOut era)),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "txfee" (Core.TxBody era) Coin,
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era))
  ) =>
  TransitionRule (UTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp stakepools genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo deposits' fees ppup = u
  let txb = _body tx

  inInterval slot (getField @"vldt" txb)
    ?! OutsideValidityIntervalUTxO (getField @"vldt" txb) slot

  txins @era txb /= Set.empty ?! InputSetEmptyUTxO

  let minFee = Shelley.minfee pp tx
      txFee = getField @"txfee" txb
  minFee <= txFee ?! FeeTooSmallUTxO minFee txFee

  eval (txins @era txb ⊆ dom utxo)
    ?! BadInputsUTxO (txins @era txb `Set.difference` eval (dom utxo))

  ni <- liftSTS $ asks networkId
  let addrsWrongNetwork =
        filter
          (\a -> getNetwork a /= ni)
          (fmap (\(TxOut a _) -> a) $ toList $ getField @"outputs" txb)
  null addrsWrongNetwork ?! WrongNetwork ni (Set.fromList addrsWrongNetwork)
  let wdrlsWrongNetwork =
        filter
          (\a -> getRwdNetwork a /= ni)
          (Map.keys . unWdrl . getField @"wdrls" $ txb)
  null wdrlsWrongNetwork
    ?! WrongNetworkWithdrawal
      ni
      (Set.fromList wdrlsWrongNetwork)

  let consumed_ = consumed pp utxo txb
      produced_ = Shelley.produced pp stakepools txb
  consumed_ == produced_ ?! ValueNotConservedUTxO (toDelta consumed_) (toDelta produced_)

  -- process Protocol Parameter Update Proposals
  ppup' <- trans @(PPUP era) $ TRC (PPUPEnv slot pp genDelegs, ppup, txup tx)

  -- Check that the mint field does not try to mint ADA. This is equivalent to
  -- the check `adaPolicy ∉ supp mint tx` in the spec.
  Val.coin (getField @"mint" txb) == Val.zero ?! TriesToForgeADA

  let outputs = Map.elems $ unUTxO (txouts txb)
      minUTxOValue = _minUTxOValue pp
      outputsTooSmall =
        [ out
          | out@(TxOut _ c) <- outputs,
            not $
              Val.pointwise
                (>=)
                c
                (Val.inject $ Val.scaledMinDeposit c minUTxOValue)
        ]
  null outputsTooSmall ?! OutputTooSmallUTxO outputsTooSmall

  -- Bootstrap (i.e. Byron) addresses have variable sized attributes in them.
  -- It is important to limit their overall size.
  let outputsAttrsTooBig =
        [ out
          | out@(TxOut (AddrBootstrap addr) _) <- outputs,
            bootstrapAddressAttrsSize addr > 64
        ]
  null outputsAttrsTooBig ?! OutputBootAddrAttrsTooBig outputsAttrsTooBig

  let maxTxSize_ = fromIntegral (_maxTxSize pp)
      txSize_ = Shelley.txsize tx
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  let refunded = Shelley.keyRefunds pp txb
  let txCerts = toList $ getField @"certs" txb
  let depositChange = totalDeposits pp stakepools txCerts Val.<-> refunded

  pure
    Shelley.UTxOState
      { Shelley._utxo = eval ((txins @era txb ⋪ utxo) ∪ txouts txb),
        Shelley._deposited = deposits' <> depositChange,
        Shelley._fees = fees <> (getField @"txfee" txb),
        Shelley._ppups = ppup'
      }

--------------------------------------------------------------------------------
-- UTXO STS
--------------------------------------------------------------------------------

instance
  forall c (ma :: MaryOrAllegra).
  ( CryptoClass.Crypto c,
    Typeable ma,
    UsesValue (ShelleyMAEra ma c),
    Show (Delta (MAValue ma c)),
    Val.DecodeMint (MAValue ma c),
    Core.TxBody (ShelleyMAEra ma c) ~ TxBody (ShelleyMAEra ma c)
  ) =>
  STS (UTXO (ShelleyMAEra ma c))
  where
  type State (UTXO (ShelleyMAEra ma c)) = Shelley.UTxOState (ShelleyMAEra ma c)
  type Signal (UTXO (ShelleyMAEra ma c)) = Tx (ShelleyMAEra ma c)
  type
    Environment (UTXO (ShelleyMAEra ma c)) =
      Shelley.UtxoEnv (ShelleyMAEra ma c)
  type BaseM (UTXO (ShelleyMAEra ma c)) = ShelleyBase
  type
    PredicateFailure (UTXO (ShelleyMAEra ma c)) =
      UtxoPredicateFailure (ShelleyMAEra ma c)

  initialRules = []
  transitionRules = [utxoTransition]

instance
  (CryptoClass.Crypto c, Typeable ma) =>
  Embed (PPUP (ShelleyMAEra (ma :: MaryOrAllegra) c)) (UTXO (ShelleyMAEra ma c))
  where
  wrapFailed = UpdateFailure

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------
instance
  TransValue ToCBOR era =>
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

instance
  (UsesValue era, FromCBOR (Delta (Core.Value era))) =>
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
        k -> invalidKey k
