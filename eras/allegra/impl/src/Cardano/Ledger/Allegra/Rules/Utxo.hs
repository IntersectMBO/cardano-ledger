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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Utxo (
  AllegraUTXO,
  AllegraUtxoPredFailure (..),
  validateOutsideValidityIntervalUTxO,
)
where

import Cardano.Ledger.Address (Addr, RewardAcnt)
import Cardano.Ledger.Allegra.Era (AllegraUTXO)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (ValidityInterval), inInterval)
import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..))
import Cardano.Ledger.BaseTypes (
  Network,
  ProtVer (pvMajor),
  ShelleyBase,
  StrictMaybe (..),
  networkId,
 )
import Cardano.Ledger.Binary (
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordSum,
  encodeListLen,
  invalidKey,
  serialize,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Rules.ValidationMode (
  Inject (..),
  InjectMaybe (..),
  Test,
  runTest,
 )
import Cardano.Ledger.Shelley.LedgerState (PPUPState, keyTxRefunds, totalTxDeposits)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules (PpupEnv (..), ShelleyPPUP, ShelleyPpupPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..), TxIn)
import Cardano.Ledger.Shelley.UTxO (txup)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), txouts)
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)
import Validation

-- ==========================================================

data AllegraUtxoPredFailure era
  = BadInputsUTxO
      !(Set (TxIn (EraCrypto era))) -- The bad transaction inputs
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
      !(Value era) -- the Coin consumed by this transaction
      !(Value era) -- the Coin produced by this transaction
  | WrongNetwork
      !Network -- the expected network id
      !(Set (Addr (EraCrypto era))) -- the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      !Network -- the expected network id
      !(Set (RewardAcnt (EraCrypto era))) -- the set of reward addresses with incorrect network IDs
  | OutputTooSmallUTxO
      ![TxOut era] -- list of supplied transaction outputs that are too small
  | UpdateFailure !(PredicateFailure (EraRule "PPUP" era)) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      ![TxOut era] -- list of supplied bad transaction outputs
  | -- Kept for backwards compatibility: no longer used because the `MultiAsset` type of mint doesn't allow for this possibility
    TriesToForgeADA
  | OutputTooBigUTxO
      ![TxOut era] -- list of supplied bad transaction outputs
  deriving (Generic)

deriving stock instance
  ( Show (TxOut era)
  , Show (Value era)
  , Show (Shelley.UTxOState era)
  , Show (PredicateFailure (EraRule "PPUP" era))
  ) =>
  Show (AllegraUtxoPredFailure era)

deriving stock instance
  ( Eq (TxOut era)
  , Eq (Value era)
  , Eq (Shelley.UTxOState era)
  , Eq (PredicateFailure (EraRule "PPUP" era))
  ) =>
  Eq (AllegraUtxoPredFailure era)

instance
  ( NoThunks (TxOut era)
  , NoThunks (Value era)
  , NoThunks (Shelley.UTxOState era)
  , NoThunks (PredicateFailure (EraRule "PPUP" era))
  ) =>
  NoThunks (AllegraUtxoPredFailure era)

newtype AllegraUtxoEvent era
  = UpdateEvent (Event (EraRule "PPUP" era))

-- | The UTxO transition rule for the Allegra era.
utxoTransition ::
  forall era.
  ( EraTx era
  , EraUTxO era
  , AllegraEraTxBody era
  , STS (AllegraUTXO era)
  , Tx era ~ ShelleyTx era
  , Embed (EraRule "PPUP" era) (AllegraUTXO era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , State (EraRule "PPUP" era) ~ PPUPState era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , ProtVerAtMost era 8
  ) =>
  TransitionRule (AllegraUTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp dpstate genDelegs, u, tx) <- judgmentContext
  let Shelley.UTxOState utxo _ _ ppup _ = u
  let txb = tx ^. bodyTxL

  {- ininterval slot (txvld tx) -}
  runTest $ validateOutsideValidityIntervalUTxO slot txb

  {- txins txb ≠ ∅ -}
  -- runValidationTransMaybe fromShelleyFailure $ Shelley.validateInputSetEmptyUTxO txb
  runTest $ Shelley.validateInputSetEmptyUTxO txb

  {- minfee pp tx ≤ txfee txb -}
  runTest $ Shelley.validateFeeTooSmallUTxO pp tx

  {- txins txb ⊆ dom utxo -}
  runTest $ Shelley.validateBadInputsUTxO utxo $ txb ^. inputsTxBodyL

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runTest $ Shelley.validateWrongNetwork netId . toList $ txb ^. outputsTxBodyL

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTest $ Shelley.validateWrongNetworkWithdrawal netId txb

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $ Shelley.validateValueNotConservedUTxO pp utxo dpstate txb

  -- process Protocol Parameter Update Proposals
  ppup' <-
    trans @(EraRule "PPUP" era) $ TRC (PPUPEnv slot pp genDelegs, ppup, txup tx)

  {- adaPolicy ∉ supp mint tx
     above check not needed because mint field of type MultiAsset cannot contain ada -}

  let outputs = txouts txb
  {- ∀ txout ∈ txouts txb, getValue txout ≥ inject (scaledMinDeposit v (minUTxOValue pp)) -}
  runTest $ validateOutputTooSmallUTxO pp outputs

  {- ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ MaxValSize -}
  -- MaxValSize = 4000
  runTest $ validateOutputTooBigUTxO pp outputs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTest $ Shelley.validateOutputBootAddrAttrsTooBig (Map.elems (unUTxO outputs))

  {- txsize tx ≤ maxTxSize pp -}
  runTest $ Shelley.validateMaxTxSizeUTxO pp tx

  let refunded = keyTxRefunds pp dpstate txb
  let depositChange = totalTxDeposits pp dpstate txb Val.<-> refunded

  pure $! Shelley.updateUTxOState u txb depositChange ppup'

-- | Ensure the transaction is within the validity window.
--
-- > ininterval slot (txvld tx)
validateOutsideValidityIntervalUTxO ::
  AllegraEraTxBody era =>
  SlotNo ->
  TxBody era ->
  Test (AllegraUtxoPredFailure era)
validateOutsideValidityIntervalUTxO slot txb =
  failureUnless (inInterval slot (txb ^. vldtTxBodyL)) $
    OutsideValidityIntervalUTxO (txb ^. vldtTxBodyL) slot

-- | Ensure that there are no `TxOut`s that have `Value` of size larger than @MaxValSize@
--
-- > ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ MaxValSize
validateOutputTooBigUTxO ::
  EraTxOut era =>
  PParams era ->
  UTxO era ->
  Test (AllegraUtxoPredFailure era)
validateOutputTooBigUTxO pp (UTxO outputs) =
  failureUnless (null outputsTooBig) $ OutputTooBigUTxO outputsTooBig
  where
    version = pvMajor (pp ^. ppProtocolVersionL)
    maxValSize = 4000 :: Int64
    outputsTooBig =
      filter
        ( \out ->
            let v = out ^. valueTxOutL
             in BSL.length (serialize version v) > maxValSize
        )
        (Map.elems outputs)

-- | Ensure that there are no `TxOut`s that have value less than the scaled @minUTxOValue@
--
-- > ∀ txout ∈ txouts txb, getValue txout ≥ inject (scaledMinDeposit v (minUTxOValue pp))
validateOutputTooSmallUTxO ::
  EraTxOut era =>
  PParams era ->
  UTxO era ->
  Test (AllegraUtxoPredFailure era)
validateOutputTooSmallUTxO pp (UTxO outputs) =
  failureUnless (null outputsTooSmall) $ OutputTooSmallUTxO outputsTooSmall
  where
    outputsTooSmall =
      filter
        ( \txOut ->
            let v = txOut ^. valueTxOutL
             in Val.pointwise (<) v (Val.inject $ getMinCoinTxOut pp txOut)
        )
        (Map.elems outputs)

--------------------------------------------------------------------------------
-- UTXO STS
--------------------------------------------------------------------------------
instance
  forall era.
  ( EraTx era
  , EraUTxO era
  , AllegraEraTxBody era
  , Eq (PParamsUpdate era)
  , Show (PParamsUpdate era)
  , Tx era ~ ShelleyTx era
  , Embed (EraRule "PPUP" era) (AllegraUTXO era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , State (EraRule "PPUP" era) ~ PPUPState era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , ProtVerAtMost era 8
  ) =>
  STS (AllegraUTXO era)
  where
  type State (AllegraUTXO era) = Shelley.UTxOState era
  type Signal (AllegraUTXO era) = ShelleyTx era
  type Environment (AllegraUTXO era) = Shelley.UtxoEnv era
  type BaseM (AllegraUTXO era) = ShelleyBase
  type PredicateFailure (AllegraUTXO era) = AllegraUtxoPredFailure era
  type Event (AllegraUTXO era) = AllegraUtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]

instance
  ( Era era
  , STS (ShelleyPPUP era)
  , PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era
  , Event (EraRule "PPUP" era) ~ Event (ShelleyPPUP era)
  ) =>
  Embed (ShelleyPPUP era) (AllegraUTXO era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = UpdateEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------
instance
  ( Typeable era
  , Crypto (EraCrypto era)
  , ToCBOR (Value era)
  , ToCBOR (TxOut era)
  , ToCBOR (Shelley.UTxOState era)
  , ToCBOR (PredicateFailure (EraRule "PPUP" era))
  ) =>
  ToCBOR (AllegraUtxoPredFailure era)
  where
  toCBOR = \case
    BadInputsUTxO ins ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR ins
    (OutsideValidityIntervalUTxO a b) ->
      encodeListLen 3
        <> toCBOR (1 :: Word8)
        <> toCBOR a
        <> toCBOR b
    (MaxTxSizeUTxO a b) ->
      encodeListLen 3
        <> toCBOR (2 :: Word8)
        <> toCBOR a
        <> toCBOR b
    InputSetEmptyUTxO -> encodeListLen 1 <> toCBOR (3 :: Word8)
    (FeeTooSmallUTxO a b) ->
      encodeListLen 3
        <> toCBOR (4 :: Word8)
        <> toCBOR a
        <> toCBOR b
    (ValueNotConservedUTxO a b) ->
      encodeListLen 3
        <> toCBOR (5 :: Word8)
        <> toCBOR a
        <> toCBOR b
    OutputTooSmallUTxO outs ->
      encodeListLen 2
        <> toCBOR (6 :: Word8)
        <> toCBOR outs
    (UpdateFailure a) ->
      encodeListLen 2
        <> toCBOR (7 :: Word8)
        <> toCBOR a
    (WrongNetwork right wrongs) ->
      encodeListLen 3
        <> toCBOR (8 :: Word8)
        <> toCBOR right
        <> toCBOR wrongs
    (WrongNetworkWithdrawal right wrongs) ->
      encodeListLen 3
        <> toCBOR (9 :: Word8)
        <> toCBOR right
        <> toCBOR wrongs
    OutputBootAddrAttrsTooBig outs ->
      encodeListLen 2
        <> toCBOR (10 :: Word8)
        <> toCBOR outs
    TriesToForgeADA -> encodeListLen 1 <> toCBOR (11 :: Word8)
    OutputTooBigUTxO outs ->
      encodeListLen 2
        <> toCBOR (12 :: Word8)
        <> toCBOR outs

instance
  ( EraTxOut era
  , FromCBOR (PredicateFailure (EraRule "PPUP" era))
  ) =>
  FromCBOR (AllegraUtxoPredFailure era)
  where
  fromCBOR =
    decodeRecordSum "PredicateFailureUTXO" $
      \case
        0 -> do
          ins <- fromCBOR
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
          outs <- fromCBOR
          pure (2, OutputTooSmallUTxO outs)
        7 -> do
          a <- fromCBOR
          pure (2, UpdateFailure a)
        8 -> do
          right <- fromCBOR
          wrongs <- fromCBOR
          pure (3, WrongNetwork right wrongs)
        9 -> do
          right <- fromCBOR
          wrongs <- fromCBOR
          pure (3, WrongNetworkWithdrawal right wrongs)
        10 -> do
          outs <- fromCBOR
          pure (2, OutputBootAddrAttrsTooBig outs)
        11 -> pure (1, TriesToForgeADA)
        12 -> do
          outs <- fromCBOR
          pure (2, OutputTooBigUTxO outs)
        k -> invalidKey k

-- ===============================================
-- Inject instances

fromShelleyFailure :: Shelley.ShelleyUtxoPredFailure era -> Maybe (AllegraUtxoPredFailure era)
fromShelleyFailure = \case
  Shelley.BadInputsUTxO ins -> Just $ BadInputsUTxO ins
  Shelley.ExpiredUTxO {} -> Nothing -- Rule was replaced with `OutsideValidityIntervalUTxO`
  Shelley.MaxTxSizeUTxO a m -> Just $ MaxTxSizeUTxO a m
  Shelley.InputSetEmptyUTxO -> Just InputSetEmptyUTxO
  Shelley.FeeTooSmallUTxO mf af -> Just $ FeeTooSmallUTxO mf af
  Shelley.ValueNotConservedUTxO c p -> Just $ ValueNotConservedUTxO c p
  Shelley.WrongNetwork n as -> Just $ WrongNetwork n as
  Shelley.WrongNetworkWithdrawal n as -> Just $ WrongNetworkWithdrawal n as
  Shelley.OutputTooSmallUTxO {} -> Nothing -- Rule was updated
  Shelley.UpdateFailure ppf -> Just $ UpdateFailure ppf
  Shelley.OutputBootAddrAttrsTooBig outs -> Just $ OutputBootAddrAttrsTooBig outs

instance InjectMaybe (Shelley.ShelleyUtxoPredFailure era) (AllegraUtxoPredFailure era) where
  injectMaybe = fromShelleyFailure

instance Inject (AllegraUtxoPredFailure era) (AllegraUtxoPredFailure era) where
  inject = id

instance Inject (Shelley.ShelleyUtxoPredFailure era) (AllegraUtxoPredFailure era) where
  inject (Shelley.BadInputsUTxO ins) = BadInputsUTxO ins
  inject (Shelley.ExpiredUTxO ttl current) =
    OutsideValidityIntervalUTxO (ValidityInterval SNothing (SJust ttl)) current
  inject (Shelley.MaxTxSizeUTxO a m) = MaxTxSizeUTxO a m
  inject Shelley.InputSetEmptyUTxO = InputSetEmptyUTxO
  inject (Shelley.FeeTooSmallUTxO mf af) = FeeTooSmallUTxO mf af
  inject (Shelley.ValueNotConservedUTxO vc vp) = ValueNotConservedUTxO vc vp
  inject (Shelley.WrongNetwork n as) = WrongNetwork n as
  inject (Shelley.WrongNetworkWithdrawal n as) = WrongNetworkWithdrawal n as
  inject (Shelley.OutputTooSmallUTxO x) = OutputTooSmallUTxO x
  inject (Shelley.UpdateFailure x) = UpdateFailure x
  inject (Shelley.OutputBootAddrAttrsTooBig outs) = OutputTooBigUTxO outs
