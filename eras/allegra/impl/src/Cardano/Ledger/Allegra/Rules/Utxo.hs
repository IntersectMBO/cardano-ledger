{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Utxo (
  AllegraUTXO,
  AllegraUtxoEvent (..),
  AllegraUtxoPredFailure (..),
  validateOutsideValidityIntervalUTxO,
  shelleyToAllegraUtxoPredFailure,
)
where

import Cardano.Ledger.Address (Addr, RewardAccount)
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Era (AllegraEra, AllegraUTXO)
import Cardano.Ledger.Allegra.Rules.Ppup ()
import Cardano.Ledger.Allegra.Scripts (inInterval)
import Cardano.Ledger.BaseTypes (
  Network,
  ProtVer (pvMajor),
  ShelleyBase,
  StrictMaybe (..),
  networkId,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
  invalidKey,
  serialize,
 )
import Cardano.Ledger.CertState (certDState, dsGenDelegs)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules (
  PpupEnv (..),
  ShelleyPPUP,
  ShelleyPpupPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (
  EraUTxO (..),
  UTxO (..),
  txouts,
 )
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo)
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
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
      !(Set (RewardAccount (EraCrypto era))) -- the set of reward addresses with incorrect network IDs
  | OutputTooSmallUTxO
      ![TxOut era] -- list of supplied transaction outputs that are too small
  | UpdateFailure (EraRuleFailure "PPUP" era) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      ![TxOut era] -- list of supplied bad transaction outputs
  | -- Kept for backwards compatibility: no longer used because the `MultiAsset` type of mint doesn't allow for this possibility
    TriesToForgeADA -- TODO: remove
  | OutputTooBigUTxO
      ![TxOut era] -- list of supplied bad transaction outputs
  deriving (Generic)

type instance EraRuleFailure "UTXO" (AllegraEra c) = AllegraUtxoPredFailure (AllegraEra c)

instance InjectRuleFailure "UTXO" AllegraUtxoPredFailure (AllegraEra c)

instance InjectRuleFailure "UTXO" ShelleyPpupPredFailure (AllegraEra c) where
  injectFailure = UpdateFailure

instance InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure (AllegraEra c) where
  injectFailure = shelleyToAllegraUtxoPredFailure

deriving stock instance
  ( Show (TxOut era)
  , Show (Value era)
  , Show (EraRuleFailure "PPUP" era)
  ) =>
  Show (AllegraUtxoPredFailure era)

deriving stock instance
  ( Eq (TxOut era)
  , Eq (Value era)
  , Eq (EraRuleFailure "PPUP" era)
  ) =>
  Eq (AllegraUtxoPredFailure era)

instance
  ( NoThunks (TxOut era)
  , NoThunks (Value era)
  , NoThunks (EraRuleFailure "PPUP" era)
  ) =>
  NoThunks (AllegraUtxoPredFailure era)

instance
  ( Era era
  , NFData (TxOut era)
  , NFData (Value era)
  , NFData (EraRuleFailure "PPUP" era)
  ) =>
  NFData (AllegraUtxoPredFailure era)

data AllegraUtxoEvent era
  = UpdateEvent (Event (EraRule "PPUP" era))
  | TotalDeposits (SafeHash (EraCrypto era) EraIndependentTxBody) Coin
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created
      (UTxO era)
  deriving (Generic)

deriving instance
  ( Era era
  , Eq (TxOut era)
  , Eq (Event (EraRule "PPUP" era))
  ) =>
  Eq (AllegraUtxoEvent era)

instance
  ( Era era
  , NFData (TxOut era)
  , NFData (Event (EraRule "PPUP" era))
  ) =>
  NFData (AllegraUtxoEvent era)

-- | The UTxO transition rule for the Allegra era.
utxoTransition ::
  forall era.
  ( EraUTxO era
  , ShelleyEraTxBody era
  , AllegraEraTxBody era
  , Eq (EraRuleFailure "PPUP" era)
  , Show (EraRuleFailure "PPUP" era)
  , Embed (EraRule "PPUP" era) (EraRule "UTXO" era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , GovState era ~ ShelleyGovState era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure era
  , EraRule "UTXO" era ~ AllegraUTXO era
  ) =>
  TransitionRule (EraRule "UTXO" era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp certState, utxos, tx) <- judgmentContext
  let Shelley.UTxOState utxo _ _ ppup _ _ = utxos
      txBody = tx ^. bodyTxL
      genDelegs = dsGenDelegs (certDState certState)

  {- ininterval slot (txvld tx) -}
  runTest $ validateOutsideValidityIntervalUTxO slot txBody

  {- txins txb ≠ ∅ -}
  runTest $ Shelley.validateInputSetEmptyUTxO txBody

  {- minfee pp tx ≤ txfee txb -}
  runTest $ Shelley.validateFeeTooSmallUTxO pp tx utxo

  {- txins txb ⊆ dom utxo -}
  runTest $ Shelley.validateBadInputsUTxO utxo $ txBody ^. inputsTxBodyL

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ txouts txb, netId a = NetworkId -}
  runTest $ Shelley.validateWrongNetwork netId . toList $ txBody ^. outputsTxBodyL

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTest $ Shelley.validateWrongNetworkWithdrawal netId txBody

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $ Shelley.validateValueNotConservedUTxO pp utxo certState txBody

  -- process Protocol Parameter Update Proposals
  ppup' <-
    trans @(EraRule "PPUP" era) $ TRC (PPUPEnv slot pp genDelegs, ppup, txBody ^. updateTxBodyL)

  {- adaPolicy ∉ supp mint tx
     above check not needed because mint field of type MultiAsset cannot contain ada -}

  let outputs = txouts txBody
  {- ∀ txout ∈ txouts txb, getValue txout ≥ inject (scaledMinDeposit v (minUTxOValue pp)) -}
  runTest $ validateOutputTooSmallUTxO pp outputs

  {- ∀ txout ∈ txouts txb, serSize (getValue txout) ≤ MaxValSize -}
  -- MaxValSize = 4000
  runTest $ validateOutputTooBigUTxO pp outputs

  {- ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTest $ Shelley.validateOutputBootAddrAttrsTooBig (Map.elems (unUTxO outputs))

  {- txsize tx ≤ maxTxSize pp -}
  runTest $ Shelley.validateMaxTxSizeUTxO pp tx

  Shelley.updateUTxOState
    pp
    utxos
    txBody
    certState
    ppup'
    (tellEvent . TotalDeposits (hashAnnotated txBody))
    (\a b -> tellEvent $ TxUTxODiff a b)

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
  , ShelleyEraTxBody era
  , AllegraEraTxBody era
  , Embed (EraRule "PPUP" era) (AllegraUTXO era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , ProtVerAtMost era 8
  , Eq (EraRuleFailure "PPUP" era)
  , Show (EraRuleFailure "PPUP" era)
  , EraRule "UTXO" era ~ AllegraUTXO era
  , GovState era ~ ShelleyGovState era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure era
  ) =>
  STS (AllegraUTXO era)
  where
  type State (AllegraUTXO era) = Shelley.UTxOState era
  type Signal (AllegraUTXO era) = Tx era
  type Environment (AllegraUTXO era) = Shelley.UtxoEnv era
  type BaseM (AllegraUTXO era) = ShelleyBase
  type PredicateFailure (AllegraUTXO era) = AllegraUtxoPredFailure era
  type Event (AllegraUTXO era) = AllegraUtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]

instance
  ( Era era
  , STS (ShelleyPPUP era)
  , EraRuleFailure "PPUP" era ~ ShelleyPpupPredFailure era
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
  ( Era era
  , Crypto (EraCrypto era)
  , EncCBOR (Value era)
  , EncCBOR (TxOut era)
  , EncCBOR (EraRuleFailure "PPUP" era)
  ) =>
  EncCBOR (AllegraUtxoPredFailure era)
  where
  encCBOR = \case
    BadInputsUTxO ins ->
      encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR ins
    (OutsideValidityIntervalUTxO a b) ->
      encodeListLen 3
        <> encCBOR (1 :: Word8)
        <> encCBOR a
        <> encCBOR b
    (MaxTxSizeUTxO a b) ->
      encodeListLen 3
        <> encCBOR (2 :: Word8)
        <> encCBOR a
        <> encCBOR b
    InputSetEmptyUTxO -> encodeListLen 1 <> encCBOR (3 :: Word8)
    (FeeTooSmallUTxO a b) ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR a
        <> encCBOR b
    (ValueNotConservedUTxO a b) ->
      encodeListLen 3
        <> encCBOR (5 :: Word8)
        <> encCBOR a
        <> encCBOR b
    OutputTooSmallUTxO outs ->
      encodeListLen 2
        <> encCBOR (6 :: Word8)
        <> encCBOR outs
    (UpdateFailure a) ->
      encodeListLen 2
        <> encCBOR (7 :: Word8)
        <> encCBOR a
    (WrongNetwork right wrongs) ->
      encodeListLen 3
        <> encCBOR (8 :: Word8)
        <> encCBOR right
        <> encCBOR wrongs
    (WrongNetworkWithdrawal right wrongs) ->
      encodeListLen 3
        <> encCBOR (9 :: Word8)
        <> encCBOR right
        <> encCBOR wrongs
    OutputBootAddrAttrsTooBig outs ->
      encodeListLen 2
        <> encCBOR (10 :: Word8)
        <> encCBOR outs
    TriesToForgeADA -> encodeListLen 1 <> encCBOR (11 :: Word8)
    OutputTooBigUTxO outs ->
      encodeListLen 2
        <> encCBOR (12 :: Word8)
        <> encCBOR outs

instance
  ( EraTxOut era
  , DecCBOR (EraRuleFailure "PPUP" era)
  ) =>
  DecCBOR (AllegraUtxoPredFailure era)
  where
  decCBOR =
    decodeRecordSum "PredicateFailureUTXO" $
      \case
        0 -> do
          ins <- decCBOR
          pure (2, BadInputsUTxO ins) -- The (2,..) indicates the number of things decoded, INCLUDING the tags, which are decoded by decodeRecordSumNamed
        1 -> do
          a <- decCBOR
          b <- decCBOR
          pure (3, OutsideValidityIntervalUTxO a b)
        2 -> do
          a <- decCBOR
          b <- decCBOR
          pure (3, MaxTxSizeUTxO a b)
        3 -> pure (1, InputSetEmptyUTxO)
        4 -> do
          a <- decCBOR
          b <- decCBOR
          pure (3, FeeTooSmallUTxO a b)
        5 -> do
          a <- decCBOR
          b <- decCBOR
          pure (3, ValueNotConservedUTxO a b)
        6 -> do
          outs <- decCBOR
          pure (2, OutputTooSmallUTxO outs)
        7 -> do
          a <- decCBOR
          pure (2, UpdateFailure a)
        8 -> do
          right <- decCBOR
          wrongs <- decCBOR
          pure (3, WrongNetwork right wrongs)
        9 -> do
          right <- decCBOR
          wrongs <- decCBOR
          pure (3, WrongNetworkWithdrawal right wrongs)
        10 -> do
          outs <- decCBOR
          pure (2, OutputBootAddrAttrsTooBig outs)
        11 -> pure (1, TriesToForgeADA)
        12 -> do
          outs <- decCBOR
          pure (2, OutputTooBigUTxO outs)
        k -> invalidKey k

shelleyToAllegraUtxoPredFailure :: Shelley.ShelleyUtxoPredFailure era -> AllegraUtxoPredFailure era
shelleyToAllegraUtxoPredFailure = \case
  Shelley.BadInputsUTxO ins -> BadInputsUTxO ins
  Shelley.ExpiredUTxO ttl current ->
    OutsideValidityIntervalUTxO (ValidityInterval SNothing (SJust ttl)) current
  Shelley.MaxTxSizeUTxO a m -> MaxTxSizeUTxO a m
  Shelley.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Shelley.FeeTooSmallUTxO mf af -> FeeTooSmallUTxO mf af
  Shelley.ValueNotConservedUTxO vc vp -> ValueNotConservedUTxO vc vp
  Shelley.WrongNetwork n as -> WrongNetwork n as
  Shelley.WrongNetworkWithdrawal n as -> WrongNetworkWithdrawal n as
  Shelley.OutputTooSmallUTxO x -> OutputTooSmallUTxO x
  Shelley.UpdateFailure x -> UpdateFailure x
  Shelley.OutputBootAddrAttrsTooBig outs -> OutputTooBigUTxO outs
