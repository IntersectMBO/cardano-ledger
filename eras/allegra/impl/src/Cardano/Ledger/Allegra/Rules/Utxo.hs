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
  Mismatch (..),
  Network,
  ProtVer (pvMajor),
  Relation (..),
  ShelleyBase,
  StrictMaybe (..),
  networkId,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), serialize)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules (
  PpupEnv (..),
  ShelleyPPUP,
  ShelleyPpupPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn)
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
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)
import Validation

-- ==========================================================

data AllegraUtxoPredFailure era
  = BadInputsUTxO (Set TxIn) -- The bad transaction inputs
  | OutsideValidityIntervalUTxO
      ValidityInterval -- transaction's validity interval
      SlotNo -- current slot
  | MaxTxSizeUTxO (Mismatch 'RelLTEQ Integer)
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO (Mismatch 'RelGTEQ Coin)
  | ValueNotConservedUTxO (Mismatch 'RelEQ (Value era)) -- Consumed, then produced
  | WrongNetwork
      Network -- the expected network id
      (Set Addr) -- the set of addresses with incorrect network IDs
  | WrongNetworkWithdrawal
      Network -- the expected network id
      (Set RewardAccount) -- the set of reward addresses with incorrect network IDs
  | OutputTooSmallUTxO
      [TxOut era] -- list of supplied transaction outputs that are too small
  | UpdateFailure (EraRuleFailure "PPUP" era) -- Subtransition Failures
  | OutputBootAddrAttrsTooBig
      [TxOut era] -- list of supplied bad transaction outputs
  | -- Kept for backwards compatibility: no longer used because the `MultiAsset` type of mint doesn't allow for this possibility
    TriesToForgeADA -- TODO: remove
  | OutputTooBigUTxO
      [TxOut era] -- list of supplied bad transaction outputs
  deriving (Generic)

type instance EraRuleFailure "UTXO" AllegraEra = AllegraUtxoPredFailure AllegraEra

instance InjectRuleFailure "UTXO" AllegraUtxoPredFailure AllegraEra

instance InjectRuleFailure "UTXO" ShelleyPpupPredFailure AllegraEra where
  injectFailure = UpdateFailure

instance InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure AllegraEra where
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
  | TotalDeposits (SafeHash EraIndependentTxBody) Coin
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created (produced)
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
  , EraStake era
  , EraCertState era
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
  , SafeToHash (TxWits era)
  ) =>
  TransitionRule (EraRule "UTXO" era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp certState, utxos, tx) <- judgmentContext
  let Shelley.UTxOState utxo _ _ ppup _ _ = utxos
      txBody = tx ^. bodyTxL
      genDelegs = certState ^. Shelley.certDStateL . Shelley.dsGenDelegsL

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
  ( EraTx era
  , EraUTxO era
  , EraStake era
  , EraCertState era
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
  , SafeToHash (TxWits era)
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
  assertions = [Shelley.validSizeComputationCheck]

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
  , EncCBOR (Value era)
  , EncCBOR (TxOut era)
  , EncCBOR (EraRuleFailure "PPUP" era)
  ) =>
  EncCBOR (AllegraUtxoPredFailure era)
  where
  encCBOR =
    encode . \case
      BadInputsUTxO ins -> Sum BadInputsUTxO 0 !> To ins
      OutsideValidityIntervalUTxO validityInterval slot ->
        Sum OutsideValidityIntervalUTxO 1 !> To validityInterval !> To slot
      MaxTxSizeUTxO m -> Sum MaxTxSizeUTxO 2 !> To m
      InputSetEmptyUTxO -> Sum InputSetEmptyUTxO 3
      FeeTooSmallUTxO m -> Sum FeeTooSmallUTxO 4 !> To m
      ValueNotConservedUTxO m -> Sum ValueNotConservedUTxO 5 !> To m
      OutputTooSmallUTxO outs -> Sum OutputTooSmallUTxO 6 !> To outs
      UpdateFailure fails -> Sum UpdateFailure 7 !> To fails
      WrongNetwork right wrongs -> Sum WrongNetwork 8 !> To right !> To wrongs
      WrongNetworkWithdrawal right wrongs -> Sum WrongNetworkWithdrawal 9 !> To right !> To wrongs
      OutputBootAddrAttrsTooBig outs -> Sum OutputBootAddrAttrsTooBig 10 !> To outs
      TriesToForgeADA -> Sum TriesToForgeADA 11
      OutputTooBigUTxO outs -> Sum OutputTooBigUTxO 12 !> To outs

instance
  ( EraTxOut era
  , DecCBOR (EraRuleFailure "PPUP" era)
  ) =>
  DecCBOR (AllegraUtxoPredFailure era)
  where
  decCBOR = decode . Summands "AllegraUtxoPredFailure" $ \case
    0 -> SumD BadInputsUTxO <! From
    1 -> SumD OutsideValidityIntervalUTxO <! From <! From
    2 -> SumD MaxTxSizeUTxO <! From
    3 -> SumD InputSetEmptyUTxO
    4 -> SumD FeeTooSmallUTxO <! From
    5 -> SumD ValueNotConservedUTxO <! From
    6 -> SumD OutputTooSmallUTxO <! From
    7 -> SumD UpdateFailure <! From
    8 -> SumD WrongNetwork <! From <! From
    9 -> SumD WrongNetworkWithdrawal <! From <! From
    10 -> SumD OutputBootAddrAttrsTooBig <! From
    11 -> SumD TriesToForgeADA
    12 -> SumD OutputTooBigUTxO <! From
    k -> Invalid k

shelleyToAllegraUtxoPredFailure :: Shelley.ShelleyUtxoPredFailure era -> AllegraUtxoPredFailure era
shelleyToAllegraUtxoPredFailure = \case
  Shelley.BadInputsUTxO ins -> BadInputsUTxO ins
  Shelley.ExpiredUTxO Mismatch {mismatchSupplied = ttl, mismatchExpected = current} ->
    OutsideValidityIntervalUTxO (ValidityInterval SNothing (SJust ttl)) current
  Shelley.MaxTxSizeUTxO m -> MaxTxSizeUTxO m
  Shelley.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Shelley.FeeTooSmallUTxO m -> FeeTooSmallUTxO m
  Shelley.ValueNotConservedUTxO m -> ValueNotConservedUTxO m
  Shelley.WrongNetwork n as -> WrongNetwork n as
  Shelley.WrongNetworkWithdrawal n as -> WrongNetworkWithdrawal n as
  Shelley.OutputTooSmallUTxO x -> OutputTooSmallUTxO x
  Shelley.UpdateFailure x -> UpdateFailure x
  Shelley.OutputBootAddrAttrsTooBig outs -> OutputTooBigUTxO outs
