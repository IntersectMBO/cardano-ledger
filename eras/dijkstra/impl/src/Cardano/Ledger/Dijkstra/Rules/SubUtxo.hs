{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubUtxo (
  DijkstraSUBUTXO,
  DijkstraSubUtxoPredFailure (..),
  DijkstraSubUtxoEvent (..),
  SubUtxoEnv (..),
) where

import Cardano.Ledger.Allegra.Rules (
  AllegraUtxoPredFailure,
  shelleyToAllegraUtxoPredFailure,
 )
import qualified Cardano.Ledger.Allegra.Rules as Allegra (
  validateOutsideValidityIntervalUTxO,
 )
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (
  validateOutputTooBigUTxO,
  validateOutsideForecast,
  validateWrongNetworkInTxBody,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)
import qualified Cardano.Ledger.Babbage.Rules as Babbage (
  validateOutputTooSmallUTxO,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
  encodeWord,
  sizedValue,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  ConwayUtxoPredFailure,
  allegraToConwayUtxoPredFailure,
  alonzoToConwayUtxoPredFailure,
  babbageToConwayUtxoPredFailure,
 )
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBUTXO,
 )
import Cardano.Ledger.Dijkstra.Rules.Utxo (
  DijkstraUtxoPredFailure (..),
  conwayToDijkstraUtxoPredFailure,
  validateWrongNetworkInDirectDeposit,
 )
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody)
import Cardano.Ledger.Rules.ValidationMode
import Cardano.Ledger.Shelley.LedgerState (UTxOState, utxosDonationL, utxosGovState, utxosUtxo)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, updateUTxOStateNoFees)
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  validateBadInputsUTxO,
  validateInputSetEmptyUTxO,
  validateOutputBootAddrAttrsTooBig,
  validateWrongNetwork,
  validateWrongNetworkWithdrawal,
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro

data SubUtxoEnv era = SubUtxoEnv
  { sueSlot :: SlotNo
  , suePParams :: PParams era
  , sueCertState :: CertState era
  , sueScriptsProvided :: ScriptsProvided era
  , sueOriginalUtxo :: UTxO era
  , sueTopTxIsValid :: IsValid
  }

data DijkstraSubUtxoPredFailure era
  = -- | The bad transaction inputs
    SubBadInputsUTxO (NonEmptySet TxIn)
  | SubOutsideValidityIntervalUTxO
      -- | transaction's validity interval
      ValidityInterval
      -- | current slot
      SlotNo
  | SubMaxTxSizeUTxO (Mismatch RelLTEQ Word32)
  | SubInputSetEmptyUTxO
  | -- | the set of addresses with incorrect network IDs
    SubWrongNetwork
      -- | the expected network id
      Network
      -- | the set of addresses with incorrect network IDs
      (NonEmptySet Addr)
  | SubWrongNetworkWithdrawal
      -- | the expected network id
      Network
      -- | the set of reward addresses with incorrect network IDs
      (NonEmptySet AccountAddress)
  | -- | list of supplied bad transaction outputs
    SubOutputBootAddrAttrsTooBig (NonEmpty (TxOut era))
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    SubOutputTooBigUTxO (NonEmpty (Int, Int, TxOut era))
  | -- | Wrong Network ID in body
    SubWrongNetworkInTxBody
      (Mismatch RelEQ Network)
  | -- | slot number outside consensus forecast range
    SubOutsideForecast SlotNo
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    SubBabbageOutputTooSmallUTxO (NonEmpty (TxOut era, Coin))
  | SubWrongNetworkInDirectDeposit
      -- | the expected network id
      Network
      -- | the set of account addresses with incorrect network IDs
      (NonEmptySet AccountAddress)
  deriving (Generic)

deriving stock instance
  ( Era era
  , Eq (Value era)
  , Eq (TxOut era)
  , Eq (Script era)
  , Eq TxIn
  ) =>
  Eq (DijkstraSubUtxoPredFailure era)

deriving stock instance
  ( Era era
  , Show (Value era)
  , Show (TxOut era)
  , Show (Script era)
  , Show TxIn
  ) =>
  Show (DijkstraSubUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  ) =>
  NFData (DijkstraSubUtxoPredFailure era)

type instance EraRuleFailure "SUBUTXO" DijkstraEra = DijkstraSubUtxoPredFailure DijkstraEra

type instance EraRuleEvent "SUBUTXO" DijkstraEra = DijkstraSubUtxoEvent DijkstraEra

instance InjectRuleFailure "SUBUTXO" DijkstraSubUtxoPredFailure DijkstraEra

instance InjectRuleFailure "SUBUTXO" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = dijkstraUtxoToDijkstraSubUtxoPredFailure

instance InjectRuleFailure "SUBUTXO" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = dijkstraUtxoToDijkstraSubUtxoPredFailure . conwayToDijkstraUtxoPredFailure

instance InjectRuleFailure "SUBUTXO" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxoToDijkstraSubUtxoPredFailure
      . conwayToDijkstraUtxoPredFailure
      . alonzoToConwayUtxoPredFailure

instance InjectRuleFailure "SUBUTXO" BabbageUtxoPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxoToDijkstraSubUtxoPredFailure
      . conwayToDijkstraUtxoPredFailure
      . babbageToConwayUtxoPredFailure

instance InjectRuleFailure "SUBUTXO" AllegraUtxoPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxoToDijkstraSubUtxoPredFailure
      . conwayToDijkstraUtxoPredFailure
      . allegraToConwayUtxoPredFailure

instance InjectRuleFailure "SUBUTXO" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxoToDijkstraSubUtxoPredFailure
      . conwayToDijkstraUtxoPredFailure
      . allegraToConwayUtxoPredFailure
      . shelleyToAllegraUtxoPredFailure

instance InjectRuleEvent "SUBUTXO" DijkstraSubUtxoEvent DijkstraEra

data DijkstraSubUtxoEvent era
  = TotalDeposits (SafeHash EraIndependentTxBody) Coin
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created
      (UTxO era)
  deriving (Generic)

deriving instance (Era era, Eq (TxOut era)) => Eq (DijkstraSubUtxoEvent era)

instance (Era era, NFData (TxOut era)) => NFData (DijkstraSubUtxoEvent era)

instance
  ( EraTx era
  , EraStake era
  , EraCertState era
  , DijkstraEraTxBody era
  , AlonzoEraTxWits era
  , ConwayEraGov era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , InjectRuleFailure "SUBUTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" BabbageUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" DijkstraUtxoPredFailure era
  ) =>
  STS (DijkstraSUBUTXO era)
  where
  type State (DijkstraSUBUTXO era) = UTxOState era
  type Signal (DijkstraSUBUTXO era) = Tx SubTx era
  type Environment (DijkstraSUBUTXO era) = SubUtxoEnv era
  type BaseM (DijkstraSUBUTXO era) = ShelleyBase
  type PredicateFailure (DijkstraSUBUTXO era) = DijkstraSubUtxoPredFailure era
  type Event (DijkstraSUBUTXO era) = DijkstraSubUtxoEvent era

  transitionRules = [dijkstraSubUtxoTransition @era]

dijkstraSubUtxoTransition ::
  forall era.
  ( EraTx era
  , EraStake era
  , EraCertState era
  , DijkstraEraTxBody era
  , AlonzoEraTxWits era
  , STS (EraRule "SUBUTXO" era)
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , InjectRuleFailure "SUBUTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" BabbageUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" DijkstraUtxoPredFailure era
  ) =>
  TransitionRule (EraRule "SUBUTXO" era)
dijkstraSubUtxoTransition = do
  TRC (SubUtxoEnv slot pp certState _ originalUtxo (IsValid isValid), utxoState, tx) <-
    judgmentContext

  let txBody = tx ^. bodyTxL

  runTest $ Allegra.validateOutsideValidityIntervalUTxO slot txBody

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo
  runTest $ Alonzo.validateOutsideForecast ei slot sysSt tx

  let allSizedOutputs = txBody ^. allSizedOutputsTxBodyF
  let allOutputs = fmap sizedValue allSizedOutputs
  runTest $ Alonzo.validateOutputTooBigUTxO pp allOutputs

  runTest $ Shelley.validateInputSetEmptyUTxO txBody

  let inputs = txBody ^. inputsTxBodyL
  let refInputs = txBody ^. referenceInputsTxBodyL
  runTest $ Shelley.validateBadInputsUTxO originalUtxo (inputs `Set.union` refInputs)
  runTest $ Shelley.validateBadInputsUTxO (utxosUtxo utxoState) inputs

  runTestOnSignal $ Shelley.validateOutputBootAddrAttrsTooBig allOutputs

  runTestOnSignal $ Babbage.validateOutputTooSmallUTxO pp allSizedOutputs

  netId <- liftSTS $ asks networkId
  runTestOnSignal $ Shelley.validateWrongNetwork netId allOutputs
  runTestOnSignal $ Shelley.validateWrongNetworkWithdrawal netId txBody
  runTestOnSignal $ validateWrongNetworkInDirectDeposit netId txBody
  runTestOnSignal $ Alonzo.validateWrongNetworkInTxBody netId txBody

  if isValid
    then do
      newState <-
        updateUTxOStateNoFees
          pp
          utxoState
          txBody
          certState
          (utxosGovState utxoState)
          (tellEvent . TotalDeposits (hashAnnotated txBody))
          (\a b -> tellEvent $ TxUTxODiff a b)
      pure $ newState & utxosDonationL <>~ txBody ^. treasuryDonationTxBodyL
    else
      pure utxoState

instance
  ( Era era
  , EncCBOR (TxOut era)
  ) =>
  EncCBOR (DijkstraSubUtxoPredFailure era)
  where
  encCBOR =
    \case
      SubBadInputsUTxO ins -> encodeListLen 2 <> encodeWord 0 <> encCBOR ins
      SubOutsideValidityIntervalUTxO a b -> encodeListLen 3 <> encodeWord 1 <> encCBOR a <> encCBOR b
      SubMaxTxSizeUTxO mm -> encodeListLen 2 <> encodeWord 2 <> encCBOR mm
      SubInputSetEmptyUTxO -> encodeListLen 1 <> encodeWord 3
      SubWrongNetwork right wrongs -> encodeListLen 3 <> encodeWord 4 <> encCBOR right <> encCBOR wrongs
      SubWrongNetworkWithdrawal right wrongs -> encodeListLen 3 <> encodeWord 5 <> encCBOR right <> encCBOR wrongs
      SubOutputBootAddrAttrsTooBig outs -> encodeListLen 2 <> encodeWord 6 <> encCBOR outs
      SubOutputTooBigUTxO outs -> encodeListLen 2 <> encodeWord 7 <> encCBOR outs
      SubWrongNetworkInTxBody mm -> encodeListLen 2 <> encodeWord 8 <> encCBOR mm
      SubOutsideForecast a -> encodeListLen 2 <> encodeWord 9 <> encCBOR a
      SubBabbageOutputTooSmallUTxO x -> encodeListLen 2 <> encodeWord 10 <> encCBOR x
      SubWrongNetworkInDirectDeposit right wrongs -> encodeListLen 3 <> encodeWord 11 <> encCBOR right <> encCBOR wrongs

instance
  ( Era era
  , DecCBOR (TxOut era)
  , EncCBOR (Value era)
  , DecCBOR (Value era)
  ) =>
  DecCBOR (DijkstraSubUtxoPredFailure era)
  where
  decCBOR = decodeRecordSum "DijkstraSubUtxoPredFailure" $ \case
    0 -> fmap (2,) $ SubBadInputsUTxO <$> decCBOR
    1 -> fmap (3,) $ SubOutsideValidityIntervalUTxO <$> decCBOR <*> decCBOR
    2 -> fmap (2,) $ SubMaxTxSizeUTxO <$> decCBOR
    3 -> pure (1, SubInputSetEmptyUTxO)
    4 -> fmap (3,) $ SubWrongNetwork <$> decCBOR <*> decCBOR
    5 -> fmap (3,) $ SubWrongNetworkWithdrawal <$> decCBOR <*> decCBOR
    6 -> fmap (2,) $ SubOutputBootAddrAttrsTooBig <$> decCBOR
    7 -> fmap (2,) $ SubOutputTooBigUTxO <$> decCBOR
    8 -> fmap (2,) $ SubWrongNetworkInTxBody <$> decCBOR
    9 -> fmap (2,) $ SubOutsideForecast <$> decCBOR
    10 -> fmap (2,) $ SubBabbageOutputTooSmallUTxO <$> decCBOR
    11 -> fmap (3,) $ SubWrongNetworkInDirectDeposit <$> decCBOR <*> decCBOR
    n -> invalidKey n

dijkstraUtxoToDijkstraSubUtxoPredFailure ::
  DijkstraUtxoPredFailure era -> DijkstraSubUtxoPredFailure era
dijkstraUtxoToDijkstraSubUtxoPredFailure = \case
  UtxosFailure _ -> error "Impossible: `UtxosFailure` for SUBUTXO"
  BadInputsUTxO x -> SubBadInputsUTxO x
  OutsideValidityIntervalUTxO vi slotNo -> SubOutsideValidityIntervalUTxO vi slotNo
  MaxTxSizeUTxO m -> SubMaxTxSizeUTxO m
  InputSetEmptyUTxO -> SubInputSetEmptyUTxO
  FeeTooSmallUTxO _ -> error "Impossible: `FeeTooSmallUTxO` for SUBUTXO"
  ValueNotConservedUTxO _ -> error "Impossible: `ValueNotConservedUTxO` for SUBUTXO"
  WrongNetwork x y -> SubWrongNetwork x y
  WrongNetworkWithdrawal x y -> SubWrongNetworkWithdrawal x y
  OutputBootAddrAttrsTooBig xs -> SubOutputBootAddrAttrsTooBig xs
  OutputTooBigUTxO xs -> SubOutputTooBigUTxO xs
  InsufficientCollateral _ _ -> error "Impossible: `InsufficientCollateral` for SUBUTXO"
  ScriptsNotPaidUTxO _ -> error "Impossible: `ScriptsNotPaidUTxO` for SUBUTXO"
  ExUnitsTooBigUTxO _ -> error "Impossible: `ExUnitsTooBigUTxO` for SUBUTXO"
  CollateralContainsNonADA _ -> error "Impossible: `CollateralContainsNonADA` for SUBUTXO"
  WrongNetworkInTxBody m -> SubWrongNetworkInTxBody m
  OutsideForecast sno -> SubOutsideForecast sno
  TooManyCollateralInputs _ -> error "Impossible: `TooManyCollateralInputs` for SUBUTXO"
  NoCollateralInputs -> error "Impossible: `NoCollateralInputs` for SUBUTXO"
  IncorrectTotalCollateralField _ _ -> error "Impossible: `IncorrectTotalCollateralField` for SUBUTXO"
  BabbageOutputTooSmallUTxO outs -> SubBabbageOutputTooSmallUTxO outs
  BabbageNonDisjointRefInputs _ -> error "Impossible: `BabbageNonDisjointRefInputs` for SUBUTXO"
  PtrPresentInCollateralReturn _ -> error "Impossible: `PtrPresentInCollateralReturn` for SUBUTXO"
  WrongNetworkInDirectDeposit x y -> SubWrongNetworkInDirectDeposit x y
