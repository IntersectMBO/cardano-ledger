{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxo (
  DijkstraUTXO,
  DijkstraUtxoPredFailure (..),
  conwayToDijkstraUtxoPredFailure,
  validateWrongNetworkInDirectDeposit,
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure, shelleyToAllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  babbageUtxoValidation,
  updateUTxOStateByTxValidity,
 )
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Network,
  Relation (..),
  ShelleyBase,
  SlotNo,
  StrictMaybe (..),
  networkId,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
  encodeWord,
  invalidKey,
 )
import Cardano.Ledger.Coin (Coin, DeltaCoin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayUTXOS,
  ConwayUtxoPredFailure,
  ConwayUtxosEnv (..),
  ConwayUtxosPredFailure (..),
  allegraToConwayUtxoPredFailure,
  alonzoToConwayUtxoPredFailure,
  babbageToConwayUtxoPredFailure,
  updateTreasuryDonation,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraUTXO)
import Cardano.Ledger.Dijkstra.Rules.Utxos ()
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Plutus (ExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test, failOnJustStatic, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  UtxoEnv (..),
  validSizeComputationCheck,
 )
import Cardano.Ledger.State (
  EraCertState (..),
  EraStake,
  EraUTxO,
 )
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed (..),
  Rule,
  STS (..),
  TRC (..),
  TransitionRule,
  failureOnNonEmptySet,
  judgmentContext,
  liftSTS,
  trans,
 )
import Data.List.NonEmpty (NonEmpty)
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.Strict as Map
import Data.Set.NonEmpty (NonEmptySet)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

-- ======================================================

-- | Predicate failure for the Dijkstra Era
data DijkstraUtxoPredFailure era
  = -- | Subtransition Failures
    UtxosFailure (PredicateFailure (EraRule "UTXOS" era))
  | -- | The bad transaction inputs
    BadInputsUTxO (NonEmptySet TxIn)
  | OutsideValidityIntervalUTxO
      -- | transaction's validity interval
      ValidityInterval
      -- | current slot
      SlotNo
  | MaxTxSizeUTxO (Mismatch RelLTEQ Word32)
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      (Mismatch RelGTEQ Coin)
  | ValueNotConservedUTxO
      (Mismatch RelEQ (Value era)) -- Serialise consumed first, then produced
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      -- | the expected network id
      Network
      -- | the set of addresses with incorrect network IDs
      (NonEmptySet Addr)
  | WrongNetworkWithdrawal
      -- | the expected network id
      Network
      -- | the set of reward addresses with incorrect network IDs
      (NonEmptySet AccountAddress)
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig (NonEmpty (TxOut era))
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO (NonEmpty (Int, Int, TxOut era))
  | InsufficientCollateral
      -- | balance computed
      DeltaCoin
      -- | the required collateral for the given fee
      Coin
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO (NonEmptyMap TxIn (TxOut era))
  | ExUnitsTooBigUTxO
      (Mismatch RelLTEQ ExUnits)
  | -- | The inputs marked for use as fees contain non-ADA tokens
    CollateralContainsNonADA (Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody
      (Mismatch RelEQ Network)
  | -- | slot number outside consensus forecast range
    OutsideForecast SlotNo
  | -- | There are too many collateral inputs
    TooManyCollateralInputs
      (Mismatch RelLTEQ Word16)
  | NoCollateralInputs
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    IncorrectTotalCollateralField
      -- | collateral provided
      DeltaCoin
      -- | collateral amount declared in transaction body
      Coin
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    BabbageOutputTooSmallUTxO (NonEmpty (TxOut era, Coin))
  | -- | TxIns that appear in both inputs and reference inputs
    BabbageNonDisjointRefInputs (NonEmpty TxIn)
  | PtrPresentInCollateralReturn (TxOut era)
  | WrongNetworkInDirectDeposit
      -- | the expected network id
      Network
      -- | the set of account addresses with incorrect network IDs
      (NonEmptySet AccountAddress)
  deriving (Generic)

type instance EraRuleFailure "UTXO" DijkstraEra = DijkstraUtxoPredFailure DijkstraEra

type instance EraRuleEvent "UTXO" DijkstraEra = AlonzoUtxoEvent DijkstraEra

instance InjectRuleFailure "UTXO" DijkstraUtxoPredFailure DijkstraEra

instance InjectRuleFailure "UTXO" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure

instance InjectRuleFailure "UTXO" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure . babbageToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure . alonzoToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraUtxoPredFailure . allegraToConwayUtxoPredFailure . shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure . allegraToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = UtxosFailure

instance InjectRuleFailure "UTXO" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraUtxoPredFailure
      . alonzoToConwayUtxoPredFailure
      . Alonzo.UtxosFailure
      . injectFailure

deriving instance
  ( Era era
  , Show (Value era)
  , Show (PredicateFailure (EraRule "UTXOS" era))
  , Show (TxOut era)
  , Show (Script era)
  , Show TxIn
  ) =>
  Show (DijkstraUtxoPredFailure era)

deriving instance
  ( Era era
  , Eq (Value era)
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Eq (TxOut era)
  , Eq (Script era)
  , Eq TxIn
  ) =>
  Eq (DijkstraUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  , NFData (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  NFData (DijkstraUtxoPredFailure era)

--------------------------------------------------------------------------------
-- DijkstraUTXO STS
--------------------------------------------------------------------------------

validateNoPtrInCollateralReturn ::
  ( BabbageEraTxBody era
  , InjectRuleFailure rule DijkstraUtxoPredFailure era
  ) =>
  TxBody TopTx era ->
  Rule (EraRule rule era) ctx ()
validateNoPtrInCollateralReturn txBody = do
  let hasCollateralTxOut = do
        SJust collateralReturn <- pure $ txBody ^. collateralReturnTxBodyL
        Addr _ _ (StakeRefPtr {}) <- pure $ collateralReturn ^. addrTxOutL
        Just collateralReturn
  failOnJustStatic hasCollateralTxOut (injectFailure . PtrPresentInCollateralReturn)

validateWrongNetworkInDirectDeposit ::
  DijkstraEraTxBody era =>
  Network ->
  TxBody t era ->
  Test (DijkstraUtxoPredFailure era)
validateWrongNetworkInDirectDeposit netId txb =
  failureOnNonEmptySet depositsWrongNetwork (WrongNetworkInDirectDeposit netId)
  where
    depositsWrongNetwork =
      Map.keysSet $
        Map.filterWithKey
          (\a _ -> aaNetworkId a /= netId)
          (unDirectDeposits $ txb ^. directDepositsTxBodyL)

dijkstraUtxoTransition ::
  forall era.
  ( EraUTxO era
  , EraCertState era
  , DijkstraEraTxBody era
  , AlonzoEraTx era
  , EraStake era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , InjectRuleFailure "UTXO" DijkstraUtxoPredFailure era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , STS (EraRule "UTXO" era)
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , -- In this function we we call the UTXOS rule, so we need some assumptions
    Environment (EraRule "UTXOS" era) ~ ConwayUtxosEnv era
  , State (EraRule "UTXOS" era) ~ ()
  , Signal (EraRule "UTXOS" era) ~ Tx TopTx era
  , Embed (EraRule "UTXOS" era) (EraRule "UTXO" era)
  ) =>
  TransitionRule (EraRule "UTXO" era)
dijkstraUtxoTransition = do
  TRC (UtxoEnv _ pp certState, utxos, tx) <- judgmentContext
  babbageUtxoValidation
  validateNoPtrInCollateralReturn $ tx ^. bodyTxL
  netId <- liftSTS $ asks networkId
  runTestOnSignal $ validateWrongNetworkInDirectDeposit netId (tx ^. bodyTxL)
  () <- trans @(EraRule "UTXOS" era) $ TRC (ConwayUtxosEnv pp (utxosUtxo utxos), (), tx)
  updateUTxOStateByTxValidity
    pp
    certState
    (utxosGovState utxos)
    tx
    (updateTreasuryDonation tx utxos)

instance
  forall era.
  ( EraTx era
  , EraUTxO era
  , EraStake era
  , DijkstraEraTxBody era
  , AlonzoEraTx era
  , EraRule "UTXO" era ~ DijkstraUTXO era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , InjectRuleFailure "UTXO" ConwayUtxoPredFailure era
  , InjectRuleFailure "UTXO" DijkstraUtxoPredFailure era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , STS (EraRule "UTXO" era)
  , -- In this function we we call the UTXOS rule, so we need some assumptions
    Embed (EraRule "UTXOS" era) (DijkstraUTXO era)
  , Environment (EraRule "UTXOS" era) ~ ConwayUtxosEnv era
  , State (EraRule "UTXOS" era) ~ ()
  , Signal (EraRule "UTXOS" era) ~ Tx TopTx era
  , EraCertState era
  , EraRule "UTXO" era ~ DijkstraUTXO era
  , SafeToHash (TxWits era)
  ) =>
  STS (DijkstraUTXO era)
  where
  type State (DijkstraUTXO era) = UTxOState era
  type Signal (DijkstraUTXO era) = Tx TopTx era
  type Environment (DijkstraUTXO era) = UtxoEnv era
  type BaseM (DijkstraUTXO era) = ShelleyBase
  type PredicateFailure (DijkstraUTXO era) = DijkstraUtxoPredFailure era
  type Event (DijkstraUTXO era) = AlonzoUtxoEvent era

  initialRules = []

  transitionRules = [dijkstraUtxoTransition @era]

  assertions = [validSizeComputationCheck]

instance
  ( Era era
  , STS (ConwayUTXOS era)
  , PredicateFailure (EraRule "UTXOS" era) ~ ConwayUtxosPredFailure era
  , Event (EraRule "UTXOS" era) ~ Event (ConwayUTXOS era)
  ) =>
  Embed (ConwayUTXOS era) (DijkstraUTXO era)
  where
  wrapFailed = UtxosFailure
  wrapEvent = Alonzo.UtxosEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Era era
  , EncCBOR (TxOut era)
  , EncCBOR (Value era)
  , EncCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  EncCBOR (DijkstraUtxoPredFailure era)
  where
  encCBOR =
    \case
      UtxosFailure a -> encodeListLen 2 <> encodeWord 0 <> encCBOR a
      BadInputsUTxO ins -> encodeListLen 2 <> encodeWord 1 <> encCBOR ins
      OutsideValidityIntervalUTxO a b -> encodeListLen 3 <> encodeWord 2 <> encCBOR a <> encCBOR b
      MaxTxSizeUTxO mm -> encodeListLen 2 <> encodeWord 3 <> encCBOR mm
      InputSetEmptyUTxO -> encodeListLen 1 <> encodeWord 4
      FeeTooSmallUTxO mm -> encodeListLen 2 <> encodeWord 5 <> encCBOR mm
      ValueNotConservedUTxO mm -> encodeListLen 2 <> encodeWord 6 <> encCBOR mm
      WrongNetwork right wrongs -> encodeListLen 3 <> encodeWord 7 <> encCBOR right <> encCBOR wrongs
      WrongNetworkWithdrawal right wrongs -> encodeListLen 3 <> encodeWord 8 <> encCBOR right <> encCBOR wrongs
      OutputBootAddrAttrsTooBig outs -> encodeListLen 2 <> encodeWord 9 <> encCBOR outs
      OutputTooBigUTxO outs -> encodeListLen 2 <> encodeWord 10 <> encCBOR outs
      InsufficientCollateral a b -> encodeListLen 3 <> encodeWord 11 <> encCBOR a <> encCBOR b
      ScriptsNotPaidUTxO a -> encodeListLen 2 <> encodeWord 12 <> encCBOR a
      ExUnitsTooBigUTxO mm -> encodeListLen 2 <> encodeWord 13 <> encCBOR mm
      CollateralContainsNonADA a -> encodeListLen 2 <> encodeWord 14 <> encCBOR a
      WrongNetworkInTxBody mm -> encodeListLen 2 <> encodeWord 15 <> encCBOR mm
      OutsideForecast a -> encodeListLen 2 <> encodeWord 16 <> encCBOR a
      TooManyCollateralInputs mm -> encodeListLen 2 <> encodeWord 17 <> encCBOR mm
      NoCollateralInputs -> encodeListLen 1 <> encodeWord 18
      IncorrectTotalCollateralField c1 c2 -> encodeListLen 3 <> encodeWord 19 <> encCBOR c1 <> encCBOR c2
      BabbageOutputTooSmallUTxO x -> encodeListLen 2 <> encodeWord 20 <> encCBOR x
      BabbageNonDisjointRefInputs x -> encodeListLen 2 <> encodeWord 21 <> encCBOR x
      PtrPresentInCollateralReturn x -> encodeListLen 2 <> encodeWord 22 <> encCBOR x
      WrongNetworkInDirectDeposit right wrongs -> encodeListLen 3 <> encodeWord 23 <> encCBOR right <> encCBOR wrongs

instance
  ( Era era
  , DecCBOR (TxOut era)
  , EncCBOR (Value era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  DecCBOR (DijkstraUtxoPredFailure era)
  where
  decCBOR = decodeRecordSum "DijkstraUtxoPredFailure" $ \case
    0 -> fmap (2,) $ UtxosFailure <$> decCBOR
    1 -> fmap (2,) $ BadInputsUTxO <$> decCBOR
    2 -> fmap (3,) $ OutsideValidityIntervalUTxO <$> decCBOR <*> decCBOR
    3 -> fmap (2,) $ MaxTxSizeUTxO <$> decCBOR
    4 -> pure (1, InputSetEmptyUTxO)
    5 -> fmap (2,) $ FeeTooSmallUTxO <$> decCBOR
    6 -> fmap (2,) $ ValueNotConservedUTxO <$> decCBOR
    7 -> fmap (3,) $ WrongNetwork <$> decCBOR <*> decCBOR
    8 -> fmap (3,) $ WrongNetworkWithdrawal <$> decCBOR <*> decCBOR
    9 -> fmap (2,) $ OutputBootAddrAttrsTooBig <$> decCBOR
    10 -> fmap (2,) $ OutputTooBigUTxO <$> decCBOR
    11 -> fmap (3,) $ InsufficientCollateral <$> decCBOR <*> decCBOR
    12 -> fmap (2,) $ ScriptsNotPaidUTxO <$> decCBOR
    13 -> fmap (2,) $ ExUnitsTooBigUTxO <$> decCBOR
    14 -> fmap (2,) $ CollateralContainsNonADA <$> decCBOR
    15 -> fmap (2,) $ WrongNetworkInTxBody <$> decCBOR
    16 -> fmap (2,) $ OutsideForecast <$> decCBOR
    17 -> fmap (2,) $ TooManyCollateralInputs <$> decCBOR
    18 -> pure (1, NoCollateralInputs)
    19 -> fmap (3,) $ IncorrectTotalCollateralField <$> decCBOR <*> decCBOR
    20 -> fmap (2,) $ BabbageOutputTooSmallUTxO <$> decCBOR
    21 -> fmap (2,) $ BabbageNonDisjointRefInputs <$> decCBOR
    22 -> fmap (2,) $ PtrPresentInCollateralReturn <$> decCBOR
    23 -> fmap (3,) $ WrongNetworkInDirectDeposit <$> decCBOR <*> decCBOR
    n -> invalidKey n

-- =====================================================
-- Injecting from one PredicateFailure to another

conwayToDijkstraUtxoPredFailure ::
  forall era.
  ConwayUtxoPredFailure era ->
  DijkstraUtxoPredFailure era
conwayToDijkstraUtxoPredFailure = \case
  Conway.BadInputsUTxO x -> BadInputsUTxO x
  Conway.OutsideValidityIntervalUTxO vi slotNo -> OutsideValidityIntervalUTxO vi slotNo
  Conway.MaxTxSizeUTxO m -> MaxTxSizeUTxO m
  Conway.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Conway.FeeTooSmallUTxO m -> FeeTooSmallUTxO m
  Conway.ValueNotConservedUTxO m -> ValueNotConservedUTxO m
  Conway.WrongNetwork x y -> WrongNetwork x y
  Conway.WrongNetworkWithdrawal x y -> WrongNetworkWithdrawal x y
  Conway.OutputTooSmallUTxO _ -> error "Impossible: `OutputTooSmallUTxO` for DijkstraUTXO"
  Conway.UtxosFailure x -> UtxosFailure x
  Conway.OutputBootAddrAttrsTooBig xs -> OutputBootAddrAttrsTooBig xs
  Conway.OutputTooBigUTxO xs -> OutputTooBigUTxO xs
  Conway.InsufficientCollateral c1 c2 -> InsufficientCollateral c1 c2
  Conway.ScriptsNotPaidUTxO u -> ScriptsNotPaidUTxO u
  Conway.ExUnitsTooBigUTxO m -> ExUnitsTooBigUTxO m
  Conway.CollateralContainsNonADA v -> CollateralContainsNonADA v
  Conway.WrongNetworkInTxBody m -> WrongNetworkInTxBody m
  Conway.OutsideForecast sno -> OutsideForecast sno
  Conway.TooManyCollateralInputs m -> TooManyCollateralInputs m
  Conway.NoCollateralInputs -> NoCollateralInputs
  Conway.IncorrectTotalCollateralField dc c -> IncorrectTotalCollateralField dc c
  Conway.BabbageOutputTooSmallUTxO txouts -> BabbageOutputTooSmallUTxO txouts
  Conway.BabbageNonDisjointRefInputs txin -> BabbageNonDisjointRefInputs txin
