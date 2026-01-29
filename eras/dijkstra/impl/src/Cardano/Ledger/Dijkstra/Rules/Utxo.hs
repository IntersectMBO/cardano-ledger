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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxo (
  DijkstraUTXO,
  DijkstraUtxoPredFailure (..),
  conwayToDijkstraUtxoPredFailure,
) where

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
 )
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Network,
  Relation (..),
  ShelleyBase,
  SlotNo,
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin, DeltaCoin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayUTXOS,
  ConwayUtxoPredFailure,
  ConwayUtxosPredFailure (..),
  UtxoEnv,
  allegraToConwayUtxoPredFailure,
  alonzoToConwayUtxoPredFailure,
  babbageToConwayUtxoPredFailure,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraUTXO)
import Cardano.Ledger.Dijkstra.Rules.Utxos ()
import Cardano.Ledger.Plutus (ExUnits)
import Cardano.Ledger.Rules.ValidationMode (failOnJustStatic)
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley (UTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley (UtxoEnv, validSizeComputationCheck)
import Cardano.Ledger.State (
  EraCertState (..),
  EraUTxO,
 )
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  Embed (..),
  Rule,
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Lens.Micro ((^.))
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

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
  | -- | list of supplied transaction outputs that are too small
    OutputTooSmallUTxO (NonEmpty (TxOut era))
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
      (Mismatch RelLTEQ Natural)
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

deriving via
  InspectHeapNamed "ConwayUtxoPred" (DijkstraUtxoPredFailure era)
  instance
    NoThunks (DijkstraUtxoPredFailure era)

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

utxoTransition ::
  forall era.
  ( EraUTxO era
  , EraCertState era
  , BabbageEraTxBody era
  , AlonzoEraTxWits era
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
  , -- In this function we we call the UTXOS rule, so we need some assumptions
    Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Signal (EraRule "UTXOS" era) ~ Tx TopTx era
  , Embed (EraRule "UTXOS" era) (EraRule "UTXO" era)
  ) =>
  TransitionRule (EraRule "UTXO" era)
utxoTransition = do
  TRC (_, _, tx) <- judgmentContext
  babbageUtxoValidation

  validateNoPtrInCollateralReturn $ tx ^. bodyTxL

  trans @(EraRule "UTXOS" era) =<< coerce <$> judgmentContext

instance
  forall era.
  ( EraTx era
  , EraUTxO era
  , ConwayEraTxBody era
  , AlonzoEraTxWits era
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
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Signal (EraRule "UTXOS" era) ~ Tx TopTx era
  , EraCertState era
  , EraRule "UTXO" era ~ DijkstraUTXO era
  , SafeToHash (TxWits era)
  ) =>
  STS (DijkstraUTXO era)
  where
  type State (DijkstraUTXO era) = Shelley.UTxOState era
  type Signal (DijkstraUTXO era) = Tx TopTx era
  type Environment (DijkstraUTXO era) = Shelley.UtxoEnv era
  type BaseM (DijkstraUTXO era) = ShelleyBase
  type PredicateFailure (DijkstraUTXO era) = DijkstraUtxoPredFailure era
  type Event (DijkstraUTXO era) = AlonzoUtxoEvent era

  initialRules = []

  transitionRules = [utxoTransition @era]

  assertions = [Shelley.validSizeComputationCheck]

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
    encode . \case
      UtxosFailure a -> Sum (UtxosFailure @era) 0 !> To a
      BadInputsUTxO ins -> Sum (BadInputsUTxO @era) 1 !> To ins
      OutsideValidityIntervalUTxO a b -> Sum OutsideValidityIntervalUTxO 2 !> To a !> To b
      MaxTxSizeUTxO mm -> Sum MaxTxSizeUTxO 3 !> To mm
      InputSetEmptyUTxO -> Sum InputSetEmptyUTxO 4
      FeeTooSmallUTxO mm -> Sum FeeTooSmallUTxO 5 !> To mm
      ValueNotConservedUTxO mm -> Sum (ValueNotConservedUTxO @era) 6 !> To mm
      WrongNetwork right wrongs -> Sum (WrongNetwork @era) 7 !> To right !> To wrongs
      WrongNetworkWithdrawal right wrongs -> Sum (WrongNetworkWithdrawal @era) 8 !> To right !> To wrongs
      OutputTooSmallUTxO outs -> Sum (OutputTooSmallUTxO @era) 9 !> To outs
      OutputBootAddrAttrsTooBig outs -> Sum (OutputBootAddrAttrsTooBig @era) 10 !> To outs
      OutputTooBigUTxO outs -> Sum (OutputTooBigUTxO @era) 11 !> To outs
      InsufficientCollateral a b -> Sum InsufficientCollateral 12 !> To a !> To b
      ScriptsNotPaidUTxO a -> Sum ScriptsNotPaidUTxO 13 !> To a
      ExUnitsTooBigUTxO mm -> Sum ExUnitsTooBigUTxO 14 !> To mm
      CollateralContainsNonADA a -> Sum CollateralContainsNonADA 15 !> To a
      WrongNetworkInTxBody mm -> Sum WrongNetworkInTxBody 16 !> To mm
      OutsideForecast a -> Sum OutsideForecast 17 !> To a
      TooManyCollateralInputs mm -> Sum TooManyCollateralInputs 18 !> To mm
      NoCollateralInputs -> Sum NoCollateralInputs 19
      IncorrectTotalCollateralField c1 c2 -> Sum IncorrectTotalCollateralField 20 !> To c1 !> To c2
      BabbageOutputTooSmallUTxO x -> Sum BabbageOutputTooSmallUTxO 21 !> To x
      BabbageNonDisjointRefInputs x -> Sum BabbageNonDisjointRefInputs 22 !> To x
      PtrPresentInCollateralReturn x -> Sum PtrPresentInCollateralReturn 23 !> To x

instance
  ( Era era
  , DecCBOR (TxOut era)
  , EncCBOR (Value era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  DecCBOR (DijkstraUtxoPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraUtxoPredFailure" $ \case
    0 -> SumD UtxosFailure <! From
    1 -> SumD BadInputsUTxO <! From
    2 -> SumD OutsideValidityIntervalUTxO <! From <! From
    3 -> SumD MaxTxSizeUTxO <! From
    4 -> SumD InputSetEmptyUTxO
    5 -> SumD FeeTooSmallUTxO <! From
    6 -> SumD ValueNotConservedUTxO <! From
    7 -> SumD WrongNetwork <! From <! From
    8 -> SumD WrongNetworkWithdrawal <! From <! From
    9 -> SumD OutputTooSmallUTxO <! From
    10 -> SumD OutputBootAddrAttrsTooBig <! From
    11 -> SumD OutputTooBigUTxO <! From
    12 -> SumD InsufficientCollateral <! From <! From
    13 -> SumD ScriptsNotPaidUTxO <! From
    14 -> SumD ExUnitsTooBigUTxO <! From
    15 -> SumD CollateralContainsNonADA <! From
    16 -> SumD WrongNetworkInTxBody <! From
    17 -> SumD OutsideForecast <! From
    18 -> SumD TooManyCollateralInputs <! From
    19 -> SumD NoCollateralInputs
    20 -> SumD IncorrectTotalCollateralField <! From <! From
    21 -> SumD BabbageOutputTooSmallUTxO <! From
    22 -> SumD BabbageNonDisjointRefInputs <! From
    23 -> SumD PtrPresentInCollateralReturn <! From
    n -> Invalid n

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
  Conway.OutputTooSmallUTxO x -> OutputTooSmallUTxO x
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
