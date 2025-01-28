{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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

module Cardano.Ledger.Conway.Rules.Utxo (
  allegraToConwayUtxoPredFailure,
  babbageToConwayUtxoPredFailure,
  alonzoToConwayUtxoPredFailure,
  ConwayUtxoPredFailure (..),
) where

import Cardano.Ledger.Address (Addr, RewardAccount)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure, shelleyToAllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra (AllegraUtxoPredFailure (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (
  AlonzoUtxoEvent (UtxosEvent),
  AlonzoUtxoPredFailure (..),
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)
import qualified Cardano.Ledger.Babbage.Rules as Babbage (
  BabbageUtxoPredFailure (..),
  utxoTransition,
 )
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Network,
  Relation (..),
  ShelleyBase,
  SlotNo,
  swapMismatch,
  unswapMismatch,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
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
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayUTXO, ConwayUTXOS)
import Cardano.Ledger.Conway.Rules.Utxos (
  ConwayUtxosPredFailure (..),
 )
import Cardano.Ledger.Plutus (ExUnits)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley (UTxOState)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley (UtxoEnv)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (EraUTxO, UTxO (..))
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (Embed (..), STS (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

-- ======================================================

-- | Predicate failure for the Conway Era
data ConwayUtxoPredFailure era
  = -- | Subtransition Failures
    UtxosFailure (PredicateFailure (EraRule "UTXOS" era))
  | -- | The bad transaction inputs
    BadInputsUTxO
      (Set TxIn)
  | OutsideValidityIntervalUTxO
      -- | transaction's validity interval
      ValidityInterval
      -- | current slot
      SlotNo
  | MaxTxSizeUTxO
      (Mismatch 'RelLTEQ Integer)
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      (Mismatch 'RelGTEQ Coin) -- The values are serialised in reverse order
  | ValueNotConservedUTxO
      (Mismatch 'RelEQ (Value era)) -- Serialise consumed first, then produced
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      -- | the expected network id
      Network
      -- | the set of addresses with incorrect network IDs
      (Set Addr)
  | WrongNetworkWithdrawal
      -- | the expected network id
      Network
      -- | the set of reward addresses with incorrect network IDs
      (Set RewardAccount)
  | -- | list of supplied transaction outputs that are too small
    OutputTooSmallUTxO
      [TxOut era]
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig
      [TxOut era]
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO
      [(Int, Int, TxOut era)]
  | InsufficientCollateral
      -- | balance computed
      DeltaCoin
      -- | the required collateral for the given fee
      Coin
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO
      (UTxO era)
  | ExUnitsTooBigUTxO
      (Mismatch 'RelLTEQ ExUnits) -- The values are serialised in reverse order
  | -- | The inputs marked for use as fees contain non-ADA tokens
    CollateralContainsNonADA (Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody
      (Mismatch 'RelEQ Network) -- The values are serialised in reverse order
  | -- | slot number outside consensus forecast range
    OutsideForecast
      SlotNo
  | -- | There are too many collateral inputs
    TooManyCollateralInputs
      (Mismatch 'RelLTEQ Natural) -- The values are serialised in reverse order
  | NoCollateralInputs
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    IncorrectTotalCollateralField
      -- | collateral provided
      DeltaCoin
      -- | collateral amount declared in transaction body
      Coin
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    BabbageOutputTooSmallUTxO
      [(TxOut era, Coin)]
  | -- | TxIns that appear in both inputs and reference inputs
    BabbageNonDisjointRefInputs
      (NonEmpty TxIn)
  deriving (Generic)

type instance EraRuleFailure "UTXO" ConwayEra = ConwayUtxoPredFailure ConwayEra

type instance EraRuleEvent "UTXO" ConwayEra = AlonzoUtxoEvent ConwayEra

instance InjectRuleFailure "UTXO" ConwayUtxoPredFailure ConwayEra

instance InjectRuleFailure "UTXO" BabbageUtxoPredFailure ConwayEra where
  injectFailure = babbageToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" AlonzoUtxoPredFailure ConwayEra where
  injectFailure = alonzoToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure ConwayEra where
  injectFailure =
    allegraToConwayUtxoPredFailure
      . shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure ConwayEra where
  injectFailure = allegraToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ConwayUtxosPredFailure ConwayEra where
  injectFailure = UtxosFailure

instance InjectRuleFailure "UTXO" AlonzoUtxosPredFailure ConwayEra where
  injectFailure =
    alonzoToConwayUtxoPredFailure
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
  Show (ConwayUtxoPredFailure era)

deriving instance
  ( Era era
  , Eq (Value era)
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Eq (TxOut era)
  , Eq (Script era)
  , Eq TxIn
  ) =>
  Eq (ConwayUtxoPredFailure era)

deriving via
  InspectHeapNamed "ConwayUtxoPred" (ConwayUtxoPredFailure era)
  instance
    NoThunks (ConwayUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  , NFData (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  NFData (ConwayUtxoPredFailure era)

--------------------------------------------------------------------------------
-- ConwayUTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( EraTx era
  , EraUTxO era
  , ConwayEraTxBody era
  , AlonzoEraTxWits era
  , Tx era ~ AlonzoTx era
  , EraRule "UTXO" era ~ ConwayUTXO era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , InjectRuleFailure "UTXO" ConwayUtxoPredFailure era
  , Embed (EraRule "UTXOS" era) (ConwayUTXO era)
  , Environment (EraRule "UTXOS" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXOS" era) ~ Shelley.UTxOState era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , PredicateFailure (EraRule "UTXO" era) ~ ConwayUtxoPredFailure era
  ) =>
  STS (ConwayUTXO era)
  where
  type State (ConwayUTXO era) = Shelley.UTxOState era
  type Signal (ConwayUTXO era) = AlonzoTx era
  type Environment (ConwayUTXO era) = Shelley.UtxoEnv era
  type BaseM (ConwayUTXO era) = ShelleyBase
  type PredicateFailure (ConwayUTXO era) = ConwayUtxoPredFailure era
  type Event (ConwayUTXO era) = AlonzoUtxoEvent era

  initialRules = []

  transitionRules = [Babbage.utxoTransition @era]

instance
  ( Era era
  , STS (ConwayUTXOS era)
  , PredicateFailure (EraRule "UTXOS" era) ~ ConwayUtxosPredFailure era
  , Event (EraRule "UTXOS" era) ~ Event (ConwayUTXOS era)
  ) =>
  Embed (ConwayUTXOS era) (ConwayUTXO era)
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
  EncCBOR (ConwayUtxoPredFailure era)
  where
  encCBOR =
    encode . \case
      UtxosFailure a -> Sum (UtxosFailure @era) 0 !> To a
      BadInputsUTxO ins -> Sum (BadInputsUTxO @era) 1 !> To ins
      OutsideValidityIntervalUTxO a b -> Sum OutsideValidityIntervalUTxO 2 !> To a !> To b
      MaxTxSizeUTxO mm -> Sum MaxTxSizeUTxO 3 !> ToGroup mm
      InputSetEmptyUTxO -> Sum InputSetEmptyUTxO 4
      FeeTooSmallUTxO mm -> Sum (FeeTooSmallUTxO . unswapMismatch) 5 !> ToGroup (swapMismatch mm)
      ValueNotConservedUTxO mm -> Sum (ValueNotConservedUTxO @era) 6 !> ToGroup mm
      WrongNetwork right wrongs -> Sum (WrongNetwork @era) 7 !> To right !> To wrongs
      WrongNetworkWithdrawal right wrongs -> Sum (WrongNetworkWithdrawal @era) 8 !> To right !> To wrongs
      OutputTooSmallUTxO outs -> Sum (OutputTooSmallUTxO @era) 9 !> To outs
      OutputBootAddrAttrsTooBig outs -> Sum (OutputBootAddrAttrsTooBig @era) 10 !> To outs
      OutputTooBigUTxO outs -> Sum (OutputTooBigUTxO @era) 11 !> To outs
      InsufficientCollateral a b -> Sum InsufficientCollateral 12 !> To a !> To b
      ScriptsNotPaidUTxO a -> Sum ScriptsNotPaidUTxO 13 !> To a
      ExUnitsTooBigUTxO mm -> Sum (ExUnitsTooBigUTxO . unswapMismatch) 14 !> ToGroup (swapMismatch mm)
      CollateralContainsNonADA a -> Sum CollateralContainsNonADA 15 !> To a
      WrongNetworkInTxBody mm -> Sum (WrongNetworkInTxBody . unswapMismatch) 16 !> ToGroup (swapMismatch mm)
      OutsideForecast a -> Sum OutsideForecast 17 !> To a
      TooManyCollateralInputs mm -> Sum (TooManyCollateralInputs . unswapMismatch) 18 !> ToGroup (swapMismatch mm)
      NoCollateralInputs -> Sum NoCollateralInputs 19
      IncorrectTotalCollateralField c1 c2 -> Sum IncorrectTotalCollateralField 20 !> To c1 !> To c2
      BabbageOutputTooSmallUTxO x -> Sum BabbageOutputTooSmallUTxO 21 !> To x
      BabbageNonDisjointRefInputs x -> Sum BabbageNonDisjointRefInputs 22 !> To x

instance
  ( Era era
  , DecCBOR (TxOut era)
  , EncCBOR (Value era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  DecCBOR (ConwayUtxoPredFailure era)
  where
  decCBOR = decode . Summands "ConwayUtxoPredFailure" $ \case
    0 -> SumD UtxosFailure <! From
    1 -> SumD BadInputsUTxO <! From
    2 -> SumD OutsideValidityIntervalUTxO <! From <! From
    3 -> SumD MaxTxSizeUTxO <! FromGroup
    4 -> SumD InputSetEmptyUTxO
    5 -> SumD FeeTooSmallUTxO <! (unswapMismatch <$> FromGroup)
    6 -> SumD ValueNotConservedUTxO <! FromGroup
    7 -> SumD WrongNetwork <! From <! From
    8 -> SumD WrongNetworkWithdrawal <! From <! From
    9 -> SumD OutputTooSmallUTxO <! From
    10 -> SumD OutputBootAddrAttrsTooBig <! From
    11 -> SumD OutputTooBigUTxO <! From
    12 -> SumD InsufficientCollateral <! From <! From
    13 -> SumD ScriptsNotPaidUTxO <! D (UTxO <$> decCBOR)
    14 -> SumD ExUnitsTooBigUTxO <! (unswapMismatch <$> FromGroup)
    15 -> SumD CollateralContainsNonADA <! From
    16 -> SumD WrongNetworkInTxBody <! (unswapMismatch <$> FromGroup)
    17 -> SumD OutsideForecast <! From
    18 -> SumD TooManyCollateralInputs <! (unswapMismatch <$> FromGroup)
    19 -> SumD NoCollateralInputs
    20 -> SumD IncorrectTotalCollateralField <! From <! From
    21 -> SumD BabbageOutputTooSmallUTxO <! From
    22 -> SumD BabbageNonDisjointRefInputs <! From
    n -> Invalid n

-- =====================================================
-- Injecting from one PredicateFailure to another

babbageToConwayUtxoPredFailure ::
  forall era.
  BabbageUtxoPredFailure era ->
  ConwayUtxoPredFailure era
babbageToConwayUtxoPredFailure = \case
  Babbage.AlonzoInBabbageUtxoPredFailure a -> alonzoToConwayUtxoPredFailure a
  Babbage.IncorrectTotalCollateralField c1 c2 -> IncorrectTotalCollateralField c1 c2
  Babbage.BabbageOutputTooSmallUTxO ts -> BabbageOutputTooSmallUTxO ts
  Babbage.BabbageNonDisjointRefInputs ts -> BabbageNonDisjointRefInputs ts

alonzoToConwayUtxoPredFailure ::
  forall era.
  AlonzoUtxoPredFailure era ->
  ConwayUtxoPredFailure era
alonzoToConwayUtxoPredFailure = \case
  Alonzo.BadInputsUTxO x -> BadInputsUTxO x
  Alonzo.OutsideValidityIntervalUTxO vi slotNo -> OutsideValidityIntervalUTxO vi slotNo
  Alonzo.MaxTxSizeUTxO m -> MaxTxSizeUTxO m
  Alonzo.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Alonzo.FeeTooSmallUTxO m -> FeeTooSmallUTxO m
  Alonzo.ValueNotConservedUTxO m -> ValueNotConservedUTxO m
  Alonzo.WrongNetwork x y -> WrongNetwork x y
  Alonzo.WrongNetworkWithdrawal x y -> WrongNetworkWithdrawal x y
  Alonzo.OutputTooSmallUTxO x -> OutputTooSmallUTxO x
  Alonzo.UtxosFailure x -> UtxosFailure x
  Alonzo.OutputBootAddrAttrsTooBig xs -> OutputBootAddrAttrsTooBig xs
  Alonzo.TriesToForgeADA ->
    error
      "Impossible case, soon to be removed. See: https://github.com/IntersectMBO/cardano-ledger/issues/4085"
  Alonzo.OutputTooBigUTxO xs ->
    let
      -- TODO: Remove this once the other eras will make the switch from Integer to Int
      -- as per #4015.
      -- https://github.com/IntersectMBO/cardano-ledger/issues/4085
      toRestricted :: (Integer, Integer, TxOut era) -> (Int, Int, TxOut era)
      toRestricted (sz, mv, out) = (fromIntegral sz, fromIntegral mv, out)
     in
      OutputTooBigUTxO $ map toRestricted xs
  Alonzo.InsufficientCollateral c1 c2 -> InsufficientCollateral c1 c2
  Alonzo.ScriptsNotPaidUTxO u -> ScriptsNotPaidUTxO u
  Alonzo.ExUnitsTooBigUTxO m -> ExUnitsTooBigUTxO m
  Alonzo.CollateralContainsNonADA v -> CollateralContainsNonADA v
  Alonzo.WrongNetworkInTxBody m -> WrongNetworkInTxBody m
  Alonzo.OutsideForecast sno -> OutsideForecast sno
  Alonzo.TooManyCollateralInputs m -> TooManyCollateralInputs m
  Alonzo.NoCollateralInputs -> NoCollateralInputs

allegraToConwayUtxoPredFailure ::
  forall era.
  EraRuleFailure "PPUP" era ~ VoidEraRule "PPUP" era =>
  Allegra.AllegraUtxoPredFailure era ->
  ConwayUtxoPredFailure era
allegraToConwayUtxoPredFailure = \case
  Allegra.BadInputsUTxO x -> BadInputsUTxO x
  Allegra.OutsideValidityIntervalUTxO vi slotNo -> OutsideValidityIntervalUTxO vi slotNo
  Allegra.MaxTxSizeUTxO m -> MaxTxSizeUTxO m
  Allegra.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Allegra.FeeTooSmallUTxO m -> FeeTooSmallUTxO m
  Allegra.ValueNotConservedUTxO m -> ValueNotConservedUTxO m
  Allegra.WrongNetwork x y -> WrongNetwork x y
  Allegra.WrongNetworkWithdrawal x y -> WrongNetworkWithdrawal x y
  Allegra.OutputTooSmallUTxO x -> OutputTooSmallUTxO x
  Allegra.UpdateFailure x -> absurdEraRule @"PPUP" @era x
  Allegra.OutputBootAddrAttrsTooBig xs -> OutputBootAddrAttrsTooBig xs
  Allegra.TriesToForgeADA ->
    error
      "Impossible case, soon to be removed. See: https://github.com/IntersectMBO/cardano-ledger/issues/4085"
  Allegra.OutputTooBigUTxO xs -> OutputTooBigUTxO (map (0,0,) xs)
