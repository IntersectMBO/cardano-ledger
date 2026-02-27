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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubUtxo (
  DijkstraSUBUTXO,
  DijkstraSubUtxoPredFailure (..),
  DijkstraSubUtxoEvent (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayUtxoPredFailure, ConwayUtxosPredFailure)
import qualified Cardano.Ledger.Conway.Rules as Conway (ConwayUtxoPredFailure (..))
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBUTXO,
  DijkstraSUBUTXOS,
 )
import Cardano.Ledger.Dijkstra.Rules.SubUtxos (DijkstraSubUtxosPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import Cardano.Ledger.Shelley.Rules (UtxoEnv)
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

data DijkstraSubUtxoPredFailure era
  = SubUtxosFailure (PredicateFailure (EraRule "SUBUTXOS" era))
  | -- | The bad transaction inputs
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
  | -- | list of supplied transaction outputs that are too small
    SubOutputTooSmallUTxO (NonEmpty (TxOut era))
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
  deriving (Generic)

deriving stock instance
  ( Era era
  , Eq (Value era)
  , Eq (PredicateFailure (EraRule "SUBUTXOS" era))
  , Eq (TxOut era)
  , Eq (Script era)
  , Eq TxIn
  ) =>
  Eq (DijkstraSubUtxoPredFailure era)

deriving stock instance
  ( Era era
  , Show (Value era)
  , Show (PredicateFailure (EraRule "SUBUTXOS" era))
  , Show (TxOut era)
  , Show (Script era)
  , Show TxIn
  ) =>
  Show (DijkstraSubUtxoPredFailure era)

deriving via
  InspectHeapNamed "DijkstraUtxosPred" (DijkstraSubUtxoPredFailure era)
  instance
    NoThunks (DijkstraSubUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  , NFData (PredicateFailure (EraRule "SUBUTXOS" era))
  ) =>
  NFData (DijkstraSubUtxoPredFailure era)

type instance EraRuleFailure "SUBUTXO" DijkstraEra = DijkstraSubUtxoPredFailure DijkstraEra

type instance EraRuleEvent "SUBUTXO" DijkstraEra = DijkstraSubUtxoEvent DijkstraEra

instance InjectRuleFailure "SUBUTXO" DijkstraSubUtxoPredFailure DijkstraEra

instance InjectRuleFailure "SUBUTXO" DijkstraSubUtxosPredFailure DijkstraEra where
  injectFailure = SubUtxosFailure

instance InjectRuleFailure "SUBUTXO" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraSubUtxoPredFailure

instance InjectRuleFailure "SUBUTXO" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = dijkstraUtxoToDijkstraSubUtxoPredFailure

instance InjectRuleEvent "SUBUTXO" DijkstraSubUtxoEvent DijkstraEra

newtype DijkstraSubUtxoEvent era = SubUtxosEvent (Event (EraRule "SUBUTXOS" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBUTXOS" era)) => Eq (DijkstraSubUtxoEvent era)

instance NFData (Event (EraRule "SUBUTXOS" era)) => NFData (DijkstraSubUtxoEvent era)

instance
  ( ConwayEraGov era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , Embed (EraRule "SUBUTXOS" era) (DijkstraSUBUTXO era)
  , BabbageEraTxBody era
  ) =>
  STS (DijkstraSUBUTXO era)
  where
  type State (DijkstraSUBUTXO era) = UTxOState era
  type Signal (DijkstraSUBUTXO era) = Tx SubTx era
  type Environment (DijkstraSUBUTXO era) = UtxoEnv era
  type BaseM (DijkstraSUBUTXO era) = ShelleyBase
  type PredicateFailure (DijkstraSUBUTXO era) = DijkstraSubUtxoPredFailure era
  type Event (DijkstraSUBUTXO era) = DijkstraSubUtxoEvent era

  transitionRules = [dijkstraSubUtxoTransition @era]

dijkstraSubUtxoTransition ::
  forall era.
  ( EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , Embed (EraRule "SUBUTXOS" era) (DijkstraSUBUTXO era)
  ) =>
  TransitionRule (EraRule "SUBUTXO" era)
dijkstraSubUtxoTransition = do
  TRC (env, state, signal) <- judgmentContext
  trans @(EraRule "SUBUTXOS" era) $
    TRC (env, state, signal)

instance
  ( ConwayEraGov era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  ) =>
  Embed (DijkstraSUBUTXOS era) (DijkstraSUBUTXO era)
  where
  wrapFailed = SubUtxosFailure
  wrapEvent = SubUtxosEvent

instance
  ( Era era
  , EncCBOR (TxOut era)
  , EncCBOR (PredicateFailure (EraRule "SUBUTXOS" era))
  ) =>
  EncCBOR (DijkstraSubUtxoPredFailure era)
  where
  encCBOR =
    encode . \case
      SubUtxosFailure a -> Sum (SubUtxosFailure @era) 0 !> To a
      SubBadInputsUTxO ins -> Sum (SubBadInputsUTxO @era) 1 !> To ins
      SubOutsideValidityIntervalUTxO a b -> Sum SubOutsideValidityIntervalUTxO 2 !> To a !> To b
      SubMaxTxSizeUTxO mm -> Sum SubMaxTxSizeUTxO 3 !> To mm
      SubInputSetEmptyUTxO -> Sum SubInputSetEmptyUTxO 4
      SubWrongNetwork right wrongs -> Sum (SubWrongNetwork @era) 5 !> To right !> To wrongs
      SubWrongNetworkWithdrawal right wrongs -> Sum (SubWrongNetworkWithdrawal @era) 6 !> To right !> To wrongs
      SubOutputTooSmallUTxO outs -> Sum (SubOutputTooSmallUTxO @era) 7 !> To outs
      SubOutputBootAddrAttrsTooBig outs -> Sum (SubOutputBootAddrAttrsTooBig @era) 8 !> To outs
      SubOutputTooBigUTxO outs -> Sum (SubOutputTooBigUTxO @era) 9 !> To outs
      SubWrongNetworkInTxBody mm -> Sum SubWrongNetworkInTxBody 10 !> To mm
      SubOutsideForecast a -> Sum SubOutsideForecast 11 !> To a
      SubBabbageOutputTooSmallUTxO x -> Sum SubBabbageOutputTooSmallUTxO 12 !> To x

instance
  ( Era era
  , DecCBOR (TxOut era)
  , EncCBOR (Value era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "SUBUTXOS" era))
  ) =>
  DecCBOR (DijkstraSubUtxoPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraSubUtxoPredFailure" $ \case
    0 -> SumD SubUtxosFailure <! From
    1 -> SumD SubBadInputsUTxO <! From
    2 -> SumD SubOutsideValidityIntervalUTxO <! From <! From
    3 -> SumD SubMaxTxSizeUTxO <! From
    4 -> SumD SubInputSetEmptyUTxO
    5 -> SumD SubWrongNetwork <! From <! From
    6 -> SumD SubWrongNetworkWithdrawal <! From <! From
    7 -> SumD SubOutputTooSmallUTxO <! From
    8 -> SumD SubOutputBootAddrAttrsTooBig <! From
    9 -> SumD SubOutputTooBigUTxO <! From
    10 -> SumD SubWrongNetworkInTxBody <! From
    11 -> SumD SubOutsideForecast <! From
    12 -> SumD SubBabbageOutputTooSmallUTxO <! From
    n -> Invalid n

conwayToDijkstraSubUtxoPredFailure ::
  forall era.
  ( InjectRuleFailure "SUBUTXOS" ConwayUtxosPredFailure era
  , PredicateFailure (EraRule "UTXOS" era) ~ ConwayUtxosPredFailure era
  ) =>
  ConwayUtxoPredFailure era ->
  DijkstraSubUtxoPredFailure era
conwayToDijkstraSubUtxoPredFailure = \case
  Conway.UtxosFailure f -> SubUtxosFailure (injectFailure @"SUBUTXOS" f)
  Conway.BadInputsUTxO x -> SubBadInputsUTxO x
  Conway.OutsideValidityIntervalUTxO vi slotNo -> SubOutsideValidityIntervalUTxO vi slotNo
  Conway.MaxTxSizeUTxO m -> SubMaxTxSizeUTxO m
  Conway.InputSetEmptyUTxO -> SubInputSetEmptyUTxO
  Conway.FeeTooSmallUTxO _ -> error "Impossible: `FeeTooSmallUTxO` for SUBUTXO"
  Conway.ValueNotConservedUTxO _ -> error "Impossible: `ValueNotConservedUTxO` for SUBUTXO"
  Conway.WrongNetwork x y -> SubWrongNetwork x y
  Conway.WrongNetworkWithdrawal x y -> SubWrongNetworkWithdrawal x y
  Conway.OutputTooSmallUTxO x -> SubOutputTooSmallUTxO x
  Conway.OutputBootAddrAttrsTooBig xs -> SubOutputBootAddrAttrsTooBig xs
  Conway.OutputTooBigUTxO xs -> SubOutputTooBigUTxO xs
  Conway.InsufficientCollateral _ _ -> error "Impossible: `InsufficientCollateral` for SUBUTXO"
  Conway.ScriptsNotPaidUTxO _ -> error "Impossible: `ScriptsNotPaidUTxO` for SUBUTXO"
  Conway.ExUnitsTooBigUTxO _ -> error "Impossible: `ExUnitsTooBigUTxO` for SUBUTXO"
  Conway.CollateralContainsNonADA _ -> error "Impossible: `CollateralContainsNonADA` for SUBUTXO"
  Conway.WrongNetworkInTxBody m -> SubWrongNetworkInTxBody m
  Conway.OutsideForecast sno -> SubOutsideForecast sno
  Conway.TooManyCollateralInputs _ -> error "Impossible: `TooManyCollateralInputs` for SUBUTXO"
  Conway.NoCollateralInputs -> error "Impossible: `NoCollateralInputs` for SUBUTXO"
  Conway.IncorrectTotalCollateralField _ _ -> error "Impossible: `IncorrectTotalCollateralField` for SUBUTXO"
  Conway.BabbageOutputTooSmallUTxO txouts -> SubBabbageOutputTooSmallUTxO txouts
  Conway.BabbageNonDisjointRefInputs _ -> error "Impossible: `BabbageNonDisjointRefInputs2` for SUBUTXO"

dijkstraUtxoToDijkstraSubUtxoPredFailure ::
  forall era.
  ( InjectRuleFailure "SUBUTXOS" ConwayUtxosPredFailure era
  , PredicateFailure (EraRule "UTXOS" era) ~ ConwayUtxosPredFailure era
  ) =>
  DijkstraUtxoPredFailure era ->
  DijkstraSubUtxoPredFailure era
dijkstraUtxoToDijkstraSubUtxoPredFailure = \case
  UtxosFailure f -> SubUtxosFailure (injectFailure @"SUBUTXOS" f)
  BadInputsUTxO x -> SubBadInputsUTxO x
  OutsideValidityIntervalUTxO vi slotNo -> SubOutsideValidityIntervalUTxO vi slotNo
  MaxTxSizeUTxO m -> SubMaxTxSizeUTxO m
  InputSetEmptyUTxO -> SubInputSetEmptyUTxO
  FeeTooSmallUTxO _ -> error "Impossible: `FeeTooSmallUTxO` for SUBUTXO"
  ValueNotConservedUTxO _ -> error "Impossible: `ValueNotConservedUTxO` for SUBUTXO"
  WrongNetwork x y -> SubWrongNetwork x y
  WrongNetworkWithdrawal x y -> SubWrongNetworkWithdrawal x y
  OutputTooSmallUTxO x -> SubOutputTooSmallUTxO x
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
  BabbageOutputTooSmallUTxO txouts -> SubBabbageOutputTooSmallUTxO txouts
  BabbageNonDisjointRefInputs _ -> error "Impossible: `BabbageNonDisjointRefInputs` for SUBUTXO"
  PtrPresentInCollateralReturn _ -> error "Impossible: `PtrPresentInCollateralReturn` for SUBUTXO"
