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

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra (
  validateOutsideValidityIntervalUTxO,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  ConwayUtxoPredFailure,
  allegraToConwayUtxoPredFailure,
 )
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBUTXO,
 )
import Cardano.Ledger.Dijkstra.Rules.Utxo (
  DijkstraUtxoPredFailure (..),
  conwayToDijkstraUtxoPredFailure,
 )
import Cardano.Ledger.Rules.ValidationMode
import Cardano.Ledger.Shelley.LedgerState (UTxO, UTxOState)
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

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

deriving via
  InspectHeapNamed "DijkstraUtxosPred" (DijkstraSubUtxoPredFailure era)
  instance
    NoThunks (DijkstraSubUtxoPredFailure era)

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

instance InjectRuleFailure "SUBUTXO" AllegraUtxoPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxoToDijkstraSubUtxoPredFailure
      . conwayToDijkstraUtxoPredFailure
      . allegraToConwayUtxoPredFailure

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
  , BabbageEraTxBody era
  , ConwayEraGov era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , InjectRuleFailure "SUBUTXO" AllegraUtxoPredFailure era
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
  ( EraTx era
  , AllegraEraTxBody era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , InjectRuleFailure "SUBUTXO" AllegraUtxoPredFailure era
  ) =>
  TransitionRule (EraRule "SUBUTXO" era)
dijkstraSubUtxoTransition = do
  TRC (UtxoEnv slot _ _, utxoState, tx) <- judgmentContext

  let txBody = tx ^. bodyTxL

  runTest $ Allegra.validateOutsideValidityIntervalUTxO slot txBody

  pure utxoState

instance
  ( Era era
  , EncCBOR (TxOut era)
  ) =>
  EncCBOR (DijkstraSubUtxoPredFailure era)
  where
  encCBOR =
    encode . \case
      SubBadInputsUTxO ins -> Sum (SubBadInputsUTxO @era) 0 !> To ins
      SubOutsideValidityIntervalUTxO a b -> Sum SubOutsideValidityIntervalUTxO 1 !> To a !> To b
      SubMaxTxSizeUTxO mm -> Sum SubMaxTxSizeUTxO 2 !> To mm
      SubInputSetEmptyUTxO -> Sum SubInputSetEmptyUTxO 3
      SubWrongNetwork right wrongs -> Sum (SubWrongNetwork @era) 4 !> To right !> To wrongs
      SubWrongNetworkWithdrawal right wrongs -> Sum (SubWrongNetworkWithdrawal @era) 5 !> To right !> To wrongs
      SubOutputBootAddrAttrsTooBig outs -> Sum (SubOutputBootAddrAttrsTooBig @era) 6 !> To outs
      SubOutputTooBigUTxO outs -> Sum (SubOutputTooBigUTxO @era) 7 !> To outs
      SubWrongNetworkInTxBody mm -> Sum SubWrongNetworkInTxBody 8 !> To mm
      SubOutsideForecast a -> Sum SubOutsideForecast 9 !> To a
      SubBabbageOutputTooSmallUTxO x -> Sum SubBabbageOutputTooSmallUTxO 10 !> To x

instance
  ( Era era
  , DecCBOR (TxOut era)
  , EncCBOR (Value era)
  , DecCBOR (Value era)
  ) =>
  DecCBOR (DijkstraSubUtxoPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraSubUtxoPredFailure" $ \case
    0 -> SumD SubBadInputsUTxO <! From
    1 -> SumD SubOutsideValidityIntervalUTxO <! From <! From
    2 -> SumD SubMaxTxSizeUTxO <! From
    3 -> SumD SubInputSetEmptyUTxO
    4 -> SumD SubWrongNetwork <! From <! From
    5 -> SumD SubWrongNetworkWithdrawal <! From <! From
    6 -> SumD SubOutputBootAddrAttrsTooBig <! From
    7 -> SumD SubOutputTooBigUTxO <! From
    8 -> SumD SubWrongNetworkInTxBody <! From
    9 -> SumD SubOutsideForecast <! From
    10 -> SumD SubBabbageOutputTooSmallUTxO <! From
    n -> Invalid n

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
  BabbageOutputTooSmallUTxO txouts -> SubBabbageOutputTooSmallUTxO txouts
  BabbageNonDisjointRefInputs _ -> error "Impossible: `BabbageNonDisjointRefInputs` for SUBUTXO"
  PtrPresentInCollateralReturn _ -> error "Impossible: `PtrPresentInCollateralReturn` for SUBUTXO"
