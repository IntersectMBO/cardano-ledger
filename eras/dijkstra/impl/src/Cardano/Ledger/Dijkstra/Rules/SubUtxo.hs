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

import Cardano.Ledger.Address (
  Addr (..),
  RewardAccount,
 )
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin, DeltaCoin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBUTXO,
  DijkstraSUBUTXOS,
 )
import Cardano.Ledger.Dijkstra.Rules.SubUtxos (DijkstraSubUtxosPredFailure)
import Cardano.Ledger.Plutus (ExUnits)
import Cardano.Ledger.Shelley.LedgerState (UTxO (..), UTxOState)
import Cardano.Ledger.Shelley.Rules (UtxoEnv)
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
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
  | SubFeeTooSmallUTxO
      (Mismatch RelGTEQ Coin)
  | SubValueNotConservedUTxO
      (Mismatch RelEQ (Value era)) -- Serialise consumed first, then produced
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
      (NonEmptySet RewardAccount)
  | -- | list of supplied transaction outputs that are too small
    SubOutputTooSmallUTxO (NonEmpty (TxOut era))
  | -- | list of supplied bad transaction outputs
    SubOutputBootAddrAttrsTooBig (NonEmpty (TxOut era))
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    SubOutputTooBigUTxO (NonEmpty (Int, Int, TxOut era))
  | SubInsufficientCollateral
      -- | balance computed
      DeltaCoin
      -- | the required collateral for the given fee
      Coin
  | -- | The UTxO entries which have the wrong kind of script
    SubScriptsNotPaidUTxO (UTxO era)
  | SubExUnitsTooBigUTxO
      (Mismatch RelLTEQ ExUnits)
  | -- | The inputs marked for use as fees contain non-ADA tokens
    SubCollateralContainsNonADA (Value era)
  | -- | Wrong Network ID in body
    SubWrongNetworkInTxBody
      (Mismatch RelEQ Network)
  | -- | slot number outside consensus forecast range
    SubOutsideForecast SlotNo
  | -- | There are too many collateral inputs
    SubTooManyCollateralInputs
      (Mismatch RelLTEQ Natural)
  | SubNoCollateralInputs
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    SubIncorrectTotalCollateralField
      -- | collateral provided
      DeltaCoin
      -- | collateral amount declared in transaction body
      Coin
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    SubBabbageOutputTooSmallUTxO (NonEmpty (TxOut era, Coin))
  | -- | TxIns that appear in both inputs and reference inputs
    SubBabbageNonDisjointRefInputs (NonEmpty TxIn)
  | SubPtrPresentInCollateralReturn (TxOut era)
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
  , EncCBOR (Value era)
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
      SubFeeTooSmallUTxO mm -> Sum SubFeeTooSmallUTxO 5 !> To mm
      SubValueNotConservedUTxO mm -> Sum (SubValueNotConservedUTxO @era) 6 !> To mm
      SubWrongNetwork right wrongs -> Sum (SubWrongNetwork @era) 7 !> To right !> To wrongs
      SubWrongNetworkWithdrawal right wrongs -> Sum (SubWrongNetworkWithdrawal @era) 8 !> To right !> To wrongs
      SubOutputTooSmallUTxO outs -> Sum (SubOutputTooSmallUTxO @era) 9 !> To outs
      SubOutputBootAddrAttrsTooBig outs -> Sum (SubOutputBootAddrAttrsTooBig @era) 10 !> To outs
      SubOutputTooBigUTxO outs -> Sum (SubOutputTooBigUTxO @era) 11 !> To outs
      SubInsufficientCollateral a b -> Sum SubInsufficientCollateral 12 !> To a !> To b
      SubScriptsNotPaidUTxO a -> Sum SubScriptsNotPaidUTxO 13 !> To a
      SubExUnitsTooBigUTxO mm -> Sum SubExUnitsTooBigUTxO 14 !> To mm
      SubCollateralContainsNonADA a -> Sum SubCollateralContainsNonADA 15 !> To a
      SubWrongNetworkInTxBody mm -> Sum SubWrongNetworkInTxBody 16 !> To mm
      SubOutsideForecast a -> Sum SubOutsideForecast 17 !> To a
      SubTooManyCollateralInputs mm -> Sum SubTooManyCollateralInputs 18 !> To mm
      SubNoCollateralInputs -> Sum SubNoCollateralInputs 19
      SubIncorrectTotalCollateralField c1 c2 -> Sum SubIncorrectTotalCollateralField 20 !> To c1 !> To c2
      SubBabbageOutputTooSmallUTxO x -> Sum SubBabbageOutputTooSmallUTxO 21 !> To x
      SubBabbageNonDisjointRefInputs x -> Sum SubBabbageNonDisjointRefInputs 22 !> To x
      SubPtrPresentInCollateralReturn x -> Sum SubPtrPresentInCollateralReturn 23 !> To x

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
    5 -> SumD SubFeeTooSmallUTxO <! From
    6 -> SumD SubValueNotConservedUTxO <! From
    7 -> SumD SubWrongNetwork <! From <! From
    8 -> SumD SubWrongNetworkWithdrawal <! From <! From
    9 -> SumD SubOutputTooSmallUTxO <! From
    10 -> SumD SubOutputBootAddrAttrsTooBig <! From
    11 -> SumD SubOutputTooBigUTxO <! From
    12 -> SumD SubInsufficientCollateral <! From <! From
    13 -> SumD SubScriptsNotPaidUTxO <! From
    14 -> SumD SubExUnitsTooBigUTxO <! From
    15 -> SumD SubCollateralContainsNonADA <! From
    16 -> SumD SubWrongNetworkInTxBody <! From
    17 -> SumD SubOutsideForecast <! From
    18 -> SumD SubTooManyCollateralInputs <! From
    19 -> SumD SubNoCollateralInputs
    20 -> SumD SubIncorrectTotalCollateralField <! From <! From
    21 -> SumD SubBabbageOutputTooSmallUTxO <! From
    22 -> SumD SubBabbageNonDisjointRefInputs <! From
    23 -> SumD SubPtrPresentInCollateralReturn <! From
    n -> Invalid n
