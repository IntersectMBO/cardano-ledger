{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Cardano.Ledger.BaseTypes (Network, ShelleyBase, SlotNo)
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
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
 )
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

-- ======================================================

-- | Predicate failure for the Conway Era
data ConwayUtxoPredFailure era
  = -- | Subtransition Failures
    UtxosFailure (PredicateFailure (EraRule "UTXOS" era))
  | -- | The bad transaction inputs
    BadInputsUTxO
      !(Set (TxIn (EraCrypto era)))
  | OutsideValidityIntervalUTxO
      -- | transaction's validity interval
      !ValidityInterval
      -- | current slot
      !SlotNo
  | MaxTxSizeUTxO
      -- | the actual transaction size
      !Integer
      -- | the max transaction size
      !Integer
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      -- | the minimum fee for this transaction
      !Coin
      -- | the fee supplied in this transaction
      !Coin
  | ValueNotConservedUTxO
      -- | the Coin consumed by this transaction
      !(Value era)
      -- | the Coin produced by this transaction
      !(Value era)
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      -- | the expected network id
      !Network
      -- | the set of addresses with incorrect network IDs
      !(Set (Addr (EraCrypto era)))
  | WrongNetworkWithdrawal
      -- | the expected network id
      !Network
      -- | the set of reward addresses with incorrect network IDs
      !(Set (RewardAccount (EraCrypto era)))
  | -- | list of supplied transaction outputs that are too small
    OutputTooSmallUTxO
      ![TxOut era]
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig
      ![TxOut era]
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO
      ![(Int, Int, TxOut era)]
  | InsufficientCollateral
      -- | balance computed
      !DeltaCoin
      -- | the required collateral for the given fee
      !Coin
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO
      !(UTxO era)
  | ExUnitsTooBigUTxO
      -- | Max EXUnits from the protocol parameters
      !ExUnits
      -- | EXUnits supplied
      !ExUnits
  | -- | The inputs marked for use as fees contain non-ADA tokens
    CollateralContainsNonADA !(Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody
      -- | Actual Network ID
      !Network
      -- | Network ID in transaction body
      !Network
  | -- | slot number outside consensus forecast range
    OutsideForecast
      !SlotNo
  | -- | There are too many collateral inputs
    TooManyCollateralInputs
      -- | Max allowed collateral inputs
      !Natural
      -- | Number of collateral inputs
      !Natural
  | NoCollateralInputs
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    IncorrectTotalCollateralField
      -- | collateral provided
      !DeltaCoin
      -- | collateral amount declared in transaction body
      !Coin
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    BabbageOutputTooSmallUTxO
      ![(TxOut era, Coin)]
  | -- | TxIns that appear in both inputs and reference inputs
    BabbageNonDisjointRefInputs
      !(NonEmpty (TxIn (EraCrypto era)))
  deriving (Generic)

type instance EraRuleFailure "UTXO" (ConwayEra c) = ConwayUtxoPredFailure (ConwayEra c)

type instance EraRuleEvent "UTXO" (ConwayEra c) = AlonzoUtxoEvent (ConwayEra c)

instance InjectRuleFailure "UTXO" ConwayUtxoPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXO" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = babbageToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = alonzoToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure =
    allegraToConwayUtxoPredFailure
      . shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = allegraToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = UtxosFailure

instance InjectRuleFailure "UTXO" AlonzoUtxosPredFailure (ConwayEra c) where
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
  , Show (TxIn (EraCrypto era))
  ) =>
  Show (ConwayUtxoPredFailure era)

deriving instance
  ( Era era
  , Eq (Value era)
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Eq (TxOut era)
  , Eq (Script era)
  , Eq (TxIn (EraCrypto era))
  ) =>
  Eq (ConwayUtxoPredFailure era)

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
      MaxTxSizeUTxO a b -> Sum MaxTxSizeUTxO 3 !> To a !> To b
      InputSetEmptyUTxO -> Sum InputSetEmptyUTxO 4
      FeeTooSmallUTxO a b -> Sum FeeTooSmallUTxO 5 !> To a !> To b
      ValueNotConservedUTxO a b -> Sum (ValueNotConservedUTxO @era) 6 !> To a !> To b
      WrongNetwork right wrongs -> Sum (WrongNetwork @era) 7 !> To right !> To wrongs
      WrongNetworkWithdrawal right wrongs -> Sum (WrongNetworkWithdrawal @era) 8 !> To right !> To wrongs
      OutputTooSmallUTxO outs -> Sum (OutputTooSmallUTxO @era) 9 !> To outs
      OutputBootAddrAttrsTooBig outs -> Sum (OutputBootAddrAttrsTooBig @era) 10 !> To outs
      OutputTooBigUTxO outs -> Sum (OutputTooBigUTxO @era) 11 !> To outs
      InsufficientCollateral a b -> Sum InsufficientCollateral 12 !> To a !> To b
      ScriptsNotPaidUTxO a -> Sum ScriptsNotPaidUTxO 13 !> To a
      ExUnitsTooBigUTxO a b -> Sum ExUnitsTooBigUTxO 14 !> To a !> To b
      CollateralContainsNonADA a -> Sum CollateralContainsNonADA 15 !> To a
      WrongNetworkInTxBody a b -> Sum WrongNetworkInTxBody 16 !> To a !> To b
      OutsideForecast a -> Sum OutsideForecast 17 !> To a
      TooManyCollateralInputs a b -> Sum TooManyCollateralInputs 18 !> To a !> To b
      NoCollateralInputs -> Sum NoCollateralInputs 19
      IncorrectTotalCollateralField c1 c2 -> Sum IncorrectTotalCollateralField 20 !> To c1 !> To c2
      BabbageOutputTooSmallUTxO x -> Sum BabbageOutputTooSmallUTxO 21 !> To x
      BabbageNonDisjointRefInputs x -> Sum BabbageNonDisjointRefInputs 22 !> To x

instance
  ( Era era
  , DecCBOR (TxOut era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  DecCBOR (ConwayUtxoPredFailure era)
  where
  decCBOR = decode . Summands "ConwayUtxoPred" $ \case
    0 -> SumD UtxosFailure <! From
    1 -> SumD BadInputsUTxO <! From
    2 -> SumD OutsideValidityIntervalUTxO <! From <! From
    3 -> SumD MaxTxSizeUTxO <! From <! From
    4 -> SumD InputSetEmptyUTxO
    5 -> SumD FeeTooSmallUTxO <! From <! From
    6 -> SumD ValueNotConservedUTxO <! From <! From
    7 -> SumD WrongNetwork <! From <! From
    8 -> SumD WrongNetworkWithdrawal <! From <! From
    9 -> SumD OutputTooSmallUTxO <! From
    10 -> SumD OutputBootAddrAttrsTooBig <! From
    11 -> SumD OutputTooBigUTxO <! From
    12 -> SumD InsufficientCollateral <! From <! From
    13 -> SumD ScriptsNotPaidUTxO <! D (UTxO <$> decCBOR)
    14 -> SumD ExUnitsTooBigUTxO <! From <! From
    15 -> SumD CollateralContainsNonADA <! From
    16 -> SumD WrongNetworkInTxBody <! From <! From
    17 -> SumD OutsideForecast <! From
    18 -> SumD TooManyCollateralInputs <! From <! From
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
  Alonzo.MaxTxSizeUTxO x y -> MaxTxSizeUTxO x y
  Alonzo.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Alonzo.FeeTooSmallUTxO c1 c2 -> FeeTooSmallUTxO c1 c2
  Alonzo.ValueNotConservedUTxO vc vp -> ValueNotConservedUTxO vc vp
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
  Alonzo.ExUnitsTooBigUTxO e1 e2 -> ExUnitsTooBigUTxO e1 e2
  Alonzo.CollateralContainsNonADA v -> CollateralContainsNonADA v
  Alonzo.WrongNetworkInTxBody nid nidb -> WrongNetworkInTxBody nid nidb
  Alonzo.OutsideForecast sno -> OutsideForecast sno
  Alonzo.TooManyCollateralInputs n1 n2 -> TooManyCollateralInputs n1 n2
  Alonzo.NoCollateralInputs -> NoCollateralInputs

allegraToConwayUtxoPredFailure ::
  forall era.
  EraRuleFailure "PPUP" era ~ VoidEraRule "PPUP" era =>
  Allegra.AllegraUtxoPredFailure era ->
  ConwayUtxoPredFailure era
allegraToConwayUtxoPredFailure = \case
  Allegra.BadInputsUTxO x -> BadInputsUTxO x
  Allegra.OutsideValidityIntervalUTxO vi slotNo -> OutsideValidityIntervalUTxO vi slotNo
  Allegra.MaxTxSizeUTxO x y -> MaxTxSizeUTxO x y
  Allegra.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Allegra.FeeTooSmallUTxO c1 c2 -> FeeTooSmallUTxO c1 c2
  Allegra.ValueNotConservedUTxO vc vp -> ValueNotConservedUTxO vc vp
  Allegra.WrongNetwork x y -> WrongNetwork x y
  Allegra.WrongNetworkWithdrawal x y -> WrongNetworkWithdrawal x y
  Allegra.OutputTooSmallUTxO x -> OutputTooSmallUTxO x
  Allegra.UpdateFailure x -> absurdEraRule @"PPUP" @era x
  Allegra.OutputBootAddrAttrsTooBig xs -> OutputBootAddrAttrsTooBig xs
  Allegra.TriesToForgeADA ->
    error
      "Impossible case, soon to be removed. See: https://github.com/IntersectMBO/cardano-ledger/issues/4085"
  Allegra.OutputTooBigUTxO xs -> OutputTooBigUTxO (map (0,0,) xs)
