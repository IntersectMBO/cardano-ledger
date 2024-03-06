{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxo (allegraToConwayUtxoPredFailure) where

import Cardano.Ledger.Allegra.Rules (shelleyToAllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra (AllegraUtxoPredFailure (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure)

data ConwayUtxoPredFailure era
  = -- | The bad transaction inputs
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
  | -- | Subtransition Failures
    UtxosFailure (PredicateFailure (EraRule "UTXOS" era))
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig
      ![TxOut era]
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO
      ![(Integer, Integer, TxOut era)]
  | InsufficientCollateral
      -- | balance computed
      !Coin
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
      !Coin
      -- | collateral amount declared in transaction body
      !Coin
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    BabbageOutputTooSmallUTxO
      ![(TxOut era, Coin)]
  | -- | TxIns that appear in both inputs and reference inputs
    BabbageNonDisjointRefInputs
      !(NonEmpty (TxIn (EraCrypto era)))

type instance EraRuleFailure "UTXO" (ConwayEra c) = ConwayUtxoPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXO" BabbageUtxoPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXO" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxoPredFailure

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure =
    AlonzoInBabbageUtxoPredFailure
      . allegraToConwayUtxoPredFailure
      . shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxoPredFailure . allegraToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxoPredFailure . UtxosFailure

instance InjectRuleFailure "UTXO" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxoPredFailure . UtxosFailure . injectFailure

allegraToConwayUtxoPredFailure ::
  forall era.
  EraRuleFailure "PPUP" era ~ VoidEraRule "PPUP" era =>
  Allegra.AllegraUtxoPredFailure era ->
  AlonzoUtxoPredFailure era
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
  Allegra.TriesToForgeADA -> TriesToForgeADA
  Allegra.OutputTooBigUTxO xs -> OutputTooBigUTxO (map (0,0,) xs)
