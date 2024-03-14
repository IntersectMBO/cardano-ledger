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
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure)

type instance EraRuleFailure "UTXO" (ConwayEra c) = BabbageUtxoPredFailure (ConwayEra c)

type instance EraRuleEvent "UTXO" (ConwayEra c) = AlonzoUtxoEvent (ConwayEra c)

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
