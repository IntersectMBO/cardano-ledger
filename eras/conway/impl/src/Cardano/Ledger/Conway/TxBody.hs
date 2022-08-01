{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxBody
  ( module BabbageTxBodyReExports,
    module ConwayTxOutReExports,
  )
where

import Cardano.Ledger.Babbage.TxBody
  ( allInputsBabbageTxBodyF,
    auxDataHashBabbageTxBodyL,
    certsBabbageTxBodyL,
    collateralInputsBabbageTxBodyL,
    collateralReturnBabbageTxBodyL,
    feeBabbageTxBodyL,
    inputsBabbageTxBodyL,
    mintBabbageTxBodyL,
    mintedBabbageTxBodyF,
    mkBabbageTxBody,
    networkIdBabbageTxBodyL,
    outputsBabbageTxBodyL,
    referenceInputsBabbageTxBodyL,
    reqSignerHashesBabbageTxBodyL,
    scriptIntegrityHashBabbageTxBodyL,
    sizedCollateralReturnBabbageTxBodyL,
    sizedOutputsBabbageTxBodyL,
    totalCollateralBabbageTxBodyL,
    updateBabbageTxBodyL,
    vldtBabbageTxBodyL,
    wdrlsBabbbageTxBodyL,
  )
import Cardano.Ledger.Babbage.TxBody as BabbageTxBodyReExports
  ( AlonzoEraTxBody (..),
    BabbageEraTxBody (..),
    BabbageTxBody (..),
    ShelleyEraTxBody (..),
    ShelleyMAEraTxBody (..),
  )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Conway.TxOut as ConwayTxOutReExports
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC

instance CC.Crypto c => EraTxBody (ConwayEra c) where
  type TxBody (ConwayEra c) = BabbageTxBody (ConwayEra c)

  mkBasicTxBody = mkBabbageTxBody

  inputsTxBodyL = inputsBabbageTxBodyL

  outputsTxBodyL = outputsBabbageTxBodyL

  feeTxBodyL = feeBabbageTxBodyL

  auxDataHashTxBodyL = auxDataHashBabbageTxBodyL

  allInputsTxBodyF = allInputsBabbageTxBodyF

  mintedTxBodyF = mintedBabbageTxBodyF

instance CC.Crypto c => ShelleyEraTxBody (ConwayEra c) where
  wdrlsTxBodyL = wdrlsBabbbageTxBodyL

  ttlTxBodyL = notSupportedInThisEraL

  updateTxBodyL = updateBabbageTxBodyL

  certsTxBodyL = certsBabbageTxBodyL

instance CC.Crypto c => ShelleyMAEraTxBody (ConwayEra c) where
  vldtTxBodyL = vldtBabbageTxBodyL

  mintTxBodyL = mintBabbageTxBodyL

instance CC.Crypto c => AlonzoEraTxBody (ConwayEra c) where
  collateralInputsTxBodyL = collateralInputsBabbageTxBodyL

  reqSignerHashesTxBodyL = reqSignerHashesBabbageTxBodyL

  scriptIntegrityHashTxBodyL = scriptIntegrityHashBabbageTxBodyL

  networkIdTxBodyL = networkIdBabbageTxBodyL

instance CC.Crypto c => BabbageEraTxBody (ConwayEra c) where
  sizedOutputsTxBodyL = sizedOutputsBabbageTxBodyL

  referenceInputsTxBodyL = referenceInputsBabbageTxBodyL

  totalCollateralTxBodyL = totalCollateralBabbageTxBodyL

  collateralReturnTxBodyL = collateralReturnBabbageTxBodyL

  sizedCollateralReturnTxBodyL = sizedCollateralReturnBabbageTxBodyL
