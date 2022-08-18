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
    mintValueBabbageTxBodyF,
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
  {-# SPECIALIZE instance EraTxBody (ConwayEra CC.StandardCrypto) #-}

  type TxBody (ConwayEra c) = BabbageTxBody (ConwayEra c)

  mkBasicTxBody = mkBabbageTxBody

  inputsTxBodyL = inputsBabbageTxBodyL
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL = outputsBabbageTxBodyL
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = feeBabbageTxBodyL
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = auxDataHashBabbageTxBodyL
  {-# INLINE auxDataHashTxBodyL #-}

  allInputsTxBodyF = allInputsBabbageTxBodyF
  {-# INLINE allInputsTxBodyF #-}

  mintedTxBodyF = mintedBabbageTxBodyF
  {-# INLINE mintedTxBodyF #-}

instance CC.Crypto c => ShelleyEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (ConwayEra CC.StandardCrypto) #-}

  wdrlsTxBodyL = wdrlsBabbbageTxBodyL
  {-# INLINE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINE ttlTxBodyL #-}

  updateTxBodyL = updateBabbageTxBodyL
  {-# INLINE updateTxBodyL #-}

  certsTxBodyL = certsBabbageTxBodyL
  {-# INLINE certsTxBodyL #-}

instance CC.Crypto c => ShelleyMAEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance ShelleyMAEraTxBody (ConwayEra CC.StandardCrypto) #-}

  vldtTxBodyL = vldtBabbageTxBodyL
  {-# INLINE vldtTxBodyL #-}

  mintTxBodyL = mintBabbageTxBodyL
  {-# INLINE mintTxBodyL #-}

  mintValueTxBodyF = mintValueBabbageTxBodyF

instance CC.Crypto c => AlonzoEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTxBody (ConwayEra CC.StandardCrypto) #-}

  collateralInputsTxBodyL = collateralInputsBabbageTxBodyL
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL = reqSignerHashesBabbageTxBodyL
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL = scriptIntegrityHashBabbageTxBodyL
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = networkIdBabbageTxBodyL
  {-# INLINE networkIdTxBodyL #-}

instance CC.Crypto c => BabbageEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance BabbageEraTxBody (ConwayEra CC.StandardCrypto) #-}

  sizedOutputsTxBodyL = sizedOutputsBabbageTxBodyL
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL = referenceInputsBabbageTxBodyL
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL = totalCollateralBabbageTxBodyL
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL = collateralReturnBabbageTxBodyL
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL = sizedCollateralReturnBabbageTxBodyL
  {-# INLINE sizedCollateralReturnTxBodyL #-}
