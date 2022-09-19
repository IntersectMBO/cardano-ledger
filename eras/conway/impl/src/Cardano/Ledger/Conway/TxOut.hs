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

module Cardano.Ledger.Conway.TxOut
  ( module BabbageTxOutReExports,
  )
where

import Cardano.Ledger.Alonzo.Data (Datum (NoDatum))
import Cardano.Ledger.Babbage.TxBody
  ( addrEitherBabbageTxOutL,
    babbageMinUTxOValue,
    dataBabbageTxOutL,
    dataHashBabbageTxOutL,
    datumBabbageTxOutL,
    getDatumBabbageTxOut,
    referenceScriptBabbageTxOutL,
    valueEitherBabbageTxOutL,
  )
import Cardano.Ledger.Babbage.TxBody as BabbageTxOutReExports
  ( AlonzoEraTxOut (..),
    BabbageEraTxOut (..),
    BabbageTxOut (..),
  )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro

instance CC.Crypto c => EraTxOut (ConwayEra c) where
  {-# SPECIALIZE instance EraTxOut (ConwayEra CC.StandardCrypto) #-}

  type TxOut (ConwayEra c) = BabbageTxOut (ConwayEra c)

  mkBasicTxOut addr vl = BabbageTxOut addr vl NoDatum SNothing

  addrEitherTxOutL = addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinSizedTxOut = babbageMinUTxOValue

instance CC.Crypto c => AlonzoEraTxOut (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTxOut (ConwayEra CC.StandardCrypto) #-}

  dataHashTxOutL = dataHashBabbageTxOutL
  {-# INLINE dataHashTxOutL #-}

  datumTxOutF = to getDatumBabbageTxOut
  {-# INLINE datumTxOutF #-}

instance CC.Crypto c => BabbageEraTxOut (ConwayEra c) where
  {-# SPECIALIZE instance BabbageEraTxOut (ConwayEra CC.StandardCrypto) #-}

  dataTxOutL = dataBabbageTxOutL
  {-# INLINE dataTxOutL #-}

  datumTxOutL = datumBabbageTxOutL
  {-# INLINE datumTxOutL #-}

  referenceScriptTxOutL = referenceScriptBabbageTxOutL
  {-# INLINE referenceScriptTxOutL #-}
