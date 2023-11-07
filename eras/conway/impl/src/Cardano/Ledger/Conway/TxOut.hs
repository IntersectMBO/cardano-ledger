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

module Cardano.Ledger.Conway.TxOut (
  module BabbageTxOutReExports,
)
where

import Cardano.Ledger.Address (addrPtrNormalize)
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxOut (..),
  Datum (..),
  addrEitherBabbageTxOutL,
  babbageMinUTxOValue,
  dataBabbageTxOutL,
  dataHashBabbageTxOutL,
  datumBabbageTxOutL,
  getDatumBabbageTxOut,
  referenceScriptBabbageTxOutL,
  valueEitherBabbageTxOutL,
 )
import Cardano.Ledger.Babbage.TxBody as BabbageTxOutReExports (
  AlonzoEraTxOut (..),
  BabbageEraTxOut (..),
  BabbageTxOut (..),
 )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Plutus.Data (translateDatum)
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro

instance Crypto c => EraTxOut (ConwayEra c) where
  {-# SPECIALIZE instance EraTxOut (ConwayEra StandardCrypto) #-}

  type TxOut (ConwayEra c) = BabbageTxOut (ConwayEra c)

  mkBasicTxOut addr vl = BabbageTxOut addr vl NoDatum SNothing

  upgradeTxOut (BabbageTxOut addr value d s) =
    BabbageTxOut (addrPtrNormalize addr) value (translateDatum d) (upgradeScript <$> s)

  addrEitherTxOutL = addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinSizedTxOut = babbageMinUTxOValue

instance Crypto c => AlonzoEraTxOut (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTxOut (ConwayEra StandardCrypto) #-}

  dataHashTxOutL = dataHashBabbageTxOutL
  {-# INLINE dataHashTxOutL #-}

  datumTxOutF = to getDatumBabbageTxOut
  {-# INLINE datumTxOutF #-}

instance Crypto c => BabbageEraTxOut (ConwayEra c) where
  {-# SPECIALIZE instance BabbageEraTxOut (ConwayEra StandardCrypto) #-}

  dataTxOutL = dataBabbageTxOutL
  {-# INLINE dataTxOutL #-}

  datumTxOutL = datumBabbageTxOutL
  {-# INLINE datumTxOutL #-}

  referenceScriptTxOutL = referenceScriptBabbageTxOutL
  {-# INLINE referenceScriptTxOutL #-}
