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

module Cardano.Ledger.Babel.TxOut () where

import Cardano.Ledger.Address (addrPtrNormalize)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxOut (
  BabbageTxOut (..),
  addrEitherBabbageTxOutL,
  babbageMinUTxOValue,
  dataBabbageTxOutL,
  dataHashBabbageTxOutL,
  datumBabbageTxOutL,
  getDatumBabbageTxOut,
  referenceScriptBabbageTxOutL,
  valueEitherBabbageTxOutL,
 )
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.PParams ()
import Cardano.Ledger.Babel.Scripts ()
import Cardano.Ledger.Crypto
import Cardano.Ledger.Plutus.Data (Datum (..), translateDatum)
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro

instance Crypto c => EraTxOut (BabelEra c) where
  {-# SPECIALIZE instance EraTxOut (BabelEra StandardCrypto) #-}

  type TxOut (BabelEra c) = BabbageTxOut (BabelEra c)

  mkBasicTxOut addr vl = BabbageTxOut addr vl NoDatum SNothing

  upgradeTxOut (BabbageTxOut addr value d s) =
    BabbageTxOut (addrPtrNormalize addr) value (translateDatum d) (upgradeScript <$> s)

  addrEitherTxOutL = addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinSizedTxOut = babbageMinUTxOValue

instance Crypto c => AlonzoEraTxOut (BabelEra c) where
  {-# SPECIALIZE instance AlonzoEraTxOut (BabelEra StandardCrypto) #-}

  dataHashTxOutL = dataHashBabbageTxOutL
  {-# INLINE dataHashTxOutL #-}

  datumTxOutF = to getDatumBabbageTxOut
  {-# INLINE datumTxOutF #-}

instance Crypto c => BabbageEraTxOut (BabelEra c) where
  {-# SPECIALIZE instance BabbageEraTxOut (BabelEra StandardCrypto) #-}

  dataTxOutL = dataBabbageTxOutL
  {-# INLINE dataTxOutL #-}

  datumTxOutL = datumBabbageTxOutL
  {-# INLINE datumTxOutL #-}

  referenceScriptTxOutL = referenceScriptBabbageTxOutL
  {-# INLINE referenceScriptTxOutL #-}
