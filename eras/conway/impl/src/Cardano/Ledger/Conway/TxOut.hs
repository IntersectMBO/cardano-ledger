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

module Cardano.Ledger.Conway.TxOut () where

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
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Plutus.Data (Datum (..), translateDatum)
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro

instance EraTxOut ConwayEra where
  type TxOut ConwayEra = BabbageTxOut ConwayEra

  mkBasicTxOut addr vl = BabbageTxOut addr vl NoDatum SNothing

  upgradeTxOut (BabbageTxOut addr value d s) =
    BabbageTxOut addr value (translateDatum d) (upgradeScript <$> s)

  addrEitherTxOutL = addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinSizedTxOut = babbageMinUTxOValue

instance AlonzoEraTxOut ConwayEra where
  dataHashTxOutL = dataHashBabbageTxOutL
  {-# INLINE dataHashTxOutL #-}

  datumTxOutF = to getDatumBabbageTxOut
  {-# INLINE datumTxOutF #-}

instance BabbageEraTxOut ConwayEra where
  dataTxOutL = dataBabbageTxOutL
  {-# INLINE dataTxOutL #-}

  datumTxOutL = datumBabbageTxOutL
  {-# INLINE datumTxOutL #-}

  referenceScriptTxOutL = referenceScriptBabbageTxOutL
  {-# INLINE referenceScriptTxOutL #-}
