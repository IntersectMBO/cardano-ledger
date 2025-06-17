{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxOut (upgradeBabbageTxOut) where

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
import Data.Coerce (coerce)
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro

instance EraTxOut ConwayEra where
  type TxOut ConwayEra = BabbageTxOut ConwayEra

  mkBasicTxOut addr vl = BabbageTxOut addr vl NoDatum SNothing

  upgradeTxOut = upgradeBabbageTxOut

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

upgradeBabbageTxOut ::
  ( Value era ~ Value (PreviousEra era)
  , EraScript (PreviousEra era)
  , EraScript era
  ) =>
  BabbageTxOut (PreviousEra era) ->
  BabbageTxOut era
upgradeBabbageTxOut = \case
  TxOutCompact' ca cv -> TxOutCompact' ca cv
  TxOutCompactDH' ca cv dh -> TxOutCompactDH' ca cv dh
  TxOutCompactDatum ca cv bd -> TxOutCompactDatum ca cv (coerce bd)
  TxOutCompactRefScript ca cv d s -> TxOutCompactRefScript ca cv (translateDatum d) (upgradeScript s)
  TxOut_AddrHash28_AdaOnly c a28e cc -> TxOut_AddrHash28_AdaOnly c a28e cc
  TxOut_AddrHash28_AdaOnly_DataHash32 c a28e cc dh32 -> TxOut_AddrHash28_AdaOnly_DataHash32 c a28e cc dh32
