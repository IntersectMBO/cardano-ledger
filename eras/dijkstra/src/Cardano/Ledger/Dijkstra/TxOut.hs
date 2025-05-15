{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxOut () where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (..))
import Cardano.Ledger.Babbage (BabbageTxOut)
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Babbage.TxOut (
  BabbageEraTxOut (..),
  addrEitherBabbageTxOutL,
  babbageMinUTxOValue,
  dataBabbageTxOutL,
  dataHashBabbageTxOutL,
  datumBabbageTxOutL,
  getDatumBabbageTxOut,
  referenceScriptBabbageTxOutL,
  valueEitherBabbageTxOutL,
 )
import Cardano.Ledger.Core (EraScript (..), EraTxOut (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Plutus (Datum (..), translateDatum)
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro (to)

instance EraTxOut DijkstraEra where
  type TxOut DijkstraEra = BabbageTxOut DijkstraEra

  mkBasicTxOut addr vl = BabbageTxOut addr vl NoDatum SNothing

  upgradeTxOut (BabbageTxOut addr value d s) =
    BabbageTxOut addr value (translateDatum d) (upgradeScript <$> s)

  addrEitherTxOutL = addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinSizedTxOut = babbageMinUTxOValue

instance AlonzoEraTxOut DijkstraEra where
  dataHashTxOutL = dataHashBabbageTxOutL
  {-# INLINE dataHashTxOutL #-}

  datumTxOutF = to getDatumBabbageTxOut
  {-# INLINE datumTxOutF #-}

instance BabbageEraTxOut DijkstraEra where
  dataTxOutL = dataBabbageTxOutL
  {-# INLINE dataTxOutL #-}

  datumTxOutL = datumBabbageTxOutL
  {-# INLINE datumTxOutL #-}

  referenceScriptTxOutL = referenceScriptBabbageTxOutL
  {-# INLINE referenceScriptTxOutL #-}
