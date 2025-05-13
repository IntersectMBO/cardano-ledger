{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxOut () where

import Cardano.Ledger.Babbage (BabbageTxOut)
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Babbage.TxOut (
  addrEitherBabbageTxOutL,
  babbageMinUTxOValue,
  valueEitherBabbageTxOutL,
 )
import Cardano.Ledger.Core (EraScript (..), EraTxOut (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Plutus (Datum (..), translateDatum)
import Data.Maybe.Strict (StrictMaybe (..))
import Cardano.Ledger.Dijkstra.Scripts ()

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
