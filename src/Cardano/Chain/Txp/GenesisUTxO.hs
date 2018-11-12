module Cardano.Chain.Txp.GenesisUTxO
  ( genesisUtxo
  )
where

import Cardano.Prelude

import Data.Coerce (coerce)
import qualified Data.Map.Strict as M

import Cardano.Chain.Common (Address, Lovelace, makeRedeemAddress)
import Cardano.Chain.Genesis
  (GenesisData(..), getGenesisAvvmBalances, getGenesisNonAvvmBalances)
import Cardano.Chain.Txp.Tx (TxIn(..), TxOut(..))
import Cardano.Chain.Txp.UTxO (UTxO)
import qualified Cardano.Chain.Txp.UTxO as UTxO
import Cardano.Crypto (hash)


genesisUtxo :: GenesisData -> UTxO
genesisUtxo genesisData = UTxO.fromList $ utxoEntry <$> preUtxo
 where
  preUtxo :: [(Address, Lovelace)]
  preUtxo = avvmBalances <> nonAvvmBalances

  avvmBalances :: [(Address, Lovelace)]
  avvmBalances = first makeRedeemAddress
    <$> M.toList (getGenesisAvvmBalances $ gdAvvmDistr genesisData)

  nonAvvmBalances :: [(Address, Lovelace)]
  nonAvvmBalances =
    M.toList $ getGenesisNonAvvmBalances $ gdNonAvvmBalances genesisData

  utxoEntry :: (Address, Lovelace) -> (TxIn, TxOut)
  utxoEntry (addr, lovelace) =
    (TxInUtxo (coerce $ hash addr) 0, TxOut addr lovelace)
