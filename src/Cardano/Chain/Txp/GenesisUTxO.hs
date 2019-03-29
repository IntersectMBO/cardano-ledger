module Cardano.Chain.Txp.GenesisUTxO
  ( genesisUtxo
  )
where

import Cardano.Prelude

import Data.Coerce (coerce)
import qualified Data.Map.Strict as M

import Cardano.Chain.Common (Address, Lovelace, makeRedeemAddress)
import Cardano.Chain.Common.NetworkMagic (NetworkMagic, makeNetworkMagic)
import Cardano.Chain.Genesis
  (GenesisData(..), getGenesisAvvmBalances, getGenesisNonAvvmBalances)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Txp.Tx (TxIn(..), TxOut(..))
import Cardano.Chain.Txp.UTxO (UTxO)
import qualified Cardano.Chain.Txp.UTxO as UTxO
import Cardano.Crypto (hash)


genesisUtxo :: Genesis.Config -> UTxO
genesisUtxo genesisConfig = UTxO.fromList $ utxoEntry <$> preUtxo
 where
  genesisData :: GenesisData
  genesisData = Genesis.configGenesisData genesisConfig

  networkMagic :: NetworkMagic
  networkMagic = makeNetworkMagic (Genesis.configProtocolMagic genesisConfig)

  preUtxo :: [(Address, Lovelace)]
  preUtxo = avvmBalances <> nonAvvmBalances

  avvmBalances :: [(Address, Lovelace)]
  avvmBalances = first (makeRedeemAddress networkMagic)
    <$> M.toList (getGenesisAvvmBalances $ gdAvvmDistr genesisData)

  nonAvvmBalances :: [(Address, Lovelace)]
  nonAvvmBalances =
    M.toList $ getGenesisNonAvvmBalances $ gdNonAvvmBalances genesisData

  utxoEntry :: (Address, Lovelace) -> (TxIn, TxOut)
  utxoEntry (addr, lovelace) =
    (TxInUtxo (coerce $ hash addr) 0, TxOut addr lovelace)
