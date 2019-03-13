module Cardano.Chain.Txp.GenesisUTxO
  ( genesisUtxo
  )
where

import Cardano.Prelude

import qualified Data.Map.Strict as M

import Cardano.Chain.Common (Address, Lovelace, makeRedeemAddress)
import Cardano.Chain.Common.NetworkMagic (NetworkMagic, makeNetworkMagic)
import Cardano.Chain.Genesis (unGenesisAvvmBalances, unGenesisNonAvvmBalances)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Txp.UTxO (UTxO)
import qualified Cardano.Chain.Txp.UTxO as UTxO


-- | Create initial 'UTxO' from balances defined in the genesis config
genesisUtxo :: Genesis.Config -> UTxO
genesisUtxo config = UTxO.fromBalances balances
 where
  balances :: [(Address, Lovelace)]
  balances = avvmBalances <> nonAvvmBalances

  avvmBalances :: [(Address, Lovelace)]
  avvmBalances = first (makeRedeemAddress networkMagic)
    <$> M.toList (unGenesisAvvmBalances $ Genesis.configAvvmDistr config)

  networkMagic :: NetworkMagic
  networkMagic = makeNetworkMagic (Genesis.configProtocolMagic config)

  nonAvvmBalances :: [(Address, Lovelace)]
  nonAvvmBalances =
    M.toList $ unGenesisNonAvvmBalances $ Genesis.configNonAvvmBalances config
