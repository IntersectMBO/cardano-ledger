module Cardano.Chain.UTxO.GenesisUTxO
  ( genesisUtxo,
  )
where

import Cardano.Chain.Common (Address, Lovelace, makeRedeemAddress)
import Cardano.Chain.Common.NetworkMagic (NetworkMagic, makeNetworkMagic)
import Cardano.Chain.Genesis (unGenesisAvvmBalances, unGenesisNonAvvmBalances)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.UTxO.UTxO (UTxO)
import qualified Cardano.Chain.UTxO.UTxO as UTxO
import Cardano.Crypto (fromCompactRedeemVerificationKey)
import Cardano.Prelude
import qualified Data.Map.Strict as M

-- | Create initial 'UTxO' from balances defined in the genesis config
genesisUtxo :: Genesis.Config -> UTxO
genesisUtxo config = UTxO.fromBalances balances
  where
    balances :: [(Address, Lovelace)]
    balances = avvmBalances <> nonAvvmBalances

    avvmBalances :: [(Address, Lovelace)]
    avvmBalances =
      first (makeRedeemAddress networkMagic . fromCompactRedeemVerificationKey)
        <$> M.toList (unGenesisAvvmBalances $ Genesis.configAvvmDistr config)

    networkMagic :: NetworkMagic
    networkMagic = makeNetworkMagic (Genesis.configProtocolMagic config)

    nonAvvmBalances :: [(Address, Lovelace)]
    nonAvvmBalances =
      M.toList $ unGenesisNonAvvmBalances $ Genesis.configNonAvvmBalances config
