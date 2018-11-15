-- | Defines elementary stuff related to the genesis block
module Chain.GenesisBlock
  (
    initVKeys
  , genesisBlock
  )
 where

import           Crypto.Hash (hashlazy)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Set (Set)
import           Types

-- | Verification keys located in the genesis block
initVKeys :: Set VKey
initVKeys = undefined -- TODO(md): this is to be imported or implemented

genesisBlock :: Block
genesisBlock = GBlock { gbKeys = initVKeys, gbHash = hashlazy (pack . show $ 0) }
