{-# LANGUAGE TypeFamilies #-}

module Chain.Blockchain where

import           Control.State.Transition
import qualified Data.Map.Strict as Map
import           Data.Queue

data VKey
data VKeyGen -- somehow make sure this is a subtype of VKey
data Sig
data BHash

newtype BlockIx = MkBlockIx Int deriving (Eq, Ord)

data Block
  -- a genesis block
  = GBlock {
      gbKeys :: [VKey]
    , gbHash :: BHash -- ^ Hash of itself
    }
  -- a non-genesis block
  | RBlock {
      rbHash :: BHash -- ^ Hash of the predecessor block
    }

-- TODO(md): implement this function
-- Computes the hash of a block
hash :: Block -> BHash
hash RBlock {} = undefined
hash g@GBlock {} = gbHash g

-- TODO(md): This is to be constructed with GBlock
-- it should be that gbHash genesisBlock == hash genesisBlock
genesisBlock :: Block
genesisBlock = undefined

type KeyToQMap = Map.Map VKey (Queue BlockIx)

newtype Slot = MkSlot Int deriving (Eq, Ord)

-- NOTE: To be defined elsewhere as part of the delegation interface between
-- the ledger layer and the blockchain layer.
--
-- For a given delegation state, it returns a mapping from delegatee keys
-- to delegator keys. If a key is not present in the key set of the returned
-- map, it has no right to sign a block in the current slot
delegates :: DSIState -> Map.Map VKey VKeyGen
delegates = undefined

data DSIState -- TODO(md): to be imported as an abstract signature
initDSIState :: DSIState
initDSIState = undefined -- TODO: This is to be imported from somewhere

data UTxO -- TODO: to be removed

-- | Blockchain extension transition system
data BC

instance STS BC where
  type State BC = (KeyToQMap, Block, DSIState)
  type Signal BC = Block
  type Environment BC = Slot
  data PredicateFailure BC
    = NoDelegationRight
    | InvalidBlockSignature
    | InvalidPredecessor
    | InvalidCertificates
    | SignedMaximumNumberBlocks
    deriving Show

  rules =
    [
      initStateRule
    -- , Rule
    --     [
    --       validPredecessor
    --       -- has delegation rights
    --     ]
    --     ( Extension . Transition $ \_ st _ -> st )
      -- [ SubTrans _1 (_3 . to body) utxoInductive
      -- , Predicate $ \pc utxo tw -> witnessed tw utxo
    ]
    where
      initStateRule :: Rule BC
      initStateRule =
        Rule [] $ Base (Map.empty, genesisBlock, initDSIState)
      -- valid predecessor
      -- validPredecessor :: Antecedent BC
      -- validPredecessor = Predicate $ \env st b ->
      --   let (m, p, ds) = env
      --    in if hash p == rbHash b
      --         then Passed
      --         else Failed InvalidPredecessor
