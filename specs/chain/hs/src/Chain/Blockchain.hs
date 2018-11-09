{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Chain.Blockchain where

import           Control.Lens
import           Control.State.Transition
import qualified Data.Map.Strict as Map
import           Data.Queue

newtype VKey = MkVKey Word deriving (Eq, Ord)
-- data VKeyGen -- not sure how to encode VKeyGen such that it is a subset
-- of the VKey type. Therefore, find some other way of ensuring this invariant.

data Sig
data Data

data BHash = MkBHash Word deriving Eq -- TODO(md): put something meaningful here

newtype BlockIx = MkBlockIx Word deriving (Eq, Ord)

data Block
  -- a genesis block
  = GBlock {
      gbKeys :: [VKey]
    , gbHash :: BHash -- ^ Hash of itself
    }
  -- a non-genesis block
  | RBlock {
      rbHash :: BHash -- ^ Hash of the predecessor block
    , rbSigner :: VKey -- ^ Block signer
    , rbData :: Data -- ^ Body of the block
    , rbSig :: Sig -- ^ Cryptographic signature of the block
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

newtype Slot = MkSlot Word deriving (Eq, Ord)

data HCert

--------------------------------------------------------------------------------
--  Delegation interface
--------------------------------------------------------------------------------

data DSIState -- TODO(md): to be imported as an abstract signature

-- NOTE: To be defined elsewhere as part of the delegation interface between
-- the ledger layer and the blockchain layer.
--
-- For a given delegation state, it returns a mapping from delegatee keys
-- to delegator keys. If a key is not present in the key set of the returned
-- map, it has no right to sign a block in the current slot
delegates :: DSIState -> Map.Map VKey VKey
delegates = undefined

initDSIState :: DSIState
initDSIState = undefined -- TODO: This is to be imported from somewhere

--------------------------------------------------------------------------------

-- | Size of the block sliding window
newtype K = MkK Word deriving (Eq, Ord)

-- | The 't' parameter in K * t in the range 0.2 <= t <= 0.25
-- that limits the number of blocks a signer can signed in a
-- block sliding window of size K
newtype T = MkT Double deriving (Eq, Ord)

-- | Checks whether the signature of data is valid
verify :: VKey -> Data -> Sig -> Bool
verify = undefined

-- Gives a map from delegator keys to a queue of block IDs of blocks that
-- the given key (indirectly) signed in the block sliding window of size K
type KeyToQMap = Map.Map VKey (Queue BlockIx)

-- | Delegation interface transition system
data Interf

instance STS Interf where
  type State Interf = DSIState
  type Signal Interf = [HCert]
  type Environment Interf = Slot
  data PredicateFailure Interf
    = ConflictWithExistingCerts
    | InvalidNewCertificates
    deriving Show

  rules = undefined -- TODO(md): this is yet to be implemented

-- | Blockchain extension transition system
data BC

instance STS BC where
  type State BC = (KeyToQMap, Block, DSIState)
  type Signal BC = Block
  type Environment BC = (Slot, K, T)
  data PredicateFailure BC
    = InvalidPredecessor
    | NoDelegationRight
    | InvalidBlockSignature
    | SignedMaximumNumberBlocks
    | InvalidCertificates
    deriving Show

  rules =
    [
      initStateRule
    , Rule
        [
          validPredecessor
        , hasRight
        , validSignature
        , lessThanLimitSigned
        -- , legalCerts
        ]
        ( Extension . Transition $ \_ st _ -> st ) -- TODO(md): implement this
      -- [ SubTrans _1 (_3 . to body) utxoInductive
      -- , Predicate $ \pc utxo tw -> witnessed tw utxo
    ]
    where
      initStateRule :: Rule BC
      initStateRule =
        Rule [] $ Base (Map.empty, genesisBlock, initDSIState)
      -- valid predecessor
      validPredecessor :: Antecedent BC
      validPredecessor = Predicate $ \_ st b@(RBlock {}) ->
        let (_, p, _) = st
         in if hash p == rbHash b
              then Passed
              else Failed InvalidPredecessor
      -- has a delegation right
      hasRight :: Antecedent BC
      hasRight = Predicate $ \_ st b@(RBlock {}) ->
        let (_, _, ds) = st
            vk_d = rbSigner b
         in case Map.lookup vk_d (delegates ds) of
              Nothing -> Failed NoDelegationRight
              _       -> Passed
      -- valid signature
      validSignature :: Antecedent BC
      validSignature = Predicate $ \_ _ b@(RBlock {}) ->
        let vk_d = rbSigner b
         in if verify vk_d (rbData b) (rbSig b)
              then Passed
              else Failed InvalidBlockSignature
      -- the delegator has not signed more than an allowed number of blocks
      -- in a sliding window of the last k blocks
      lessThanLimitSigned :: Antecedent BC
      lessThanLimitSigned = Predicate $ \env st b@(RBlock {}) ->
        let (m, _, ds) = st
            (_, (MkK k), (MkT t)) = env
            vk_d = rbSigner b
            dsm = delegates ds
        in case Map.lookup vk_d dsm of
          Nothing   -> Failed NoDelegationRight
          Just vk_s ->
            if fromIntegral (sizeQueue (m Map.! vk_s)) <= (fromIntegral k) * t
              then Passed
              else Failed SignedMaximumNumberBlocks
      -- checks that delegation certificates in the signal block are legal
      -- with regard to delegation certificates in the delegation state
      legalCerts :: Embed Interf BC => Antecedent BC
      -- legalCerts = SubTrans env signal rule where
      legalCerts = SubTrans env undefined rule where
        env = undefined :: Getter (JudgmentContext BC) (Environment Interf)
        -- signal = undefined :: Getter (JudgmentContext BC) (Signal Interf)
        rule = undefined :: Rule Interf

instance Embed Interf BC where
  -- stateLens :: Lens' (State BC) (State Interf)
  stateLens = lens getter setter where
    getter :: State BC -> State Interf
    getter (_, _, ds) = ds
    setter :: State BC -> State Interf -> State BC
    setter (m, p, _) ds' = (m, p, ds')
