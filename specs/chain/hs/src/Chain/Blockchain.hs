{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Chain.Blockchain where

import           Control.Lens
import           Control.State.Transition
import qualified Data.Map.Strict as Map
import           Data.Queue
import           Data.Set (Set)

import           Crypto.Hash (hashlazy)
import           Data.ByteString.Lazy.Char8 (pack)
import           UTxO (Hash)

newtype VKey = MkVKey Word deriving (Eq, Ord)
-- data VKeyGen -- not sure how to encode VKeyGen such that it is a subset
-- of the VKey type. Therefore, find some other way of ensuring this invariant.

data Sig
data Data

newtype BlockIx = MkBlockIx Word deriving (Eq, Ord)

data Block
  -- a genesis block
  = GBlock {
      gbKeys :: [VKey]
    , gbHash :: Hash -- ^ Hash of itself
    }
  -- a non-genesis block
  | RBlock {
      rbHash   :: Hash -- ^ Hash of the predecessor block
    , rbIx     :: BlockIx -- ^ Index of the block
    , rbSigner :: VKey -- ^ Block signer
    , rbCerts  :: Set HCert -- ^ New certificates posted to the blockchain
    , rbData   :: Data -- ^ Body of the block
    , rbSig    :: Sig -- ^ Cryptographic signature of the block
    }

-- | Computes the hash of a block
hashBlock :: Block -> Hash
hashBlock b@(RBlock {}) = hashlazy (pack . show $ i) where
  MkBlockIx i = rbIx b
hashBlock b@(GBlock {}) = gbHash b

-- | Verification keys located in the genesis block
initVKeys :: [VKey]
initVKeys = undefined -- TODO(md): this is to be imported or implemented

genesisBlock :: Block
genesisBlock = GBlock { gbKeys = initVKeys, gbHash = hashlazy (pack . show $ 0) }

newtype Slot = MkSlot Word deriving (Eq, Ord)

-- | A heavyweight delegation certificate
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

-- TODO(md): This rule is to be implemented on the ledger layer's side
newCertsRule :: Rule Interf
newCertsRule = undefined

-- TODO(md): The ledger spec should implement this as part of the delegation
-- interface. This function is closely related to *newCertsRule*
updateCerts :: Slot -> Set HCert -> DSIState -> DSIState
updateCerts = undefined

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
  type Signal Interf = Set HCert
  type Environment Interf = Slot
  data PredicateFailure Interf
    = ConflictWithExistingCerts
    | InvalidNewCertificates
    deriving Show

  rules = [newCertsRule]

-- | Blockchain extension transition system
data BC

-- | Remove the oldest entry in the queues in the range of the map if it is
--   more than *K* blocks away from the given block index
trimIx :: KeyToQMap -> K -> BlockIx -> KeyToQMap
trimIx m (MkK k) ix = foldl (flip f) m (Map.keysSet m) where
  f :: VKey -> KeyToQMap -> KeyToQMap
  f = Map.adjust (qRestrict ix)
  qRestrict :: BlockIx -> Queue BlockIx -> Queue BlockIx
  qRestrict (MkBlockIx ix') q =
    case headQueue q of
      Nothing     -> q
      Just (MkBlockIx h, r) -> if h + k < ix' then r else q

-- | Updates a map of genesis verification keys to their signed blocks in
-- a sliding window by adding a block index to a specified key's list
incIxMap :: BlockIx -> VKey -> KeyToQMap -> KeyToQMap
incIxMap ix = Map.adjust (pushQueue ix)

-- | Extends a chain by a block
extendChain :: Environment BC -> State BC -> Signal BC -> State BC
extendChain env st b@(RBlock {}) =
  let p' = b
      (sl, k, t) = env
      (m, p, ds) = st
      vk_d = rbSigner b
      ix = rbIx b
      vk_s = delegates ds Map.! vk_d
      m' = incIxMap ix vk_s (trimIx m k ix)
      ds' = updateCerts sl (rbCerts b) ds
  in (m', p', ds')

instance STS BC where
  -- | The state comprises a map of genesis block verification keys to a queue
  -- of at most K blocks each key signed in a sliding window of size K,
  -- the previous block and the delegation interface state
  type State BC = (KeyToQMap, Block, DSIState)
  -- | Transitions in the system are triggered by a new block
  type Signal BC = Block
  -- | The environment consists of K and t parameters. To support a state
  -- transition subsystem, the environment also includes the slot of the
  -- block to be added.
  type Environment BC = (Slot, K, T)
  data PredicateFailure BC
    = InvalidPredecessor
    | NoDelegationRight
    | InvalidBlockSignature
    | SignedMaximumNumberBlocks
    | InvalidCertificates
    deriving Show

  -- There are only two inference rules: 1) for the initial state and 2) for
  -- extending the blockchain by a new block
  rules =
    [
      initStateRule
    , Rule
        [
          validPredecessor
        , hasRight
        , validSignature
        , lessThanLimitSigned
        , legalCerts
        ]
        (Extension . Transition $ extendChain)
    ]
    where
      initStateRule :: Rule BC
      initStateRule =
        Rule [] $ Base (Map.empty, genesisBlock, initDSIState)
      -- valid predecessor
      validPredecessor :: Antecedent BC
      validPredecessor = Predicate $ \_ st b@(RBlock {}) ->
        let (_, p, _) = st
         in if hashBlock p == rbHash b
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
      legalCerts = SubTrans (_1 . _1) (_3 . to rbCerts) newCertsRule

instance Embed Interf BC where
  -- stateLens :: Lens' (State BC) (State Interf)
  stateLens = lens getter setter where
    getter :: State BC -> State Interf
    getter (_, _, ds) = ds
    setter :: State BC -> State Interf -> State BC
    setter (m, p, _) ds' = (m, p, ds')
