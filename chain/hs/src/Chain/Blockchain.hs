{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Chain.Blockchain where

import Control.Lens
import qualified Data.Map.Strict as Map
import Data.Bits (shift)
import Data.Set (Set)
import Numeric.Natural

import Crypto.Hash (hashlazy)
import Data.ByteString.Lazy.Char8 (pack)

import Chain.GenesisBlock (genesisBlock)
import Control.State.Transition
import Data.Maybe (isJust, fromJust, listToMaybe)
import Data.Queue
import Ledger.Core (VKey(..), VKeyGen, Slot, SlotCount(SlotCount), verify)
import Ledger.Delegation (DCert, DIState, DELEG, DIEnv, delegationMap)
import Ledger.Signatures (Hash)
import Types (BC, Block(..), BlockIx(..), ProtParams(..))


-- | Returns a key from a map for a given value.
maybeMapKeyForValue :: (Eq a, Ord k) => a -> Map.Map k a -> Maybe k
maybeMapKeyForValue v = listToMaybe . map fst . Map.toList . Map.filter (== v)

-- | Unsafely returns a key from a map for a given value. It assumes there is
-- exactly one key mapping to the given value. If there is no such key, it will
-- result in a runtime exception.
mapKeyForValue :: (Eq a, Ord k) => a -> Map.Map k a -> k
mapKeyForValue v = fromJust . maybeMapKeyForValue v


-- | Computes the hash of a block
hashBlock :: Block -> Hash
hashBlock b@(RBlock{}) = hashlazy (pack . show $ i) where MkBlockIx i = rbIx b
hashBlock b@(GBlock{}) = gbHash b

-- | Computes the block size in bytes
bSize :: Block -> Natural
bSize b@(GBlock{}) = gbSize b
bSize b@(RBlock{}) = rbSize b

-- | Computes the block header size in bytes
bHeaderSize :: Block -> Natural
bHeaderSize b@(GBlock{}) = gbHeaderSize b
bHeaderSize b@(RBlock{}) = rbHeaderSize b

-- | The 't' parameter in K * t in the range 0.2 <= t <= 0.25
-- that limits the number of blocks a signer can signed in a
-- block sliding window of size K
newtype T = MkT Double deriving (Eq, Ord)

-- Gives a map from delegator keys to a queue of block IDs of blocks that
-- the given key (indirectly) signed in the block sliding window of size K
type KeyToQMap = Map.Map VKeyGen (Queue BlockIx)


-- | Remove the oldest entry in the queues in the range of the map if it is
--   more than *K* blocks away from the given block index
trimIx :: KeyToQMap -> SlotCount -> BlockIx -> KeyToQMap
trimIx m (SlotCount k) ix = foldl (flip f) m (Map.keysSet m)
 where
  f :: VKeyGen -> KeyToQMap -> KeyToQMap
  f = Map.adjust (qRestrict ix)
  qRestrict :: BlockIx -> Queue BlockIx -> Queue BlockIx
  qRestrict (MkBlockIx ix') q = case headQueue q of
    Nothing               -> q
    Just (MkBlockIx h, r) -> if h + k < ix' then r else q

-- | Updates a map of genesis verification keys to their signed blocks in
-- a sliding window by adding a block index to a specified key's list
incIxMap :: BlockIx -> VKeyGen -> KeyToQMap -> KeyToQMap
incIxMap ix = Map.adjust (pushQueue ix)

-- | Environment for blockchain rules
data BlockchainEnv = MkBlockChainEnv
  {
    bcEnvPp    :: ProtParams
  , bcEnvDIEnv :: DIEnv
  , bcEnvK     :: SlotCount
  , bcEnvT     :: T
  }

-- | Extends a chain by a block
extendChain :: (Environment BC, State BC, Signal BC) -> DIState -> State BC
extendChain (env, st, b@(RBlock{})) dis =
  let
    p'             = b
    (dienv, k, t ) = (bcEnvDIEnv env, bcEnvK env, bcEnvT env)
    (m    , p, ds) = st
    vk_d           = rbSigner b
    ix             = rbIx b
    vk_s           = mapKeyForValue vk_d . (^. delegationMap) $ ds
    m'             = incIxMap ix vk_s (trimIx m k ix)
  in (m', p', dis)

instance STS BC where
  -- | The state comprises a map of genesis block verification keys to a queue
  -- of at most K blocks each key signed in a sliding window of size K,
  -- the previous block and the delegation interface state
  type State BC = (KeyToQMap, Block, DIState)
  -- | Transitions in the system are triggered by a new block
  type Signal BC = Block
  -- | The environment consists of K and t parameters. To support a state
  -- transition subsystem, the environment also includes the environment of the
  -- subsystem.
  type Environment BC = BlockchainEnv
  data PredicateFailure BC
    = InvalidPredecessor
    | NoDelegationRight
    | InvalidBlockSignature
    | InvalidBlockSize
    | InvalidHeaderSize
    | SignedMaximumNumberBlocks
    | LedgerFailure (PredicateFailure DELEG)
    deriving (Eq, Show)

  -- There are only two inference rules: 1) for the initial state and 2) for
  -- extending the blockchain by a new block
  initialRules =
    [ do
        IRC env <- judgmentContext
        initDIState <- trans @DELEG $ IRC (bcEnvDIEnv env)
        return (Map.empty, genesisBlock, initDIState)
    ]
  transitionRules =
    [ do
        TRC jc <- judgmentContext
        validPredecessor jc ?! InvalidPredecessor
        validBlockSize jc ?! InvalidBlockSize
        validHeaderSize jc ?! InvalidHeaderSize
        hasRight jc ?! NoDelegationRight
        validSignature jc ?! InvalidBlockSignature
        lessThanLimitSigned jc
        dis <- trans @DELEG $ TRC (proj jc)
        return $ extendChain jc dis
    ]
    where
      -- valid predecessor
      validPredecessor (_, (_, p, _), b@(RBlock {})) =
         hashBlock p == rbHash b
      -- has a block size within protocol limits
      validBlockSize (env, _, b@(RBlock {})) =
           bSize b <= blockSizeLimit b (bcEnvPp env)
         where
           blockSizeLimit :: Block -> ProtParams -> Natural
           blockSizeLimit (GBlock {}) pp = maxBlockSize pp
           blockSizeLimit b@(RBlock {}) pp =
             if rbIsEBB b
               then 1 `shift` 21
               else maxBlockSize pp
      -- has a block header size within protocol limits
      validHeaderSize (env, _, b@(RBlock {})) =
        bHeaderSize b <= maxHeaderSize (bcEnvPp env)
      -- has a delegation right
      hasRight (_, (_, _, ds), b@(RBlock {})) =
        isJust $ maybeMapKeyForValue (rbSigner b) (ds ^. delegationMap)
      -- valid signature
      validSignature (_, _, b@(RBlock {})) =
         verify (rbSigner b) (rbData b) (rbSig b)
      -- the delegator has not signed more than an allowed number of blocks
      -- in a sliding window of the last k blocks
      lessThanLimitSigned (env, st, b@(RBlock {})) =
        let
          (m, _, ds) = st
          (SlotCount k, MkT t) = (bcEnvK env, bcEnvT env)
          vk_d = rbSigner b
          dsm = ds ^. delegationMap
        in case maybeMapKeyForValue vk_d dsm of
          Nothing   -> False ?! NoDelegationRight
          Just vk_s ->
            fromIntegral (sizeQueue (m Map.! vk_s)) <= fromIntegral k * t
              ?! SignedMaximumNumberBlocks
      proj (env, (_, _, d), b@(RBlock {})) = (bcEnvDIEnv env, d, rbCerts b)

instance Embed DELEG BC where
  wrapFailed = LedgerFailure
