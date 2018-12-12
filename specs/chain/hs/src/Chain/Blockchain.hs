{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Chain.Blockchain where

import Control.Lens
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Bits (shift)
import Data.Queue
import Data.Set (Set)
import Numeric.Natural

import Crypto.Hash (hashlazy)
import Data.ByteString.Lazy.Char8 (pack)
import UTxO (Hash)

import Chain.GenesisBlock (genesisBlock)
import Delegation.Interface
  (DSIState, delegates, initDSIState, newCertsRule, updateCerts)
import Types
  ( VKey
  , Sig
  , Data
  , HCert
  , Interf
  , BC
  , Slot
  , Block(..)
  , BlockIx(..)
  , ProtParams(..)
  )


-- | Computes the hash of a block
hashBlock :: Block -> Hash
hashBlock b@(RBlock{}) = hashlazy (pack . show $ i) where MkBlockIx i = rbIx b
hashBlock b@(GBlock{}) = gbHash b

-- | Computes the block size in bytes
bSize :: Block -> Natural
bSize = undefined

-- | Computes the block header size in bytes
bHeaderSize :: Block -> Natural
bHeaderSize = undefined

-- | Size of the block sliding window
newtype K = MkK Natural deriving (Eq, Ord)

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


instance STS Interf where
  type State Interf = DSIState
  type Signal Interf = Set HCert
  type Environment Interf = Slot
  data PredicateFailure Interf
    = ConflictWithExistingCerts
    | InvalidNewCertificates
    deriving (Eq, Show)

  initialRules = []
  transitionRules = [newCertsRule]

-- | Remove the oldest entry in the queues in the range of the map if it is
--   more than *K* blocks away from the given block index
trimIx :: KeyToQMap -> K -> BlockIx -> KeyToQMap
trimIx m (MkK k) ix = foldl (flip f) m (Map.keysSet m)
 where
  f :: VKey -> KeyToQMap -> KeyToQMap
  f = Map.adjust (qRestrict ix)
  qRestrict :: BlockIx -> Queue BlockIx -> Queue BlockIx
  qRestrict (MkBlockIx ix') q = case headQueue q of
    Nothing               -> q
    Just (MkBlockIx h, r) -> if h + k < ix' then r else q

-- | Updates a map of genesis verification keys to their signed blocks in
-- a sliding window by adding a block index to a specified key's list
incIxMap :: BlockIx -> VKey -> KeyToQMap -> KeyToQMap
incIxMap ix = Map.adjust (pushQueue ix)

-- | Environment for blockchain rules
data BlockchainEnv = MkBlockChainEnv
  {
    bcEnvPp :: ProtParams
  , bcEnvSl :: Slot
  , bcEnvK :: K
  , bcEnvT :: T
  }

-- | Extends a chain by a block
extendChain :: (Environment BC, State BC, Signal BC) -> State BC
extendChain (env, st, b@(RBlock{})) =
  let
    p'          = b
    (sl, k, t ) = (bcEnvSl env, bcEnvK env, bcEnvT env)
    (m , p, ds) = st
    vk_d        = rbSigner b
    ix          = rbIx b
    vk_s        = delegates ds Map.! vk_d
    m'          = incIxMap ix vk_s (trimIx m k ix)
    ds'         = updateCerts sl (rbCerts b) ds
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
  type Environment BC = BlockchainEnv
  data PredicateFailure BC
    = InvalidPredecessor
    | NoDelegationRight
    | InvalidBlockSignature
    | InvalidBlockSize
    | InvalidHeaderSize
    | SignedMaximumNumberBlocks
    | LedgerFailure (PredicateFailure Interf)
    deriving (Eq, Show)

  -- There are only two inference rules: 1) for the initial state and 2) for
  -- extending the blockchain by a new block
  initialRules = [ return $ (Map.empty, genesisBlock, initDSIState) ]
  transitionRules =
    [ do
        TRC jc <- judgmentContext
        validPredecessor jc ?! InvalidPredecessor
        validBlockSize jc ?! InvalidBlockSize
        validHeaderSize jc ?! InvalidHeaderSize
        hasRight jc ?! NoDelegationRight
        validSignature jc ?! InvalidBlockSignature
        lessThanLimitSigned jc
        _ <- trans @Interf $ TRC (proj jc)
        return $ extendChain jc
    ]
    where
      -- valid predecessor
      validPredecessor (_, (_, p, _), b@(RBlock {})) =
         hashBlock p == rbHash b
      -- has a block size within protocol limits
      validBlockSize (env, _, b@(RBlock {})) =
           bSize b <= (blockSizeLimit b (bcEnvPp env))
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
        Map.member (rbSigner b) (delegates ds)
      -- valid signature
      validSignature (_, _, b@(RBlock {})) =
         verify (rbSigner b) (rbData b) (rbSig b)
      -- the delegator has not signed more than an allowed number of blocks
      -- in a sliding window of the last k blocks
      lessThanLimitSigned (env, st, b@(RBlock {})) =
        let
          (m, _, ds) = st
          ((MkK k), (MkT t)) = (bcEnvK env, bcEnvT env)
          vk_d = rbSigner b
          dsm = delegates ds
        in case Map.lookup vk_d dsm of
          Nothing   -> False ?! NoDelegationRight
          Just vk_s ->
            fromIntegral (sizeQueue (m Map.! vk_s)) <= (fromIntegral k) * t
              ?! SignedMaximumNumberBlocks
      proj (env, (_, _, d), b@(RBlock {})) = (bcEnvSl env, d, rbCerts b)

instance Embed Interf BC where
  wrapFailed = LedgerFailure
