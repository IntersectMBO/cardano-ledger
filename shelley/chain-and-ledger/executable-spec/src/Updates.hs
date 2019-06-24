{-# LANGUAGE EmptyDataDecls #-}

module Updates
  ( PPUpdateEnv(..)
  , PPUpdate(..)
  , updatePPup
  , ApName(..)
  , ApVer(..)
  , Metadata
  , Applications(..)
  , AVUpdate(..)
  , Update(..)
  , newAVs
  , votedValue
  , emptyUpdateState
  , emptyUpdate
  )
where

import qualified Data.Map.Strict               as Map
import qualified Data.List                     as List
                                                ( group )

import           BaseTypes
import           Coin
import           Keys
import           Slot

import           Numeric.Natural

newtype ApVer = ApVer Natural
  deriving (Show, Ord, Eq)

newtype ApName = ApName String
  deriving (Show, Ord, Eq)

data Metadata = Metadata -- for now, there are no requirements on Metadata
  deriving (Show, Ord, Eq)

data Applications = Applications {
  apps :: Map.Map ApName (ApVer, Metadata)
  } deriving (Show, Ord, Eq)

data AVUpdate = AVUpdate {
  aup :: Map.Map VKeyGenesis Applications
  } deriving (Show, Ord, Eq)

-- | Update Proposal
data Update = Update PPUpdate AVUpdate
  deriving (Show, Ord, Eq)

data PPUpdateEnv = PPUpdateEnv {
    slot :: Slot
  , dms  :: Dms
  } deriving (Show, Ord, Eq)

data Ppm = MinFeeA Integer
  | MinFeeB Natural
  | MaxBBSize Natural
  | MaxTxSize Natural
  | KeyDeposit Coin
  | KeyMinRefund UnitInterval
  | KeyDecayRate Rational
  | PoolDeposit Coin
  | PoolMinRefund UnitInterval
  | PoolDecayRate Rational
  | EMax Epoch
  | Nopt Natural
  | A0 Rational
  | Rho UnitInterval
  | Tau UnitInterval
  | ActiveSlotCoefficient UnitInterval
  | D UnitInterval
  | ExtraEntropy Seed
  | ProtocolVersion (Natural, Natural, Natural)
  deriving (Show, Ord, Eq)

data PPUpdate = PPUpdate (Map.Map VKeyGenesis (Map.Map Ppm Seed))
 deriving (Show, Ord, Eq)

-- | Update Protocol Parameter update with new values, prefer value from `pup1`
-- in case of already existing value in `pup0`
updatePPup :: PPUpdate -> PPUpdate -> PPUpdate
updatePPup (PPUpdate pup0') (PPUpdate pup1') = PPUpdate $ Map.union pup1' pup0'

newAVs :: Applications -> Map.Map Slot Applications -> Applications
newAVs avs favs = if not $ Map.null favs
  then let maxSlot = maximum $ Map.keys favs in favs Map.! maxSlot  -- value exists because maxSlot is in keys
  else avs

votedValue :: Eq a => Map.Map VKeyGenesis a -> Maybe a
votedValue vs | null elemLists = Nothing
              | otherwise      = Just $ (head . head) elemLists  -- elemLists contains an element
                                               -- and that list contains at
                                               -- least 5 elements
 where
  elemLists =
    filter (\l -> length l >= 5) $ List.group $ map snd $ Map.toList vs

emptyUpdateState
  :: ( Updates.PPUpdate
     , Updates.AVUpdate
     , Map.Map Slot Updates.Applications
     , Updates.Applications
     )
emptyUpdateState =
  (PPUpdate Map.empty, AVUpdate Map.empty, Map.empty, Applications Map.empty)

emptyUpdate :: Update
emptyUpdate = Update (PPUpdate Map.empty) (AVUpdate Map.empty)
