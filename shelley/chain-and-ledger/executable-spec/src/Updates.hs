{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Updates
  ( Ppm(..)
  , PPUpdateEnv(..)
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

import           Data.ByteString (ByteString)
import qualified Data.List as List (group)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Word (Word8)

import           Cardano.Binary (ToCBOR (toCBOR), encodeListLen)

import           BaseTypes (Seed, UnitInterval)
import           Coin (Coin)
import           Keys (DSIGNAlgorithm, Dms, VKeyGenesis)
import           Slot (Epoch, Slot)

import           Numeric.Natural (Natural)

import           Ledger.Core ((∪))

newtype ApVer = ApVer Natural
  deriving (Show, Ord, Eq, ToCBOR)

newtype ApName = ApName ByteString
  deriving (Show, Ord, Eq, ToCBOR)

data Metadata = Metadata -- for now, there are no requirements on Metadata
  deriving (Show, Ord, Eq)

instance ToCBOR Metadata where
  toCBOR _ = toCBOR ()

newtype Applications = Applications {
  apps :: Map.Map ApName (ApVer, Metadata)
  } deriving (Show, Ord, Eq, ToCBOR)

newtype AVUpdate dsignAlgo = AVUpdate {
  aup :: Map.Map (VKeyGenesis dsignAlgo) Applications
  } deriving (Show, Ord, Eq, ToCBOR)

-- | Update Proposal
data Update dsignAlgo = Update (PPUpdate dsignAlgo) (AVUpdate dsignAlgo)
  deriving (Show, Ord, Eq)

instance DSIGNAlgorithm dsignAlgo => ToCBOR (Update dsignAlgo) where
  toCBOR (Update ppUpdate avUpdate) =
    encodeListLen 2
      <> toCBOR ppUpdate
      <> toCBOR avUpdate

data PPUpdateEnv dsignAlgo = PPUpdateEnv {
    slot :: Slot
  , dms  :: Dms dsignAlgo
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

instance ToCBOR Ppm where
  toCBOR = \case
    MinFeeA a -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR a

    MinFeeB b -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR b

    MaxBBSize maxBBSize ->
      encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR maxBBSize

    MaxTxSize maxTxSize ->
      encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR maxTxSize

    KeyDeposit keyDeposit ->
      encodeListLen 2 <> toCBOR (4 :: Word8) <> toCBOR keyDeposit

    KeyMinRefund keyMinRefund ->
      encodeListLen 2 <> toCBOR (5 :: Word8) <> toCBOR keyMinRefund

    KeyDecayRate keyDecayRate ->
      encodeListLen 2 <> toCBOR (6 :: Word8) <> toCBOR keyDecayRate

    PoolDeposit poolDeposit ->
      encodeListLen 2 <> toCBOR (7 :: Word8) <> toCBOR poolDeposit

    PoolMinRefund poolMinRefund ->
      encodeListLen 2 <> toCBOR (8 :: Word8) <> toCBOR poolMinRefund

    PoolDecayRate poolDecayRate ->
      encodeListLen 2 <> toCBOR (9 :: Word8) <> toCBOR poolDecayRate

    EMax eMax -> encodeListLen 2 <> toCBOR (10 :: Word8) <> toCBOR eMax

    Nopt nopt -> encodeListLen 2 <> toCBOR (11 :: Word8) <> toCBOR nopt

    A0 a0 -> encodeListLen 2 <> toCBOR (12 :: Word8) <> toCBOR a0

    Rho rho -> encodeListLen 2 <> toCBOR (13 :: Word8) <> toCBOR rho

    Tau tau -> encodeListLen 2 <> toCBOR (14 :: Word8) <> toCBOR tau

    ActiveSlotCoefficient activeSlotCoeff ->
      encodeListLen 2 <> toCBOR (15 :: Word8) <> toCBOR activeSlotCoeff

    D d -> encodeListLen 2 <> toCBOR (16 :: Word8) <> toCBOR d

    ExtraEntropy extraEntropy ->
      encodeListLen 2 <> toCBOR (17 :: Word8) <> toCBOR extraEntropy

    ProtocolVersion protocolVersion ->
      encodeListLen 2 <> toCBOR (18 :: Word8) <> toCBOR protocolVersion

newtype PPUpdate dsignAlgo
  = PPUpdate (Map.Map (VKeyGenesis dsignAlgo) (Set.Set Ppm))
  deriving (Show, Ord, Eq, ToCBOR)

-- | Update Protocol Parameter update with new values, prefer value from `pup1`
-- in case of already existing value in `pup0`
updatePPup
  :: DSIGNAlgorithm dsignAlgo
  => PPUpdate dsignAlgo
  -> PPUpdate dsignAlgo
  -> PPUpdate dsignAlgo
updatePPup (PPUpdate pup0') (PPUpdate pup1') = PPUpdate (pup1' ∪ pup0')

newAVs :: Applications -> Map.Map Slot Applications -> Applications
newAVs avs favs = if not $ Map.null favs
  then let maxSlot = maximum $ Map.keys favs in favs Map.! maxSlot  -- value exists because maxSlot is in keys
  else avs

votedValue :: Eq a => Map.Map (VKeyGenesis dsignAlgo) a -> Maybe a
votedValue vs | null elemLists = Nothing
              | otherwise      = Just $ (head . head) elemLists  -- elemLists contains an element
                                               -- and that list contains at
                                               -- least 5 elements
 where
  elemLists =
    filter (\l -> length l >= 5) $ List.group $ map snd $ Map.toList vs

emptyUpdateState
  :: ( Updates.PPUpdate dsignAlgo
     , Updates.AVUpdate dsignAlgo
     , Map.Map Slot Updates.Applications
     , Updates.Applications
     )
emptyUpdateState =
  (PPUpdate Map.empty, AVUpdate Map.empty, Map.empty, Applications Map.empty)

emptyUpdate :: Update dsignAlgo
emptyUpdate = Update (PPUpdate Map.empty) (AVUpdate Map.empty)
