{-# LANGUAGE EmptyDataDecls #-}

module Updates
  ( PPUpdateEnv(..)
  , PPUpdate(..)
  , updatePPup
  )
where

import qualified Data.Map.Strict               as Map

import           BaseTypes
import           Coin
import           Keys
import           Slot

import           Numeric.Natural

data PPUpdateEnv = PPUpdateEnv {
    slot :: Slot
  , dms  :: Dms
  } deriving Show

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
  deriving Show

data PPUpdate = PPUpdate (Map.Map VKeyGenesis (Map.Map Ppm Seed))
 deriving Show

-- | Update Protocol Parameter update with new values, prefer value from `pup1`
-- in case of already existing value in `pup0`
updatePPup :: PPUpdate -> PPUpdate -> PPUpdate
updatePPup (PPUpdate pup0') (PPUpdate pup1') = PPUpdate $ Map.union pup1' pup0'
