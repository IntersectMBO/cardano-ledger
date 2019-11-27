{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module CostModel
    (
      ExUnitsMSIG(..)
    , ExUnitsPLC(..)
    , ExUnits(..)
    , CostModMSIG(..)
    , CostModPLC(..)
    , CostMod(..)
    , defaultUnits
    , defaultModel
    ) where

import           Cardano.Binary (ToCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Coin (Coin (..))
import           GHC.Generics (Generic)

-- | comparing required resources for Plutus scripts
instance Ord ExUnitsPLC where
   (<=) (ExUnitsPLC rs1 mu1) (ExUnitsPLC rs2 mu2) =
     ((<=) rs1 rs2) && ((<=) mu1 mu2)

-- | comparing required resources for MSIG scripts
instance Ord ExUnitsMSIG where
  (<=) (ExUnitsMSIG ns1 m1) (ExUnitsMSIG ns2 m2) =
    ((<=) ns1 ns2) && ((<=) m1 m2)

-- | Temporary stand-in for actual types in the execution cost
-- for Plutus script execution
data ExUnitsPLC = ExUnitsPLC
  { -- | The types of computational resources relevant to the cost model
      reductionSteps           :: Integer
    , memoryUnits              :: Integer
  } deriving (Show, Eq, Generic, NoUnexpectedThunks)

-- | Temporary stand-in for actual types in the execution cost
-- for MSIG script execution
data ExUnitsMSIG = ExUnitsMSIG
  { -- | The types of computational resources relevant to the cost model
      numSigs                  :: Integer
    , maybeSomething           :: Integer
  } deriving (Show, Eq, Generic, NoUnexpectedThunks)

-- | The execution units of arbitrary scripts (MSIG, PLC so far)
data ExUnits =
       PLCUnits ExUnitsPLC
    |  MSIGUnits ExUnitsMSIG
  deriving (Show, Eq, Generic, NoUnexpectedThunks)


-- | Temporary stand-in for actual types in the cost model "prices"
-- for Plutus script execution
data CostModPLC = CostModPLC
  { -- | The types of computational resources relevant to the cost model
      stepPrice             :: Coin
    , memPrice              :: Coin
  } deriving (Show, Eq, Generic, NoUnexpectedThunks)

-- | Temporary stand-in for actual types in the cost model "prices"
-- for MSIG script execution
data CostModMSIG = CostModMSIG
  { -- | The types of computational resources relevant to the cost model
      sigPrice           :: Coin
    , smtPrice           :: Coin
  } deriving (Show, Eq, Generic, NoUnexpectedThunks)

-- | The "prices" of the resources in ExUnits (i.e. coefficients for conversion to Ada)
data CostMod = CostMod CostModPLC CostModMSIG
  deriving (Show, Eq, Generic, NoUnexpectedThunks)


-- | Default values
-- | Default execution units
defaultUnits :: ExUnits
defaultUnits = PLCUnits (ExUnitsPLC 0 0)

-- | Default prices
defaultModel :: CostMod
defaultModel = CostMod (CostModPLC 0 0) (CostModMSIG 0 0)

-- -- | The formula for conversion of execution cost into Ada
-- scriptFee :: CostMod -> ExUnits -> Coin
-- scriptFee (CostModPLC sp mp) (CostModPLC sp mp)
