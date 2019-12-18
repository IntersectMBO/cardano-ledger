{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scripts
  where

import           Cardano.Binary (ToCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Map
import           Cardano.Ledger.Shelley.Crypto
import           Keys (Hash)
import           CostModel

-- | Tag
data IsThing = Yes | Nope
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR)

-- | Validation tag
newtype IsValidating = IsValidating IsThing
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR)
-- | For-fee tag
newtype IsFee = IsFee IsThing
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR)

newtype DataHash crypto = DataHash (Hash (HASH crypto) (Data crypto))
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR)

-- STAND-IN things!!
-- temp plc script! Use these from Plutus
newtype ScriptPLC crypto = ScriptPLC Integer
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR)


-- | Use these from Plutus
newtype Data crypto = Data Integer
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR)

-- | temporary validator always returns true and same amount of resources
valPLCupTo :: CostMod -> ScriptPLC crypto -> (Data crypto, Data crypto, Data crypto, ExUnits)
  -> (IsValidating, ExUnits)
valPLCupTo _ _ _ = (IsValidating Yes, (PLCUnits (ExUnitsPLC 0 0)))
