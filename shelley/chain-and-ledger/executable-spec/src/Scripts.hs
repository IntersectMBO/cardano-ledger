{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Scripts
  where

import           Cardano.Binary (ToCBOR, FromCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
-- import           Coin (Coin (..))
import           GHC.Generics (Generic)
-- import           Data.Map
import           Cardano.Ledger.Shelley.Crypto
import           Keys (Hash)
import           CostModel

import           Keys (AnyKeyHash, pattern AnyKeyHash) --, GenKeyHash, Hash, KeyHash, pattern KeyHash,
--                     Sig, VKey, VKeyGenesis, VerKeyVRF, hashAnyKey, hash)


-- | A simple language for expressing conditions under which it is valid to
-- withdraw from a normal UTxO payment address or to use a stake address.
--
-- The use case is for expressing multi-signature payment addresses and
-- multi-signature stake addresses. These can be combined arbitrarily using
-- logical operations:
--
-- * multi-way \"and\";
-- * multi-way \"or\";
-- * multi-way \"N of M\".
--
-- This makes it easy to express multi-signature addresses, and provides an
-- extension point to express other validity conditions, e.g., as needed for
-- locking funds used with lightning.
--
data MultiSig crypto =
       -- | Require the redeeming transaction be witnessed by the spending key
       --   corresponding to the given verification key hash.
       RequireSignature   (AnyKeyHash crypto)

       -- | Require all the sub-terms to be satisfied.
     | RequireAllOf      [MultiSig crypto]

       -- | Require any one of the sub-terms to be satisfied.
     | RequireAnyOf      [MultiSig crypto]

       -- | Require M of the given sub-terms to be satisfied.
     | RequireMOf    Int [MultiSig crypto]
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (MultiSig crypto)

data Script crypto =
  MSig (MultiSig crypto) | SPLC (ScriptPLC crypto)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (Script crypto)

data ScriptHash crypto =
  ScriptHashMSig (Hash (HASH crypto) (MultiSig crypto))
  | ScriptHashPLC (Hash (HASH crypto) (ScriptPLC crypto))
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (ScriptHash crypto)

-- | Tag
data IsThing = Yes | Nope
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord)

-- | Validation tag
newtype IsValidating = IsValidating IsThing
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)
-- | For-fee tag
newtype IsFee = IsFee IsThing
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)

newtype DataHash crypto = DataHash (Hash (HASH crypto) (Data crypto))
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)

-- STAND-IN things!!
-- temp plc script! Use these from Plutus
newtype ScriptPLC crypto = ScriptPLC Integer
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)


-- | Use these from Plutus
newtype Data crypto = Data Integer
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord, ToCBOR, FromCBOR)

-- | temporary validator always returns true and same amount of resources
valPLCupTo :: CostMod -> ScriptPLC crypto -> ([Data crypto], ExUnits)
  -> (IsValidating, ExUnits)
valPLCupTo _ _ _ = (IsValidating Yes, (PLCUnits (ExUnitsPLC 0 0)))
