{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Core.KeyPair (
  MakeCredential (..),
  MakeStakeReference (..),
  mkAddr,
  mkScriptAddr,
  mkCred,
  KeyPair (..),
  KeyPairs,
  mkWitnessVKey,
  mkWitnessesVKey,
  makeWitnessesFromScriptKeys,
  mkKeyHashWitFunPair,
  mkVKeyRewardAccount,
  mkKeyPair,
  mkKeyPairWithSeed,
  mkKeyHash,
  ByronKeyPair (..),
  mkBootKeyPairWithSeed,
  genByronVKeyAddr,
  genByronAddrFromVKey,
) where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash (hashToBytes)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.Signing as Byron (
  SigningKey,
  VerificationKey (..),
  deterministicKeyGen,
 )
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Binary (EncCBOR (encCBOR))
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import qualified Cardano.Ledger.Binary.Coders as Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (
  Credential (..),
  Ptr,
  StakeReference (..),
 )
import Cardano.Ledger.Keys (
  DSIGN,
  HasKeyRole,
  VKey (..),
  asWitness,
  signedDSIGN,
 )
import Cardano.Ledger.Keys.WitVKey
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.TreeDiff as Tree (Expr (..))
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import System.Random.Stateful
import qualified Test.Cardano.Chain.Common.Gen as Byron
import qualified Test.Cardano.Crypto.Gen as Byron
import Test.Cardano.Ledger.Binary.Random (QC (..))
import Test.Cardano.Ledger.Common (ToExpr (..))
import Test.Cardano.Ledger.TreeDiff ()
import Test.QuickCheck
import Test.QuickCheck.Hedgehog (hedgehog)

data KeyPair (kd :: KeyRole) = KeyPair
  { vKey :: !(VKey kd)
  , sKey :: !(DSIGN.SignKeyDSIGN DSIGN)
  }
  deriving (Generic, Show)

-- | Representation of a list of pairs of key pairs, e.g., pay and stake keys
type KeyPairs = [(KeyPair 'Payment, KeyPair 'Staking)]

instance NFData (KeyPair kd)

instance NoThunks (KeyPair kd)

instance HasKeyRole KeyPair

instance Arbitrary (KeyPair kd) where
  arbitrary = uniformM QC

instance Uniform (KeyPair kd) where
  uniformM g =
    mkKeyPairWithSeed
      <$> uniformByteStringM (fromIntegral (DSIGN.seedSizeDSIGN (Proxy @DSIGN))) g

instance Typeable r => EncCBOR (KeyPair r) where
  encCBOR (KeyPair x y) = encode $ Coders.Rec KeyPair !> To x !> To y

deriving instance Typeable r => Eq (KeyPair r)

instance ToExpr (KeyPair r) where
  toExpr (KeyPair x y) = Tree.App "KeyPair" [toExpr x, Tree.App (take 10 (show y)) []]

class MakeCredential c r where
  mkCredential :: c -> Credential r

instance MakeCredential (Credential r) r where
  mkCredential = id

instance MakeCredential (KeyPair r) r where
  mkCredential = KeyHashObj . hashKey . vKey

instance MakeCredential (KeyHash r) r where
  mkCredential = KeyHashObj

instance MakeCredential ScriptHash r where
  mkCredential = ScriptHashObj

class MakeStakeReference c where
  mkStakeRef :: c -> StakeReference
  default mkStakeRef :: MakeCredential c 'Staking => c -> StakeReference
  mkStakeRef = StakeRefBase . mkCredential

instance MakeStakeReference StakeReference where
  mkStakeRef = id

instance MakeStakeReference (Credential 'Staking)

instance MakeStakeReference (KeyPair 'Staking)

instance MakeStakeReference (KeyHash 'Staking)

instance MakeStakeReference ScriptHash

instance MakeStakeReference Ptr where
  mkStakeRef = StakeRefPtr

instance MakeStakeReference (Maybe StakeReference) where
  mkStakeRef = mkStakeRefMaybe

instance MakeStakeReference (Maybe (Credential 'Staking)) where
  mkStakeRef = mkStakeRefMaybe

instance MakeStakeReference (Maybe (KeyPair 'Staking)) where
  mkStakeRef = mkStakeRefMaybe

instance MakeStakeReference (Maybe (KeyHash 'Staking)) where
  mkStakeRef = mkStakeRefMaybe

instance MakeStakeReference (Maybe ScriptHash) where
  mkStakeRef = mkStakeRefMaybe

mkStakeRefMaybe :: MakeStakeReference c => Maybe c -> StakeReference
mkStakeRefMaybe = \case
  Nothing -> StakeRefNull
  Just c -> mkStakeRef c

-- | Construct a `Testnet` address from payment and staking components
mkAddr :: (MakeCredential p 'Payment, MakeStakeReference s) => p -> s -> Addr
mkAddr pay stake = Addr Testnet (mkCredential pay) (mkStakeRef stake)

mkCred :: KeyPair kr -> Credential kr
mkCred = mkCredential
{-# DEPRECATED mkCred "In favor of `mkCredential`" #-}

mkScriptAddr :: ScriptHash -> KeyPair 'Staking -> Addr
mkScriptAddr = mkAddr
{-# DEPRECATED mkScriptAddr "In favor of `mkAddr`" #-}

-- | Create a witness for transaction
mkWitnessVKey ::
  SafeHash EraIndependentTxBody ->
  KeyPair kr ->
  WitVKey 'Witness
mkWitnessVKey safe keys =
  WitVKey (asWitness $ vKey keys) (coerce $ signedDSIGN (sKey keys) (extractHash safe))

-- | Create witnesses for transaction
mkWitnessesVKey ::
  SafeHash EraIndependentTxBody ->
  [KeyPair kr] ->
  Set (WitVKey 'Witness)
mkWitnessesVKey safe xs = Set.fromList (fmap (mkWitnessVKey safe) xs)

-- | From a list of key pairs and a set of key hashes required for a multi-sig
-- scripts, return the set of required keys.
makeWitnessesFromScriptKeys ::
  SafeHash EraIndependentTxBody ->
  Map (KeyHash kr) (KeyPair kr) ->
  Set (KeyHash kr) ->
  Set (WitVKey 'Witness)
makeWitnessesFromScriptKeys txbodyHash hashKeyMap scriptHashes =
  let witKeys = Map.restrictKeys hashKeyMap scriptHashes
   in mkWitnessesVKey txbodyHash (Map.elems witKeys)

-- | When wrting a test which needs a KeyHash, and you will also later need a witness
--   for that function, use 'keyHashWitFunPair'. Since one cannot make a witness until
--   one has SafeHash of the TxBody that the KeyHash is embedded. The second part of the
--   pair is a (SafeHash to WitVKey) function. Use it something like this
--   do (key,witfun) <- keyHashWitFunPair
--      txbody <- ... key ...
--      let safehash = hashAnnotated txbody
--          tx = ... txbody ... (witfun safehash) ...
mkKeyHashWitFunPair ::
  forall kr.
  Gen (KeyHash kr, SafeHash EraIndependentTxBody -> WitVKey 'Witness)
mkKeyHashWitFunPair = do
  keyPair@(KeyPair vk _) <- arbitrary @(KeyPair kr)
  pure (hashKey vk, \safeHash -> mkWitnessVKey safeHash keyPair)

mkVKeyRewardAccount ::
  Network ->
  KeyPair 'Staking ->
  RewardAccount
mkVKeyRewardAccount network keys = RewardAccount network $ KeyHashObj (hashKey $ vKey keys)

mkKeyHash :: Int -> KeyHash kd
mkKeyHash = hashKey . vKey . mkKeyPair

mkKeyPair :: Int -> KeyPair r
mkKeyPair = mkKeyPairWithSeed . Plain.serialize'

mkKeyPairWithSeed :: BS.ByteString -> KeyPair r
mkKeyPairWithSeed inputSeed = KeyPair vk sk
  where
    vk = VKey (DSIGN.deriveVerKeyDSIGN sk)
    sk = DSIGN.genKeyDSIGN $ mkSeedFromBytes $ ensure32ByteSeed inputSeed

data ByronKeyPair = ByronKeyPair
  { bkpVerificationKey :: !Byron.VerificationKey
  , bkpSigningKey :: !Byron.SigningKey
  }
  deriving (Generic, Show)

instance Arbitrary ByronKeyPair where
  arbitrary = uniformM QC

instance Uniform ByronKeyPair where
  uniformM g = mkBootKeyPairWithSeed <$> uniformByteStringM 32 g

mkBootKeyPairWithSeed :: BS.ByteString -> ByronKeyPair
mkBootKeyPairWithSeed = uncurry ByronKeyPair . Byron.deterministicKeyGen . ensure32ByteSeed

ensure32ByteSeed :: BS.ByteString -> BS.ByteString
ensure32ByteSeed inputSeed
  | BS.length inputSeed /= seedSize =
      hashToBytes $ Hash.hashWith @Hash.Blake2b_256 id inputSeed
  | otherwise = inputSeed
  where
    seedSize = 32

genByronVKeyAddr :: Gen (Byron.VerificationKey, Byron.Address)
genByronVKeyAddr = do
  vkey <- hedgehog Byron.genVerificationKey
  addr <- genByronAddrFromVKey vkey
  pure (vkey, addr)

genByronAddrFromVKey :: Byron.VerificationKey -> Gen Byron.Address
genByronAddrFromVKey vkey =
  Byron.makeAddress (Byron.VerKeyASD vkey) <$> hedgehog Byron.genAddrAttributes
