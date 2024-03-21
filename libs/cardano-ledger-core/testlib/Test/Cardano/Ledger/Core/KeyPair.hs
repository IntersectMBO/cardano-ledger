{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Core.KeyPair (
  mkAddr,
  mkScriptAddr,
  mkCred,
  KeyPair (..),
  KeyPairs,
  mkWitnessVKey,
  mkWitnessesVKey,
  makeWitnessesFromScriptKeys,
  mkVKeyRewardAccount,
  mkKeyPair,
  mkKeyPairWithSeed,
  mkKeyHash,
  ByronKeyPair (..),
  mkBootKeyPairWithSeed,
  genByronVKeyAddr,
  genByronAddrFromVKey,
  mkSeedFromWords,
  mkKeyPairWithRawSeed,
  RawSeed(..),

  -- * Deprecations
  mkVKeyRwdAcnt,
)
where

import Cardano.Ledger.Binary (EncCBOR (..), hashWithEncoder, shelleyProtVer)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash (hashToBytes)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Seed (mkSeedFromBytes,Seed)
import qualified Cardano.Crypto.Signing as Byron (
  SigningKey,
  VerificationKey (..),
  deterministicKeyGen,
 )
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (Network (Testnet))
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (
  Credential (..),
  StakeReference (..),
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  DSignable,
  HasKeyRole,
  Hash,
  KeyHash (..),
  KeyRole (..),
  VKey (..),
  asWitness,
  hashKey,
  signedDSIGN,
 )
import Cardano.Ledger.Keys.WitVKey
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import System.Random.Stateful
import qualified Test.Cardano.Chain.Common.Gen as Byron
import qualified Test.Cardano.Crypto.Gen as Byron
import Test.Cardano.Ledger.Binary.Random (QC (..))
import Test.QuickCheck
import Test.QuickCheck.Hedgehog (hedgehog)
import Data.Word(Word64)

data KeyPair (kd :: KeyRole) c = KeyPair
  { vKey :: !(VKey kd c)
  , sKey :: !(DSIGN.SignKeyDSIGN (DSIGN c))
  }
  deriving (Generic, Show)

-- | Representation of a list of pairs of key pairs, e.g., pay and stake keys
type KeyPairs c = [(KeyPair 'Payment c, KeyPair 'Staking c)]

instance
  ( Crypto c
  , NFData (DSIGN.VerKeyDSIGN (DSIGN c))
  , NFData (DSIGN.SignKeyDSIGN (DSIGN c))
  ) =>
  NFData (KeyPair kd c)

instance Crypto c => NoThunks (KeyPair kd c)

instance HasKeyRole KeyPair

instance Crypto c => Arbitrary (KeyPair kd c) where
  arbitrary = uniformM QC

instance Crypto c => Uniform (KeyPair kd c) where
  uniformM g =
    mkKeyPairWithSeed
      <$> uniformByteStringM (fromIntegral (DSIGN.seedSizeDSIGN (Proxy @(DSIGN c)))) g

mkAddr ::
  Crypto c =>
  (KeyPair 'Payment c, KeyPair 'Staking c) ->
  Addr c
mkAddr (payKey, stakeKey) = Addr Testnet (mkCred payKey) (StakeRefBase $ mkCred stakeKey)

mkScriptAddr ::
  Crypto c =>
  ScriptHash c ->
  KeyPair 'Staking c ->
  Addr c
mkScriptAddr scriptHash stakeKey =
  Addr Testnet (ScriptHashObj scriptHash) (StakeRefBase $ mkCred stakeKey)

mkCred ::
  Crypto c =>
  KeyPair kr c ->
  Credential kr c
mkCred k = KeyHashObj . hashKey $ vKey k

-- | Create a witness for transaction
mkWitnessVKey ::
  forall c kr.
  ( Crypto c
  , DSignable c (Hash.Hash (HASH c) EraIndependentTxBody)
  ) =>
  SafeHash c EraIndependentTxBody ->
  KeyPair kr c ->
  WitVKey 'Witness c
mkWitnessVKey safe keys =
  WitVKey (asWitness $ vKey keys) (coerce $ signedDSIGN @c (sKey keys) (extractHash safe))

-- | Create witnesses for transaction
mkWitnessesVKey ::
  forall c kr.
  ( Crypto c
  , DSignable c (Hash.Hash (HASH c) EraIndependentTxBody)
  ) =>
  SafeHash c EraIndependentTxBody ->
  [KeyPair kr c] ->
  Set (WitVKey 'Witness c)
mkWitnessesVKey safe xs = Set.fromList (fmap (mkWitnessVKey safe) xs)

-- | From a list of key pairs and a set of key hashes required for a multi-sig
-- scripts, return the set of required keys.
makeWitnessesFromScriptKeys ::
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  SafeHash c EraIndependentTxBody ->
  Map (KeyHash kr c) (KeyPair kr c) ->
  Set (KeyHash kr c) ->
  Set (WitVKey 'Witness c)
makeWitnessesFromScriptKeys txbodyHash hashKeyMap scriptHashes =
  let witKeys = Map.restrictKeys hashKeyMap scriptHashes
   in mkWitnessesVKey txbodyHash (Map.elems witKeys)

mkVKeyRewardAccount ::
  Crypto c =>
  Network ->
  KeyPair 'Staking c ->
  RewardAccount c
mkVKeyRewardAccount network keys = RewardAccount network $ KeyHashObj (hashKey $ vKey keys)

mkVKeyRwdAcnt ::
  Crypto c =>
  Network ->
  KeyPair 'Staking c ->
  RewardAccount c
mkVKeyRwdAcnt = mkVKeyRewardAccount
{-# DEPRECATED mkVKeyRwdAcnt "Use `mkVKeyRewardAccount` instead" #-}

mkKeyHash :: Crypto c => Int -> KeyHash kd c
mkKeyHash = hashKey . vKey . mkKeyPair

mkKeyPair :: Crypto c => Int -> KeyPair r c
mkKeyPair = mkKeyPairWithSeed . Plain.serialize'

mkKeyPairWithSeed :: forall r c. Crypto c => BS.ByteString -> KeyPair r c
mkKeyPairWithSeed inputSeed = assert (seedSize == 32) $ KeyPair vk sk
  where
    seedSize = DSIGN.seedSizeDSIGN (Proxy @(DSIGN c))
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

-- ===============================================================

data RawSeed = RawSeed !Word64 !Word64 !Word64 !Word64 !Word64
  deriving (Eq, Show)

instance EncCBOR RawSeed where
  encCBOR (RawSeed w1 w2 w3 w4 w5) = encCBOR (w1, w2, w3, w4, w5)
  encodedSizeExpr size _ = 1 + size (Proxy :: Proxy Word64) * 5

mkKeyPairWithRawSeed ::
  forall c kd.
  DSIGN.DSIGNAlgorithm (DSIGN c) =>
  RawSeed ->
  (DSIGN.SignKeyDSIGN (DSIGN c), VKey kd c)
mkKeyPairWithRawSeed seed =
  let sk = DSIGN.genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ DSIGN.deriveVerKeyDSIGN sk)

-- | Construct a seed from a bunch of Word64s
--
--   We multiply these words by some extra stuff to make sure they contain
--   enough bits for our seed.
mkSeedFromWords ::
  RawSeed ->
  Seed
mkSeedFromWords stuff =
  mkSeedFromBytes . hashToBytes $ hashWithEncoder @Hash.Blake2b_256 shelleyProtVer encCBOR stuff
