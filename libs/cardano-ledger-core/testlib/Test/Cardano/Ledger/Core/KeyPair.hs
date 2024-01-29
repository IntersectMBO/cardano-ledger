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
  mkCred,
  KeyPair (..),
  KeyPairs,
  mkWitnessVKey,
  mkWitnessesVKey,
  makeWitnessesFromScriptKeys,
  mkVKeyRewardAccount,
  mkKeyPair,
  mkKeyHash,
  ByronKeyPair (..),

  -- * Deprecations
  mkVKeyRwdAcnt,
)
where

import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.Hash (hashToBytes)
import qualified Cardano.Crypto.Hash as CH
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.Signing as Byron (
  SigningKey,
  VerificationKey (..),
  deterministicKeyGen,
 )
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (Network (Testnet), shelleyProtVer)
import Cardano.Ledger.Binary (EncCBOR (..), hashWithEncoder)
import Cardano.Ledger.Core (EraIndependentTxBody)
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
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Test.Cardano.Ledger.Binary.Arbitrary (genByteString)
import Test.QuickCheck

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

instance DSIGN.DSIGNAlgorithm (DSIGN c) => Arbitrary (KeyPair kd c) where
  arbitrary = do
    seed <- mkSeedFromBytes <$> genByteString (fromIntegral (DSIGN.seedSizeDSIGN (Proxy @(DSIGN c))))
    let signKey = DSIGN.genKeyDSIGN seed
    pure $
      KeyPair
        { vKey = VKey $ DSIGN.deriveVerKeyDSIGN signKey
        , sKey = signKey
        }

mkAddr ::
  Crypto c =>
  (KeyPair 'Payment c, KeyPair 'Staking c) ->
  Addr c
mkAddr (payKey, stakeKey) = Addr Testnet (mkCred payKey) (StakeRefBase $ mkCred stakeKey)

mkCred ::
  Crypto c =>
  KeyPair kr c ->
  Credential kr c
mkCred k = KeyHashObj . hashKey $ vKey k

-- | Create a witness for transaction
mkWitnessVKey ::
  forall c kr.
  ( Crypto c
  , DSignable c (CH.Hash (HASH c) EraIndependentTxBody)
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
  , DSignable c (CH.Hash (HASH c) EraIndependentTxBody)
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
mkKeyPair seed = KeyPair vk sk
  where
    vk = VKey (DSIGN.deriveVerKeyDSIGN sk)
    sk =
      DSIGN.genKeyDSIGN $
        mkSeedFromBytes . hashToBytes $
          hashWithEncoder @CH.Blake2b_256 shelleyProtVer encCBOR seed

data ByronKeyPair = ByronKeyPair
  { bkpVerificationKey :: !Byron.VerificationKey
  , bkpSigningKey :: !Byron.SigningKey
  }
  deriving (Generic, Show)

instance Arbitrary ByronKeyPair where
  arbitrary = uncurry ByronKeyPair . Byron.deterministicKeyGen <$> genByteString 32
