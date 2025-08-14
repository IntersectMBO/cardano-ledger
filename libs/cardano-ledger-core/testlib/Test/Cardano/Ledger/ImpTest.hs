{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.ImpTest (
  EraImp (..),
  HasKeyPairStore (..),
  keyPairsL,
  keyPairsByronL,
  KeyPairStore (..),
  freshKeyAddr,
  freshKeyAddr_,
  freshKeyHash,
  freshKeyPair,
  getKeyPair,
  freshByronKeyHash,
  freshBootstapAddress,
  getByronKeyPair,
  impAnn,
  impAnnDoc,
  impLogToExpr,
  impSetSeed,
  genSafeHash,
  genVRFVerKeyHash,
  genPoolParams,
  genProtVerCantFollow,

  -- * Logging
  Doc,
  AnsiStyle,
  logDoc,
  logText,
  logString,
  logToExpr,

  -- * Combinators
  simulateThenRestore,

  -- * ImpSpec re-exports
  ImpM,
  ImpInit,
  ImpException (..),
) where

import qualified Cardano.Chain.Common as Byron
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Ptr)
import Cardano.Ledger.Genesis (EraGenesis (..), NoGenesis (..))
import Cardano.Ledger.Keys (HasKeyRole (..), asWitness)
import Cardano.Ledger.State
import Control.Monad.State.Strict (MonadState (..), get, modify, put)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.TreeDiff (ansiWlExpr)
import Lens.Micro (Lens', lens, (%~))
import Lens.Micro.Mtl (use)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (ByronKeyPair (..), mkStakeRef)
import Test.Cardano.Ledger.Era (EraTest)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Slotting.Numeric ()
import Test.ImpSpec

-- | This is a preliminary state that is used to prepare the actual `ImpTestState`
data KeyPairStore = KeyPairStore
  { keyPairStore :: !(Map (KeyHash 'Witness) (KeyPair 'Witness))
  , keyPairByronStore :: !(Map BootstrapAddress ByronKeyPair)
  }

instance Semigroup KeyPairStore where
  (<>) ips1 ips2 =
    KeyPairStore
      { keyPairStore = keyPairStore ips1 <> keyPairStore ips2
      , keyPairByronStore = keyPairByronStore ips1 <> keyPairByronStore ips2
      }

instance Monoid KeyPairStore where
  mempty =
    KeyPairStore
      { keyPairStore = mempty
      , keyPairByronStore = mempty
      }

class HasKeyPairStore t where
  keyPairStoreL :: Lens' t KeyPairStore

keyPairsL :: HasKeyPairStore t => Lens' t (Map (KeyHash 'Witness) (KeyPair 'Witness))
keyPairsL = keyPairStoreL . lens keyPairStore (\x y -> x {keyPairStore = y})

keyPairsByronL :: HasKeyPairStore t => Lens' t (Map BootstrapAddress ByronKeyPair)
keyPairsByronL = keyPairStoreL . lens keyPairByronStore (\x y -> x {keyPairByronStore = y})

instance HasKeyPairStore KeyPairStore where
  keyPairStoreL = id

class EraTest era => EraImp era where
  initGenesis ::
    (HasKeyPairStore s, MonadState s m, HasStatefulGen g m, MonadFail m) =>
    m (Genesis era)
  default initGenesis ::
    (Monad m, Genesis era ~ NoGenesis era) =>
    m (Genesis era)
  initGenesis = pure NoGenesis

-- | Adds a ToExpr to the log, which is only shown if the test fails
logToExpr :: (HasCallStack, ToExpr a) => a -> ImpM t ()
logToExpr = logWithCallStack ?callStack . ansiWlExpr . toExpr

-- | Adds the result of an action to the log, which is only shown if the test fails
impLogToExpr :: (HasCallStack, ToExpr a) => ImpM t a -> ImpM t a
impLogToExpr action = do
  e <- action
  logWithCallStack ?callStack . ansiWlExpr $ toExpr e
  pure e

-- | Generates a random @SafeHash@
genSafeHash :: MonadGen m => m (SafeHash a)
genSafeHash = arbitrary

-- | Generates a random @VRFVerKeyHash@
genVRFVerKeyHash :: MonadGen m => m (VRFVerKeyHash (r :: KeyRoleVRF))
genVRFVerKeyHash = arbitrary

genPoolParams ::
  MonadGen m =>
  Coin ->
  KeyHash 'StakePool ->
  RewardAccount ->
  m PoolParams
genPoolParams ppMinCost khPool rewardAccount = do
  vrfHash <- genVRFVerKeyHash
  poolCostExtra <- arbitrary
  pledge <- arbitrary
  margin <- arbitrary
  pure
    PoolParams
      { ppVrf = vrfHash
      , ppRewardAccount = rewardAccount
      , ppRelays = mempty
      , ppPledge = pledge
      , ppOwners = mempty
      , ppMetadata = SNothing
      , ppMargin = margin
      , ppId = khPool
      , ppCost = ppMinCost <> poolCostExtra
      }

-- | Adds a key pair to the keyhash lookup map
addKeyPair ::
  (HasKeyPairStore s, MonadState s m) =>
  KeyPair r ->
  m (KeyHash r)
addKeyPair keyPair@(KeyPair vk _) = do
  let keyHash = hashKey vk
  modify $ keyPairsL %~ Map.insert (coerceKeyRole keyHash) (coerce keyPair)
  pure keyHash

-- | Looks up the `KeyPair` corresponding to the `KeyHash`. The `KeyHash` must be
-- created with `freshKeyHash` for this to work.
getKeyPair ::
  (HasCallStack, HasKeyPairStore s, MonadState s m) =>
  KeyHash r ->
  m (KeyPair r)
getKeyPair keyHash = do
  keyPairs <- use keyPairsL
  case Map.lookup (asWitness keyHash) keyPairs of
    Just keyPair -> pure $ coerce keyPair
    Nothing ->
      error $
        "Could not find a keypair corresponding to: "
          ++ show keyHash
          ++ "\nAlways use `freshKeyHash` to create key hashes."

-- | Generates a fresh `KeyHash` and stores the corresponding `KeyPair` in the
-- ImpTestState. If you also need the `KeyPair` consider using `freshKeyPair` for
-- generation or `getKeyPair` to look up the `KeyPair` corresponding to the `KeyHash`
freshKeyHash ::
  forall r s g m.
  (HasKeyPairStore s, MonadState s m, HasStatefulGen g m) =>
  m (KeyHash r)
freshKeyHash = fst <$> freshKeyPair

-- | Generate a random `KeyPair` and add it to the known keys in the Imp state
freshKeyPair ::
  forall r s g m.
  (HasKeyPairStore s, MonadState s m, HasStatefulGen g m) =>
  m (KeyHash r, KeyPair r)
freshKeyPair = do
  keyPair <- uniformM
  keyHash <- addKeyPair keyPair
  pure (keyHash, keyPair)

-- | Generate a random `Addr` that uses a `KeyHash`, and add the corresponding `KeyPair`
-- to the known keys in the Imp state.
freshKeyAddr_ ::
  (HasKeyPairStore s, MonadState s m, HasStatefulGen g m, MonadGen m) => m Addr
freshKeyAddr_ = snd <$> freshKeyAddr

-- | Generate a random `Addr` that uses a `KeyHash`, add the corresponding `KeyPair`
-- to the known keys in the Imp state, and return the `KeyHash` as well as the `Addr`.
freshKeyAddr ::
  (HasKeyPairStore s, MonadState s m, HasStatefulGen g m, MonadGen m) =>
  m (KeyHash 'Payment, Addr)
freshKeyAddr = do
  paymentKeyHash <- freshKeyHash @'Payment
  stakingKeyHash <-
    oneof
      [Just . mkStakeRef <$> freshKeyHash @'Staking, Just . mkStakeRef @Ptr <$> arbitrary, pure Nothing]
  pure (paymentKeyHash, mkAddr paymentKeyHash stakingKeyHash)

-- | Looks up the keypair corresponding to the `BootstrapAddress`. The `BootstrapAddress`
-- must be created with `freshBootstrapAddess` for this to work.
getByronKeyPair ::
  (HasCallStack, HasKeyPairStore s, MonadState s m) =>
  BootstrapAddress ->
  m ByronKeyPair
getByronKeyPair bootAddr = do
  keyPairs <- use keyPairsByronL
  case Map.lookup bootAddr keyPairs of
    Just keyPair -> pure keyPair
    Nothing ->
      error $
        "Could not find a keypair corresponding to: "
          ++ show bootAddr
          ++ "\nAlways use `freshByronKeyHash` to create key hashes."

-- | Generates a fresh `KeyHash` and stores the corresponding `ByronKeyPair` in the
-- ImpTestState. If you also need the `ByronKeyPair` consider using `freshByronKeyPair` for
-- generation or `getByronKeyPair` to look up the `ByronKeyPair` corresponding to the `KeyHash`
freshByronKeyHash ::
  (HasKeyPairStore s, MonadState s m, HasStatefulGen g m) =>
  m (KeyHash r)
freshByronKeyHash = coerceKeyRole . bootstrapKeyHash <$> freshBootstapAddress

freshBootstapAddress ::
  (HasKeyPairStore s, MonadState s m, HasStatefulGen g m) =>
  m BootstrapAddress
freshBootstapAddress = do
  keyPair@(ByronKeyPair verificationKey _) <- uniformM
  hasPayload <- uniformM
  payload <-
    if hasPayload
      then Just . Byron.HDAddressPayload <$> (uniformByteStringM =<< uniformRM (0, 63))
      else pure Nothing
  let asd = Byron.VerKeyASD verificationKey
      attrs = Byron.AddrAttributes payload (Byron.NetworkTestnet 0)
      bootAddr = BootstrapAddress $ Byron.makeAddress asd attrs
  modify $ keyPairsByronL %~ Map.insert bootAddr keyPair
  pure bootAddr

-- | An illegal ProtVer that skips 3 minor versions
genProtVerCantFollow :: MonadGen m => ProtVer -> m ProtVer
genProtVerCantFollow (ProtVer x y) =
  -- TODO Generate at random
  pure $ ProtVer x (y + 3)

-- | Runs a simulation action and then restores the ImpSpec state to what it was before the
-- simulation started.  This is useful for testing or running actions whose effects on the state
-- should not persist. The return value of the simulation is preserved, but any changes to the
-- internal state are discarded and replaced with the original snapshot.
simulateThenRestore ::
  ImpM t a ->
  ImpM t a
simulateThenRestore simulate = do
  stateSnapshot <- get
  result <- simulate
  result <$ put stateSnapshot
