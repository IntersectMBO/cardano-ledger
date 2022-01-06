{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Shelley.Utils
  ( mkSeedFromWords,
    mkCertifiedVRF,
    epochFromSlotNo,
    evolveKESUntil,
    slotFromEpoch,
    epochSize,
    mkHash,
    mkKeyPair,
    mkKeyPair',
    mkGenKey,
    mkKESKeyPair,
    mkVRFKeyPair,
    mkAddr,
    runShelleyBase,
    testGlobals,
    maxKESIterations,
    unsafeBoundRational,
    slotsPerKESIteration,
    testSTS,
    maxLLSupply,
    applySTSTest,
    GenesisKeyPair,
    getBlockNonce,
    ShelleyTest,
    ChainProperty,
    Split (..),
    RawSeed (..),
  )
where

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash
  ( Blake2b_256,
    Hash,
    HashAlgorithm,
    hashToBytes,
    hashWithSerialiser,
  )
import Cardano.Crypto.KES
  ( KESAlgorithm,
    SignKeyKES,
    VerKeyKES,
    deriveVerKeyKES,
    genKeyKES,
  )
import Cardano.Crypto.KES.Class (ContextKES)
import Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import Cardano.Crypto.VRF
  ( CertifiedVRF,
    SignKeyVRF,
    VRFAlgorithm (..),
    VerKeyVRF,
    certifiedOutput,
    deriveVerKeyVRF,
    evalCertified,
    genKeyVRF,
  )
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Address (Addr, pattern Addr)
import Cardano.Ledger.BaseTypes
  ( BoundedRational (..),
    Globals (..),
    Network (..),
    Nonce,
    ShelleyBase,
    epochInfo,
    mkActiveSlotCoeff,
    mkNonceFromOutputVRF,
  )
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (DSIGN)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto (..))
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Keys
  ( KeyPair,
    KeyRole (..),
    VKey (..),
    hashKey,
    updateKES,
    vKey,
    pattern KeyPair,
  )
import Cardano.Ledger.Shelley.API
  ( ApplyBlock,
    PParams,
  )
import Cardano.Ledger.Shelley.BlockChain (TxSeq)
import Cardano.Ledger.Shelley.Constraints
import Cardano.Ledger.Shelley.PParams (PParamsUpdate)
import Cardano.Ledger.Shelley.Tx (Tx, TxOut, WitnessSet)
import Cardano.Ledger.Slot (EpochNo, EpochSize (..), SlotNo)
import Cardano.Prelude (Coercible, asks)
import Cardano.Protocol.TPraos.API (GetLedgerView)
import Cardano.Protocol.TPraos.BHeader (BHBody (..), BHeader, bhbody)
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Slotting.EpochInfo
  ( epochInfoEpoch,
    epochInfoFirst,
    epochInfoSize,
    fixedEpochInfo,
  )
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace
  ( applySTSTest,
    checkTrace,
    (.-),
    (.->),
  )
import Data.Coerce (coerce)
import Data.Default.Class (Default)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX
import Data.Typeable (Proxy (Proxy))
import Data.Word (Word64)
import GHC.Stack
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.QuickCheck (Arbitrary (..), chooseAny)
import Test.Tasty.HUnit
  ( Assertion,
    (@?=),
  )

type ChainProperty era =
  ( UsesTxOut era,
    UsesPParams era,
    UsesValue era,
    UsesTxBody era,
    UsesAuxiliary era,
    Mock (Crypto era),
    ApplyBlock era,
    GetLedgerView era,
    Show (Core.Tx era),
    Eq (Core.Tx era)
  )

-- ================================================

type ShelleyTest era =
  ( UsesTxBody era,
    UsesValue era,
    UsesTxOut era,
    UsesScript era,
    UsesAuxiliary era,
    UsesPParams era,
    Core.Tx era ~ Tx era,
    Era.TxSeq era ~ TxSeq era,
    TxOut era ~ Core.TxOut era,
    PParams era ~ Core.PParams era,
    Core.PParamsDelta era ~ PParamsUpdate era,
    Core.Witnesses era ~ WitnessSet era,
    Split (Core.Value era),
    Default (State (Core.EraRule "PPUP" era)),
    Core.AnnotatedData (Core.Witnesses era)
  )

class Split v where
  vsplit :: v -> Integer -> ([v], Coin)

type GenesisKeyPair crypto = KeyPair 'Genesis crypto

data RawSeed = RawSeed !Word64 !Word64 !Word64 !Word64 !Word64
  deriving (Eq, Show)

instance Arbitrary RawSeed where
  arbitrary =
    RawSeed
      <$> chooseAny <*> chooseAny <*> chooseAny <*> chooseAny <*> chooseAny

instance ToCBOR RawSeed where
  toCBOR (RawSeed w1 w2 w3 w4 w5) = toCBOR (w1, w2, w3, w4, w5)
  encodedSizeExpr size _ = 1 + size (Proxy :: Proxy Word64) * 5

-- | Construct a seed from a bunch of Word64s
--
--   We multiply these words by some extra stuff to make sure they contain
--   enough bits for our seed.
mkSeedFromWords ::
  RawSeed ->
  Seed
mkSeedFromWords stuff =
  mkSeedFromBytes . hashToBytes $ hashWithSerialiser @Blake2b_256 toCBOR stuff

-- | For testing purposes, generate a deterministic genesis key pair given a seed.
mkGenKey ::
  DSIGNAlgorithm (DSIGN crypto) =>
  RawSeed ->
  (SignKeyDSIGN (DSIGN crypto), VKey kd crypto)
mkGenKey seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair ::
  forall crypto kd.
  DSIGNAlgorithm (DSIGN crypto) =>
  RawSeed ->
  (SignKeyDSIGN (DSIGN crypto), VKey kd crypto)
mkKeyPair seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair' ::
  DSIGNAlgorithm (DSIGN crypto) =>
  RawSeed ->
  KeyPair kd crypto
mkKeyPair' seed = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair seed

instance DSIGNAlgorithm (DSIGN crypto) => Arbitrary (KeyPair kd crypto) where
  arbitrary = mkKeyPair' <$> arbitrary

-- | For testing purposes, generate a deterministic VRF key pair given a seed.
mkVRFKeyPair ::
  VRFAlgorithm v =>
  RawSeed ->
  (SignKeyVRF v, VerKeyVRF v)
mkVRFKeyPair seed =
  let sk = genKeyVRF $ mkSeedFromWords seed
   in (sk, deriveVerKeyVRF sk)

-- | For testing purposes, create a VRF value
mkCertifiedVRF ::
  ( VRF.Signable v a,
    VRFAlgorithm v,
    ContextVRF v ~ (),
    Coercible b (CertifiedVRF v a)
  ) =>
  a ->
  SignKeyVRF v ->
  b
mkCertifiedVRF a sk =
  coerce $ evalCertified () a sk

-- | For testing purposes, generate a deterministic KES key pair given a seed.
mkKESKeyPair ::
  KESAlgorithm v =>
  RawSeed ->
  (SignKeyKES v, VerKeyKES v)
mkKESKeyPair seed =
  let sk = genKeyKES $ mkSeedFromWords seed
   in (sk, deriveVerKeyKES sk)

mkAddr ::
  CC.Crypto crypto =>
  (KeyPair 'Payment crypto, KeyPair 'Staking crypto) ->
  Addr crypto
mkAddr (payKey, stakeKey) =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey payKey)
    (StakeRefBase . KeyHashObj . hashKey $ vKey stakeKey)

-- | Convert to a bounded rational type why throwing an error on failure
unsafeBoundRational :: (HasCallStack, BoundedRational r) => Rational -> r
unsafeBoundRational r =
  fromMaybe (error $ "Could not convert from Rational: " ++ show r) $ boundRational r

testGlobals :: Globals
testGlobals =
  Globals
    { epochInfoWithErr = fixedEpochInfo (EpochSize 100) (mkSlotLength 1),
      slotsPerKESPeriod = 20,
      stabilityWindow = 33,
      randomnessStabilisationWindow = 33,
      securityParameter = 10,
      maxKESEvo = 10,
      quorum = 5,
      maxMajorPV = 1000,
      maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000,
      activeSlotCoeff = mkActiveSlotCoeff . unsafeBoundRational $ 0.9,
      networkId = Testnet,
      systemStart = SystemStart $ posixSecondsToUTCTime 0
    }

runShelleyBase :: ShelleyBase a -> a
runShelleyBase act = runIdentity $ runReaderT act testGlobals

epochFromSlotNo :: SlotNo -> EpochNo
epochFromSlotNo = runIdentity . epochInfoEpoch (epochInfo testGlobals)

slotFromEpoch :: EpochNo -> SlotNo
slotFromEpoch = runIdentity . epochInfoFirst (epochInfo testGlobals)

epochSize :: EpochNo -> EpochSize
epochSize = runIdentity . epochInfoSize (epochInfo testGlobals)

-- | Try to evolve KES key until specific KES period is reached, given the
-- current KES period.
evolveKESUntil ::
  (KESAlgorithm v, ContextKES v ~ ()) =>
  SignKeyKES v ->
  -- | Current KES period
  KESPeriod ->
  -- | Target KES period
  KESPeriod ->
  Maybe (SignKeyKES v)
evolveKESUntil sk1 (KESPeriod current) (KESPeriod target) = go sk1 current target
  where
    go !_ c t | t < c = Nothing
    go !sk c t | c == t = Just sk
    go !sk c t = case updateKES () sk c of
      Nothing -> Nothing
      Just sk' -> go sk' (c + 1) t

maxKESIterations :: Word64
maxKESIterations = runShelleyBase (asks maxKESEvo)

slotsPerKESIteration :: Word64
slotsPerKESIteration = runShelleyBase (asks slotsPerKESPeriod)

maxLLSupply :: Coin
maxLLSupply = Coin $ fromIntegral $ runShelleyBase (asks maxLovelaceSupply)

testSTS ::
  forall s.
  (BaseM s ~ ShelleyBase, STS s, Eq (State s), Show (State s)) =>
  Environment s ->
  State s ->
  Signal s ->
  Either [PredicateFailure s] (State s) ->
  Assertion
testSTS env initSt signal (Right expectedSt) = do
  checkTrace @s runShelleyBase env $ pure initSt .- signal .-> expectedSt
testSTS env initSt sig predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @s (TRC (env, initSt, sig))
  st @?= predicateFailure

mkHash :: forall a h. HashAlgorithm h => Int -> Hash h a
mkHash i = coerce (hashWithSerialiser @h toCBOR i)

getBlockNonce :: forall era. Era era => Block (BHeader (Crypto era)) era -> Nonce
getBlockNonce =
  mkNonceFromOutputVRF . certifiedOutput . bheaderEta . bhbody . bheader
