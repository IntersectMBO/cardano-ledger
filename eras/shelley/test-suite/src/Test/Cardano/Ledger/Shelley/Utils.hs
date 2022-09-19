{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
    RawSeed (..),
  )
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash
  ( Blake2b_256,
    Hash,
    HashAlgorithm,
    hashToBytes,
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
import Cardano.Ledger.BaseTypes
  ( Globals (..),
    Network (..),
    Nonce,
    ShelleyBase,
    epochInfoPure,
    mkActiveSlotCoeff,
    mkNonceFromOutputVRF,
  )
import Cardano.Ledger.Binary (ToCBOR (..), hashWithEncoder, shelleyProtVer)
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (DSIGN)
import Cardano.Ledger.Keys
  ( KeyRole (..),
    VKey (..),
    updateKES,
  )
import Cardano.Ledger.Shelley.API (ApplyBlock)
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq)
import Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses)
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Tx (ShelleyTx, ShelleyTxOut)
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody)
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits)
import Cardano.Ledger.Slot (EpochNo, EpochSize (..), SlotNo)
import Cardano.Ledger.TreeDiff (ToExpr)
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
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace
  ( applySTSTest,
    checkTrace,
    (.-),
    (.->>),
  )
import Data.Coerce (Coercible, coerce)
import Data.Default.Class (Default)
import Data.Functor.Identity (runIdentity)
import Data.Time.Clock.POSIX
import Data.Typeable (Proxy (Proxy))
import Data.Word (Word64)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair, pattern KeyPair)
import Test.Cardano.Ledger.Core.Utils (unsafeBoundRational)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.QuickCheck (Arbitrary (..), chooseAny)
import Test.Tasty.HUnit
  ( Assertion,
    (@?=),
  )

type ChainProperty era =
  ( Mock (EraCrypto era),
    ApplyBlock era,
    GetLedgerView era,
    EraTx era
  )

-- ================================================

type ShelleyTest era =
  ( EraTx era,
    ShelleyEraTxBody era,
    Tx era ~ ShelleyTx era,
    TxSeq era ~ ShelleyTxSeq era,
    TxOut era ~ ShelleyTxOut era,
    TxWits era ~ ShelleyTxWits era,
    Split (Value era),
    Default (State (EraRule "PPUP" era)),
    Default (StashedAVVMAddresses era)
  )

class Split v where
  vsplit :: v -> Integer -> ([v], Coin)

type GenesisKeyPair c = KeyPair 'Genesis c

data RawSeed = RawSeed !Word64 !Word64 !Word64 !Word64 !Word64
  deriving (Eq, Show)

instance Arbitrary RawSeed where
  arbitrary =
    RawSeed
      <$> chooseAny
      <*> chooseAny
      <*> chooseAny
      <*> chooseAny
      <*> chooseAny

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
  mkSeedFromBytes . hashToBytes $ hashWithEncoder @Blake2b_256 shelleyProtVer toCBOR stuff

-- | For testing purposes, generate a deterministic genesis key pair given a seed.
mkGenKey ::
  DSIGNAlgorithm (DSIGN c) =>
  RawSeed ->
  (SignKeyDSIGN (DSIGN c), VKey kd c)
mkGenKey seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair ::
  forall c kd.
  DSIGNAlgorithm (DSIGN c) =>
  RawSeed ->
  (SignKeyDSIGN (DSIGN c), VKey kd c)
mkKeyPair seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair' ::
  DSIGNAlgorithm (DSIGN c) =>
  RawSeed ->
  KeyPair kd c
mkKeyPair' seed = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair seed

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

testGlobals :: Globals
testGlobals =
  Globals
    { epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1),
      slotsPerKESPeriod = 20,
      stabilityWindow = 33,
      randomnessStabilisationWindow = 33,
      securityParameter = 10,
      maxKESEvo = 10,
      quorum = 5,
      maxMajorPV = maxBound,
      maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000,
      activeSlotCoeff = mkActiveSlotCoeff . unsafeBoundRational $ 0.9,
      networkId = Testnet,
      systemStart = SystemStart $ posixSecondsToUTCTime 0
    }

runShelleyBase :: ShelleyBase a -> a
runShelleyBase act = runIdentity $ runReaderT act testGlobals

epochFromSlotNo :: SlotNo -> EpochNo
epochFromSlotNo = runIdentity . epochInfoEpoch (epochInfoPure testGlobals)

slotFromEpoch :: EpochNo -> SlotNo
slotFromEpoch = runIdentity . epochInfoFirst (epochInfoPure testGlobals)

epochSize :: EpochNo -> EpochSize
epochSize = runIdentity . epochInfoSize (epochInfoPure testGlobals)

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
  (BaseM s ~ ShelleyBase, STS s, Eq (State s), Show (State s), ToExpr (State s)) =>
  Environment s ->
  State s ->
  Signal s ->
  Either [PredicateFailure s] (State s) ->
  Assertion
testSTS env initSt signal (Right expectedSt) = do
  checkTrace @s runShelleyBase env $ pure initSt .- signal .->> expectedSt
testSTS env initSt sig predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @s (TRC (env, initSt, sig))
  st @?= predicateFailure

mkHash :: forall a h. HashAlgorithm h => Int -> Hash h a
mkHash i = coerce (hashWithEncoder @h shelleyProtVer toCBOR i)

getBlockNonce :: forall era. Era era => Block (BHeader (EraCrypto era)) era -> Nonce
getBlockNonce =
  mkNonceFromOutputVRF . certifiedOutput . bheaderEta . bhbody . bheader
