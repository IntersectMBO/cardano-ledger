{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Shelley.Utils (
  mkSeedFromWords,
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
  maxKESIterations,
  slotsPerKESIteration,
  runSTS,
  testSTS,
  maxLLSupply,
  applySTSTest,
  GenesisKeyPair,
  getBlockNonce,
  ChainProperty,
  RawSeed (..),
  Split (..),
  module CoreUtils,
) where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Crypto.KES (
  UnsoundPureKESAlgorithm (..),
  unsoundPureDeriveVerKeyKES,
  unsoundPureGenKeyKES,
 )
import Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import Cardano.Crypto.VRF (
  CertifiedVRF,
  SignKeyVRF,
  VRFAlgorithm (..),
  certifiedOutput,
  deriveVerKeyVRF,
  evalCertified,
  genKeyVRF,
 )
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Nonce,
  ShelleyBase,
  epochInfoPure,
  mkNonceFromOutputVRF,
 )
import Cardano.Ledger.Binary (EncCBOR (..), hashWithEncoder, shelleyProtVer)
import Cardano.Ledger.Block (Block, bheader)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Keys (DSIGN, VKey (..))
import Cardano.Ledger.Shelley.API (ApplyBlock)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Slot (EpochNo, EpochSize (..), SlotNo)
import Cardano.Protocol.Crypto (Crypto)
import Cardano.Protocol.TPraos.API (GetLedgerView)
import Cardano.Protocol.TPraos.BHeader (BHBody (..), BHeader, bhbody)
import Cardano.Slotting.EpochInfo (
  epochInfoEpoch,
  epochInfoFirst,
  epochInfoSize,
 )
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Reader (runReader, runReaderT)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word64)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair, pattern KeyPair)
import Test.Cardano.Ledger.Core.Utils as CoreUtils
import Test.Cardano.Ledger.Shelley.Arbitrary (RawSeed (..))
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.TreeDiff (ToExpr)
import Test.Cardano.Protocol.TPraos.Create (KESKeyPair (..), VRFKeyPair (..), evolveKESUntil)
import Test.Control.State.Transition.Trace (
  applySTSTest,
  checkTrace,
  (.-),
  (.->>),
 )
import Test.Tasty.HUnit (
  Assertion,
  (@?=),
 )

type ChainProperty era =
  ( ApplyBlock era
  , GetLedgerView era
  , EraTx era
  )

-- ================================================

class Split v where
  vsplit :: v -> Integer -> ([v], Coin)

-- ===============================================================================
-- Generating random transactions requires splitting Values into multiple Values
-- with the same underlying amount of Coin. This property is crucial to generating
-- transactions which have the preservation of ADA property. (vsplit n v) breaks
-- v into n different values, and one remainder Coin, where the sum of the Coin
-- in the original value, and the sum of the underlying Coin in the list plus the
-- remainder coin are equal.
-- Given:    let (vs,coin) = split n value
-- Then:     (coin value) == sum(map coin vs) <+> coin

-- We introduce a new class Split which supplies this operation.
-- As new kinds of values become instances of the Val class, and we want to generate
-- transactions over these values, we will have to add additional instances here.

instance Split Coin where
  vsplit (Coin n) 0 = ([], Coin n)
  vsplit (Coin n) m
    | m <= 0 = error "must split coins into positive parts"
    | otherwise = (take (fromIntegral m) (repeat (Coin (n `div` m))), Coin (n `rem` m))

type GenesisKeyPair c = KeyPair 'Genesis

-- | Construct a seed from a bunch of Word64s
--
--   We multiply these words by some extra stuff to make sure they contain
--   enough bits for our seed.
mkSeedFromWords ::
  RawSeed ->
  Seed
mkSeedFromWords stuff =
  mkSeedFromBytes . hashToBytes $ hashWithEncoder @HASH shelleyProtVer encCBOR stuff

-- | For testing purposes, generate a deterministic genesis key pair given a seed.
mkGenKey ::
  RawSeed ->
  (SignKeyDSIGN DSIGN, VKey kd)
mkGenKey seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair ::
  forall kd.
  RawSeed ->
  (SignKeyDSIGN DSIGN, VKey kd)
mkKeyPair seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair' ::
  RawSeed ->
  KeyPair kd
mkKeyPair' seed = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair seed

-- | For testing purposes, generate a deterministic VRF key pair given a seed.
mkVRFKeyPair :: Crypto c => RawSeed -> VRFKeyPair c
mkVRFKeyPair seed =
  let sk = genKeyVRF $ mkSeedFromWords seed
   in VRFKeyPair
        { vrfSignKey = sk
        , vrfVerKey = deriveVerKeyVRF sk
        }

-- | For testing purposes, create a VRF value
mkCertifiedVRF ::
  ( VRF.Signable v a
  , VRFAlgorithm v
  , ContextVRF v ~ ()
  , Coercible b (CertifiedVRF v a)
  ) =>
  a ->
  SignKeyVRF v ->
  b
mkCertifiedVRF a sk =
  coerce $ evalCertified () a sk

-- | For testing purposes, generate a deterministic KES key pair given a seed.
mkKESKeyPair :: Crypto c => RawSeed -> KESKeyPair c
mkKESKeyPair seed =
  let sk = unsoundPureGenKeyKES (mkSeedFromWords seed)
      vk = unsoundPureDeriveVerKeyKES sk
   in KESKeyPair
        { kesSignKey = sk
        , kesVerKey = vk
        }

runShelleyBase :: ShelleyBase a -> a
runShelleyBase act = runIdentity $ runReaderT act testGlobals

epochFromSlotNo :: SlotNo -> EpochNo
epochFromSlotNo = runIdentity . epochInfoEpoch (epochInfoPure testGlobals)

slotFromEpoch :: EpochNo -> SlotNo
slotFromEpoch = runIdentity . epochInfoFirst (epochInfoPure testGlobals)

epochSize :: EpochNo -> EpochSize
epochSize = runIdentity . epochInfoSize (epochInfoPure testGlobals)

maxKESIterations :: Word64
maxKESIterations = runShelleyBase (asks maxKESEvo)

slotsPerKESIteration :: Word64
slotsPerKESIteration = runShelleyBase (asks slotsPerKESPeriod)

maxLLSupply :: Coin
maxLLSupply = Coin $ fromIntegral $ runShelleyBase (asks maxLovelaceSupply)

runSTS ::
  forall rule era.
  ( BaseM (EraRule rule era) ~ ShelleyBase
  , STS (EraRule rule era)
  ) =>
  Globals ->
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  Either
    (NonEmpty (PredicateFailure (EraRule rule era)))
    (State (EraRule rule era), [Event (EraRule rule era)])
runSTS globals env st sig =
  let
    stsOpts =
      ApplySTSOpts
        { asoValidation = ValidateAll
        , asoEvents = EPReturn
        , asoAssertions = AssertionsAll
        }
   in
    (`runReader` globals) (applySTSOptsEither @(EraRule rule era) stsOpts (TRC (env, st, sig)))

testSTS ::
  forall s.
  (BaseM s ~ ShelleyBase, STS s, Eq (State s), Show (State s), ToExpr (State s)) =>
  Environment s ->
  State s ->
  Signal s ->
  Either (NonEmpty (PredicateFailure s)) (State s) ->
  Assertion
testSTS env initSt signal (Right expectedSt) = do
  checkTrace @s runShelleyBase env $ pure initSt .- signal .->> expectedSt
testSTS env initSt sig predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @s (TRC (env, initSt, sig))
  st @?= predicateFailure

mkHash :: forall a h. HashAlgorithm h => Int -> Hash h a
mkHash i = coerce (hashWithEncoder @h shelleyProtVer encCBOR i)

getBlockNonce :: forall era. Block (BHeader MockCrypto) era -> Nonce
getBlockNonce =
  mkNonceFromOutputVRF . certifiedOutput . bheaderEta . bhbody . bheader
