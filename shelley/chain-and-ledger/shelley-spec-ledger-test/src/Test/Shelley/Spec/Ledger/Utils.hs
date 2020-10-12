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

module Test.Shelley.Spec.Ledger.Utils
  ( assertAll,
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
    mkAddr,
    runShelleyBase,
    testGlobals,
    maxKESIterations,
    unsafeMkUnitInterval,
    slotsPerKESIteration,
    testSTS,
    maxLLSupply,
    applySTSTest,
    GenesisKeyPair,
    MultiSigPairs,
    getBlockNonce,
    ShelleyTest,
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
import Cardano.Crypto.KES (KESAlgorithm, SignKeyKES, VerKeyKES, deriveVerKeyKES, genKeyKES)
import Cardano.Crypto.KES.Class (ContextKES)
import Cardano.Crypto.Libsodium.MLockedBytes (mlsbFromByteString)
import Cardano.Crypto.Seed (Seed, getSeedBytes, mkSeedFromBytes)
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
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN)
import Cardano.Ledger.Era (Crypto (..))
import Cardano.Ledger.Shelley (ShelleyBased)
import Cardano.Prelude (Coercible, asks)
import Cardano.Slotting.EpochInfo
  ( epochInfoEpoch,
    epochInfoFirst,
    epochInfoSize,
    fixedSizeEpochInfo,
  )
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace
  ( checkTrace,
    (.-),
    (.->),
  )
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word64)
import Hedgehog (MonadTest, (===))
import Shelley.Spec.Ledger.Address (Addr, pattern Addr)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Network (..),
    Nonce,
    ShelleyBase,
    UnitInterval,
    mkActiveSlotCoeff,
    mkNonceFromOutputVRF,
    mkUnitInterval,
  )
import Shelley.Spec.Ledger.BlockChain (BHBody (..), Block, bhbody, bheader)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..), StakeReference (..))
import Shelley.Spec.Ledger.Keys
  ( KeyPair,
    KeyRole (..),
    VKey (..),
    hashKey,
    updateKES,
    vKey,
    pattern KeyPair,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.STS.Bbody (BBODY, BbodyPredicateFailure)
import Shelley.Spec.Ledger.STS.Chain (CHAIN, ChainPredicateFailure)
import Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerPredicateFailure)
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS, LedgersPredicateFailure)
import Shelley.Spec.Ledger.Scripts (MultiSig)
import Shelley.Spec.Ledger.Slot (EpochNo, EpochSize (..), SlotNo)
import Shelley.Spec.Ledger.Tx (TxBody)
import Test.Tasty.HUnit
  ( Assertion,
    (@?=),
  )
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure, UTXOW)
import Shelley.Spec.Ledger.STS.Utxo (UtxoPredicateFailure, UTXO)
import Shelley.Spec.Ledger.STS.Deleg (DELEG, DelegPredicateFailure)
import Shelley.Spec.Ledger.STS.Delegs (DELEGS, DelegsPredicateFailure)

type ShelleyTest era =
  ( ShelleyBased era,
    Core.Value era ~ Coin,
    Core.TxBody era ~ TxBody era,
    PredicateFailure (CHAIN era) ~ ChainPredicateFailure era,
    PredicateFailure (LEDGERS era) ~ LedgersPredicateFailure era,
    PredicateFailure (LEDGER era) ~ LedgerPredicateFailure era,
    PredicateFailure (BBODY era) ~ BbodyPredicateFailure era,
    PredicateFailure (DELEGS era) ~ DelegsPredicateFailure era,
    PredicateFailure (DELEG era) ~ DelegPredicateFailure era,
    PredicateFailure (UTXOW era) ~ UtxowPredicateFailure era,
    PredicateFailure (UTXO era) ~ UtxoPredicateFailure era
  )

-- =======================================================

type GenesisKeyPair c = KeyPair 'Genesis c

type MultiSigPairs c = [(MultiSig c, MultiSig c)]

-- ================================================

assertAll :: (MonadTest m, Show a, Eq a) => (a -> Bool) -> [a] -> m ()
assertAll p xs = [] === filter (not . p) xs

-- | Construct a seed from a bunch of Word64s
--
--   We multiply these words by some extra stuff to make sure they contain
--   enough bits for our seed.
mkSeedFromWords ::
  (Word64, Word64, Word64, Word64, Word64) ->
  Seed
mkSeedFromWords stuff =
  mkSeedFromBytes . hashToBytes $ hashWithSerialiser @Blake2b_256 toCBOR stuff

-- | For testing purposes, generate a deterministic genesis key pair given a seed.
mkGenKey ::
  DSIGNAlgorithm (DSIGN (Crypto era)) =>
  (Word64, Word64, Word64, Word64, Word64) ->
  (SignKeyDSIGN (DSIGN (Crypto era)), VKey kd era)
mkGenKey seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair ::
  forall era kd.
  DSIGNAlgorithm (DSIGN (Crypto era)) =>
  (Word64, Word64, Word64, Word64, Word64) ->
  (SignKeyDSIGN (DSIGN (Crypto era)), VKey kd era)
mkKeyPair seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
-- mkKeyPair' :: (Word64, Word64, Word64, Word64, Word64) -> KeyPair kr
mkKeyPair' ::
  DSIGNAlgorithm (DSIGN (Crypto era)) =>
  (Word64, Word64, Word64, Word64, Word64) ->
  KeyPair kd era
mkKeyPair' seed = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair seed

-- | For testing purposes, generate a deterministic VRF key pair given a seed.
mkVRFKeyPair :: VRFAlgorithm v => (Word64, Word64, Word64, Word64, Word64) -> (SignKeyVRF v, VerKeyVRF v)
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
mkKESKeyPair :: KESAlgorithm v => (Word64, Word64, Word64, Word64, Word64) -> (SignKeyKES v, VerKeyKES v)
mkKESKeyPair seed =
  let sk = genKeyKES $ mlsbFromByteString $ getSeedBytes (mkSeedFromWords seed)
   in (sk, deriveVerKeyKES sk)

mkAddr :: Era era => (KeyPair 'Payment era, KeyPair 'Staking era) -> Addr era
mkAddr (payKey, stakeKey) =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey payKey)
    (StakeRefBase . KeyHashObj . hashKey $ vKey stakeKey)

-- | You vouch that argument is in [0; 1].
unsafeMkUnitInterval :: Ratio Word64 -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (error "could not construct unit interval") $ mkUnitInterval r

testGlobals :: Globals
testGlobals =
  Globals
    { epochInfo = fixedSizeEpochInfo $ EpochSize 100,
      slotsPerKESPeriod = 20,
      stabilityWindow = 33,
      randomnessStabilisationWindow = 33,
      securityParameter = 10,
      maxKESEvo = 10,
      quorum = 5,
      maxMajorPV = 1000,
      maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000,
      activeSlotCoeff = mkActiveSlotCoeff . unsafeMkUnitInterval $ 0.9,
      networkId = Testnet
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

applySTSTest ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (Either [[PredicateFailure s]] (State s))
applySTSTest ctx =
  applySTSOpts defaultOpts ctx <&> \case
    (st, []) -> Right st
    (_, pfs) -> Left pfs
  where
    defaultOpts =
      ApplySTSOpts
        { asoAssertions = AssertionsAll,
          asoValidation = ValidateAll
        }

testSTS ::
  forall s.
  (BaseM s ~ ShelleyBase, STS s, Eq (State s), Show (State s)) =>
  Environment s ->
  State s ->
  Signal s ->
  Either [[PredicateFailure s]] (State s) ->
  Assertion
testSTS env initSt signal (Right expectedSt) = do
  checkTrace @s runShelleyBase env $ pure initSt .- signal .-> expectedSt
testSTS env initSt sig predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @s (TRC (env, initSt, sig))
  st @?= predicateFailure

mkHash :: forall a h. HashAlgorithm h => Int -> Hash h a
mkHash i = coerce (hashWithSerialiser @h toCBOR i)

getBlockNonce :: forall era. Era era => Block era -> Nonce
getBlockNonce =
  mkNonceFromOutputVRF . certifiedOutput . bheaderEta . bhbody . bheader
