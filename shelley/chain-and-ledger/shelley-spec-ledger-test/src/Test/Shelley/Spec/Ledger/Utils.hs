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

module Test.Shelley.Spec.Ledger.Utils
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
    unsafeMkUnitInterval,
    slotsPerKESIteration,
    testSTS,
    maxLLSupply,
    applySTSTest,
    GenesisKeyPair,
    getBlockNonce,
    ShelleyTest,
    ShelleyUtxoSTS,
    ShelleyLedgerSTS,
    ShelleyChainSTS,
    ChainProperty,
    Split (..),
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
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto (..))
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Shelley.Constraints
import Cardano.Prelude (Coercible, asks)
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
  ( checkTrace,
    (.-),
    (.->),
  )
import Data.Coerce (coerce)
import Data.Default.Class (Default)
import Data.Functor ((<&>))
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import Data.Time.Clock.POSIX
import Data.Word (Word64)
import Shelley.Spec.Ledger.API
  ( ApplyBlock,
    CHAIN,
    ChainState,
    DPState,
    GetLedgerView,
    LedgerEnv,
    LedgerState,
    LedgersEnv,
    PParams,
  )
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
import Shelley.Spec.Ledger.BlockChain (BHBody (..), Block, TxSeq, bhbody, bheader)
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
import Shelley.Spec.Ledger.LedgerState (UTxOState (..))
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams (PParamsUpdate)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv)
import Shelley.Spec.Ledger.Slot (EpochNo, EpochSize (..), SlotNo)
import Shelley.Spec.Ledger.Tx (Tx, TxOut, WitnessSet)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Tasty.HUnit
  ( Assertion,
    (@?=),
  )

type ShelleyTest era =
  ( UsesTxBody era,
    UsesValue era,
    UsesTxOut era,
    UsesScript era,
    UsesAuxiliary era,
    UsesPParams era,
    Era.TxSeq era ~ TxSeq era,
    Era.TxInBlock era ~ Tx era,
    TxOut era ~ Core.TxOut era,
    PParams era ~ Core.PParams era,
    PParamsDelta era ~ PParamsUpdate era,
    Core.Witnesses era ~ WitnessSet era,
    Split (Core.Value era),
    Default (State (Core.EraRule "PPUP" era)),
    Core.AnnotatedData (Core.Witnesses era)
  )

type ChainProperty era =
  ( ShelleyBased era,
    Mock (Crypto era),
    ShelleyUtxoSTS era,
    ApplyBlock era,
    GetLedgerView era
  )

type ShelleyUtxoSTS era =
  ( STS (Core.EraRule "UTXOW" era),
    BaseM (Core.EraRule "UTXOW" era) ~ ShelleyBase,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era
  )

type ShelleyLedgerSTS era =
  ( STS (Core.EraRule "LEDGER" era),
    BaseM (Core.EraRule "LEDGER" era) ~ ShelleyBase,
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ (UTxOState era, DPState (Crypto era)),
    Signal (Core.EraRule "LEDGER" era) ~ Tx era,
    STS (Core.EraRule "LEDGERS" era),
    BaseM (Core.EraRule "LEDGERS" era) ~ ShelleyBase,
    Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (Core.EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Tx era)
  )

type ShelleyChainSTS era =
  ( STS (CHAIN era),
    BaseM (CHAIN era) ~ ShelleyBase,
    Environment (CHAIN era) ~ (),
    State (CHAIN era) ~ ChainState era,
    Signal (CHAIN era) ~ Block era
  )

class Split v where
  vsplit :: v -> Integer -> ([v], Coin)

type GenesisKeyPair crypto = KeyPair 'Genesis crypto

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
  DSIGNAlgorithm (DSIGN crypto) =>
  (Word64, Word64, Word64, Word64, Word64) ->
  (SignKeyDSIGN (DSIGN crypto), VKey kd crypto)
mkGenKey seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair ::
  forall crypto kd.
  DSIGNAlgorithm (DSIGN crypto) =>
  (Word64, Word64, Word64, Word64, Word64) ->
  (SignKeyDSIGN (DSIGN crypto), VKey kd crypto)
mkKeyPair seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
-- mkKeyPair' :: (Word64, Word64, Word64, Word64, Word64) -> KeyPair kr
mkKeyPair' ::
  DSIGNAlgorithm (DSIGN crypto) =>
  (Word64, Word64, Word64, Word64, Word64) ->
  KeyPair kd crypto
mkKeyPair' seed = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair seed

-- | For testing purposes, generate a deterministic VRF key pair given a seed.
mkVRFKeyPair ::
  VRFAlgorithm v =>
  (Word64, Word64, Word64, Word64, Word64) ->
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
  (Word64, Word64, Word64, Word64, Word64) ->
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

-- | You vouch that argument is in [0; 1].
unsafeMkUnitInterval :: Ratio Word64 -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (error "could not construct unit interval") $ mkUnitInterval r

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
      maxMajorPV = 1000,
      maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000,
      activeSlotCoeff = mkActiveSlotCoeff . unsafeMkUnitInterval $ 0.9,
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
