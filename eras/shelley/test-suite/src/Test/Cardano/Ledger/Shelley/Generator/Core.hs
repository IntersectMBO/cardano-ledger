{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Generator.Core
  ( AllIssuerKeys (..),
    GenEnv (..),
    ScriptSpace (..),
    TwoPhase3ArgInfo (..),
    TwoPhase2ArgInfo (..),
    ScriptInfo,
    KeySpace (..),
    pattern KeySpace,
    NatNonce (..),
    findPayKeyPairAddr,
    findPayKeyPairCred,
    findPayScriptFromCred,
    findStakeScriptFromCred,
    findPayScriptFromAddr,
    genBool,
    genCoinList,
    genInteger,
    genNatural,
    genWord64,
    genTxOut,
    genesisCoins,
    increasingProbabilityAt,
    pickStakeKey,
    toAddr,
    toCred,
    unitIntervalToNatural,
    mkBlock,
    mkBlockHeader,
    mkBlockFakeVRF,
    mkOCert,
    getKESPeriodRenewalNo,
    tooLateInEpoch,
    RawSeed (..),
    mkKeyPair,
    mkKeyPairs,
    mkGenKey,
    genesisAccountState,
    genCoin,
    PreAlonzo,
    hashData,
    findPlutus,
  )
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.VRF (evalCertified)
import Cardano.Ledger.Address (Addr (..), toAddr, toCred)
import Cardano.Ledger.BaseTypes
  ( BoundedRational (..),
    Nonce (..),
    ProtVer (..),
    StrictMaybe (..),
    UnitInterval,
    epochInfoPure,
    stabilityWindow,
  )
import Cardano.Ledger.Binary (ToCBOR)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core hiding (DataHash)
import Cardano.Ledger.Credential
  ( Credential (..),
    pattern KeyHashObj,
    pattern ScriptHashObj,
    pattern StakeRefBase,
    pattern StakeRefPtr,
  )
import Cardano.Ledger.Crypto (DSIGN)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys
  ( HasKeyRole (coerceKeyRole),
    Hash,
    KeyHash,
    KeyPair (..),
    KeyRole (..),
    SignKeyKES,
    SignKeyVRF,
    VKey,
    VerKeyKES,
    VerKeyVRF,
    asWitness,
    hashKey,
    signedDSIGN,
    signedKES,
    vKey,
  )
import Cardano.Ledger.SafeHash (SafeHash, unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.BlockChain (bBodySize)
import Cardano.Ledger.Shelley.LedgerState (AccountState (..), KeyPairs)
import Cardano.Ledger.Shelley.Tx
  ( pattern TxIn,
  )
import qualified Cardano.Ledger.Shelley.Tx as Ledger
import Cardano.Ledger.Shelley.TxWits
  ( ShelleyTxWits,
  )
import Cardano.Ledger.Slot
  ( BlockNo (..),
    Duration (..),
    SlotNo (..),
    epochInfoFirst,
    (*-),
  )
import Cardano.Ledger.UTxO (UTxO (UTxO))
import Cardano.Protocol.TPraos.BHeader
  ( BHeader,
    HashHeader,
    mkSeed,
    seedEta,
    seedL,
    pattern BHBody,
    pattern BHeader,
    pattern BlockHash,
  )
import Cardano.Protocol.TPraos.OCert
  ( KESPeriod (..),
    OCert,
    OCertSignable (..),
    pattern OCert,
  )
import Codec.Serialise (serialise)
import Control.Monad (replicateM)
import Control.Monad.Trans.Reader (asks)
import Data.ByteString.Lazy (toStrict)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Crypto.VRF.Fake (WithResult (..))
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (ExMock, Mock)
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass
  ( ScriptClass,
    exponential,
    mkKeyPairs,
    mkPayScriptHashMap,
    mkStakeScriptHashMap,
  )
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Utils
  ( GenesisKeyPair,
    RawSeed (..),
    epochFromSlotNo,
    evolveKESUntil,
    maxKESIterations,
    maxLLSupply,
    mkCertifiedVRF,
    mkGenKey,
    mkKeyPair,
    runShelleyBase,
  )
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

-- | For use in the Serialisation and Example Tests, which assume Shelley, Allegra, or Mary Eras.
type PreAlonzo era =
  ( TxWits era ~ ShelleyTxWits era,
    ToCBOR (TxAuxData era)
  )

-- =========================================

data AllIssuerKeys v (r :: KeyRole) = AllIssuerKeys
  { cold :: KeyPair r v,
    vrf :: (SignKeyVRF v, VerKeyVRF v),
    hot :: [(KESPeriod, (SignKeyKES v, VerKeyKES v))],
    hk :: KeyHash r v
  }
  deriving (Show)

type DataHash c = SafeHash c EraIndependentData

type ScriptInfo era =
  ( Map (ScriptHash (EraCrypto era)) (TwoPhase3ArgInfo era),
    Map (ScriptHash (EraCrypto era)) (TwoPhase2ArgInfo era)
  )

data TwoPhase3ArgInfo era = TwoPhase3ArgInfo
  { -- | A Plutus Script
    getScript3 :: Script era,
    -- | Its ScriptHash
    getHash3 :: ScriptHash (EraCrypto era),
    -- | A Data that will make it succeed
    getData3 :: PV1.Data,
    -- | A Redeemer that will make it succeed
    getRedeemer3 ::
      ( PV1.Data, -- The redeeming data
        Natural, -- The ExUnits memory count
        Natural -- The ExUnits steps count
      ),
    getSucceeds3 :: Bool
  }

data TwoPhase2ArgInfo era = TwoPhase2ArgInfo
  { -- | A Plutus Script
    getScript2 :: Script era,
    -- | Its ScriptHash
    getHash2 :: ScriptHash (EraCrypto era),
    -- | A Redeemer that will make it succeed
    getRedeemer2 ::
      ( PV1.Data, -- The redeeming data
        Natural, -- The ExUnits memory count
        Natural -- The ExUnits steps count
      ),
    getSucceeds2 :: Bool
  }

deriving instance Show (Script era) => Show (TwoPhase3ArgInfo era)

deriving instance Show (Script era) => Show (TwoPhase2ArgInfo era)

data ScriptSpace era = ScriptSpace
  { -- | A list of Two Phase 3 Arg Scripts and their associated data we can use.
    ssScripts3 :: [TwoPhase3ArgInfo era],
    -- | A list of Two Phase 2 Arg Scripts and their associated data we can use.
    ssScripts2 :: [TwoPhase2ArgInfo era],
    ssHash3 :: Map (ScriptHash (EraCrypto era)) (TwoPhase3ArgInfo era),
    ssHash2 :: Map (ScriptHash (EraCrypto era)) (TwoPhase2ArgInfo era)
  }

deriving instance Show (Script era) => Show (ScriptSpace era)

-- | Generator environment.
data GenEnv era = GenEnv
  { geKeySpace :: KeySpace era,
    geScriptSpapce :: ScriptSpace era,
    geConstants :: Constants
  }

-- | Collection of all keys which are required to generate a trace.
--
--   These are the _only_ keys which should be involved in the trace.
data KeySpace era = KeySpace_
  { ksCoreNodes :: [(GenesisKeyPair (EraCrypto era), AllIssuerKeys (EraCrypto era) 'GenesisDelegate)],
    -- | Bag of keys to be used for future genesis delegates
    ksGenesisDelegates :: [AllIssuerKeys (EraCrypto era) 'GenesisDelegate],
    -- | Bag of keys to be used for future stake pools
    ksStakePools :: [AllIssuerKeys (EraCrypto era) 'StakePool],
    -- | Bag of keys to be used for future payment/staking addresses
    ksKeyPairs :: KeyPairs (EraCrypto era),
    ksMSigScripts :: [(Script era, Script era)],
    -- | Index over the payment keys in 'ksKeyPairs'
    ksIndexedPaymentKeys :: Map (KeyHash 'Payment (EraCrypto era)) (KeyPair 'Payment (EraCrypto era)),
    -- | Index over the staking keys in 'ksKeyPairs'
    ksIndexedStakingKeys :: Map (KeyHash 'Staking (EraCrypto era)) (KeyPair 'Staking (EraCrypto era)),
    -- | Index over the cold key hashes in Genesis Delegates
    ksIndexedGenDelegates :: Map (KeyHash 'GenesisDelegate (EraCrypto era)) (AllIssuerKeys (EraCrypto era) 'GenesisDelegate),
    -- | Index over the pay script hashes in Script pairs
    ksIndexedPayScripts :: Map (ScriptHash (EraCrypto era)) (Script era, Script era),
    -- | Index over the stake script hashes in Script pairs
    ksIndexedStakeScripts :: Map (ScriptHash (EraCrypto era)) (Script era, Script era)
  }

deriving instance (Era era, Show (Script era)) => Show (KeySpace era)

pattern KeySpace ::
  forall era.
  ScriptClass era =>
  [(GenesisKeyPair (EraCrypto era), AllIssuerKeys (EraCrypto era) 'GenesisDelegate)] ->
  [AllIssuerKeys (EraCrypto era) 'GenesisDelegate] ->
  [AllIssuerKeys (EraCrypto era) 'StakePool] ->
  KeyPairs (EraCrypto era) ->
  [(Script era, Script era)] ->
  KeySpace era
pattern KeySpace
  ksCoreNodes
  ksGenesisDelegates
  ksStakePools
  ksKeyPairs
  ksMSigScripts <-
  KeySpace_
    { ksCoreNodes,
      ksGenesisDelegates,
      ksStakePools,
      ksKeyPairs,
      ksMSigScripts
    }
  where
    KeySpace ksCoreNodes ksGenesisDelegates ksStakePools ksKeyPairs ksMSigScripts =
      KeySpace_
        { ksCoreNodes,
          ksGenesisDelegates,
          ksStakePools,
          ksKeyPairs,
          ksIndexedPaymentKeys = mkPayKeyHashMap ksKeyPairs,
          ksIndexedStakingKeys = mkStakeKeyHashMap ksKeyPairs,
          ksIndexedGenDelegates = mkGenesisDelegatesHashMap ksCoreNodes ksGenesisDelegates,
          ksIndexedPayScripts = mkPayScriptHashMap @era ksMSigScripts,
          ksIndexedStakeScripts = mkStakeScriptHashMap @era ksMSigScripts,
          ksMSigScripts
        }

genCoin :: Integer -> Integer -> Gen Coin
genCoin minCoin maxCoin = Coin <$> exponential minCoin maxCoin

genBool :: Gen Bool
genBool = QC.arbitraryBoundedRandom

genInteger :: Integer -> Integer -> Gen Integer
genInteger lower upper = QC.choose (lower, upper)

-- | Generator for a natural number between 'lower' and 'upper'
genNatural :: Natural -> Natural -> Gen Natural
genNatural lower upper = fromInteger <$> QC.choose (lower', upper')
  where
    lower' = fromIntegral lower
    upper' = fromIntegral upper

-- | Generator for a Word64 between 'lower' and 'upper'
genWord64 :: Word64 -> Word64 -> Gen Word64
genWord64 lower upper =
  fromIntegral
    <$> genNatural (fromIntegral lower) (fromIntegral upper)

-- | Generate a mapping from genesis delegate cold key hash to the issuer keys.
-- Note: we index all possible genesis delegate keys, that is,
-- core nodes and all potential keys.
mkGenesisDelegatesHashMap ::
  (CC.Crypto c) =>
  [(GenesisKeyPair c, AllIssuerKeys c 'GenesisDelegate)] ->
  [AllIssuerKeys c 'GenesisDelegate] ->
  Map (KeyHash 'GenesisDelegate c) (AllIssuerKeys c 'GenesisDelegate)
mkGenesisDelegatesHashMap coreNodes genesisDelegates =
  Map.fromList (f <$> allDelegateKeys)
  where
    f issuerKeys = ((hashKey . vKey . cold) issuerKeys, issuerKeys)
    allDelegateKeys = (snd <$> coreNodes) <> genesisDelegates

-- | Generate a mapping from stake key hash to stake key pair, from a list of
-- (payment, staking) key pairs.
mkStakeKeyHashMap :: (CC.Crypto c) => KeyPairs c -> Map (KeyHash 'Staking c) (KeyPair 'Staking c)
mkStakeKeyHashMap keyPairs =
  Map.fromList (f <$> keyPairs)
  where
    f (_payK, stakeK) = ((hashKey . vKey) stakeK, stakeK)

-- | Generate a mapping from payment key hash to keypair
-- from a list of (payment, staking) key pairs.
mkPayKeyHashMap ::
  (CC.Crypto c) =>
  KeyPairs c ->
  Map (KeyHash 'Payment c) (KeyPair 'Payment c)
mkPayKeyHashMap keyPairs =
  Map.fromList (f <$> keyPairs)
  where
    f (payK, _stakeK) = ((hashKey . vKey) payK, payK)

-- | Find first matching key pair for a credential. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairCred ::
  forall era kr.
  Credential kr (EraCrypto era) ->
  Map (KeyHash kr (EraCrypto era)) (KeyPair kr (EraCrypto era)) ->
  KeyPair kr (EraCrypto era)
findPayKeyPairCred (KeyHashObj addr) keyHashMap =
  fromMaybe
    (error $ "findPayKeyPairCred: could not find a match for the given credential: " <> show addr)
    (Map.lookup addr keyHashMap)
findPayKeyPairCred _ _ =
  error "findPayKeyPairCred: expects only KeyHashObj"

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairAddr ::
  forall era.
  Addr (EraCrypto era) ->
  Map (KeyHash 'Payment (EraCrypto era)) (KeyPair 'Payment (EraCrypto era)) ->
  KeyPair 'Payment (EraCrypto era)
findPayKeyPairAddr a keyHashMap =
  case a of
    Addr _ addr (StakeRefBase _) -> findPayKeyPairCred @era addr keyHashMap
    Addr _ addr (StakeRefPtr _) -> findPayKeyPairCred @era addr keyHashMap
    _ ->
      error "findPayKeyPairAddr: expects only Base or Ptr addresses"

-- | Find matching multisig scripts for a credential.
findPayScriptFromCred ::
  forall era.
  Credential 'Witness (EraCrypto era) ->
  Map (ScriptHash (EraCrypto era)) (Script era, Script era) ->
  (Script era, Script era)
findPayScriptFromCred (ScriptHashObj scriptHash) scriptsByPayHash =
  fromMaybe
    (error "findPayScript: could not find matching script for given credential")
    (Map.lookup scriptHash scriptsByPayHash)
findPayScriptFromCred _ _ =
  error "findPayScriptFromCred: expects only ScriptHashObj"

-- | Find first matching script for a credential.
findStakeScriptFromCred ::
  Credential 'Witness (EraCrypto era) ->
  Map (ScriptHash (EraCrypto era)) (Script era, Script era) ->
  (Script era, Script era)
findStakeScriptFromCred (ScriptHashObj scriptHash) scriptsByStakeHash =
  fromMaybe
    (error "findStakeScriptFromCred: could not find matching script for given credential")
    (Map.lookup scriptHash scriptsByStakeHash)
findStakeScriptFromCred _ _ =
  error "findStakeScriptFromCred: expects only ScriptHashObj"

-- | Find first matching multisig script for an address.
findPayScriptFromAddr ::
  forall era.
  Addr (EraCrypto era) ->
  Map (ScriptHash (EraCrypto era)) (Script era, Script era) ->
  (Script era, Script era)
findPayScriptFromAddr (Addr _ scriptHash (StakeRefBase _)) scriptsByPayHash =
  findPayScriptFromCred @era (asWitness scriptHash) scriptsByPayHash
findPayScriptFromAddr (Addr _ scriptHash (StakeRefPtr _)) scriptsByPayHash =
  findPayScriptFromCred @era (asWitness scriptHash) scriptsByPayHash
findPayScriptFromAddr _ _ =
  error "findPayScriptFromAddr: expects only base and pointer script addresses"

-- | Select one random verification staking key from list of pairs of KeyPair.
pickStakeKey :: KeyPairs c -> Gen (VKey 'Staking c)
pickStakeKey keys = vKey . snd <$> QC.elements keys

-- | Generates a list of coins for the given 'Addr' and produced a 'TxOut' for each 'Addr'
--
-- Note: we need to keep the initial utxo coin sizes large enough so that
-- when we simulate sequences of transactions, we have enough funds available
-- to include certificates that require deposits.
genTxOut ::
  forall era.
  EraTxOut era =>
  Gen (Value era) ->
  [Addr (EraCrypto era)] ->
  Gen [TxOut era]
genTxOut genEraVal addrs = do
  values <- replicateM (length addrs) genEraVal
  return (uncurry mkBasicTxOut <$> zip addrs values)

-- | Generates a list of 'Coin' values of length between 'lower' and 'upper'
-- and with values between 'minCoin' and 'maxCoin'.
genCoinList :: Integer -> Integer -> Int -> Gen [Coin]
genCoinList minCoin maxCoin len = do
  replicateM len $ genCoin minCoin maxCoin

-- | Generate values the given distribution in 90% of the cases, and values at
-- the bounds of the range in 10% of the cases.
--
-- This can be used to generate enough extreme values. The exponential and
-- linear distributions provided by @hedgehog@ will generate a small percentage
-- of these (0-1%).
increasingProbabilityAt ::
  Gen a ->
  (a, a) ->
  Gen a
increasingProbabilityAt gen (lower, upper) =
  QC.frequency
    [ (5, pure lower),
      (90, gen),
      (5, pure upper)
    ]

-- | Try to map the unit interval to a natural number. We don't care whether
-- this is surjective. But it should be right inverse to `fromNatural` - that
-- is, one should be able to recover the `UnitInterval` value used here.
unitIntervalToNatural :: UnitInterval -> Natural
unitIntervalToNatural ui =
  toNat ((toInteger (maxBound :: Word64) % 1) * unboundRational ui)
  where
    toNat r = fromInteger (numerator r `quot` denominator r)

mkBlockHeader ::
  Mock c =>
  ProtVer ->
  -- | Hash of previous block
  HashHeader c ->
  -- | All keys in the stake pool
  AllIssuerKeys c r ->
  -- | Current slot
  SlotNo ->
  -- | Block number/chain length/chain "difficulty"
  BlockNo ->
  -- | EpochNo nonce
  Nonce ->
  -- | Period of KES (key evolving signature scheme)
  Word ->
  -- | KES period of key registration
  Word ->
  -- | Operational certificate
  OCert c ->
  -- | Block size
  Natural ->
  -- | Block body hash
  Hash c EraIndependentBlockBody ->
  BHeader c
mkBlockHeader protVer prev pkeys s blockNo enonce kesPeriod c0 oCert bodySize bodyHash =
  let (_, (sHot, _)) = head $ hot pkeys
      KeyPair vKeyCold _ = cold pkeys
      nonceNonce = mkSeed seedEta s enonce
      leaderNonce = mkSeed seedL s enonce
      bhb =
        BHBody
          blockNo
          s
          (BlockHash prev)
          (coerceKeyRole vKeyCold)
          (snd $ vrf pkeys)
          (coerce $ evalCertified () nonceNonce (fst $ vrf pkeys))
          (coerce $ evalCertified () leaderNonce (fst $ vrf pkeys))
          bodySize
          bodyHash
          oCert
          protVer
      kpDiff = kesPeriod - c0
      hotKey = case evolveKESUntil sHot (KESPeriod 0) (KESPeriod kpDiff) of
        Nothing ->
          error ("could not evolve key to iteration " ++ show (c0, kesPeriod, kpDiff))
        Just hkey -> hkey
      sig = signedKES () kpDiff bhb hotKey
   in BHeader bhb sig

mkBlock ::
  forall era r.
  (EraSegWits era, Mock (EraCrypto era)) =>
  -- | Hash of previous block
  HashHeader (EraCrypto era) ->
  -- | All keys in the stake pool
  AllIssuerKeys (EraCrypto era) r ->
  -- | Transactions to record
  [Tx era] ->
  -- | Current slot
  SlotNo ->
  -- | Block number/chain length/chain "difficulty"
  BlockNo ->
  -- | EpochNo nonce
  Nonce ->
  -- | Period of KES (key evolving signature scheme)
  Word ->
  -- | KES period of key registration
  Word ->
  -- | Operational certificate
  OCert (EraCrypto era) ->
  Block (BHeader (EraCrypto era)) era
mkBlock prev pkeys txns s blockNo enonce kesPeriod c0 oCert =
  let protVer = ProtVer (eraProtVerHigh @era) 0
      txseq = (toTxSeq @era . StrictSeq.fromList) txns
      bodySize = fromIntegral $ bBodySize protVer txseq
      bodyHash = hashTxSeq @era txseq
      bh = mkBlockHeader protVer prev pkeys s blockNo enonce kesPeriod c0 oCert bodySize bodyHash
   in Block bh txseq

-- | Create a block with a faked VRF result.
mkBlockFakeVRF ::
  forall era r.
  (EraSegWits era, ExMock (EraCrypto era)) =>
  -- | Hash of previous block
  HashHeader (EraCrypto era) ->
  -- | All keys in the stake pool
  AllIssuerKeys (EraCrypto era) r ->
  -- | Transactions to record
  [Tx era] ->
  -- | Current slot
  SlotNo ->
  -- | Block number/chain length/chain "difficulty"
  BlockNo ->
  -- | EpochNo nonce
  Nonce ->
  -- | Block nonce
  NatNonce ->
  -- | Praos leader value
  UnitInterval ->
  -- | Period of KES (key evolving signature scheme)
  Word ->
  -- | KES period of key registration
  Word ->
  -- | Operational certificate
  OCert (EraCrypto era) ->
  Block (BHeader (EraCrypto era)) era
mkBlockFakeVRF prev pkeys txns s blockNo enonce (NatNonce bnonce) l kesPeriod c0 oCert =
  let (_, (sHot, _)) = head $ hot pkeys
      KeyPair vKeyCold _ = cold pkeys
      nonceNonce = mkSeed seedEta s enonce
      leaderNonce = mkSeed seedL s enonce
      txseq = toTxSeq @era (StrictSeq.fromList txns)
      protVer = ProtVer (eraProtVerHigh @era) 0
      bhb =
        BHBody
          blockNo
          s
          (BlockHash prev)
          (coerceKeyRole vKeyCold)
          (snd $ vrf pkeys)
          ( mkCertifiedVRF
              (WithResult nonceNonce (fromIntegral bnonce))
              (fst $ vrf pkeys)
          )
          ( mkCertifiedVRF
              (WithResult leaderNonce (fromIntegral $ unitIntervalToNatural l))
              (fst $ vrf pkeys)
          )
          (fromIntegral $ bBodySize protVer txseq)
          (hashTxSeq @era txseq)
          oCert
          protVer
      kpDiff = kesPeriod - c0
      hotKey = case evolveKESUntil sHot (KESPeriod 0) (KESPeriod kpDiff) of
        Nothing ->
          error ("could not evolve key to iteration " ++ show (c0, kesPeriod, kpDiff))
        Just hkey -> hkey
      sig = signedKES () kpDiff bhb hotKey
      bh = BHeader bhb sig
   in Block bh txseq

-- | We provide our own nonces to 'mkBlock', which we then wish to recover as
-- the output of the VRF functions. In general, however, we just derive them
-- from a natural. Since the nonce is a hash, we do not want to recover it to
-- find a preimage. In testing, therefore, we just wrap the raw natural, which
-- we then encode into the fake VRF implementation.
newtype NatNonce = NatNonce Natural

mkOCert ::
  forall c r.
  (CC.Crypto c, Signable (DSIGN c) (OCertSignable c)) =>
  AllIssuerKeys c r ->
  Word64 ->
  KESPeriod ->
  OCert c
mkOCert pkeys n c0 =
  let (_, (_, vKeyHot)) = head $ hot pkeys
      KeyPair _vKeyCold sKeyCold = cold pkeys
   in OCert
        vKeyHot
        n
        c0
        (signedDSIGN @c sKeyCold (OCertSignable vKeyHot n c0))

-- | Takes a set of KES hot keys and checks to see whether there is one whose
-- range contains the current KES period. If so, return its index in the list of
-- hot keys.
getKESPeriodRenewalNo :: AllIssuerKeys h r -> KESPeriod -> Integer
getKESPeriodRenewalNo keys (KESPeriod kp) =
  go (hot keys) 0 kp
  where
    go [] _ _ = error "did not find enough KES renewals"
    go ((KESPeriod p, _) : rest) n k =
      if p <= k && k < p + fromIntegral maxKESIterations
        then n
        else go rest (n + 1) k

-- | True if the given slot is within the last `2 * stabilityWindow`
-- slots of the current epoch.
tooLateInEpoch :: SlotNo -> Bool
tooLateInEpoch s = runShelleyBase $ do
  ei <- asks epochInfoPure
  firstSlotNo <- epochInfoFirst ei (epochFromSlotNo s + 1)
  stabilityWindow <- asks stabilityWindow

  return (s >= firstSlotNo *- Duration (2 * stabilityWindow))

-- | Account with empty treasury
genesisAccountState :: AccountState
genesisAccountState =
  AccountState
    { asTreasury = Coin 0,
      asReserves = maxLLSupply
    }

-- | Creates the UTxO for a new ledger with the specified
-- genesis TxId and transaction outputs.
genesisCoins ::
  Ledger.TxId (EraCrypto era) ->
  [TxOut era] ->
  UTxO era
genesisCoins genesisTxId outs =
  UTxO $
    Map.fromList [(TxIn genesisTxId idx, out) | (idx, out) <- zip [minBound ..] outs]

-- ==================================================================
-- Operations on GenEnv that deal with ScriptSpace

hashData :: forall era. Era era => PV1.Data -> DataHash (EraCrypto era)
hashData x = unsafeMakeSafeHash (Hash.castHash (Hash.hashWith (toStrict . serialise) x))

{-
-- | Choose one of the preallocated PlutusScripts, and return it and its Hash
genPlutus :: forall era. GenEnv era -> Gen (Script era, ScriptHash (Crypto era), TwoPhaseInfo era)
genPlutus (GenEnv _ (ScriptSpace scripts _) _) = gettriple <$> oneof (pure <$> scripts)
  where gettriple (info@(TwoPhaseInfo script hash _data _rdmr)) = (script,hash,info)
-}

-- | Find the preallocated Script from its Hash.
findPlutus ::
  forall era.
  Era era =>
  GenEnv era ->
  ScriptHash (EraCrypto era) ->
  (Script era, StrictMaybe (DataHash (EraCrypto era)))
findPlutus (GenEnv keyspace (ScriptSpace _ _ mp3 mp2) _) hsh =
  case Map.lookup hsh mp3 of
    Just info3 -> (getScript3 info3, SJust (hashData @era (getData3 info3)))
    Nothing ->
      case Map.lookup hsh mp2 of
        Just info2 -> (getScript2 info2, SNothing)
        Nothing -> case Map.lookup hsh (ksIndexedPayScripts keyspace) of
          Just (pay, _ssStake) -> (pay, SNothing)
          Nothing ->
            case Map.lookup hsh (ksIndexedStakeScripts keyspace) of
              Just (_pay, stake) -> (stake, SNothing)
              Nothing -> error ("Can't find a Script for the hash: " ++ show hsh)
