{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    GenEnv (..),
    KeySpace (..),
    pattern KeySpace,
    NatNonce (..),
    findPayKeyPairAddr,
    findPayKeyPairCred,
    findPayScriptFromCred,
    findStakeScriptFromCred,
    findPayScriptFromAddr,
    genBool,
    genCoin,
    genCoinList,
    genInteger,
    genNatural,
    genWord64,
    genTxOut,
    increasingProbabilityAt,
    mkScriptsFromKeyPair,
    pickStakeKey,
    toAddr,
    toCred,
    zero,
    unitIntervalToNatural,
    mkBlock,
    mkOCert,
    getKESPeriodRenewalNo,
    tooLateInEpoch,
    mkKeyPair,
    mkKeyPairs,
    mkGenKey,
    mkMSigScripts,
    mkMSigCombinations,
    genesisAccountState,
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import Control.Monad (replicateM)
import Control.Monad.Trans.Reader (asks)
import Data.Coerce (coerce)
import Data.List (foldl')
import qualified Data.List as List ((\\), findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, lookup)
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Tuple (swap)
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (toAddr, toCred, pattern Addr)
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce (..),
    UnitInterval,
    epochInfo,
    intervalValue,
    stabilityWindow,
  )
import Shelley.Spec.Ledger.BlockChain
  ( TxSeq (..),
    bBodySize,
    bbHash,
    mkSeed,
    seedEta,
    seedL,
    pattern BHBody,
    pattern BHeader,
    pattern Block,
    pattern BlockHash,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential
  ( pattern KeyHashObj,
    pattern ScriptHashObj,
    pattern StakeRefBase,
    pattern StakeRefPtr,
  )
import Shelley.Spec.Ledger.Keys
  ( HasKeyRole (coerceKeyRole),
    HashType (..),
    KeyRole (..),
    KeyRoleHashType,
    asWitness,
    hashKey,
    signedDSIGN,
    signedKES,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState (AccountState (..))
import Shelley.Spec.Ledger.OCert (KESPeriod (..), pattern OCert)
import Shelley.Spec.Ledger.PParams (ProtVer (..))
import Shelley.Spec.Ledger.Scripts
  ( pattern RequireAllOf,
    pattern RequireAnyOf,
    pattern RequireMOf,
    pattern RequireSignature,
  )
import Shelley.Spec.Ledger.Slot
  ( (*-),
    BlockNo (..),
    Duration (..),
    SlotNo (..),
    epochInfoFirst,
  )
import Shelley.Spec.Ledger.Tx (hashScript, pattern TxOut)
import Test.Cardano.Crypto.VRF.Fake (WithResult (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Addr,
    Block,
    ConcreteCrypto,
    Credential,
    GenesisKeyPair,
    HashHeader,
    KeyHash,
    KeyPair,
    KeyPairs,
    MultiSig,
    MultiSigPairs,
    OCert,
    SignKeyKES,
    SignKeyVRF,
    Tx,
    TxOut,
    VKey,
    VerKeyKES,
    VerKeyVRF,
    pattern KeyPair,
  )
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Utils
  ( epochFromSlotNo,
    evolveKESUntil,
    maxKESIterations,
    maxLLSupply,
    mkCertifiedVRF,
    mkGenKey,
    mkKeyPair,
    runShelleyBase,
    unsafeMkUnitInterval,
  )

data AllIssuerKeys h r = AllIssuerKeys
  { cold :: KeyPair h r,
    vrf :: (SignKeyVRF h, VerKeyVRF h),
    hot :: [(KESPeriod, (SignKeyKES h, VerKeyKES h))],
    hk :: KeyHash h r
  }
  deriving (Show)

-- | Generator environment.
data GenEnv h = GenEnv
  { geKeySpace :: KeySpace h,
    geConstants :: Constants
  }

-- | Collection of all keys which are required to generate a trace.
--
--   These are the _only_ keys which should be involved in the trace.
data KeySpace h = KeySpace_
  { ksCoreNodes :: [(GenesisKeyPair h, AllIssuerKeys h 'GenesisDelegate)],
    -- | Bag of keys to be used for future genesis delegates
    ksGenesisDelegates :: [AllIssuerKeys h 'GenesisDelegate],
    -- | Bag of keys to be used for future stake pools
    ksStakePools :: [AllIssuerKeys h 'StakePool],
    -- | Bag of keys to be used for future payment/staking addresses
    ksKeyPairs :: KeyPairs h,
    -- | Index over the payment keys in 'ksKeyPairs'
    ksIndexedPaymentKeys :: Map (KeyHash h 'Payment) (KeyPair h 'Payment),
    -- | Index over the staking keys in 'ksKeyPairs'
    ksIndexedStakingKeys :: Map (KeyHash h 'Staking) (KeyPair h 'Staking),
    ksMSigScripts :: MultiSigPairs h
  }

deriving instance HashAlgorithm h => Show (KeySpace h)

pattern KeySpace ::
  HashAlgorithm h =>
  [(GenesisKeyPair h, AllIssuerKeys h 'GenesisDelegate)] ->
  [AllIssuerKeys h 'GenesisDelegate] ->
  [AllIssuerKeys h 'StakePool] ->
  KeyPairs h ->
  MultiSigPairs h ->
  KeySpace h
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
          ksMSigScripts
        }

genBool :: HasCallStack => Gen Bool
genBool = QC.arbitraryBoundedRandom

genInteger :: HasCallStack => Integer -> Integer -> Gen Integer
genInteger lower upper = QC.choose (lower, upper)

-- | Generator for a natural number between 'lower' and 'upper'
genNatural :: HasCallStack => Natural -> Natural -> Gen Natural
genNatural lower upper = fromInteger <$> QC.choose (lower', upper')
  where
    lower' = fromIntegral lower
    upper' = fromIntegral upper

-- | Generator for a Word64 between 'lower' and 'upper'
genWord64 :: HasCallStack => Word64 -> Word64 -> Gen Word64
genWord64 lower upper =
  fromIntegral
    <$> genNatural (fromIntegral lower) (fromIntegral upper)

mkKeyPairs :: HasCallStack => Word64 -> (KeyPair h kr, KeyPair h kr')
mkKeyPairs n =
  (mkKeyPair_ (2 * n), mkKeyPair_ (2 * n + 1))
  where
    mkKeyPair_ n_ =
      (uncurry KeyPair . swap)
        (mkKeyPair (n_, n_, n_, n_, n_))

-- | Generate a mapping from stake key hash to stake key pair, from a list of
-- (payment, staking) key pairs.
mkStakeKeyHashMap ::
  (HasCallStack, HashAlgorithm h) => KeyPairs h -> Map (KeyHash h 'Staking) (KeyPair h 'Staking)
mkStakeKeyHashMap keyPairs =
  Map.fromList (f <$> keyPairs)
  where
    f (_payK, stakeK) = ((hashKey . vKey) stakeK, stakeK)

-- | Generate a mapping from payment key hash to keypair
-- from a list of (payment, staking) key pairs.
mkPayKeyHashMap ::
  (HasCallStack, HashAlgorithm h) =>
  KeyPairs h ->
  Map (KeyHash h 'Payment) (KeyPair h 'Payment)
mkPayKeyHashMap keyPairs =
  Map.fromList (f <$> keyPairs)
  where
    f (payK, _stakeK) = ((hashKey . vKey) payK, payK)

-- | Multi-Sig Scripts based on the given key pairs
mkMSigScripts :: (HasCallStack, HashAlgorithm h) => KeyPairs h -> MultiSigPairs h
mkMSigScripts = map mkScriptsFromKeyPair

-- | Combine a list of multisig pairs into hierarchically structured multi-sig
-- scripts, list must have at least length 3. Be careful not to call with too
-- many pairs in order not to create too many of the possible combinations.
mkMSigCombinations :: (HasCallStack, HashAlgorithm h) => MultiSigPairs h -> MultiSigPairs h
mkMSigCombinations msigs =
  if length msigs < 3
    then error "length of input msigs must be at least 3"
    else foldl' (++) [] $
      do
        (k1, k2) <- msigs
        (k3, k4) <- msigs List.\\ [(k1, k2)]
        (k5, k6) <- msigs List.\\ [(k1, k2), (k3, k4)]

        pure
          [ (pay, stake)
            | pay <-
                [ RequireAnyOf [k1, k3, k5],
                  RequireAllOf [k1, k3, k5],
                  RequireMOf 1 [k1, k3, k5],
                  RequireMOf 2 [k1, k3, k5],
                  RequireMOf 3 [k1, k3, k5]
                ],
              stake <-
                [ RequireAnyOf [k2, k4, k6],
                  RequireAllOf [k2, k4, k6],
                  RequireMOf 1 [k2, k4, k6],
                  RequireMOf 2 [k2, k4, k6],
                  RequireMOf 3 [k2, k4, k6]
                ]
          ]

mkScriptsFromKeyPair ::
  (HasCallStack, HashAlgorithm h) =>
  (KeyPair h 'Payment, KeyPair h 'Staking) ->
  (MultiSig h, MultiSig h)
mkScriptsFromKeyPair (k0, k1) =
  (mkScriptFromKey $ asWitness k0, mkScriptFromKey $ asWitness k1)

mkScriptFromKey :: (HasCallStack, HashAlgorithm h) => KeyPair h 'AWitness -> MultiSig h
mkScriptFromKey = (RequireSignature . hashKey . vKey)

-- | Find first matching key pair for a credential. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairCred ::
  HasCallStack =>
  Credential h kr ->
  Map (KeyHash h kr) (KeyPair h kr) ->
  KeyPair h kr
findPayKeyPairCred c keyHashMap =
  case c of
    KeyHashObj addr -> lookforKeyHash addr
    _ ->
      error "findPayKeyPairCred: expects only KeyHashObj"
  where
    lookforKeyHash addr' =
      case Map.lookup addr' keyHashMap of
        Nothing -> error $ "findPayKeyPairCred: could not find a match for the given credential: " <> show addr'
        Just kp -> kp

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairAddr ::
  HasCallStack =>
  Addr h ->
  Map (KeyHash h 'Payment) (KeyPair h 'Payment) ->
  KeyPair h 'Payment
findPayKeyPairAddr a keyHashMap =
  case a of
    Addr _ addr (StakeRefBase _) -> findPayKeyPairCred addr keyHashMap
    Addr _ addr (StakeRefPtr _) -> findPayKeyPairCred addr keyHashMap
    _ ->
      error "findPayKeyPairAddr: expects only Base or Ptr addresses"

-- | Find first matching script for a credential.
findPayScriptFromCred :: (HasCallStack, HashAlgorithm h) => Credential h 'AWitness -> MultiSigPairs h -> (MultiSig h, MultiSig h)
findPayScriptFromCred c scripts =
  case c of
    ScriptHashObj scriptHash -> lookForScriptHash scriptHash
    _ ->
      error "findPayScriptFromCred: expects only ScriptHashObj"
  where
    lookForScriptHash scriptHash =
      case List.findIndex (\(pay, _) -> scriptHash == hashScript pay) scripts of
        Nothing -> error "findPayScript: could not find matching script for given credential"
        Just i -> scripts !! i

-- | Find first matching script for a credential.
findStakeScriptFromCred :: (HasCallStack, HashAlgorithm h) => Credential h 'AWitness -> MultiSigPairs h -> (MultiSig h, MultiSig h)
findStakeScriptFromCred c scripts =
  case c of
    ScriptHashObj scriptHash -> lookForScriptHash scriptHash
    _ ->
      error "findStakeScriptFromCred: expects only ScriptHashObj"
  where
    lookForScriptHash scriptHash =
      case List.findIndex (\(_, scr) -> scriptHash == hashScript scr) scripts of
        Nothing -> error $ "findStakeScriptFromCred: could not find matching script for given credential"
        Just i -> scripts !! i

-- | Find first matching script for address.
findPayScriptFromAddr :: (HasCallStack, HashAlgorithm h) => Addr h -> MultiSigPairs h -> (MultiSig h, MultiSig h)
findPayScriptFromAddr a scripts =
  case a of
    Addr _ scriptHash (StakeRefBase _) ->
      findPayScriptFromCred (asWitness scriptHash) scripts
    Addr _ scriptHash (StakeRefPtr _) ->
      findPayScriptFromCred (asWitness scriptHash) scripts
    _ ->
      error "findPayScriptFromAddr: expects only base and pointer script addresses"

-- | Select one random verification staking key from list of pairs of KeyPair.
pickStakeKey :: HasCallStack => KeyPairs h -> Gen (VKey h 'Staking)
pickStakeKey keys = vKey . snd <$> QC.elements keys

-- | Generates a list of coins for the given 'Addr' and produced a 'TxOut' for each 'Addr'
--
-- Note: we need to keep the initial utxo coin sizes large enough so that
-- when we simulate sequences of transactions, we have enough funds available
-- to include certificates that require deposits.
genTxOut :: HasCallStack => Constants -> [Addr h] -> Gen [TxOut h]
genTxOut Constants {maxGenesisOutputVal, minGenesisOutputVal} addrs = do
  ys <- genCoinList minGenesisOutputVal maxGenesisOutputVal (length addrs) (length addrs)
  return (uncurry TxOut <$> zip addrs ys)

-- | Generates a list of 'Coin' values of length between 'lower' and 'upper'
-- and with values between 'minCoin' and 'maxCoin'.
genCoinList :: HasCallStack => Integer -> Integer -> Int -> Int -> Gen [Coin]
genCoinList minCoin maxCoin lower upper = do
  len <- QC.choose (lower, upper)
  replicateM len $ genCoin minCoin maxCoin

-- TODO this should be an exponential distribution, not constant
genCoin :: HasCallStack => Integer -> Integer -> Gen Coin
genCoin minCoin maxCoin = Coin <$> QC.choose (minCoin, maxCoin)

-- | Generate values the given distribution in 90% of the cases, and values at
-- the bounds of the range in 10% of the cases.
--
-- This can be used to generate enough extreme values. The exponential and
-- linear distributions provided by @hedgehog@ will generate a small percentage
-- of these (0-1%).
increasingProbabilityAt ::
  HasCallStack =>
  Gen a ->
  (a, a) ->
  Gen a
increasingProbabilityAt gen (lower, upper) =
  QC.frequency
    [ (5, pure lower),
      (90, gen),
      (5, pure upper)
    ]

zero :: HasCallStack => UnitInterval
zero = unsafeMkUnitInterval 0

-- | Try to map the unit interval to a natural number. We don't care whether
-- this is surjective. But it should be right inverse to `fromNatural` - that
-- is, one should be able to recover the `UnitInterval` value used here.
unitIntervalToNatural :: HasCallStack => UnitInterval -> Natural
unitIntervalToNatural = floor . ((10000 % 1) *) . intervalValue

mkBlock ::
  (HasCallStack, HashAlgorithm h, KeyRoleHashType r ~ 'RegularHash) =>
  -- | Hash of previous block
  HashHeader h ->
  -- | All keys in the stake pool
  AllIssuerKeys h r ->
  -- | Transactions to record
  [Tx h] ->
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
  OCert h ->
  Block h
mkBlock prev pkeys txns s blockNo enonce (NatNonce bnonce) l kesPeriod c0 oCert =
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
          ( coerce $
              mkCertifiedVRF
                (WithResult nonceNonce (fromIntegral bnonce))
                (fst $ vrf pkeys)
          )
          ( coerce $
              mkCertifiedVRF
                (WithResult leaderNonce (fromIntegral $ unitIntervalToNatural l))
                (fst $ vrf pkeys)
          )
          (fromIntegral $ bBodySize $ (TxSeq . StrictSeq.fromList) txns)
          (bbHash $ TxSeq $ StrictSeq.fromList txns)
          oCert
          (ProtVer 0 0)
      kpDiff = kesPeriod - c0
      hotKey = case evolveKESUntil sHot (KESPeriod 0) (KESPeriod kpDiff) of
        Nothing ->
          error ("could not evolve key to iteration " ++ show (c0, kesPeriod, kpDiff))
        Just hkey -> hkey
      sig = signedKES () kpDiff bhb hotKey
      bh = BHeader bhb sig
   in Block bh (TxSeq $ StrictSeq.fromList txns)

-- | We provide our own nonces to 'mkBlock', which we then wish to recover as
-- the output of the VRF functions. In general, however, we just derive them
-- from a natural. Since the nonce is a hash, we do not want to recover it to
-- find a preimage. In testing, therefore, we just wrap the raw natural, which
-- we then encode into the fake VRF implementation.
newtype NatNonce = NatNonce Natural

mkOCert :: forall h r. (HasCallStack, HashAlgorithm h) => AllIssuerKeys h r -> Natural -> KESPeriod -> OCert h
mkOCert pkeys n c0 =
  let (_, (_, vKeyHot)) = head $ hot pkeys
      KeyPair _vKeyCold sKeyCold = cold pkeys
   in OCert
        vKeyHot
        n
        c0
        (signedDSIGN @(ConcreteCrypto h) sKeyCold (vKeyHot, n, c0))

-- | Takes a set of KES hot keys and checks to see whether there is one whose
-- range contains the current KES period. If so, return its index in the list of
-- hot keys.
getKESPeriodRenewalNo :: HasCallStack => AllIssuerKeys h r -> KESPeriod -> Integer
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
tooLateInEpoch :: HasCallStack => SlotNo -> Bool
tooLateInEpoch s = runShelleyBase $ do
  ei <- asks epochInfo
  firstSlotNo <- epochInfoFirst ei (epochFromSlotNo s + 1)
  stabilityWindow <- asks stabilityWindow

  return (s >= firstSlotNo *- Duration (2 * stabilityWindow))

-- | Account with empty treasury
genesisAccountState :: HasCallStack => AccountState
genesisAccountState =
  AccountState
    { _treasury = Coin 0,
      _reserves = maxLLSupply
    }
