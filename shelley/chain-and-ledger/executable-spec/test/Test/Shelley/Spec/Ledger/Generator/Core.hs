{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Shelley.Spec.Ledger.Generator.Core
  ( AllPoolKeys (..)
  , GenEnv(..)
  , KeySpace(..)
  , pattern KeySpace
  , NatNonce (..)
  , Testing
  , findPayKeyPairAddr
  , findPayKeyPairCred
  , findPayScriptFromCred
  , findStakeScriptFromCred
  , findPayScriptFromAddr
  , genBool
  , genCoin
  , genCoinList
  , genInteger
  , genNatural
  , genWord64
  , genTxOut
  , increasingProbabilityAt
  , mkScriptsFromKeyPair
  , pickStakeKey
  , toAddr
  , toCred
  , zero
  , unitIntervalToNatural
  , mkBlock
  , mkOCert
  , getKESPeriodRenewalNo
  , tooLateInEpoch
  , mkKeyPair
  , mkKeyPairs
  , mkGenKey
  , mkMSigScripts
  , mkMSigCombinations
  , genesisAccountState
  )
  where

import           Control.Monad (replicateM)
import           Control.Monad.Trans.Reader (asks)
import           Data.Coerce (coerce)
import           Data.List (foldl')
import qualified Data.List as List (findIndex, (\\))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList, insert, lookup)
import           Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import           Data.Tuple (swap)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Test.Cardano.Crypto.VRF.Fake (WithResult (..))
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import           Numeric.Natural (Natural)
import           Shelley.Spec.Ledger.Address (pattern Addr, toAddr, toCred)
import           Shelley.Spec.Ledger.BaseTypes (Nonce (..), UnitInterval, epochInfo, intervalValue,
                     stabilityWindow)
import           Shelley.Spec.Ledger.BlockChain (pattern BHBody, pattern BHeader, pattern Block,
                     pattern BlockHash, TxSeq (..), bBodySize, bbHash, mkSeed, seedEta, seedL)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys (KeyRole(..), HasKeyRole(coerceKeyRole), asWitness, hashKey, vKey, signedKES, signedDSIGN)
import           Shelley.Spec.Ledger.Credential (pattern KeyHashObj,
                     pattern ScriptHashObj, pattern StakeRefBase, pattern StakeRefPtr)
import           Shelley.Spec.Ledger.LedgerState (AccountState (..))
import           Shelley.Spec.Ledger.OCert (KESPeriod (..), pattern OCert)
import           Shelley.Spec.Ledger.PParams (ProtVer (..))
import           Shelley.Spec.Ledger.Scripts (pattern RequireAllOf, pattern RequireAnyOf,
                     pattern RequireMOf, pattern RequireSignature)
import           Shelley.Spec.Ledger.Slot (BlockNo (..), Duration (..), SlotNo (..), epochInfoFirst,
                     (*-))
import           Shelley.Spec.Ledger.Tx (pattern TxOut, hashScript)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Addr, Block, CoreKeyPair,
                     Credential, HashHeader, KeyHash, KeyPair, KeyPairs, MultiSig, MultiSigPairs,
                     OCert, SignKeyVRF, Tx, TxOut, VKey, VerKeyKES, VRFKeyHash, VerKeyVRF,
                     hashKeyVRF, SignKeyKES, pattern KeyPair, ConcreteCrypto)
import           Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import           Test.Shelley.Spec.Ledger.Utils (epochFromSlotNo, evolveKESUntil, maxKESIterations,
                     maxLLSupply, mkCertifiedVRF, mkGenKey, mkKeyPair, runShelleyBase,
                     unsafeMkUnitInterval)

-- | I'm in two minds about this. It basically takes advantage of a GHC bug to allow
--   the inhabiting of a closed kind. On the other hand, the alternatives would be to
--   have `Testing` as a real key role (test code infiltrating the library, yuck) or
--   use a different role everywhere (confusing).
data family Testing :: k

toTesting :: HasKeyRole a => a r crypto -> a Testing crypto
toTesting = coerceKeyRole

fromTesting :: HasKeyRole a => a Testing crypto -> a r crypto
fromTesting = coerceKeyRole

data AllPoolKeys = AllPoolKeys
  { cold :: KeyPair 'StakePool
  , vrf :: (SignKeyVRF, VerKeyVRF)
  , hot :: [(KESPeriod, (SignKeyKES, VerKeyKES))]
  , hk  :: KeyHash 'StakePool
  } deriving (Show)

-- | Generator environment.
data GenEnv = GenEnv
  { geKeySpace :: KeySpace
  , geConstants :: Constants
  }

-- | Collection of all keys which are required to generate a trace.
--
--   These are the _only_ keys which should be involved in the trace.
data KeySpace = KeySpace_
  { ksCoreNodes :: [(CoreKeyPair, AllPoolKeys)]
  , ksKeyPairs :: KeyPairs
  , ksKeyPairsByHash :: Map (KeyHash Testing) (KeyPair Testing)
  , ksKeyPairsByStakeHash :: Map (KeyHash 'Staking) (KeyPair 'Staking)
  , ksMSigScripts :: MultiSigPairs
  , ksVRFKeyPairs :: [(SignKeyVRF, VerKeyVRF)]
  , ksVRFKeyPairsByHash :: Map VRFKeyHash (SignKeyVRF, VerKeyVRF)
  } deriving (Show)

pattern KeySpace
  :: [(CoreKeyPair, AllPoolKeys)]
  -> KeyPairs
  -> MultiSigPairs
  -> [(SignKeyVRF, VerKeyVRF)]
  -> KeySpace
pattern KeySpace ksCoreNodes ksKeyPairs ksMSigScripts ksVRFKeyPairs
  <- KeySpace_
      { ksCoreNodes
      , ksKeyPairs
      , ksMSigScripts
      , ksVRFKeyPairs
      }
  where
    KeySpace ksCoreNodes ksKeyPairs ksMSigScripts ksVRFKeyPairs
      = KeySpace_
        { ksCoreNodes
        , ksKeyPairs
        , ksKeyPairsByHash = mkKeyHashMap ksKeyPairs
        , ksKeyPairsByStakeHash = mkStakeKeyHashMap ksKeyPairs
        , ksMSigScripts
        , ksVRFKeyPairs
        , ksVRFKeyPairsByHash = mkVRFKeyPairsByHash ksVRFKeyPairs
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
genWord64 lower upper = fromIntegral
  <$> genNatural (fromIntegral lower) (fromIntegral upper)

mkKeyPairs :: HasCallStack => Word64 -> (KeyPair kr, KeyPair kr')
mkKeyPairs n
  = (mkKeyPair_ (2*n), mkKeyPair_ (2*n+1))
  where
    mkKeyPair_ n_ = (uncurry KeyPair . swap)
                    (mkKeyPair (n_,n_,n_,n_,n_))

-- | Generate a mapping from stake key hash to stake key pair, from a list of
-- (payment, staking) key pairs.
mkStakeKeyHashMap
  :: HasCallStack => KeyPairs -> Map (KeyHash 'Staking) (KeyPair 'Staking)
mkStakeKeyHashMap keyPairs =
  Map.fromList (f <$> keyPairs)
  where
    f (_payK, stakeK) = ((hashKey . vKey) stakeK, stakeK)

-- | Generate a mapping from key hash (both payment and stake keys) to keypair
-- from a list of (payment, staking) key pairs.
mkKeyHashMap :: HasCallStack => KeyPairs -> Map (KeyHash Testing) (KeyPair Testing)
mkKeyHashMap =
  foldl' (\m (payKey, stakeKey) ->
           let m' = Map.insert (toTesting . hashKey $ vKey payKey) (toTesting payKey) m
           in       Map.insert (toTesting . hashKey $ vKey stakeKey) (toTesting stakeKey) m')
  Map.empty

-- | Multi-Sig Scripts based on the given key pairs
mkMSigScripts :: HasCallStack => KeyPairs -> MultiSigPairs
mkMSigScripts = map mkScriptsFromKeyPair

-- | Combine a list of multisig pairs into hierarchically structured multi-sig
-- scripts, list must have at least length 3. Be careful not to call with too
-- many pairs in order not to create too many of the possible combinations.
mkMSigCombinations :: HasCallStack => MultiSigPairs -> MultiSigPairs
mkMSigCombinations msigs =
  if length msigs < 3 then error "length of input msigs must be at least 3"
  else foldl' (++) [] $
       do
         (k1, k2) <- msigs
         (k3, k4) <- msigs List.\\ [(k1, k2)]
         (k5, k6) <- msigs List.\\ [(k1, k2), (k3, k4)]

         pure [(pay, stake) | pay <- [ RequireAnyOf [k1, k3, k5]
                                     , RequireAllOf [k1, k3, k5]
                                     , RequireMOf 1 [k1, k3, k5]
                                     , RequireMOf 2 [k1, k3, k5]
                                     , RequireMOf 3 [k1, k3, k5]]
                            , stake <- [ RequireAnyOf [k2, k4, k6]
                                       , RequireAllOf [k2, k4, k6]
                                       , RequireMOf 1 [k2, k4, k6]
                                       , RequireMOf 2 [k2, k4, k6]
                                       , RequireMOf 3 [k2, k4, k6]]]

mkScriptsFromKeyPair
  :: HasCallStack
  => (KeyPair 'Payment, KeyPair 'Staking)
  -> (MultiSig, MultiSig)
mkScriptsFromKeyPair (k0, k1) =
  (mkScriptFromKey $ asWitness k0, mkScriptFromKey $ asWitness k1)

mkScriptFromKey :: HasCallStack => KeyPair 'Witness -> MultiSig
mkScriptFromKey = (RequireSignature . hashKey . vKey)

-- | Find first matching key pair for a credential. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairCred
  :: HasCallStack
  => Credential kr
  -> Map (KeyHash kr) (KeyPair kr)
  -> KeyPair kr
findPayKeyPairCred c keyHashMap =
  case c of
    KeyHashObj addr -> lookforKeyHash addr
    _                            ->
      error "findPayKeyPairCred: expects only KeyHashObj"
  where
    lookforKeyHash addr' =
      case Map.lookup addr' keyHashMap of
        Nothing -> error $ "findPayKeyPairCred: could not find a match for the given credential: " <> show addr'
        Just kp -> kp

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairAddr :: HasCallStack => Addr -> Map (KeyHash Testing) (KeyPair Testing) -> KeyPair 'Payment
findPayKeyPairAddr a keyHashMap = fromTesting $
  case a of
    Addr addr (StakeRefBase _) -> findPayKeyPairCred (toTesting addr) keyHashMap
    Addr addr (StakeRefPtr _)  -> findPayKeyPairCred (toTesting addr) keyHashMap
    _                            ->
      error "findPayKeyPairAddr: expects only Base or Ptr addresses"

-- | Find first matching script for a credential.
findPayScriptFromCred :: HasCallStack => Credential 'Witness -> MultiSigPairs -> (MultiSig, MultiSig)
findPayScriptFromCred c scripts =
  case c of
    ScriptHashObj scriptHash -> lookForScriptHash scriptHash
    _                        ->
      error "findPayScriptFromCred: expects only ScriptHashObj"
  where
    lookForScriptHash scriptHash =
      case List.findIndex (\(pay, _) -> scriptHash == hashScript pay) scripts of
        Nothing -> error "findPayScript: could not find matching script for given credential"
        Just i  -> scripts !! i

-- | Find first matching script for a credential.
findStakeScriptFromCred :: HasCallStack =>  Credential 'Witness -> MultiSigPairs -> (MultiSig, MultiSig)
findStakeScriptFromCred c scripts =
  case c of
    ScriptHashObj scriptHash -> lookForScriptHash scriptHash
    _                        ->
      error "findStakeScriptFromCred: expects only ScriptHashObj"
  where
    lookForScriptHash scriptHash =
      case List.findIndex (\(_, scr) -> scriptHash == hashScript scr) scripts of
        Nothing -> error $ "findStakeScriptFromCred: could not find matching script for given credential"
        Just i  -> scripts !! i


-- | Find first matching script for address.
findPayScriptFromAddr :: HasCallStack => Addr -> MultiSigPairs -> (MultiSig, MultiSig)
findPayScriptFromAddr a scripts =
  case a of
    Addr scriptHash (StakeRefBase _) ->
        findPayScriptFromCred (asWitness scriptHash) scripts
    Addr scriptHash (StakeRefPtr _)  ->
        findPayScriptFromCred (asWitness scriptHash) scripts
    _                     ->
      error "findPayScriptFromAddr: expects only base and pointer script addresses"

-- | Select one random verification staking key from list of pairs of KeyPair.
pickStakeKey :: HasCallStack => KeyPairs -> Gen (VKey 'Staking)
pickStakeKey keys = vKey . snd <$> QC.elements keys

-- | Generates a list of coins for the given 'Addr' and produced a 'TxOut' for each 'Addr'
--
-- Note: we need to keep the initial utxo coin sizes large enough so that
-- when we simulate sequences of transactions, we have enough funds available
-- to include certificates that require deposits.
genTxOut :: HasCallStack => Constants -> [Addr] -> Gen [TxOut]
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
increasingProbabilityAt
  :: HasCallStack
  => Gen a
  -> (a, a)
  -> Gen a
increasingProbabilityAt gen (lower, upper)
  = QC.frequency [ (5, pure lower)
                 , (90, gen)
                 , (5, pure upper)
                 ]

mkVRFKeyPairsByHash :: HasCallStack => [(SignKeyVRF, VerKeyVRF)] -> Map VRFKeyHash (SignKeyVRF, VerKeyVRF)
mkVRFKeyPairsByHash = Map.fromList . fmap (\p -> (hashKeyVRF (snd p), p))

zero :: HasCallStack => UnitInterval
zero = unsafeMkUnitInterval 0

-- | Try to map the unit interval to a natural number. We don't care whether
-- this is surjective. But it should be right inverse to `fromNatural` - that
-- is, one should be able to recover the `UnitInterval` value used here.
unitIntervalToNatural :: HasCallStack => UnitInterval -> Natural
unitIntervalToNatural = floor . ((10000 % 1) *) . intervalValue

mkBlock
  :: HasCallStack
  => HashHeader   -- ^ Hash of previous block
  -> AllPoolKeys  -- ^ All keys in the stake pool
  -> [Tx]         -- ^ Transactions to record
  -> SlotNo       -- ^ Current slot
  -> BlockNo      -- ^ Block number/chain length/chain "difficulty"
  -> Nonce        -- ^ EpochNo nonce
  -> NatNonce     -- ^ Block nonce
  -> UnitInterval -- ^ Praos leader value
  -> Word         -- ^ Period of KES (key evolving signature scheme)
  -> Word      -- ^ KES period of key registration
  -> OCert        -- ^ Operational certificate
  -> Block
mkBlock prev pkeys txns s blockNo enonce (NatNonce bnonce) l kesPeriod c0 oCert =
  let
    (_, (sHot, _)) = head $ hot pkeys
    KeyPair vKeyCold _ = cold pkeys
    nonceNonce = mkSeed seedEta s enonce
    leaderNonce = mkSeed seedL s enonce
    bhb = BHBody
            blockNo
            s
            (BlockHash prev)
            (coerceKeyRole vKeyCold)
            (snd $ vrf pkeys)
            (coerce $ mkCertifiedVRF
              (WithResult nonceNonce (fromIntegral bnonce)) (fst $ vrf pkeys))
            (coerce $ mkCertifiedVRF
              (WithResult leaderNonce (fromIntegral $ unitIntervalToNatural l))
              (fst $ vrf pkeys))
            (fromIntegral $ bBodySize $ (TxSeq . StrictSeq.fromList) txns)
            (bbHash $ TxSeq $ StrictSeq.fromList txns)
            oCert
            (ProtVer 0 0)
    kpDiff = kesPeriod - c0
    hotKey = case evolveKESUntil sHot (KESPeriod kpDiff) of
               Nothing ->
                 error ("could not evolve key to iteration " ++ show kesPeriod)
               Just hkey -> hkey
    sig = signedKES () kpDiff bhb hotKey
    bh = BHeader bhb sig
  in
    Block bh (TxSeq $ StrictSeq.fromList txns)

-- | We provide our own nonces to 'mkBlock', which we then wish to recover as
-- the output of the VRF functions. In general, however, we just derive them
-- from a natural. Since the nonce is a hash, we do not want to recover it to
-- find a preimage. In testing, therefore, we just wrap the raw natural, which
-- we then encode into the fake VRF implementation.
newtype NatNonce = NatNonce Natural

mkOCert :: HasCallStack => AllPoolKeys -> Natural -> KESPeriod -> OCert
mkOCert pkeys n c0 =
  let (_, (_, vKeyHot)) = head $ hot pkeys
      KeyPair _vKeyCold sKeyCold = cold pkeys in
  OCert
   vKeyHot
   n
   c0
   (signedDSIGN @ConcreteCrypto sKeyCold (vKeyHot, n, c0))

getKESPeriodRenewalNo :: HasCallStack => AllPoolKeys -> KESPeriod -> Integer
getKESPeriodRenewalNo keys (KESPeriod kp) =
  go (hot keys) 0 kp
  where go [] _ _ = error "did not find enough KES renewals"
        go ((KESPeriod p, _):rest) n k =
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
