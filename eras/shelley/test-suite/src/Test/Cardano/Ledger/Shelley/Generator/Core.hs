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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  VRFKeyPair (..),
  KESKeyPair (..),
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
  mkAddr,
  mkCredential,
  mkBlock,
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

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..), epochInfoPure, stabilityWindow)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (
  Credential (..),
  pattern KeyHashObj,
  pattern ScriptHashObj,
  pattern StakeRefBase,
  pattern StakeRefPtr,
 )
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Keys (VKey, asWitness)
import Cardano.Ledger.Shelley.LedgerState (AccountState (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits)
import Cardano.Ledger.Slot (
  Duration (..),
  SlotNo (..),
  epochInfoFirst,
  (*-),
 )
import Cardano.Ledger.State (UTxO (UTxO))
import Cardano.Ledger.TxIn (TxId, TxIn (TxIn))
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Codec.Serialise (serialise)
import Control.Monad (replicateM)
import Control.Monad.Trans.Reader (asks)
import Data.ByteString.Lazy (toStrict)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), KeyPairs, mkAddr, mkCredential, vKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (
  ScriptClass,
  exponential,
  mkKeyPairs,
  mkPayScriptHashMap,
  mkStakeScriptHashMap,
 )
import Test.Cardano.Ledger.Shelley.Utils (
  GenesisKeyPair,
  RawSeed (..),
  epochFromSlotNo,
  maxKESIterations,
  maxLLSupply,
  mkGenKey,
  mkKeyPair,
  runShelleyBase,
 )
import Test.Cardano.Protocol.Crypto.VRF.Fake (NatNonce (..))
import Test.Cardano.Protocol.TPraos.Create (
  AllIssuerKeys (..),
  KESKeyPair (..),
  VRFKeyPair (..),
  mkBlock,
  mkBlockFakeVRF,
  mkOCert,
 )
import Test.Cardano.Slotting.Numeric ()
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

-- | For use in the Serialisation and Example Tests, which assume Shelley, Allegra, or Mary Eras.
type PreAlonzo era =
  (TxWits era ~ ShelleyTxWits era)

-- =========================================

type ScriptInfo era =
  ( Map ScriptHash (TwoPhase3ArgInfo era)
  , Map ScriptHash (TwoPhase2ArgInfo era)
  )

data TwoPhase3ArgInfo era = TwoPhase3ArgInfo
  { getScript3 :: Script era
  -- ^ A Plutus Script
  , getHash3 :: ScriptHash
  -- ^ Its ScriptHash
  , getData3 :: PV1.Data
  -- ^ A Data that will make it succeed
  , getRedeemer3 ::
      ( PV1.Data -- The redeeming data
      , Natural -- The ExUnits memory count
      , Natural -- The ExUnits steps count
      )
  -- ^ A Redeemer that will make it succeed
  , getSucceeds3 :: Bool
  }

data TwoPhase2ArgInfo era = TwoPhase2ArgInfo
  { getScript2 :: Script era
  -- ^ A Plutus Script
  , getHash2 :: ScriptHash
  -- ^ Its ScriptHash
  , getRedeemer2 ::
      ( PV1.Data -- The redeeming data
      , Natural -- The ExUnits memory count
      , Natural -- The ExUnits steps count
      )
  -- ^ A Redeemer that will make it succeed
  , getSucceeds2 :: Bool
  }

deriving instance Show (Script era) => Show (TwoPhase3ArgInfo era)

deriving instance Show (Script era) => Show (TwoPhase2ArgInfo era)

data ScriptSpace era = ScriptSpace
  { ssScripts3 :: [TwoPhase3ArgInfo era]
  -- ^ A list of Two Phase 3 Arg Scripts and their associated data we can use.
  , ssScripts2 :: [TwoPhase2ArgInfo era]
  -- ^ A list of Two Phase 2 Arg Scripts and their associated data we can use.
  , ssHash3 :: Map ScriptHash (TwoPhase3ArgInfo era)
  , ssHash2 :: Map ScriptHash (TwoPhase2ArgInfo era)
  }

deriving instance Show (Script era) => Show (ScriptSpace era)

-- | Generator environment.
data GenEnv era = GenEnv
  { geKeySpace :: KeySpace era
  , geScriptSpapce :: ScriptSpace era
  , geConstants :: Constants
  }

-- | Collection of all keys which are required to generate a trace.
--
--   These are the _only_ keys which should be involved in the trace.
data KeySpace era = KeySpace_
  { ksCoreNodes :: [(GenesisKeyPair MockCrypto, AllIssuerKeys MockCrypto 'GenesisDelegate)]
  , ksGenesisDelegates :: [AllIssuerKeys MockCrypto 'GenesisDelegate]
  -- ^ Bag of keys to be used for future genesis delegates
  , ksStakePools :: [AllIssuerKeys MockCrypto 'StakePool]
  -- ^ Bag of keys to be used for future stake pools
  , ksKeyPairs :: KeyPairs
  -- ^ Bag of keys to be used for future payment/staking addresses
  , ksMSigScripts :: [(Script era, Script era)]
  , ksIndexedPaymentKeys :: Map (KeyHash 'Payment) (KeyPair 'Payment)
  -- ^ Index over the payment keys in 'ksKeyPairs'
  , ksIndexedStakingKeys :: Map (KeyHash 'Staking) (KeyPair 'Staking)
  -- ^ Index over the staking keys in 'ksKeyPairs'
  , ksIndexedGenDelegates ::
      Map (KeyHash 'GenesisDelegate) (AllIssuerKeys MockCrypto 'GenesisDelegate)
  -- ^ Index over the cold key hashes in Genesis Delegates
  , ksIndexedPayScripts :: Map ScriptHash (Script era, Script era)
  -- ^ Index over the pay script hashes in Script pairs
  , ksIndexedStakeScripts :: Map ScriptHash (Script era, Script era)
  -- ^ Index over the stake script hashes in Script pairs
  }

deriving instance (Era era, Show (Script era)) => Show (KeySpace era)

pattern KeySpace ::
  forall era.
  ScriptClass era =>
  [(GenesisKeyPair MockCrypto, AllIssuerKeys MockCrypto 'GenesisDelegate)] ->
  [AllIssuerKeys MockCrypto 'GenesisDelegate] ->
  [AllIssuerKeys MockCrypto 'StakePool] ->
  KeyPairs ->
  [(Script era, Script era)] ->
  KeySpace era
pattern KeySpace
  ksCoreNodes
  ksGenesisDelegates
  ksStakePools
  ksKeyPairs
  ksMSigScripts <-
  KeySpace_
    { ksCoreNodes
    , ksGenesisDelegates
    , ksStakePools
    , ksKeyPairs
    , ksMSigScripts
    }
  where
    KeySpace ksCoreNodes ksGenesisDelegates ksStakePools ksKeyPairs ksMSigScripts =
      KeySpace_
        { ksCoreNodes
        , ksGenesisDelegates
        , ksStakePools
        , ksKeyPairs
        , ksIndexedPaymentKeys = mkPayKeyHashMap ksKeyPairs
        , ksIndexedStakingKeys = mkStakeKeyHashMap ksKeyPairs
        , ksIndexedGenDelegates = mkGenesisDelegatesHashMap ksCoreNodes ksGenesisDelegates
        , ksIndexedPayScripts = mkPayScriptHashMap @era ksMSigScripts
        , ksIndexedStakeScripts = mkStakeScriptHashMap @era ksMSigScripts
        , ksMSigScripts
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
  [(GenesisKeyPair c, AllIssuerKeys c 'GenesisDelegate)] ->
  [AllIssuerKeys c 'GenesisDelegate] ->
  Map (KeyHash 'GenesisDelegate) (AllIssuerKeys c 'GenesisDelegate)
mkGenesisDelegatesHashMap coreNodes genesisDelegates =
  Map.fromList (f <$> allDelegateKeys)
  where
    f issuerKeys = (hashKey . vKey $ aikCold issuerKeys, issuerKeys)
    allDelegateKeys = (snd <$> coreNodes) <> genesisDelegates

-- | Generate a mapping from stake key hash to stake key pair, from a list of
-- (payment, staking) key pairs.
mkStakeKeyHashMap :: KeyPairs -> Map (KeyHash 'Staking) (KeyPair 'Staking)
mkStakeKeyHashMap keyPairs =
  Map.fromList (f <$> keyPairs)
  where
    f (_payK, stakeK) = ((hashKey . vKey) stakeK, stakeK)

-- | Generate a mapping from payment key hash to keypair
-- from a list of (payment, staking) key pairs.
mkPayKeyHashMap ::
  KeyPairs ->
  Map (KeyHash 'Payment) (KeyPair 'Payment)
mkPayKeyHashMap keyPairs =
  Map.fromList (f <$> keyPairs)
  where
    f (payK, _stakeK) = ((hashKey . vKey) payK, payK)

-- | Find first matching key pair for a credential. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairCred ::
  forall kr.
  Credential kr ->
  Map (KeyHash kr) (KeyPair kr) ->
  KeyPair kr
findPayKeyPairCred (KeyHashObj addr) keyHashMap =
  fromMaybe
    (error $ "findPayKeyPairCred: could not find a match for the given credential: " <> show addr)
    (Map.lookup addr keyHashMap)
findPayKeyPairCred _ _ =
  error "findPayKeyPairCred: expects only KeyHashObj"

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairAddr ::
  Addr ->
  Map (KeyHash 'Payment) (KeyPair 'Payment) ->
  KeyPair 'Payment
findPayKeyPairAddr a keyHashMap =
  case a of
    Addr _ addr (StakeRefBase _) -> findPayKeyPairCred addr keyHashMap
    Addr _ addr (StakeRefPtr _) -> findPayKeyPairCred addr keyHashMap
    _ ->
      error "findPayKeyPairAddr: expects only Base or Ptr addresses"

-- | Find matching multisig scripts for a credential.
findPayScriptFromCred ::
  forall era.
  Credential 'Witness ->
  Map ScriptHash (Script era, Script era) ->
  (Script era, Script era)
findPayScriptFromCred (ScriptHashObj scriptHash) scriptsByPayHash =
  fromMaybe
    (error "findPayScript: could not find matching script for given credential")
    (Map.lookup scriptHash scriptsByPayHash)
findPayScriptFromCred _ _ =
  error "findPayScriptFromCred: expects only ScriptHashObj"

-- | Find first matching script for a credential.
findStakeScriptFromCred ::
  Credential 'Witness ->
  Map ScriptHash (Script era, Script era) ->
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
  Addr ->
  Map ScriptHash (Script era, Script era) ->
  (Script era, Script era)
findPayScriptFromAddr (Addr _ scriptHash (StakeRefBase _)) scriptsByPayHash =
  findPayScriptFromCred @era (asWitness scriptHash) scriptsByPayHash
findPayScriptFromAddr (Addr _ scriptHash (StakeRefPtr _)) scriptsByPayHash =
  findPayScriptFromCred @era (asWitness scriptHash) scriptsByPayHash
findPayScriptFromAddr _ _ =
  error "findPayScriptFromAddr: expects only base and pointer script addresses"

-- | Select one random verification staking key from list of pairs of KeyPair.
pickStakeKey :: KeyPairs -> Gen (VKey 'Staking)
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
  [Addr] ->
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
    [ (5, pure lower)
    , (90, gen)
    , (5, pure upper)
    ]

-- | Takes a sequence of KES hot keys and checks to see whether there is one whose
-- range contains the current KES period. If so, return its index in the list of
-- hot keys.
getKESPeriodRenewalNo :: AllIssuerKeys h r -> KESPeriod -> Integer
getKESPeriodRenewalNo keys (KESPeriod kp) =
  go (NE.toList (aikHot keys)) 0 kp
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
  let firstSlotNo = epochInfoFirst ei (epochFromSlotNo s + 1)
  stabilityWindow <- asks stabilityWindow

  return (s >= firstSlotNo *- Duration (2 * stabilityWindow))

-- | Account with empty treasury
genesisAccountState :: AccountState
genesisAccountState =
  AccountState
    { asTreasury = Coin 0
    , asReserves = maxLLSupply
    }

-- | Creates the UTxO for a new ledger with the specified
-- genesis TxId and transaction outputs.
genesisCoins ::
  TxId ->
  [TxOut era] ->
  UTxO era
genesisCoins genesisTxId outs =
  UTxO $
    Map.fromList [(TxIn genesisTxId idx, out) | (idx, out) <- zip [minBound ..] outs]

-- ==================================================================
-- Operations on GenEnv that deal with ScriptSpace

hashData :: PV1.Data -> DataHash
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
  GenEnv era ->
  ScriptHash ->
  (Script era, StrictMaybe DataHash)
findPlutus (GenEnv keyspace (ScriptSpace _ _ mp3 mp2) _) hsh =
  case Map.lookup hsh mp3 of
    Just info3 -> (getScript3 info3, SJust (hashData (getData3 info3)))
    Nothing ->
      case Map.lookup hsh mp2 of
        Just info2 -> (getScript2 info2, SNothing)
        Nothing -> case Map.lookup hsh (ksIndexedPayScripts keyspace) of
          Just (pay, _ssStake) -> (pay, SNothing)
          Nothing ->
            case Map.lookup hsh (ksIndexedStakeScripts keyspace) of
              Just (_pay, stake) -> (stake, SNothing)
              Nothing -> error ("Can't find a Script for the hash: " ++ show hsh)
