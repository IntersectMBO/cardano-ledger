{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    applyTxBody,
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
    genesisCoins,
    genesisId,
    increasingProbabilityAt,
    mkScriptsFromKeyPair,
    pickStakeKey,
    toAddr,
    toCred,
    zero,
    unitIntervalToNatural,
    mkBlock,
    mkBlockHeader,
    mkBlockFakeVRF,
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

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.VRF (evalCertified)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto (..))
import qualified Cardano.Ledger.Shelley as Shelley
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Control.Monad (liftM2, replicateM)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (∪), (⋪))
import Data.Coerce (coerce)
import Data.List (foldl')
import qualified Data.List as List ((\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Word (Word64)
import GHC.Records (HasField, getField)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (Addr (..), getRwdCred, toAddr, toCred)
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce (..),
    StrictMaybe (..),
    UnitInterval,
    epochInfo,
    intervalValue,
    stabilityWindow,
  )
import Shelley.Spec.Ledger.BlockChain
  ( BHeader (BHeader),
    Block (Block),
    HashBBody,
    HashHeader,
    TxSeq (..),
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
  ( Credential (..),
    pattern KeyHashObj,
    pattern ScriptHashObj,
    pattern StakeRefBase,
    pattern StakeRefPtr,
  )
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( HasKeyRole (coerceKeyRole),
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
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    KeyPairs,
    LedgerState,
    depositPoolChange,
    reapRewards,
    _delegationState,
    _deposited,
    _dstate,
    _fees,
    _rewards,
    _utxo,
    _utxoState,
  )
import Shelley.Spec.Ledger.OCert
  ( KESPeriod (..),
    OCert,
    OCertSignable (..),
    pattern OCert,
  )
import Shelley.Spec.Ledger.PParams
  ( PParams,
    ProtVer (..),
  )
import Shelley.Spec.Ledger.Scripts
  ( MultiSig,
    ScriptHash,
    hashMultiSigScript,
    pattern RequireAllOf,
    pattern RequireAnyOf,
    pattern RequireMOf,
    pattern RequireSignature,
  )
import Shelley.Spec.Ledger.Slot
  ( BlockNo (..),
    Duration (..),
    SlotNo (..),
    epochInfoFirst,
    (*-),
  )
import Shelley.Spec.Ledger.Tx
  ( Tx,
    TxIn,
    pattern TxId,
    pattern TxIn,
    pattern TxOut,
  )
import qualified Shelley.Spec.Ledger.Tx as Ledger
import Shelley.Spec.Ledger.TxBody
  ( DCert,
    TxOut,
    unWdrl,
    pattern TxBody,
    pattern Wdrl,
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO,
    txins,
    txouts,
    pattern UTxO,
  )
import Test.Cardano.Crypto.VRF.Fake (WithResult (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ExMock, Mock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Orphans ()
import Test.Shelley.Spec.Ledger.Utils
  ( GenesisKeyPair,
    MultiSigPairs,
    ShelleyTest,
    epochFromSlotNo,
    evolveKESUntil,
    maxKESIterations,
    maxLLSupply,
    mkCertifiedVRF,
    mkGenKey,
    mkKeyPair,
    runShelleyBase,
    unsafeMkUnitInterval,
  )

-- ===========================================================================

data AllIssuerKeys v (r :: KeyRole) = AllIssuerKeys
  { cold :: KeyPair r v,
    vrf :: (SignKeyVRF v, VerKeyVRF v),
    hot :: [(KESPeriod, (SignKeyKES v, VerKeyKES v))],
    hk :: KeyHash r v
  }
  deriving (Show)

-- | Generator environment.
data GenEnv era = GenEnv
  { geKeySpace :: KeySpace era,
    geConstants :: Constants
  }

-- | Collection of all keys which are required to generate a trace.
--
--   These are the _only_ keys which should be involved in the trace.
data KeySpace era = KeySpace_
  { ksCoreNodes :: [(GenesisKeyPair (Crypto era), AllIssuerKeys (Crypto era) 'GenesisDelegate)],
    -- | Bag of keys to be used for future genesis delegates
    ksGenesisDelegates :: [AllIssuerKeys (Crypto era) 'GenesisDelegate],
    -- | Bag of keys to be used for future stake pools
    ksStakePools :: [AllIssuerKeys (Crypto era) 'StakePool],
    -- | Bag of keys to be used for future payment/staking addresses
    ksKeyPairs :: KeyPairs (Crypto era),
    ksMSigScripts :: MultiSigPairs era,
    -- | Index over the payment keys in 'ksKeyPairs'
    ksIndexedPaymentKeys :: Map (KeyHash 'Payment (Crypto era)) (KeyPair 'Payment (Crypto era)),
    -- | Index over the staking keys in 'ksKeyPairs'
    ksIndexedStakingKeys :: Map (KeyHash 'Staking (Crypto era)) (KeyPair 'Staking (Crypto era)),
    -- | Index over the cold key hashes in Genesis Delegates
    ksIndexedGenDelegates :: Map (KeyHash 'GenesisDelegate (Crypto era)) (AllIssuerKeys (Crypto era) 'GenesisDelegate),
    -- | Index over the pay script hashes in MultiSig pairs
    ksIndexedPayScripts :: Map (ScriptHash era) (MultiSig era, MultiSig era),
    -- | Index over the stake script hashes in MultiSig pairs
    ksIndexedStakeScripts :: Map (ScriptHash era) (MultiSig era, MultiSig era)
  }

deriving instance (Era era) => Show (KeySpace era)

pattern KeySpace ::
  (Shelley.TxBodyConstraints era) =>
  [(GenesisKeyPair (Crypto era), AllIssuerKeys (Crypto era) 'GenesisDelegate)] ->
  [AllIssuerKeys (Crypto era) 'GenesisDelegate] ->
  [AllIssuerKeys (Crypto era) 'StakePool] ->
  KeyPairs (Crypto era) ->
  MultiSigPairs era ->
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
          ksIndexedPayScripts = mkPayScriptHashMap ksMSigScripts,
          ksIndexedStakeScripts = mkStakeScriptHashMap ksMSigScripts,
          ksMSigScripts
        }

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

mkKeyPairs ::
  (DSIGNAlgorithm (DSIGN crypto)) =>
  Word64 ->
  (KeyPair kr crypto, KeyPair kr' crypto)
mkKeyPairs n =
  (mkKeyPair_ (2 * n), mkKeyPair_ (2 * n + 1))
  where
    mkKeyPair_ n_ =
      (uncurry KeyPair . swap)
        (mkKeyPair (n_, n_, n_, n_, n_))

-- | Generate a mapping from genesis delegate cold key hash to the issuer keys.
-- Note: we index all possible genesis delegate keys, that is,
-- core nodes and all potential keys.
mkGenesisDelegatesHashMap ::
  (CC.Crypto crypto) =>
  [(GenesisKeyPair crypto, AllIssuerKeys crypto 'GenesisDelegate)] ->
  [AllIssuerKeys crypto 'GenesisDelegate] ->
  Map (KeyHash 'GenesisDelegate crypto) (AllIssuerKeys crypto 'GenesisDelegate)
mkGenesisDelegatesHashMap coreNodes genesisDelegates =
  Map.fromList (f <$> allDelegateKeys)
  where
    f issuerKeys = ((hashKey . vKey . cold) issuerKeys, issuerKeys)
    allDelegateKeys = (snd <$> coreNodes) <> genesisDelegates

-- | Generate a mapping from stake key hash to stake key pair, from a list of
-- (payment, staking) key pairs.
mkStakeKeyHashMap :: (CC.Crypto crypto) => KeyPairs crypto -> Map (KeyHash 'Staking crypto) (KeyPair 'Staking crypto)
mkStakeKeyHashMap keyPairs =
  Map.fromList (f <$> keyPairs)
  where
    f (_payK, stakeK) = ((hashKey . vKey) stakeK, stakeK)

-- | Generate a mapping from payment key hash to keypair
-- from a list of (payment, staking) key pairs.
mkPayKeyHashMap ::
  (CC.Crypto crypto) =>
  KeyPairs crypto ->
  Map (KeyHash 'Payment crypto) (KeyPair 'Payment crypto)
mkPayKeyHashMap keyPairs =
  Map.fromList (f <$> keyPairs)
  where
    f (payK, _stakeK) = ((hashKey . vKey) payK, payK)

-- | Generate a mapping from pay script hash to multisig pair.
mkPayScriptHashMap ::
  (Shelley.TxBodyConstraints era) =>
  [(MultiSig era, MultiSig era)] ->
  Map (ScriptHash era) (MultiSig era, MultiSig era)
mkPayScriptHashMap scripts =
  Map.fromList (f <$> scripts)
  where
    f script@(pay, _stake) = (hashMultiSigScript pay, script)

-- | Generate a mapping from stake script hash to multisig pair.
mkStakeScriptHashMap ::
  (Shelley.TxBodyConstraints era) =>
  [(MultiSig era, MultiSig era)] ->
  Map (ScriptHash era) (MultiSig era, MultiSig era)
mkStakeScriptHashMap scripts =
  Map.fromList (f <$> scripts)
  where
    f script@(_pay, stake) = (hashMultiSigScript stake, script)

-- | Multi-Sig Scripts based on the given key pairs
mkMSigScripts :: (Era era) => KeyPairs (Crypto era) -> MultiSigPairs era
mkMSigScripts = map mkScriptsFromKeyPair

-- | Combine a list of multisig pairs into hierarchically structured multi-sig
-- scripts, list must have at least length 3. Be careful not to call with too
-- many pairs in order not to create too many of the possible combinations.
mkMSigCombinations :: (Era era) => MultiSigPairs era -> MultiSigPairs era
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
  (Era era) =>
  (KeyPair 'Payment (Crypto era), KeyPair 'Staking (Crypto era)) ->
  (MultiSig era, MultiSig era)
mkScriptsFromKeyPair (k0, k1) =
  (mkScriptFromKey $ asWitness k0, mkScriptFromKey $ asWitness k1)

mkScriptFromKey :: (Era era) => KeyPair 'Witness (Crypto era) -> MultiSig era
mkScriptFromKey = (RequireSignature . hashKey . vKey)

-- | Find first matching key pair for a credential. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairCred ::
  Credential kr era ->
  Map (KeyHash kr (Crypto era)) (KeyPair kr (Crypto era)) ->
  KeyPair kr (Crypto era)
findPayKeyPairCred (KeyHashObj addr) keyHashMap =
  fromMaybe
    (error $ "findPayKeyPairCred: could not find a match for the given credential: " <> show addr)
    (Map.lookup addr keyHashMap)
findPayKeyPairCred _ _ =
  error "findPayKeyPairCred: expects only KeyHashObj"

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPairAddr ::
  Addr era ->
  Map (KeyHash 'Payment (Crypto era)) (KeyPair 'Payment (Crypto era)) ->
  KeyPair 'Payment (Crypto era)
findPayKeyPairAddr a keyHashMap =
  case a of
    Addr _ addr (StakeRefBase _) -> findPayKeyPairCred addr keyHashMap
    Addr _ addr (StakeRefPtr _) -> findPayKeyPairCred addr keyHashMap
    _ ->
      error "findPayKeyPairAddr: expects only Base or Ptr addresses"

-- | Find matching multisig scripts for a credential.
findPayScriptFromCred ::
  Credential 'Witness era ->
  Map (ScriptHash era) (MultiSig era, MultiSig era) ->
  (MultiSig era, MultiSig era)
findPayScriptFromCred (ScriptHashObj scriptHash) scriptsByPayHash =
  fromMaybe
    (error "findPayScript: could not find matching script for given credential")
    (Map.lookup scriptHash scriptsByPayHash)
findPayScriptFromCred _ _ =
  error "findPayScriptFromCred: expects only ScriptHashObj"

-- | Find first matching script for a credential.
findStakeScriptFromCred ::
  Credential 'Witness era ->
  Map (ScriptHash era) (MultiSig era, MultiSig era) ->
  (MultiSig era, MultiSig era)
findStakeScriptFromCred (ScriptHashObj scriptHash) scriptsByStakeHash =
  fromMaybe
    (error "findStakeScriptFromCred: could not find matching script for given credential")
    (Map.lookup scriptHash scriptsByStakeHash)
findStakeScriptFromCred _ _ =
  error "findStakeScriptFromCred: expects only ScriptHashObj"

-- | Find first matching multisig script for an address.
findPayScriptFromAddr ::
  Addr era ->
  Map (ScriptHash era) (MultiSig era, MultiSig era) ->
  (MultiSig era, MultiSig era)
findPayScriptFromAddr (Addr _ scriptHash (StakeRefBase _)) scriptsByPayHash =
  findPayScriptFromCred (asWitness scriptHash) scriptsByPayHash
findPayScriptFromAddr (Addr _ scriptHash (StakeRefPtr _)) scriptsByPayHash =
  findPayScriptFromCred (asWitness scriptHash) scriptsByPayHash
findPayScriptFromAddr _ _ =
  error "findPayScriptFromAddr: expects only base and pointer script addresses"

-- | Select one random verification staking key from list of pairs of KeyPair.
pickStakeKey :: KeyPairs crypto -> Gen (VKey 'Staking crypto)
pickStakeKey keys = vKey . snd <$> QC.elements keys

-- | Generates a list of coins for the given 'Addr' and produced a 'TxOut' for each 'Addr'
--
-- Note: we need to keep the initial utxo coin sizes large enough so that
-- when we simulate sequences of transactions, we have enough funds available
-- to include certificates that require deposits.
genTxOut ::
  forall era.
  (ShelleyTest era) =>
  QC.Gen (Core.Value era) ->
  Constants ->
  [Addr era] ->
  Gen [TxOut era]
genTxOut gv Constants {maxGenesisOutputVal, minGenesisOutputVal} addrs = do
  let ln = length addrs
  p <- max <$> QC.choose (0, ln) <*> QC.choose (0, ln)
  cls <- genCoinList minGenesisOutputVal maxGenesisOutputVal p
  vls <- genValList @era gv minGenesisOutputVal maxGenesisOutputVal (ln - p)
  outs <- QC.shuffle ((Val.inject <$> cls) ++ vls)
  return (uncurry TxOut <$> zip addrs outs)

-- | Generates a list of 'Coin' values of length between 'lower' and 'upper'
-- and with values between 'minCoin' and 'maxCoin'.
-- NOTE we pass here a Value generator gv that is piped in from where
-- it can be defined in the necessary context (see Tests.hs)
genValList ::
  (ShelleyTest era) =>
  QC.Gen (Core.Value era) ->
  Integer ->
  Integer ->
  Int ->
  Gen [Core.Value era]
genValList gv minCoin maxCoin len = do
  let addWOCoin c v = c <+> (v <-> (Val.inject $ Val.coin v))
  replicateM len $ liftM2 addWOCoin (Val.inject <$> genCoin minCoin maxCoin) gv

-- | Generates a list of 'Coin' values of length between 'lower' and 'upper'
-- and with values between 'minCoin' and 'maxCoin'.
genCoinList :: Integer -> Integer -> Int -> Gen [Coin]
genCoinList minCoin maxCoin len = do
  replicateM len $ genCoin minCoin maxCoin

-- TODO this should be an exponential distribution, not constant
genCoin :: Integer -> Integer -> Gen Coin
genCoin minCoin maxCoin = Coin <$> QC.choose (minCoin, maxCoin)

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

zero :: UnitInterval
zero = unsafeMkUnitInterval 0

-- | Try to map the unit interval to a natural number. We don't care whether
-- this is surjective. But it should be right inverse to `fromNatural` - that
-- is, one should be able to recover the `UnitInterval` value used here.
unitIntervalToNatural :: UnitInterval -> Natural
unitIntervalToNatural = floor . ((10000 % 1) *) . intervalValue

mkBlockHeader ::
  ( Mock crypto
  ) =>
  -- | Hash of previous block
  HashHeader crypto ->
  -- | All keys in the stake pool
  AllIssuerKeys crypto r ->
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
  OCert crypto ->
  -- | Block size
  Natural ->
  -- | Block body hash
  HashBBody crypto ->
  BHeader crypto
mkBlockHeader prev pkeys s blockNo enonce kesPeriod c0 oCert bodySize bodyHash =
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
          (ProtVer 0 0)
      kpDiff = kesPeriod - c0
      hotKey = case evolveKESUntil sHot (KESPeriod 0) (KESPeriod kpDiff) of
        Nothing ->
          error ("could not evolve key to iteration " ++ show (c0, kesPeriod, kpDiff))
        Just hkey -> hkey
      sig = signedKES () kpDiff bhb hotKey
   in BHeader bhb sig

mkBlock ::
  ( Shelley.TxBodyConstraints era,
    Mock (Crypto era)
  ) =>
  -- | Hash of previous block
  HashHeader (Crypto era) ->
  -- | All keys in the stake pool
  AllIssuerKeys (Crypto era) r ->
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
  OCert (Crypto era) ->
  Block era
mkBlock prev pkeys txns s blockNo enonce kesPeriod c0 oCert =
  let bodySize = fromIntegral $ bBodySize $ (TxSeq . StrictSeq.fromList) txns
      bodyHash = bbHash $ TxSeq $ StrictSeq.fromList txns
      bh = mkBlockHeader prev pkeys s blockNo enonce kesPeriod c0 oCert bodySize bodyHash
   in Block bh (TxSeq $ StrictSeq.fromList txns)

-- | Create a block with a faked VRF result.
mkBlockFakeVRF ::
  ( Shelley.TxBodyConstraints era,
    ExMock (Crypto era)
  ) =>
  -- | Hash of previous block
  HashHeader (Crypto era) ->
  -- | All keys in the stake pool
  AllIssuerKeys (Crypto era) r ->
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
  OCert (Crypto era) ->
  Block era
mkBlockFakeVRF prev pkeys txns s blockNo enonce (NatNonce bnonce) l kesPeriod c0 oCert =
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
          ( mkCertifiedVRF
              (WithResult nonceNonce (fromIntegral bnonce))
              (fst $ vrf pkeys)
          )
          ( mkCertifiedVRF
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

mkOCert ::
  forall crypto r.
  (CC.Crypto crypto, Signable (DSIGN crypto) (OCertSignable crypto)) =>
  AllIssuerKeys crypto r ->
  Word64 ->
  KESPeriod ->
  OCert crypto
mkOCert pkeys n c0 =
  let (_, (_, vKeyHot)) = head $ hot pkeys
      KeyPair _vKeyCold sKeyCold = cold pkeys
   in OCert
        vKeyHot
        n
        c0
        (signedDSIGN @crypto sKeyCold (OCertSignable vKeyHot n c0))

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
  ei <- asks epochInfo
  firstSlotNo <- epochInfoFirst ei (epochFromSlotNo s + 1)
  stabilityWindow <- asks stabilityWindow

  return (s >= firstSlotNo *- Duration (2 * stabilityWindow))

-- | Account with empty treasury
genesisAccountState :: AccountState
genesisAccountState =
  AccountState
    { _treasury = Coin 0,
      _reserves = maxLLSupply
    }

-- | The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId :: forall era.
  (ShelleyTest era) => Ledger.TxId era
genesisId =
  TxId $
    hashAnnotated
      ( (TxBody @era)
          Set.empty
          StrictSeq.Empty
          StrictSeq.Empty
          (Wdrl Map.empty)
          (Coin 0)
          (SlotNo 0)
          SNothing
          SNothing
      )

-- | Creates the UTxO for a new ledger with the specified transaction outputs.
genesisCoins ::
  (ShelleyTest era) =>
  [TxOut era] ->
  UTxO era
genesisCoins outs =
  UTxO $
    Map.fromList [(TxIn genesisId idx, out) | (idx, out) <- zip [0 ..] outs]

-- | Apply a transaction body as a state transition function on the ledger state.
applyTxBody ::
  ( ShelleyTest era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn era)),
    HasField "outputs" (Core.TxBody era) (StrictSeq (TxOut era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert era))
  ) =>
  LedgerState era ->
  PParams era ->
  Core.TxBody era ->
  LedgerState era
applyTxBody ls pp tx =
  ls
    { _utxoState =
        us
          { _utxo = eval (txins tx ⋪ (_utxo us) ∪ txouts tx),
            _deposited = depositPoolChange ls pp tx,
            _fees = (getField @"txfee" tx) <> (_fees . _utxoState $ ls)
          },
      _delegationState =
        dels
          { _dstate = dst {_rewards = newAccounts}
          }
    }
  where
    dels = _delegationState ls
    dst = _dstate dels
    us = _utxoState ls
    newAccounts =
      reapRewards
        ((_rewards . _dstate . _delegationState) ls)
        (Map.mapKeys getRwdCred . unWdrl $ getField @"wdrls" tx)
