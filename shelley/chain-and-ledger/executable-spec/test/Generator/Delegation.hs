{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator.Delegation
  ( genDCerts )
  where

import           Data.Foldable (find)
import qualified Data.Map as Map
import           Data.Ratio ((%))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Hedgehog (Gen)
import           Lens.Micro (to, (^.))
import           Numeric.Natural (Natural)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Coin (Coin (..))
import           Delegation.Certificates (pattern DeRegKey, pattern InstantaneousRewards,
                     pattern RegKey, pattern RegPool, pattern RetirePool, decayKey, isDeRegKey)
import           Examples (unsafeMkUnitInterval)
import           Generator.Core (genCoinList, genInteger, genNatural, toCred)
import           Keys (hashKey, vKey)
import           Ledger.Core (dom, (∈), (∉))
import           LedgerState (dstate, keyRefund, pParams, pstate, stPools, stkCreds, _pstate,
                     _stPools, _stkCreds)
import           MockTypes (CoreKeyPairs, DCert, DPState, DState, KeyPair, KeyPairs, PState,
                     PoolParams, VrfKeyPairs, hashKeyVRF)
import           Mutator (getAnyStakeKey)
import           PParams (PParams (..), eMax)
import           Slot (Epoch (Epoch), Slot (Slot), epochFromSlot)
import           TxData (Credential (KeyHashObj), pattern PoolParams, RewardAcnt (..),
                     pattern StakeCreds, StakePools (StakePools), _poolPubKey, _poolVrf)
import           UTxO (deposits)

-- | Generate certificates and also return the associated witnesses and
-- deposits and refunds required.
genDCerts
  :: KeyPairs
  -> CoreKeyPairs
  -> VrfKeyPairs
  -> PParams
  -> DPState
  -> Slot
  -> Natural
  -> Gen (Seq DCert, [KeyPair], CoreKeyPairs, Coin, Coin)
genDCerts keys coreKeys vrfKeys pparams dpState slot ttl = do
  -- TODO @uroboros Generate _multiple_ certs per Tx
  -- TODO ensure that the `Seq` is constructed with the list reversed, or that
  -- later traversals are done backwards, to be consistent with the executable
  -- spec (see `delegsTransition` in `STS.Delegs`) which consumes the list
  -- starting at the tail end.
  cert <- genDCert keys coreKeys vrfKeys pparams dpState slot
  case cert of
    Nothing ->
      return (Seq.empty, [], [], Coin 0, Coin 0)
    Just (cert_, witnessOrCoreKeys) -> do
      let certs = [cert_]

          deposits_ = deposits pparams (_stPools (_pstate dpState)) certs

          deRegStakeCreds = filter isDeRegKey certs
          slotWithTTL = slot + Slot ttl
          rewardForCred crt =
            let (dval, dmin, lambda) = decayKey pparams
             in keyRefund dval
                          dmin
                          lambda
                          (dpState ^. dstate . stkCreds)
                          slotWithTTL
                          crt
          refunds_ = sum (rewardForCred <$> deRegStakeCreds)

      case witnessOrCoreKeys of
        Left witKey -> do
          let witKeys = [witKey]
          return (Seq.fromList certs, witKeys, [], deposits_, refunds_)
        Right coreKeys' ->
          pure (Seq.fromList certs, [], coreKeys', deposits_, refunds_)

-- | Occasionally generate a valid certificate
genDCert
  :: KeyPairs
  -> CoreKeyPairs
  -> VrfKeyPairs
  -> PParams
  -> DPState
  -> Slot
  -> Gen (Maybe (DCert, Either KeyPair CoreKeyPairs))
genDCert keys coreKeys vrfKeys pparams dpState slot =
  -- TODO @uroboros Generate _Delegate_ Certificates
  Gen.frequency [ (3, genRegKeyCert keys dState)
                , (3, genDeRegKeyCert keys dState)
                , (3, genRegPool keys vrfKeys dpState)
                , (3, genRetirePool keys pparams pState slot)
                , (1, genInstantaneousRewards keys coreKeys dState)
                , (1, pure Nothing)
                ]
 where
  dState = dpState ^. dstate
  pState = dpState ^. pstate

-- | Generate a RegKey certificate along and also returns the stake key
-- (needed to witness the certificate)
genRegKeyCert
  :: KeyPairs
  -> DState
  -> Gen (Maybe (DCert, Either KeyPair CoreKeyPairs))
genRegKeyCert keys delegSt =
  case availableKeys of
    [] -> pure Nothing
    _ -> do
           (_payKey, stakeKey) <- Gen.element availableKeys
           pure $ Just $ (RegKey (toCred stakeKey), Left stakeKey)
  where
    notRegistered k = k ∉ dom (_stkCreds delegSt)
    availableKeys = filter (notRegistered . toCred . snd) keys

-- | Generate an InstantaneousRewards Transfer certificate
genInstantaneousRewards
  :: KeyPairs
  -> CoreKeyPairs
  -> DState
  -> Gen (Maybe (DCert, Either KeyPair CoreKeyPairs))

genInstantaneousRewards _ coreKeys delegSt = do
  let StakeCreds credentials = _stkCreds delegSt

  winnerCreds <- take <$> Gen.integral (Range.linear 0 $ Map.size credentials)
                      <*> Gen.shuffle (Map.keys credentials)
  coins <- genCoinList 1 1000 (length winnerCreds) (length winnerCreds)

  coreSigners <- take <$> Gen.integral (Range.linear 0 $ length coreKeys)
                      <*> Gen.shuffle coreKeys

  pure $ Just ( InstantaneousRewards (Map.fromList $ zip winnerCreds coins)
              , Right coreSigners)

-- | Generate a DeRegKey certificate along with the stake key, which is needed
-- to witness the certificate.
genDeRegKeyCert
  :: KeyPairs
  -> DState
  -> Gen (Maybe (DCert, Either KeyPair CoreKeyPairs))
genDeRegKeyCert keys delegSt =
  case availableKeys of
    [] -> pure Nothing
    _ -> do
           (_payKey, stakeKey) <- Gen.element availableKeys
           pure $ Just (DeRegKey (toCred stakeKey), Left stakeKey)
  where
    registered k = k ∈ dom (_stkCreds delegSt)
    availableKeys = filter (registered . toCred . snd) keys

-- | Generate and return a RegPool certificate along with its witnessing key.
genRegPool
  :: KeyPairs
  -> VrfKeyPairs
  -> DPState
  -> Gen (Maybe (DCert, Either KeyPair CoreKeyPairs))
genRegPool keys vrfKeys dpState =
  if null availableKeys || null availableVrfKeys
     then pure Nothing
     else do
    Just <$> genDCertRegPool availableKeys availableVrfKeys
 where
  notRegistered k = k `notElem` (dpState ^. pstate . pParams . to Map.elems . to (_poolPubKey <$>))
  availableKeys = filter (notRegistered . hashKey . vKey . snd) keys

  notRegisteredVrf k = k `notElem` (dpState ^. pstate . pParams . to Map.elems . to (_poolVrf <$>))
  availableVrfKeys = filter (notRegisteredVrf . hashKeyVRF . snd) vrfKeys

-- | Generate PoolParams and the key witness.
genStakePool :: KeyPairs -> VrfKeyPairs -> Gen (PoolParams, KeyPair)
genStakePool skeys vrfKeys =
  mkPoolParams
    <$> (Gen.element skeys)
    <*> (snd <$> Gen.element vrfKeys)
    <*> (Coin <$> genInteger 1 100)
    <*> (Coin <$> genInteger 1 100)
    <*> (genNatural 0 100)
    <*> (getAnyStakeKey skeys)
 where
  mkPoolParams poolKeyPair vrfKey cost pledge marginPercent acntKey =
    let interval = unsafeMkUnitInterval $ fromIntegral marginPercent % 100
        pps = PoolParams
                (hashKey . vKey . snd $ poolKeyPair)
                (hashKeyVRF vrfKey)
                pledge
                cost
                interval
                (RewardAcnt $ KeyHashObj $ hashKey acntKey)
                Set.empty
     in (pps, snd poolKeyPair)

-- | Generate `RegPool` and the key witness.
genDCertRegPool :: KeyPairs -> VrfKeyPairs -> Gen (DCert, Either KeyPair CoreKeyPairs)
genDCertRegPool skeys vrfKeys = do
  (pps, poolKey) <- genStakePool skeys vrfKeys
  pure (RegPool pps, Left poolKey)

-- | Generate a RetirePool along with the keypair which registered it.
--
-- Choose a `KeyHash` to retire, by pulling from the set of registered
-- `KeyHash`s in the `stakePools` mapping. Generate a random epoch within an
-- acceptable range of the current epoch. In addition to the `RetirePool`
-- constructed value, return the keypair which corresponds to the selected
-- `KeyHash`, by doing a lookup in the set of `availableKeys`.
genRetirePool
  :: KeyPairs
  -> PParams
  -> PState
  -> Slot
  -> Gen (Maybe (DCert, Either KeyPair CoreKeyPairs))
genRetirePool availableKeys pp pState slot =
  if (null availableKeys || null poolHashKeys)
     then pure Nothing
     else (\keyHash epoch ->
              Just (RetirePool keyHash epoch, Left $ findKeyPair keyHash))
                <$> Gen.element poolHashKeys
                <*> (Epoch <$> Gen.integral (Range.constant epochLow epochHigh))
 where
  stakePools = pState ^. (stPools . to (\(StakePools x) -> x))
  poolHashKeys = Set.toList (Map.keysSet stakePools)
  findKeyPair keyHash =
    case find (\x -> hashKey (vKey (snd x)) == keyHash) availableKeys of
      Nothing ->
        error "genRetirePool: impossible: keyHash doesn't match availableKeys"
      Just ks -> snd ks
  Epoch cepoch = epochFromSlot slot
  Epoch maxEpoch = pp ^. eMax
  epochLow = cepoch + 1
  epochHigh = cepoch + maxEpoch - 1
