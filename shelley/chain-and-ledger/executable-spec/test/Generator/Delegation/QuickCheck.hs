{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator.Delegation.QuickCheck
  ( genDCerts )
  where
import           Data.Foldable (find)
import qualified Data.Map as Map
import           Data.Ratio ((%))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Lens.Micro (to, (^.))
import           Numeric.Natural (Natural)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           Coin (Coin (..))
import           Delegation.Certificates (pattern DeRegKey, pattern Delegate,
                     pattern GenesisDelegate, pattern InstantaneousRewards, pattern RegKey,
                     pattern RegPool, pattern RetirePool, pattern StakeCreds, decayKey, isDeRegKey)
import           Examples (unsafeMkUnitInterval)
import           Generator.Core.QuickCheck (genCoinList, genInteger, genNatural, toCred)
import           Keys (GenDelegs (..), hashKey, vKey)
import           Ledger.Core (dom, range, (∈), (∉))
import           LedgerState (dstate, keyRefund, pParams, pstate, stPools, stkCreds, _dstate,
                     _genDelegs, _pstate, _stPools, _stkCreds)
import           MockTypes (CoreKeyPair, DCert, DPState, DState, KeyPair, KeyPairs, PState,
                     PoolParams, VKey, VrfKeyPairs, hashKeyVRF)
import           PParams (PParams (..), eMax)
import           Slot (Epoch (Epoch), Slot (Slot), epochFromSlot)
import           TxData (Credential (KeyHashObj), pattern Delegation, pattern PoolParams,
                     RewardAcnt (..), StakePools (StakePools), _poolPubKey, _poolVrf)
import           UTxO (deposits)

-- | Generate certificates and also return the associated witnesses and
-- deposits and refunds required.
genDCerts
  :: KeyPairs
  -> [CoreKeyPair]
  -> VrfKeyPairs
  -> PParams
  -> DPState
  -> Slot
  -> Natural
  -> Gen (Seq DCert, [KeyPair], [CoreKeyPair], Coin, Coin)
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
        Right coreSignKeys ->
          pure (Seq.fromList certs, [], coreSignKeys, deposits_, refunds_)

-- | Occasionally generate a valid certificate
--
-- Returning `Nothing` indicates a failure to generate a value, usually due to lack of
-- available values from the pre-populated (e.g. key) spaces.
-- A `Just` represents a successfully generated value.
--
-- Different generators return witnesses that are either genesis or regular keys.
--
-- Note: we register keys and pools more often than deregistering/retiring them,
-- and we generate more delegations than registrations of keys/pools.
genDCert
  :: KeyPairs
  -> [CoreKeyPair]
  -> VrfKeyPairs
  -> PParams
  -> DPState
  -> Slot
  -> Gen (Maybe (DCert, Either KeyPair [CoreKeyPair]))
genDCert keys coreKeys vrfKeys pparams dpState slot =
  QC.frequency [ (2, genRegKeyCert keys dState)
               , (2, genRegPool keys vrfKeys dpState)
               , (3, genDelegation keys dpState)
               , (1, genGenesisDelegation keys coreKeys dpState)
               , (1, genDeRegKeyCert keys dState)
               , (1, genRetirePool keys pparams pState slot)
               -- TODO mgudemann
               -- needs to sign transaction with `coreKeys`
               -- tends to generate Txs without input which leads to error
               , (1, genInstantaneousRewards coreKeys dState)
               ]
 where
  dState = dpState ^. dstate
  pState = dpState ^. pstate

-- | Generate a RegKey certificate along and also returns the stake key
-- (needed to witness the certificate)
genRegKeyCert
  :: KeyPairs
  -> DState
  -> Gen (Maybe (DCert, Either KeyPair [CoreKeyPair]))
genRegKeyCert keys delegSt =
  case availableKeys of
    [] -> pure Nothing
    _ -> do
           (_payKey, stakeKey) <- QC.elements availableKeys
           pure $ Just (RegKey (toCred stakeKey), Left stakeKey)
  where
    notRegistered k = k ∉ dom (_stkCreds delegSt)
    availableKeys = filter (notRegistered . toCred . snd) keys

-- | Generate a DeRegKey certificate along with the stake key, which is needed
-- to witness the certificate.
genDeRegKeyCert
  :: KeyPairs
  -> DState
  -> Gen (Maybe (DCert, Either KeyPair [CoreKeyPair]))
genDeRegKeyCert keys dState =
  case availableKeys of
    [] -> pure Nothing
    _ -> do
           (_payKey, stakeKey) <- QC.elements availableKeys
           pure $ Just (DeRegKey (toCred stakeKey), Left stakeKey)
  where
    registered k = k ∈ dom (_stkCreds dState)
    availableKeys = filter (registered . toCred . snd) keys

-- | Generate a new delegation certificate by picking a registered key
-- and pool. The delegation is witnessed by the delegator's key, which
-- we return along with the certificate.
--
-- Returns nothing if there are no active keys or pools.
genDelegation
  :: KeyPairs
  -> DPState
  -> Gen (Maybe (DCert, Either KeyPair [CoreKeyPair]))
genDelegation keys dpState =
  if null availableDelegates || null availablePools
    then
      pure Nothing
    else
      mkCert <$> QC.elements availableDelegates
             <*> QC.elements availablePools
  where
    mkCert (_, delegatorKey) poolKey = Just (cert, Left delegatorKey)
      where
        cert = Delegate (Delegation (toCred delegatorKey) poolKey)

    registeredDelegate k = k ∈ dom (_stkCreds (_dstate dpState))
    availableDelegates = filter (registeredDelegate . toCred . snd) keys

    (StakePools registeredPools) = _stPools (_pstate dpState)
    availablePools = Map.keys registeredPools

genGenesisDelegation
  :: KeyPairs
  -> [CoreKeyPair]
  -> DPState
  -> Gen (Maybe (DCert, Either KeyPair [CoreKeyPair]))
genGenesisDelegation keys coreKeys dpState =
  if null genesisDelegators || null availableDelegatees
    then
      pure Nothing
    else
      mkCert <$> QC.elements genesisDelegators
             <*> QC.elements availableDelegatees
  where
    hashVKey = hashKey . vKey
    mkCert gkey key = Just
      ( GenesisDelegate (hashVKey gkey, (hashVKey . snd) key)
      , Right [gkey])

    (GenDelegs genDelegs_) = _genDelegs $ dpState ^. dstate

    genesisDelegator k = k ∈ dom genDelegs_
    genesisDelegators = filter (genesisDelegator . hashVKey) coreKeys

    notDelegatee k = k ∉ range genDelegs_
    availableDelegatees = filter (notDelegatee . hashVKey . snd) keys

-- | Generate and return a RegPool certificate along with its witnessing key.
genRegPool
  :: KeyPairs
  -> VrfKeyPairs
  -> DPState
  -> Gen (Maybe (DCert, Either KeyPair [CoreKeyPair]))
genRegPool keys vrfKeys dpState =
  if null availableKeys || null availableVrfKeys
     then pure Nothing
     else
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
    <$> (QC.elements skeys)
    <*> (snd <$> QC.elements vrfKeys)
    <*> (Coin <$> genInteger 1 100)
    <*> (Coin <$> genInteger 1 100)
    <*> (fromInteger <$> QC.choose (0, 100) :: Gen Natural)
    <*> (getAnyStakeKey skeys)
 where
  getAnyStakeKey :: KeyPairs -> Gen VKey
  getAnyStakeKey keys = vKey . snd <$> QC.elements keys

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
genDCertRegPool :: KeyPairs -> VrfKeyPairs -> Gen (DCert, Either KeyPair [CoreKeyPair])
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
  -> Gen (Maybe (DCert, Either KeyPair [CoreKeyPair]))
genRetirePool availableKeys pp pState slot =
  if (null availableKeys || null poolHashKeys)
     then pure Nothing
     else (\keyHash epoch ->
              Just (RetirePool keyHash epoch, Left $ findKeyPair keyHash))
                <$> QC.elements poolHashKeys
                <*> (Epoch <$> genNatural epochLow epochHigh)
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

-- | Generate an InstantaneousRewards Transfer certificate
genInstantaneousRewards
  :: [CoreKeyPair]
  -> DState
  -> Gen (Maybe (DCert, Either KeyPair [CoreKeyPair]))

genInstantaneousRewards coreKeys delegSt = do
  let StakeCreds credentials = _stkCreds delegSt

  winnerCreds <- take <$> QC.elements [0 .. (max 0 $ (Map.size credentials) - 1)]
                      <*> QC.shuffle (Map.keys credentials)
  coins <- genCoinList 1 1000 (length winnerCreds) (length winnerCreds)

  coreSigners <- take <$> QC.elements [5 .. (max 0 $ (length coreKeys) - 1)]
                      <*> QC.shuffle coreKeys

  let credCoinMap = Map.fromList $ zip winnerCreds coins
  pure $ Just ( InstantaneousRewards credCoinMap
              , Right coreSigners)
