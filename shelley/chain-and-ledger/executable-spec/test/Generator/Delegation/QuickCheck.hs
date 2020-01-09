{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator.Delegation.QuickCheck
  ( genDCerts
  , CertCred (..))
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
import           Test.Utils

import           Address (scriptToCred)
import           BaseTypes (interval0)
import           Coin (Coin (..))
import           ConcreteCryptoTypes (CoreKeyPair, DCert, DPState, DState, KeyPair, KeyPairs,
                     MultiSig, MultiSigPairs, PState, PoolParams, VKey, VrfKeyPairs, hashKeyVRF)
import           Delegation.Certificates (pattern DCertMir, pattern DeRegKey, pattern Delegate,
                     pattern GenesisDelegate, pattern MIRCert, pattern RegKey, pattern RegPool,
                     pattern RetirePool, pattern StakeCreds, decayKey, isDeRegKey)
import           Examples (unsafeMkUnitInterval)
import           Generator.Core.QuickCheck (genCoinList, genInteger, genWord64, toCred)
import           Keys (GenDelegs (..), hashAnyKey, hashKey, vKey)
import           Ledger.Core (dom, range, (∈), (∉))
import           LedgerState (dstate, keyRefund, pParams, pstate, stPools, stkCreds, _dstate,
                     _genDelegs, _pstate, _stPools, _stkCreds)
import           PParams (PParams (..), d, eMax)
import           Slot (EpochNo (EpochNo), SlotNo (SlotNo))
import           Tx (getKeyCombinations)
import           TxData (pattern DCertDeleg, pattern DCertGenesis, pattern DCertPool,
                     pattern Delegation, pattern KeyHashObj, pattern PoolParams, RewardAcnt (..),
                     pattern StakePools, _poolPubKey, _poolVrf)
import           UTxO (totalDeposits)

data CertCred = CoreKeyCred [CoreKeyPair]
              | KeyCred KeyPair
              | ScriptCred (MultiSig, MultiSig)
              deriving (Show)

-- | Generate certificates and also return the associated witnesses and
-- deposits and refunds required.
genDCerts
  :: KeyPairs
  -> MultiSigPairs
  -> [CoreKeyPair]
  -> VrfKeyPairs
  -> PParams
  -> DPState
  -> SlotNo
  -> Natural
  -> Gen (Seq DCert, [CertCred], Coin, Coin)
genDCerts keys scripts coreKeys vrfKeys pparams dpState slot ttl = do
  -- TODO @uroboros Generate _multiple_ certs per Tx
  -- TODO ensure that the `Seq` is constructed with the list reversed, or that
  -- later traversals are done backwards, to be consistent with the executable
  -- spec (see `delegsTransition` in `STS.Delegs`) which consumes the list
  -- starting at the tail end.
  cert <- genDCert keys scripts coreKeys vrfKeys pparams dpState slot
  case cert of
    Nothing ->
      return (Seq.empty, [], Coin 0, Coin 0)
    Just (cert_, witnessOrCoreKeys) -> do
      let certs = [cert_]
          deposits_ = totalDeposits pparams (_stPools (_pstate dpState)) certs

          deRegStakeCreds = filter isDeRegKey certs
          slotWithTTL = slot + SlotNo (fromIntegral ttl)
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
        ScriptCred (_, stakeScript) -> do
          witnessHashes <- QC.elements (getKeyCombinations stakeScript)
          let witnessHashSet = Set.fromList witnessHashes
              witnesses =
                filter (\k -> (hashAnyKey $ vKey k) `Set.member` witnessHashSet)
                (map snd keys)
          pure ( Seq.fromList certs
               , (map KeyCred witnesses) ++
                 (case cert_ of
                    DCertDeleg (RegKey _) -> []
                    _        -> [witnessOrCoreKeys])
               , deposits_
               , refunds_)
        _ -> return (Seq.fromList certs, [witnessOrCoreKeys], deposits_, refunds_)


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
  -> MultiSigPairs
  -> [CoreKeyPair]
  -> VrfKeyPairs
  -> PParams
  -> DPState
  -> SlotNo
  -> Gen (Maybe (DCert, CertCred))
genDCert keys scripts coreKeys vrfKeys pparams dpState slot =
  QC.frequency [ (2, genRegKeyCert keys scripts dState)
               , (2, genRegPool keys vrfKeys dpState)
               , (3, genDelegation keys scripts dpState)
               , (1, genGenesisDelegation keys coreKeys dpState)
               , (1, genDeRegKeyCert keys scripts dState)
               , (1, genRetirePool keys pparams pState slot)
               , (1, genInstantaneousRewards coreKeys pparams dState)
               ]
 where
  dState = dpState ^. dstate
  pState = dpState ^. pstate

-- | Generate a RegKey certificate along and also returns the staking credential
-- (needed to witness the certificate)
genRegKeyCert
  :: KeyPairs
  -> MultiSigPairs
  -> DState
  -> Gen (Maybe (DCert, CertCred))
genRegKeyCert keys scripts delegSt =
  QC.oneof
    [ case availableKeys of
       [] -> pure Nothing
       _  -> do
         (_payKey, stakeKey) <- QC.elements availableKeys
         pure $ Just (DCertDeleg (RegKey (toCred stakeKey)), KeyCred stakeKey)
    , case availableScripts of
        [] -> pure Nothing
        _  -> do
          scriptPair@(_, stakeScript) <- QC.elements availableScripts
          pure $ Just (DCertDeleg (RegKey (scriptToCred stakeScript))
                      , ScriptCred scriptPair)
    ]
  where
    notRegistered k = k ∉ dom (_stkCreds delegSt)
    availableKeys = filter (notRegistered . toCred . snd) keys
    availableScripts = filter (notRegistered . scriptToCred . snd) scripts

-- | Generate a DeRegKey certificate along with the staking credential, which is
-- needed to witness the certificate.
genDeRegKeyCert
  :: KeyPairs
  -> MultiSigPairs
  -> DState
  -> Gen (Maybe (DCert, CertCred))
genDeRegKeyCert keys scripts dState =
  QC.oneof
  [ case availableKeys of
      [] -> pure Nothing
      _ -> do
        (_payKey, stakeKey) <- QC.elements availableKeys
        pure $ Just (DCertDeleg (DeRegKey (toCred stakeKey)), KeyCred stakeKey)
  , case availableScripts of
      [] -> pure Nothing
      _  -> do
          scriptPair@(_, stakeScript) <- QC.elements availableScripts
          pure $ Just (DCertDeleg (DeRegKey (scriptToCred stakeScript))
                      , ScriptCred scriptPair)
  ]
  where
    registered k = k ∈ dom (_stkCreds dState)
    availableKeys = filter (registered . toCred . snd) keys
    availableScripts = filter (registered . scriptToCred . snd) scripts

-- | Generate a new delegation certificate by picking a registered staking
-- credential and pool. The delegation is witnessed by the delegator's
-- credential which we return along with the certificate.
--
-- Returns nothing if there are no registered staking credentials or no
-- registered pools.
genDelegation
  :: KeyPairs
  -> MultiSigPairs
  -> DPState
  -> Gen (Maybe (DCert, CertCred))
genDelegation keys scripts dpState =
  if null availablePools
    then
      pure Nothing
    else
    QC.oneof
    [ if null availableDelegates
      then pure Nothing
      else mkCert <$> QC.elements availableDelegates
                  <*> QC.elements availablePools
    , if null availableDelegatesScripts
      then pure Nothing
      else mkCertFromScript <$> QC.elements availableDelegatesScripts
                            <*> QC.elements availablePools
    ]
  where
    mkCert (_, delegatorKey) poolKey = Just (cert, KeyCred delegatorKey)
      where
        cert = DCertDeleg (Delegate (Delegation (toCred delegatorKey) poolKey))
    mkCertFromScript (s, delegatorScript) poolKey =
      Just (scriptCert, ScriptCred (s, delegatorScript))
      where
        scriptCert =
          DCertDeleg (Delegate (Delegation (scriptToCred delegatorScript) poolKey))

    registeredDelegate k = k ∈ dom (_stkCreds (_dstate dpState))
    availableDelegates = filter (registeredDelegate . toCred . snd) keys
    availableDelegatesScripts =
      filter (registeredDelegate . scriptToCred . snd) scripts

    (StakePools registeredPools) = _stPools (_pstate dpState)
    availablePools = Map.keys registeredPools

genGenesisDelegation
  :: KeyPairs
  -> [CoreKeyPair]
  -> DPState
  -> Gen (Maybe (DCert, CertCred))
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
      ( DCertGenesis (GenesisDelegate (hashVKey gkey, (hashVKey . snd) key))
      , CoreKeyCred [gkey])

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
  -> Gen (Maybe (DCert, CertCred))
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
genDCertRegPool :: KeyPairs -> VrfKeyPairs -> Gen (DCert, CertCred)
genDCertRegPool skeys vrfKeys = do
  (pps, poolKey) <- genStakePool skeys vrfKeys
  pure (DCertPool (RegPool pps), KeyCred poolKey)

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
  -> SlotNo
  -> Gen (Maybe (DCert, CertCred))
genRetirePool availableKeys pp pState slot =
  if (null availableKeys || null poolHashKeys)
     then pure Nothing
     else (\keyHash epoch ->
              Just ( DCertPool (RetirePool keyHash epoch)
                   , KeyCred $ findKeyPair keyHash))
                <$> QC.elements poolHashKeys
                <*> (EpochNo <$> genWord64 epochLow epochHigh)
 where
  stakePools = pState ^. (stPools . to (\(StakePools x) -> x))
  poolHashKeys = Set.toList (Map.keysSet stakePools)
  findKeyPair keyHash =
    case find (\x -> hashKey (vKey (snd x)) == keyHash) availableKeys of
      Nothing ->
        error "genRetirePool: impossible: keyHash doesn't match availableKeys"
      Just ks -> snd ks
  EpochNo cepoch = epochFromSlotNo slot
  EpochNo maxEpoch = pp ^. eMax
  epochLow = cepoch + 1
  epochHigh = cepoch + maxEpoch - 1

-- | Generate an InstantaneousRewards Transfer certificate
genInstantaneousRewards
  :: [CoreKeyPair]
  -> PParams
  -> DState
  -> Gen (Maybe (DCert, CertCred))
genInstantaneousRewards coreKeys pparams delegSt = do
  let StakeCreds credentials = _stkCreds delegSt

  winnerCreds <- take <$> QC.elements [0 .. (max 0 $ (Map.size credentials) - 1)]
                      <*> QC.shuffle (Map.keys credentials)
  coins <- genCoinList 1 1000 (length winnerCreds) (length winnerCreds)

  coreSigners <- take <$> QC.elements [5 .. (max 0 $ (length coreKeys) - 1)]
                      <*> QC.shuffle coreKeys

  let credCoinMap = Map.fromList $ zip winnerCreds coins
  pure $ if (-- Discard this generator (by returning Nothing) if:
             -- we are in full decentralisation mode (d=0) when IR certs are not allowed
             pparams ^. d == interval0
             -- or when we don't have keys available for generating an IR cert
             || null credCoinMap)
    then
      Nothing
    else
      Just ( DCertMir (MIRCert credCoinMap)
           , CoreKeyCred coreSigners)
