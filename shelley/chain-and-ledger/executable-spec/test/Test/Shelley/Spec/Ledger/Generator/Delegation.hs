{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Shelley.Spec.Ledger.Generator.Delegation
  ( genDCert,
    CertCred (..),
  )
where

import Byron.Spec.Ledger.Core (dom, (∈), (∉))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, findWithDefault, fromList, keys, lookup, size)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set ((\\))
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (mkRwdAcnt, scriptToCred)
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    interval0,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (pattern KeyHashObj)
import Shelley.Spec.Ledger.Delegation.Certificates
  ( pattern DCertMir,
    pattern DeRegKey,
    pattern Delegate,
    pattern GenesisDelegCert,
    pattern MIRCert,
    pattern RegKey,
    pattern RegPool,
    pattern RetirePool,
    pattern StakeCreds,
  )
import Shelley.Spec.Ledger.Keys (GenDelegs (..), KeyRole (..), coerceKeyRole, hashKey, vKey)
import Shelley.Spec.Ledger.LedgerState
  ( _dstate,
    _genDelegs,
    _pParams,
    _pstate,
    _retiring,
    _rewards,
    _stPools,
    _stkCreds,
  )
import Shelley.Spec.Ledger.PParams (PParams, _d)
import Shelley.Spec.Ledger.Slot (EpochNo (EpochNo), SlotNo)
import Shelley.Spec.Ledger.TxData
  ( MIRPot (..),
    RewardAcnt (..),
    _poolPubKey,
    _poolVrf,
    unStakePools,
    pattern DCertDeleg,
    pattern DCertGenesis,
    pattern DCertPool,
    pattern Delegation,
    pattern PoolParams,
    pattern StakePools,
  )
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( CoreKeyPair,
    DCert,
    DPState,
    DState,
    KeyHash,
    KeyPair,
    KeyPairs,
    MultiSig,
    MultiSigPairs,
    PState,
    PoolParams,
    VKey,
    VrfKeyPairs,
    hashKeyVRF,
  )
import Test.Shelley.Spec.Ledger.Examples (unsafeMkUnitInterval)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( genCoinList,
    genInteger,
    genWord64,
    toCred,
    tooLateInEpoch,
  )
import Test.Shelley.Spec.Ledger.Utils

data CertCred
  = CoreKeyCred [CoreKeyPair]
  | KeyCred (KeyPair 'Staking)
  | ScriptCred (MultiSig, MultiSig)
  | NoCred
  deriving (Show)

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
genDCert ::
  HasCallStack =>
  Constants ->
  KeyPairs ->
  MultiSigPairs ->
  [CoreKeyPair] ->
  VrfKeyPairs ->
  Map (KeyHash 'Staking) (KeyPair 'Staking) -> -- indexed keys By hash
  PParams ->
  DPState ->
  SlotNo ->
  Gen (Maybe (DCert, CertCred))
genDCert
  c@( Constants
        { frequencyRegKeyCert,
          frequencyRegPoolCert,
          frequencyDelegationCert,
          frequencyGenesisDelegationCert,
          frequencyDeRegKeyCert,
          frequencyRetirePoolCert,
          frequencyMIRCert
        }
      )
  keys
  scripts
  coreKeys
  vrfKeys
  keysByHash
  pparams
  dpState
  slot =
    QC.frequency
      [ (frequencyRegKeyCert, genRegKeyCert c keys scripts dState),
        (frequencyRegPoolCert, genRegPool keys vrfKeys dpState),
        (frequencyDelegationCert, genDelegation c keys scripts dpState),
        (frequencyGenesisDelegationCert, genGenesisDelegation keys coreKeys dpState),
        (frequencyDeRegKeyCert, genDeRegKeyCert c keys scripts dState),
        (frequencyRetirePoolCert, genRetirePool c keysByHash pState slot),
        (frequencyMIRCert, genInstantaneousRewards slot coreKeys pparams dState)
      ]
    where
      dState = _dstate dpState
      pState = _pstate dpState

-- | Generate a RegKey certificate along and also returns the staking credential
-- (needed to witness the certificate)
genRegKeyCert ::
  HasCallStack =>
  Constants ->
  KeyPairs ->
  MultiSigPairs ->
  DState ->
  Gen (Maybe (DCert, CertCred))
genRegKeyCert
  Constants {frequencyKeyCredReg, frequencyScriptCredReg}
  keys
  scripts
  delegSt =
    QC.frequency
      [ ( frequencyKeyCredReg,
          case availableKeys of
            [] -> pure Nothing
            _ -> do
              (_payKey, stakeKey) <- QC.elements availableKeys
              pure $
                Just
                  ( DCertDeleg (RegKey (toCred stakeKey)),
                    NoCred
                  )
        ),
        ( frequencyScriptCredReg,
          case availableScripts of
            [] -> pure Nothing
            _ -> do
              (_, stakeScript) <- QC.elements availableScripts
              pure $
                Just
                  ( DCertDeleg (RegKey (scriptToCred stakeScript)),
                    NoCred
                  )
        )
      ]
    where
      notRegistered k = k ∉ dom (_stkCreds delegSt)
      availableKeys = filter (notRegistered . toCred . snd) keys
      availableScripts = filter (notRegistered . scriptToCred . snd) scripts

-- | Generate a DeRegKey certificate along with the staking credential, which is
-- needed to witness the certificate.
genDeRegKeyCert ::
  HasCallStack =>
  Constants ->
  KeyPairs ->
  MultiSigPairs ->
  DState ->
  Gen (Maybe (DCert, CertCred))
genDeRegKeyCert Constants {frequencyKeyCredDeReg, frequencyScriptCredDeReg} keys scripts dState =
  QC.frequency
    [ ( frequencyKeyCredDeReg,
        case availableKeys of
          [] -> pure Nothing
          _ -> do
            (_payKey, stakeKey) <- QC.elements availableKeys
            pure $ Just (DCertDeleg (DeRegKey (toCred stakeKey)), KeyCred stakeKey)
      ),
      ( frequencyScriptCredDeReg,
        case availableScripts of
          [] -> pure Nothing
          _ -> do
            scriptPair@(_, stakeScript) <- QC.elements availableScripts
            pure $
              Just
                ( DCertDeleg (DeRegKey (scriptToCred stakeScript)),
                  ScriptCred scriptPair
                )
      )
    ]
  where
    registered k = k ∈ dom (_stkCreds dState)
    availableKeys =
      filter
        ( \(_, k) ->
            let cred = toCred k
             in ((&&) <$> registered <*> zeroRewards) cred
        )
        keys
    availableScripts =
      filter
        ( \(_, s) ->
            let cred = scriptToCred s
             in ((&&) <$> registered <*> zeroRewards) cred
        )
        scripts
    zeroRewards k =
      (Coin 0) == (Map.findWithDefault (Coin 1) (mkRwdAcnt Testnet k) (_rewards dState))

-- | Generate a new delegation certificate by picking a registered staking
-- credential and pool. The delegation is witnessed by the delegator's
-- credential which we return along with the certificate.
--
-- Returns nothing if there are no registered staking credentials or no
-- registered pools.
genDelegation ::
  HasCallStack =>
  Constants ->
  KeyPairs ->
  MultiSigPairs ->
  DPState ->
  Gen (Maybe (DCert, CertCred))
genDelegation
  Constants {frequencyKeyCredDelegation, frequencyScriptCredDelegation}
  keys
  scripts
  dpState =
    if null availablePools
      then pure Nothing
      else
        QC.frequency
          [ ( frequencyKeyCredDelegation,
              if null availableDelegates
                then pure Nothing
                else
                  mkCert <$> QC.elements availableDelegates
                    <*> QC.elements availablePools
            ),
            ( frequencyScriptCredDelegation,
              if null availableDelegatesScripts
                then pure Nothing
                else
                  mkCertFromScript <$> QC.elements availableDelegatesScripts
                    <*> QC.elements availablePools
            )
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
      availablePools = Set.toList $ dom registeredPools

genGenesisDelegation ::
  HasCallStack =>
  KeyPairs ->
  [CoreKeyPair] ->
  DPState ->
  Gen (Maybe (DCert, CertCred))
genGenesisDelegation keys coreKeys dpState =
  if null genesisDelegators || null availableDelegatees
    then pure Nothing
    else do
      gk <- QC.elements genesisDelegators
      d <- QC.elements availableDelegatees
      case Map.lookup (hashVKey gk) genDelegs_ of
        -- TODO instead of always using the existing VRF key hash,
        -- sometimes generate new VRF keys
        Nothing -> pure Nothing
        Just (_, vrf) -> return $ mkCert gk d vrf
  where
    hashVKey = hashKey . vKey
    mkCert gkey key vrf =
      Just
        ( DCertGenesis
            ( GenesisDelegCert
                (hashVKey gkey)
                (hashVKey (coerceKeyRole $ snd key))
                vrf
            ),
          CoreKeyCred [gkey]
        )
    (GenDelegs genDelegs_) = _genDelegs $ _dstate dpState
    genesisDelegator k = k ∈ dom genDelegs_
    genesisDelegators = filter (genesisDelegator . hashVKey) coreKeys
    notDelegatee k = (coerceKeyRole k) ∉ (fmap fst (Map.elems genDelegs_))
    availableDelegatees = filter (notDelegatee . hashVKey . snd) keys

-- | Generate and return a RegPool certificate along with its witnessing key.
genRegPool ::
  HasCallStack =>
  KeyPairs ->
  VrfKeyPairs ->
  DPState ->
  Gen (Maybe (DCert, CertCred))
genRegPool keys vrfKeys dpState =
  if null availableKeys || null availableVrfKeys
    then pure Nothing
    else Just <$> genDCertRegPool availableKeys' availableVrfKeys
  where
    notRegistered k = k `notElem` ((_poolPubKey <$>) . Map.elems . _pParams . _pstate) dpState
    keys' = fmap (\(p, s) -> (p, coerceKeyRole s)) keys
    availableKeys = filter (notRegistered . hashKey . vKey . snd) keys'
    availableKeys' = fmap (\(p, s) -> (p, coerceKeyRole s)) availableKeys
    notRegisteredVrf k = k `notElem` ((_poolVrf <$>) . Map.elems . _pParams . _pstate) dpState
    availableVrfKeys = filter (notRegisteredVrf . hashKeyVRF . snd) vrfKeys

-- | Generate PoolParams and the key witness.
genStakePool :: HasCallStack => KeyPairs -> VrfKeyPairs -> Gen (PoolParams, KeyPair 'Witness)
genStakePool skeys vrfKeys =
  mkPoolParams
    <$> (QC.elements skeys')
    <*> (snd <$> QC.elements vrfKeys)
    <*> ( Coin
            <$> QC.frequency
              [ (1, genInteger 1 100),
                (5, pure 0) -- pledge
              ]
        )
    <*> (Coin <$> genInteger 1 100) -- cost
    <*> (fromInteger <$> QC.choose (0, 100) :: Gen Natural)
    <*> (getAnyStakeKey skeys)
  where
    skeys' = fmap (\(p, s) -> (p, coerceKeyRole s)) skeys
    getAnyStakeKey :: KeyPairs -> Gen (VKey 'Staking)
    getAnyStakeKey keys = vKey . snd <$> QC.elements keys
    mkPoolParams poolKeyPair vrfKey pledge cost marginPercent acntKey =
      let interval = unsafeMkUnitInterval $ fromIntegral marginPercent % 100
          pps =
            PoolParams
              (hashKey . vKey . snd $ poolKeyPair)
              (hashKeyVRF vrfKey)
              pledge
              cost
              interval
              (RewardAcnt Testnet $ KeyHashObj $ hashKey acntKey)
              Set.empty
              StrictSeq.empty
              SNothing
       in (pps, coerceKeyRole $ snd poolKeyPair)

-- | Generate `RegPool` and the key witness.
genDCertRegPool :: HasCallStack => KeyPairs -> VrfKeyPairs -> Gen (DCert, CertCred)
genDCertRegPool skeys vrfKeys = do
  (pps, poolKey) <- genStakePool skeys vrfKeys
  pure (DCertPool (RegPool pps), KeyCred . coerceKeyRole $ poolKey)

-- | Generate a RetirePool along with the keypair which registered it.
--
-- Choose a `KeyHash` to retire, by pulling from the set of registered
-- `KeyHash`s in the `stakePools` mapping. Generate a random epoch within an
-- acceptable range of the current epoch. In addition to the `RetirePool`
-- constructed value, return the keypair which corresponds to the selected
-- `KeyHash`, by doing a lookup in the set of `availableKeys`.
genRetirePool ::
  HasCallStack =>
  Constants ->
  Map (KeyHash 'Staking) (KeyPair 'Staking) -> -- indexed keys By Hash
  PState ->
  SlotNo ->
  Gen (Maybe (DCert, CertCred))
genRetirePool Constants {frequencyLowMaxEpoch} keysByHash pState slot =
  if (null retireable)
    then pure Nothing
    else
      ( \keyHash epoch ->
          Just
            ( DCertPool (RetirePool keyHash epoch),
              KeyCred (lookupHash keyHash)
            )
      )
        <$> QC.elements retireable
        <*> (EpochNo <$> genWord64 epochLow epochHigh)
  where
    stakePools = (unStakePools . _stPools) pState
    registered_ = dom stakePools
    retiring_ = dom (_retiring pState)
    retireable = Set.toList (registered_ \\ retiring_)
    lookupHash hk =
      fromMaybe
        (error "genRetirePool: could not find keyHash")
        (Map.lookup (coerceKeyRole hk) keysByHash)
    EpochNo cepoch = epochFromSlotNo slot
    epochLow = cepoch + 1
    -- we use the lower bound of MaxEpoch as the high mark so that all possible
    -- future updates of the protocol parameter, MaxEpoch, will not decrease
    -- the cut-off for pool-retirement and render this RetirePool
    -- "too late in the epoch" when it is retired
    epochHigh = cepoch + frequencyLowMaxEpoch - 1

-- | Generate an InstantaneousRewards Transfer certificate
genInstantaneousRewards ::
  HasCallStack =>
  SlotNo ->
  [CoreKeyPair] ->
  PParams ->
  DState ->
  Gen (Maybe (DCert, CertCred))
genInstantaneousRewards s coreKeys pparams delegSt = do
  let StakeCreds credentials = _stkCreds delegSt

  winnerCreds <-
    take <$> QC.elements [0 .. (max 0 $ (Map.size credentials) - 1)]
      <*> QC.shuffle (Map.keys credentials)
  coins <- genCoinList 1 1000 (length winnerCreds) (length winnerCreds)
  pot <- QC.elements [ReservesMIR, TreasuryMIR]

  coreSigners <-
    take <$> QC.elements [5 .. (max 0 $ (length coreKeys) - 1)]
      <*> QC.shuffle coreKeys

  let credCoinMap = Map.fromList $ zip winnerCreds coins

  pure $
    if ( -- Discard this generator (by returning Nothing) if:
         -- we are in full decentralisation mode (d=0) when IR certs are not allowed
         _d pparams == interval0
           -- or when we don't have keys available for generating an IR cert
           || null credCoinMap
           -- or it's too late in the epoch for IR certs
           || tooLateInEpoch s
       )
      then Nothing
      else
        Just
          ( DCertMir (MIRCert pot credCoinMap),
            CoreKeyCred coreSigners
          )
