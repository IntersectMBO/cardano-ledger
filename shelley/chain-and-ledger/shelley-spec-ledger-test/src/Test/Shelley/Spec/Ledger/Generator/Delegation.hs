{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Shelley.Spec.Ledger.Generator.Delegation
  ( genDCert,
    CertCred (..),
  )
where

import Cardano.Ledger.Era (Era)
import Control.Iterate.SetAlgebra (dom, domain, eval, (∈), (∉))
import Data.Foldable (fold)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, findWithDefault, fromList, keys, lookup, size)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set ((\\))
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.API
  ( AccountState (..),
    Coin (..),
    Credential (..),
    DCert (..),
    DPState (..),
    DState (..),
    DelegCert (..),
    Delegation (..),
    GenDelegPair (..),
    GenDelegs (..),
    GenesisDelegCert (..),
    KeyHash,
    KeyPair,
    KeyPairs,
    KeyRole (..),
    MIRCert (..),
    MIRPot (..),
    MultiSig,
    Network (..),
    PParams,
    PParams' (..),
    PState (..),
    PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    StrictMaybe (..),
    VKey,
  )
import Shelley.Spec.Ledger.Address (mkRwdAcnt, scriptToCred)
import Shelley.Spec.Ledger.BaseTypes (interval0)
import Shelley.Spec.Ledger.Keys
  ( coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
    vKey,
  )
import Shelley.Spec.Ledger.Slot (EpochNo (EpochNo), SlotNo)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    KeySpace (..),
    genCoinList,
    genInteger,
    genWord64,
    toCred,
    tooLateInEpoch,
  )
import Test.Shelley.Spec.Ledger.Utils

data CertCred h
  = CoreKeyCred [GenesisKeyPair h]
  | StakeCred (KeyPair 'Staking h)
  | PoolCred (KeyPair 'StakePool h)
  | ScriptCred (MultiSig h, MultiSig h)
  | DelegateCred [KeyPair 'GenesisDelegate h]
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
  (HasCallStack, Era era) =>
  Constants ->
  KeySpace era ->
  PParams ->
  AccountState ->
  DPState era ->
  SlotNo ->
  Gen (Maybe (DCert era, CertCred era))
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
  KeySpace_
    { ksCoreNodes,
      ksKeyPairs,
      ksMSigScripts,
      ksStakePools,
      ksGenesisDelegates,
      ksIndexedGenDelegates
    }
  pparams
  accountState
  dpState
  slot =
    QC.frequency
      [ (frequencyRegKeyCert, genRegKeyCert c ksKeyPairs ksMSigScripts dState),
        (frequencyRegPoolCert, genRegPool ksStakePools ksKeyPairs (_minPoolCost pparams)),
        (frequencyDelegationCert, genDelegation c ksKeyPairs ksMSigScripts dpState),
        ( frequencyGenesisDelegationCert,
          genGenesisDelegation ksCoreNodes ksGenesisDelegates dpState
        ),
        (frequencyDeRegKeyCert, genDeRegKeyCert c ksKeyPairs ksMSigScripts dState),
        (frequencyRetirePoolCert, genRetirePool c ksStakePools pState slot),
        ( frequencyMIRCert,
          genInstantaneousRewards
            slot
            ksIndexedGenDelegates
            pparams
            accountState
            dState
        )
      ]
    where
      dState = _dstate dpState
      pState = _pstate dpState

-- | Generate a RegKey certificate
genRegKeyCert ::
  (HasCallStack, Era era) =>
  Constants ->
  KeyPairs era ->
  MultiSigPairs era ->
  DState era ->
  Gen (Maybe (DCert era, CertCred era))
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
      notRegistered k = eval (k ∉ dom (_rewards delegSt))
      availableKeys = filter (notRegistered . toCred . snd) keys
      availableScripts = filter (notRegistered . scriptToCred . snd) scripts

-- | Generate a DeRegKey certificate along with the staking credential, which is
-- needed to witness the certificate.
genDeRegKeyCert ::
  (HasCallStack, Era era) =>
  Constants ->
  KeyPairs era ->
  MultiSigPairs era ->
  DState era ->
  Gen (Maybe (DCert era, CertCred era))
genDeRegKeyCert Constants {frequencyKeyCredDeReg, frequencyScriptCredDeReg} keys scripts dState =
  QC.frequency
    [ ( frequencyKeyCredDeReg,
        case availableKeys of
          [] -> pure Nothing
          _ -> do
            (_payKey, stakeKey) <- QC.elements availableKeys
            pure $ Just (DCertDeleg (DeRegKey (toCred stakeKey)), StakeCred stakeKey)
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
    registered k = eval (k ∈ dom (_rewards dState))
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
      (Coin 0) == (Map.findWithDefault (Coin 1) (getRwdCred $ mkRwdAcnt Testnet k) (_rewards dState))

-- | Generate a new delegation certificate by picking a registered staking
-- credential and pool. The delegation is witnessed by the delegator's
-- credential which we return along with the certificate.
--
-- Returns nothing if there are no registered staking credentials or no
-- registered pools.
genDelegation ::
  (HasCallStack, Era era) =>
  Constants ->
  KeyPairs era ->
  MultiSigPairs era ->
  DPState era ->
  Gen (Maybe (DCert era, CertCred era))
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
      mkCert (_, delegatorKey) poolKey = Just (cert, StakeCred delegatorKey)
        where
          cert = DCertDeleg (Delegate (Delegation (toCred delegatorKey) poolKey))
      mkCertFromScript (s, delegatorScript) poolKey =
        Just (scriptCert, ScriptCred (s, delegatorScript))
        where
          scriptCert =
            DCertDeleg (Delegate (Delegation (scriptToCred delegatorScript) poolKey))
      registeredDelegate k = eval (k ∈ dom (_rewards (_dstate dpState)))
      availableDelegates = filter (registeredDelegate . toCred . snd) keys
      availableDelegatesScripts =
        filter (registeredDelegate . scriptToCred . snd) scripts
      registeredPools = _pParams (_pstate dpState)
      availablePools = Set.toList $ domain registeredPools

genGenesisDelegation ::
  (HasCallStack, Era era) =>
  -- | Core nodes
  [(GenesisKeyPair era, AllIssuerKeys era 'GenesisDelegate)] ->
  -- | All potential genesis delegate keys
  [AllIssuerKeys era 'GenesisDelegate] ->
  DPState era ->
  Gen (Maybe (DCert era, CertCred era))
genGenesisDelegation coreNodes delegateKeys dpState =
  if null genesisDelegators || null availableDelegatees
    then pure Nothing
    else do
      gk <- QC.elements genesisDelegators
      AllIssuerKeys {cold, vrf} <- QC.elements availableDelegatees
      case Map.lookup (hashVKey gk) genDelegs_ of
        Nothing -> pure Nothing
        Just _ -> return $ mkCert gk cold (snd vrf)
  where
    allDelegateKeys = (snd <$> coreNodes) <> delegateKeys
    hashVKey = hashKey . vKey
    mkCert gkey key vrf =
      Just
        ( DCertGenesis
            ( GenesisDelegCert
                (hashVKey gkey)
                (hashVKey key)
                (hashVerKeyVRF vrf)
            ),
          CoreKeyCred [gkey]
        )
    (GenDelegs genDelegs_) = _genDelegs $ _dstate dpState
    genesisDelegator k = eval (k ∈ dom genDelegs_)
    genesisDelegators = filter (genesisDelegator . hashVKey) (fst <$> coreNodes)
    notActiveDelegatee k = not (coerceKeyRole k `List.elem` fmap genDelegKeyHash (Map.elems genDelegs_))
    fGenDelegs = _fGenDelegs $ _dstate dpState
    notFutureDelegatee k = not (coerceKeyRole k `List.elem` fmap genDelegKeyHash (Map.elems fGenDelegs))
    notDelegatee k = notActiveDelegatee k && notFutureDelegatee k
    availableDelegatees = filter (notDelegatee . hashVKey . cold) allDelegateKeys

-- | Generate PoolParams and the key witness.
genStakePool ::
  (HasCallStack, Era era) =>
  -- | Available keys for stake pool registration
  [AllIssuerKeys era 'StakePool] ->
  -- | KeyPairs containing staking keys to act as owners/reward account
  KeyPairs era ->
  -- | Minimum pool cost Protocol Param
  Coin ->
  Gen (PoolParams era, KeyPair 'StakePool era)
genStakePool poolKeys skeys (Coin minPoolCost) =
  mkPoolParams
    <$> QC.elements poolKeys
    <*> ( Coin -- pledge
            <$> QC.frequency
              [ (1, genInteger 1 100),
                (5, pure 0)
              ]
        )
    <*> (Coin <$> genInteger minPoolCost (minPoolCost + 50)) -- cost
    <*> (fromInteger <$> QC.choose (0, 100) :: Gen Natural)
    <*> getAnyStakeKey skeys
  where
    getAnyStakeKey :: KeyPairs h -> Gen (VKey 'Staking h)
    getAnyStakeKey keys = vKey . snd <$> QC.elements keys
    mkPoolParams allPoolKeys pledge cost marginPercent acntKey =
      let interval = unsafeMkUnitInterval $ fromIntegral marginPercent % 100
          pps =
            PoolParams
              (hashKey . vKey . cold $ allPoolKeys)
              (hashVerKeyVRF . snd . vrf $ allPoolKeys)
              pledge
              cost
              interval
              (RewardAcnt Testnet $ KeyHashObj $ hashKey acntKey)
              Set.empty
              StrictSeq.empty
              SNothing
       in (pps, cold allPoolKeys)

-- | Generate `RegPool` and the key witness.
genRegPool ::
  (HasCallStack, Era era) =>
  [AllIssuerKeys era 'StakePool] ->
  KeyPairs era ->
  Coin ->
  Gen (Maybe (DCert era, CertCred era))
genRegPool poolKeys keyPairs minPoolCost = do
  (pps, poolKey) <- genStakePool poolKeys keyPairs minPoolCost
  pure $ Just (DCertPool (RegPool pps), PoolCred poolKey)

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
  [AllIssuerKeys h 'StakePool] ->
  PState h ->
  SlotNo ->
  Gen (Maybe (DCert h, CertCred h))
genRetirePool Constants {frequencyLowMaxEpoch} poolKeys pState slot =
  if (null retireable)
    then pure Nothing
    else
      ( \keyHash epoch ->
          Just
            ( DCertPool (RetirePool keyHash epoch),
              PoolCred (cold $ lookupHash keyHash)
            )
      )
        <$> QC.elements retireable
        <*> (EpochNo <$> genWord64 epochLow epochHigh)
  where
    stakePools = _pParams pState
    registered_ = eval (dom stakePools)
    retiring_ = domain (_retiring pState)
    retireable = Set.toList (registered_ \\ retiring_)
    lookupHash hk' =
      fromMaybe
        (error "genRetirePool: could not find keyHash")
        (List.find (\x -> hk x == hk') poolKeys)
    EpochNo cepoch = epochFromSlotNo slot
    epochLow = cepoch + 1
    -- we use the lower bound of MaxEpoch as the high mark so that all possible
    -- future updates of the protocol parameter, MaxEpoch, will not decrease
    -- the cut-off for pool-retirement and render this RetirePool
    -- "too late in the epoch" when it is retired
    epochHigh = cepoch + frequencyLowMaxEpoch

-- | Generate an InstantaneousRewards Transfer certificate
genInstantaneousRewards ::
  (HasCallStack, Era era) =>
  SlotNo ->
  -- | Index over the cold key hashes of all possible Genesis Delegates
  Map (KeyHash 'GenesisDelegate era) (AllIssuerKeys era 'GenesisDelegate) ->
  PParams ->
  AccountState ->
  DState era ->
  Gen (Maybe (DCert era, CertCred era))
genInstantaneousRewards s genesisDelegatesByHash pparams accountState delegSt = do
  let (GenDelegs genDelegs_) = _genDelegs delegSt
      lookupGenDelegate' gk =
        fromMaybe
          (error "genInstantaneousRewards: lookupGenDelegate failed")
          (Map.lookup gk genesisDelegatesByHash)
      credentials = _rewards delegSt

  winnerCreds <-
    take <$> QC.elements [0 .. (max 0 $ Map.size credentials - 1)]
      <*> QC.shuffle (Map.keys credentials)
  coins <- genCoinList 1 1000 (length winnerCreds) (length winnerCreds)
  let credCoinMap = Map.fromList $ zip winnerCreds coins

  coreSigners <-
    take <$> QC.elements [5 .. (max 0 $ length genDelegs_ - 1)]
      <*> QC.shuffle (lookupGenDelegate' . genDelegKeyHash <$> Map.elems genDelegs_)

  pot <- QC.elements [ReservesMIR, TreasuryMIR]
  let rewardAmount = fold $ Map.elems credCoinMap
      potAmount = case pot of
        ReservesMIR -> _reserves accountState
        TreasuryMIR -> _treasury accountState
      insufficientFunds = rewardAmount > potAmount
  pure $
    if -- Discard this generator (by returning Nothing) if:
    -- we are in full decentralisation mode (d=0) when IR certs are not allowed
    _d pparams == interval0
      -- or when we don't have keys available for generating an IR cert
      || null credCoinMap
      -- or it's too late in the epoch for IR certs
      || tooLateInEpoch s
      -- or the rewards exceed the pot amount
      || insufficientFunds
      then Nothing
      else
        Just
          ( DCertMir (MIRCert pot credCoinMap),
            DelegateCred (cold <$> coreSigners)
          )
