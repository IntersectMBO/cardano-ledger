{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Shelley.Generator.Delegation (
  genDCert,
  CertCred (..),
)
where

import Cardano.Ledger.Address (mkRwdAcnt)
import Cardano.Ledger.Coin (DeltaCoin (..), toDeltaCoin)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (
  coerceKeyRole,
  hashKey,
  hashVerKeyVRF,
 )
import Cardano.Ledger.Shelley.API (
  AccountState (..),
  Coin (..),
  ConstitutionalDelegCert (..),
  Credential (..),
  DCert (..),
  DPState (..),
  DState (..),
  DelegCert (..),
  Delegation (..),
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash,
  KeyRole (..),
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  Network (..),
  PState (..),
  PoolCert (..),
  PoolParams (..),
  RewardAcnt (..),
  StrictMaybe (..),
  VKey,
 )
import Cardano.Ledger.Shelley.Core
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState (availableAfterMIR, rewards)
import Cardano.Ledger.Slot (EpochNo (EpochNo), SlotNo)
import qualified Cardano.Ledger.UMapCompact as UM
import Control.Monad (replicateM)
import Control.SetAlgebra (dom, domain, eval, (∈))
import Data.Foldable (fold)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, fromList, lookup)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set ((\\))
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair, KeyPairs, vKey)
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  KeySpace (..),
  genInteger,
  genWord64,
  mkCred,
  tooLateInEpoch,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import Test.Cardano.Ledger.Shelley.Utils
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

-- ======================================================

data CertCred era
  = CoreKeyCred [GenesisKeyPair (EraCrypto era)]
  | StakeCred (KeyPair 'Staking (EraCrypto era))
  | PoolCred (KeyPair 'StakePool (EraCrypto era))
  | ScriptCred (Script era, Script era)
  | DelegateCred [KeyPair 'GenesisDelegate (EraCrypto era)]
  | NoCred

deriving instance (Era era, Show (Script era)) => Show (CertCred era)

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
  forall era.
  EraGen era =>
  Constants ->
  KeySpace era ->
  PParams era ->
  AccountState ->
  DPState (EraCrypto era) ->
  SlotNo ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
genDCert
  c@( Constants
        { frequencyRegKeyCert
        , frequencyRegPoolCert
        , frequencyDelegationCert
        , frequencyGenesisDelegationCert
        , frequencyDeRegKeyCert
        , frequencyRetirePoolCert
        , frequencyMIRCert
        }
      )
  KeySpace_
    { ksCoreNodes
    , ksKeyPairs
    , ksMSigScripts
    , ksStakePools
    , ksGenesisDelegates
    , ksIndexedGenDelegates
    }
  pparams
  accountState
  dpState
  slot =
    QC.frequency
      [ (frequencyRegKeyCert, genRegKeyCert c ksKeyPairs ksMSigScripts dState)
      , (frequencyRegPoolCert, genRegPool ksStakePools ksKeyPairs (pparams ^. ppMinPoolCostL))
      , (frequencyDelegationCert, genDelegation c ksKeyPairs ksMSigScripts dpState)
      ,
        ( frequencyGenesisDelegationCert
        , genGenesisDelegation ksCoreNodes ksGenesisDelegates dpState
        )
      , (frequencyDeRegKeyCert, genDeRegKeyCert c ksKeyPairs ksMSigScripts dState)
      , (frequencyRetirePoolCert, genRetirePool pparams ksStakePools pState slot)
      ,
        ( frequencyMIRCert
        , genInstantaneousRewards
            slot
            ksIndexedGenDelegates
            pparams
            accountState
            dState
        )
      ]
    where
      dState = dpsDState dpState
      pState = dpsPState dpState

-- | Generate a RegKey certificate
genRegKeyCert ::
  forall era.
  EraScript era =>
  Constants ->
  KeyPairs (EraCrypto era) ->
  [(Script era, Script era)] ->
  DState (EraCrypto era) ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
genRegKeyCert
  Constants {frequencyKeyCredReg, frequencyScriptCredReg}
  keys
  scripts
  delegSt =
    QC.frequency
      [
        ( frequencyKeyCredReg
        , case availableKeys of
            [] -> pure Nothing
            _ -> do
              (_payKey, stakeKey) <- QC.elements availableKeys
              pure $
                Just
                  ( DCertDeleg (RegKey (mkCred stakeKey))
                  , NoCred
                  )
        )
      ,
        ( frequencyScriptCredReg
        , case availableScripts of
            [] -> pure Nothing
            _ -> do
              (_, stakeScript) <- QC.elements availableScripts
              pure $
                Just
                  ( DCertDeleg (RegKey (scriptToCred' stakeScript))
                  , NoCred
                  )
        )
      ]
    where
      scriptToCred' = ScriptHashObj . hashScript @era
      notRegistered k = UM.notMember k (rewards delegSt)
      availableKeys = filter (notRegistered . mkCred . snd) keys
      availableScripts = filter (notRegistered . scriptToCred' . snd) scripts

-- | Generate a DeRegKey certificate along with the staking credential, which is
-- needed to witness the certificate.
genDeRegKeyCert ::
  forall era.
  EraScript era =>
  Constants ->
  KeyPairs (EraCrypto era) ->
  [(Script era, Script era)] ->
  DState (EraCrypto era) ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
genDeRegKeyCert Constants {frequencyKeyCredDeReg, frequencyScriptCredDeReg} keys scripts dState =
  QC.frequency
    [
      ( frequencyKeyCredDeReg
      , case availableKeys of
          [] -> pure Nothing
          _ -> do
            (_payKey, stakeKey) <- QC.elements availableKeys
            pure $ Just (DCertDeleg (DeRegKey (mkCred stakeKey)), StakeCred stakeKey)
      )
    ,
      ( frequencyScriptCredDeReg
      , case availableScripts of
          [] -> pure Nothing
          _ -> do
            scriptPair@(_, stakeScript) <- QC.elements availableScripts
            pure $
              Just
                ( DCertDeleg (DeRegKey (scriptToCred' stakeScript))
                , ScriptCred scriptPair
                )
      )
    ]
  where
    scriptToCred' = ScriptHashObj . hashScript @era
    registered k = UM.member k (rewards dState)
    availableKeys =
      filter
        ( \(_, k) ->
            let cred = mkCred k
             in ((&&) <$> registered <*> zeroRewards) cred
        )
        keys
    availableScripts =
      filter
        ( \(_, s) ->
            let cred = scriptToCred' s
             in ((&&) <$> registered <*> zeroRewards) cred
        )
        scripts
    zeroRewards k = case UM.lookup (getRwdCred $ mkRwdAcnt Testnet k) (rewards dState) of
      Nothing -> False
      Just (UM.RDPair ccoin _) -> ccoin == UM.CompactCoin 0

-- | Generate a new delegation certificate by picking a registered staking
-- credential and pool. The delegation is witnessed by the delegator's
-- credential which we return along with the certificate.
--
-- Returns nothing if there are no registered staking credentials or no
-- registered pools.
genDelegation ::
  forall era.
  EraScript era =>
  Constants ->
  KeyPairs (EraCrypto era) ->
  [(Script era, Script era)] ->
  DPState (EraCrypto era) ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
genDelegation
  Constants {frequencyKeyCredDelegation, frequencyScriptCredDelegation}
  keys
  scripts
  dpState =
    if null availablePools
      then pure Nothing
      else
        QC.frequency
          [
            ( frequencyKeyCredDelegation
            , if null availableDelegates
                then pure Nothing
                else
                  mkCert
                    <$> QC.elements availableDelegates
                    <*> QC.elements availablePools
            )
          ,
            ( frequencyScriptCredDelegation
            , if null availableDelegatesScripts
                then pure Nothing
                else
                  mkCertFromScript
                    <$> QC.elements availableDelegatesScripts
                    <*> QC.elements availablePools
            )
          ]
    where
      scriptToCred' = ScriptHashObj . hashScript @era
      mkCert (_, delegatorKey) poolKey = Just (cert, StakeCred delegatorKey)
        where
          cert = DCertDeleg (Delegate (Delegation (mkCred delegatorKey) poolKey))
      mkCertFromScript (s, delegatorScript) poolKey =
        Just (scriptCert, ScriptCred (s, delegatorScript))
        where
          scriptCert =
            DCertDeleg (Delegate (Delegation (scriptToCred' delegatorScript) poolKey))
      registeredDelegate k = UM.member k (rewards (dpsDState dpState))
      availableDelegates = filter (registeredDelegate . mkCred . snd) keys
      availableDelegatesScripts =
        filter (registeredDelegate . scriptToCred' . snd) scripts
      registeredPools = psStakePoolParams (dpsPState dpState)
      availablePools = Set.toList $ domain registeredPools

genGenesisDelegation ::
  (Era era) =>
  -- | Core nodes
  [(GenesisKeyPair (EraCrypto era), AllIssuerKeys (EraCrypto era) 'GenesisDelegate)] ->
  -- | All potential genesis delegate keys
  [AllIssuerKeys (EraCrypto era) 'GenesisDelegate] ->
  DPState (EraCrypto era) ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
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
            ( ConstitutionalDelegCert
                (hashVKey gkey)
                (hashVKey key)
                (hashVerKeyVRF vrf)
            )
        , CoreKeyCred [gkey]
        )
    (GenDelegs genDelegs_) = dsGenDelegs $ dpsDState dpState
    genesisDelegator k = eval (k ∈ dom genDelegs_)
    genesisDelegators = filter (genesisDelegator . hashVKey) (fst <$> coreNodes)
    notActiveDelegatee k = not (coerceKeyRole k `List.elem` fmap genDelegKeyHash (Map.elems genDelegs_))
    fGenDelegs = dsFutureGenDelegs $ dpsDState dpState
    notFutureDelegatee k = not (coerceKeyRole k `List.elem` fmap genDelegKeyHash (Map.elems fGenDelegs))
    notDelegatee k = notActiveDelegatee k && notFutureDelegatee k
    availableDelegatees = filter (notDelegatee . hashVKey . cold) allDelegateKeys

-- | Generate PoolParams and the key witness.
genStakePool ::
  forall c.
  Crypto c =>
  -- | Available keys for stake pool registration
  [AllIssuerKeys c 'StakePool] ->
  -- | KeyPairs containing staking keys to act as owners/reward account
  KeyPairs c ->
  -- | Minimum pool cost Protocol Param
  Coin ->
  Gen (PoolParams c, KeyPair 'StakePool c)
genStakePool poolKeys skeys (Coin minPoolCost) =
  mkPoolParams
    <$> QC.elements poolKeys
    <*> ( Coin -- pledge
            <$> QC.frequency
              [ (1, genInteger 1 100)
              , (5, pure 0)
              ]
        )
    <*> (Coin <$> genInteger minPoolCost (minPoolCost + 50)) -- cost
    <*> (fromInteger <$> QC.choose (0, 100) :: Gen Natural)
    <*> getAnyStakeKey skeys
  where
    getAnyStakeKey :: KeyPairs c -> Gen (VKey 'Staking c)
    getAnyStakeKey keys = vKey . snd <$> QC.elements keys
    mkPoolParams allPoolKeys pledge cost marginPercent acntKey =
      let interval = unsafeBoundRational $ fromIntegral marginPercent % 100
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
  (Era era) =>
  [AllIssuerKeys (EraCrypto era) 'StakePool] ->
  KeyPairs (EraCrypto era) ->
  Coin ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
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
  EraPParams era =>
  PParams era ->
  [AllIssuerKeys (EraCrypto era) 'StakePool] ->
  PState (EraCrypto era) ->
  SlotNo ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
genRetirePool _pp poolKeys pState slot =
  if (null retireable)
    then pure Nothing
    else
      ( \keyHash epoch ->
          Just
            ( DCertPool (RetirePool keyHash epoch)
            , PoolCred (cold $ lookupHash keyHash)
            )
      )
        <$> QC.elements retireable
        <*> (EpochNo <$> genWord64 epochLow epochHigh)
  where
    stakePools = psStakePoolParams pState
    registered_ = eval (dom stakePools)
    retiring_ = domain (psRetiring pState)
    retireable = Set.toList (registered_ \\ retiring_)
    lookupHash hk' =
      fromMaybe
        (error "genRetirePool: could not find keyHash")
        (List.find (\x -> hk x == hk') poolKeys)
    EpochNo cepoch = epochFromSlotNo slot
    epochLow = cepoch + 1
    -- if epochHigh is more than a few epochs above epochLow, then
    -- because our traces are at most, maybe 6 or so traces long,
    -- we will never reap any pools. Choosing a delta between 1 and 10
    -- should give good mix of sometimes reaping, but mostly not.
    epochHigh = cepoch + 10

-- | Generate an InstantaneousRewards Transfer certificate
genInstantaneousRewardsAccounts ::
  EraPParams era =>
  SlotNo ->
  -- | Index over the cold key hashes of all possible Genesis Delegates
  Map (KeyHash 'GenesisDelegate (EraCrypto era)) (AllIssuerKeys (EraCrypto era) 'GenesisDelegate) ->
  PParams era ->
  AccountState ->
  DState (EraCrypto era) ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
genInstantaneousRewardsAccounts s genesisDelegatesByHash pparams accountState delegSt = do
  let (GenDelegs genDelegs_) = dsGenDelegs delegSt
      lookupGenDelegate' gk =
        fromMaybe
          (error "genInstantaneousRewardsAccounts: lookupGenDelegate failed")
          (Map.lookup gk genesisDelegatesByHash)
      credentials = rewards delegSt
  winnerCreds <-
    take
      <$> QC.elements [0 .. (max 0 $ UM.size credentials - 1)]
      <*> QC.shuffle (Set.toList (UM.domain credentials))
  coins <- replicateM (length winnerCreds) $ genInteger 1 1000
  let credCoinMap = Map.fromList $ zip winnerCreds (fmap DeltaCoin coins)

  coreSigners <-
    take
      <$> QC.elements [5 .. (max 0 $ length genDelegs_ - 1)]
      <*> QC.shuffle (lookupGenDelegate' . genDelegKeyHash <$> Map.elems genDelegs_)

  pot <- QC.elements [ReservesMIR, TreasuryMIR]
  let available = availableAfterMIR pot accountState (dsIRewards delegSt)
  let rewardAmount = fold $ Map.elems credCoinMap
      insufficientFunds = toDeltaCoin available < rewardAmount
  pure $
    if -- Discard this generator (by returning Nothing) if:
    -- we are in full decentralisation mode (d=0) when IR certs are not allowed
    pparams ^. ppDG == minBound
      -- or when we don't have keys available for generating an IR cert
      || null credCoinMap
      -- or it's too late in the epoch for IR certs
      || tooLateInEpoch s
      -- or the rewards exceed the pot amount
      || insufficientFunds
      then Nothing
      else
        Just
          ( DCertMir (MIRCert pot (StakeAddressesMIR credCoinMap))
          , DelegateCred (cold <$> coreSigners)
          )

-- | Generate an InstantaneousRewards Transfer
genInstantaneousRewardsTransfer ::
  EraPParams era =>
  SlotNo ->
  -- | Index over the cold key hashes of all possible Genesis Delegates
  Map (KeyHash 'GenesisDelegate (EraCrypto era)) (AllIssuerKeys (EraCrypto era) 'GenesisDelegate) ->
  PParams era ->
  AccountState ->
  DState (EraCrypto era) ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
genInstantaneousRewardsTransfer s genesisDelegatesByHash pparams accountState delegSt = do
  let (GenDelegs genDelegs_) = dsGenDelegs delegSt
      lookupGenDelegate' gk =
        fromMaybe
          (error "genInstantaneousRewardsTransfer: lookupGenDelegate failed")
          (Map.lookup gk genesisDelegatesByHash)

  coreSigners <-
    take
      <$> QC.elements [5 .. (max 0 $ length genDelegs_ - 1)]
      <*> QC.shuffle (lookupGenDelegate' . genDelegKeyHash <$> Map.elems genDelegs_)

  pot <- QC.elements [ReservesMIR, TreasuryMIR]
  let Coin available = availableAfterMIR pot accountState (dsIRewards delegSt)
  amount <- if available > 0 then QC.choose (0, available) else pure 0
  pure $
    if -- Discard this generator (by returning Nothing) if:
    -- we are in full decentralisation mode (d=0) when IR certs are not allowed
    pparams ^. ppDG == minBound
      -- or it's too late in the epoch for IR certs
      || tooLateInEpoch s
      then Nothing
      else
        Just
          ( DCertMir (MIRCert pot (SendToOppositePotMIR $ Coin amount))
          , DelegateCred (cold <$> coreSigners)
          )

genInstantaneousRewards ::
  EraPParams era =>
  SlotNo ->
  -- | Index over the cold key hashes of all possible Genesis Delegates
  Map (KeyHash 'GenesisDelegate (EraCrypto era)) (AllIssuerKeys (EraCrypto era) 'GenesisDelegate) ->
  PParams era ->
  AccountState ->
  DState (EraCrypto era) ->
  Gen (Maybe (DCert (EraCrypto era), CertCred era))
genInstantaneousRewards slot genesisDelegatesByHash pparams accountState delegSt =
  if HardForks.allowMIRTransfer (pparams ^. ppProtocolVersionL)
    then
      QC.oneof
        [ genInstantaneousRewardsAccounts
            slot
            genesisDelegatesByHash
            pparams
            accountState
            delegSt
        , genInstantaneousRewardsTransfer
            slot
            genesisDelegatesByHash
            pparams
            accountState
            delegSt
        ]
    else
      genInstantaneousRewardsAccounts
        slot
        genesisDelegatesByHash
        pparams
        accountState
        delegSt
