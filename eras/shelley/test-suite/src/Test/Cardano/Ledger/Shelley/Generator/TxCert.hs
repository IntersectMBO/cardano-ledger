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

module Test.Cardano.Ledger.Shelley.Generator.TxCert (
  genTxCert,
  CertCred (..),
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Coin (DeltaCoin (..), toDeltaCoin)
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Shelley (hardforkAlonzoAllowMIRTransfer)
import Cardano.Ledger.Shelley.API (
  Coin (..),
  Credential (..),
  GenDelegPair (..),
  GenDelegs (..),
  Network (..),
  PoolParams (..),
  StrictMaybe (..),
  VKey,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (availableAfterMIR)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (EpochNo (EpochNo), SlotNo)
import Cardano.Protocol.Crypto (Crypto, hashVerKeyVRF)
import Control.Monad (replicateM)
import Control.SetAlgebra (dom, domain, eval, (∈))
import Data.Foldable (fold)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set ((\\))
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair, KeyPairs, vKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  KeySpace (..),
  genInteger,
  genWord64,
  mkCredential,
  tooLateInEpoch,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import Test.Cardano.Ledger.Shelley.Utils
import Test.Cardano.Protocol.TPraos.Create (VRFKeyPair (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

-- ======================================================

data CertCred era
  = CoreKeyCred [GenesisKeyPair MockCrypto]
  | StakeCred (KeyPair 'Staking)
  | PoolCred (KeyPair 'StakePool)
  | ScriptCred (Script era, Script era)
  | DelegateCred [KeyPair 'GenesisDelegate]
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
genTxCert ::
  forall era c.
  (EraGen era, ProtVerAtMost era 8, EraCertState era, Crypto c) =>
  Constants ->
  KeySpace c era ->
  PParams era ->
  ChainAccountState ->
  CertState era ->
  SlotNo ->
  Gen (Maybe (TxCert era, CertCred era))
genTxCert
  c@( Constants
        { frequencyRegCert
        , frequencyRegPoolCert
        , frequencyDelegCert
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
      [ (frequencyRegCert, genRegKeyCert c ksKeyPairs ksMSigScripts dState)
      , (frequencyRegPoolCert, genRegPool ksStakePools ksKeyPairs (pparams ^. ppMinPoolCostL))
      , (frequencyDelegCert, genDelegation c ksKeyPairs ksMSigScripts dpState)
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
      dState = dpState ^. certDStateL
      pState = dpState ^. certPStateL

-- | Generate a RegKey certificate
genRegKeyCert ::
  forall era.
  (EraScript era, ShelleyEraTxCert era, EraAccounts era) =>
  Constants ->
  KeyPairs ->
  [(Script era, Script era)] ->
  DState era ->
  Gen (Maybe (TxCert era, CertCred era))
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
                  ( RegTxCert (mkCredential stakeKey)
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
                  ( RegTxCert (scriptToCred' stakeScript)
                  , NoCred
                  )
        )
      ]
    where
      scriptToCred' = ScriptHashObj . hashScript @era
      notRegistered cred = not (isAccountRegistered cred (delegSt ^. accountsL))
      availableKeys = filter (notRegistered . mkCredential . snd) keys
      availableScripts = filter (notRegistered . scriptToCred' . snd) scripts

-- | Generate a DeRegKey certificate along with the staking credential, which is
-- needed to witness the certificate.
genDeRegKeyCert ::
  forall era.
  (EraScript era, EraAccounts era, ShelleyEraTxCert era) =>
  Constants ->
  KeyPairs ->
  [(Script era, Script era)] ->
  DState era ->
  Gen (Maybe (TxCert era, CertCred era))
genDeRegKeyCert Constants {frequencyKeyCredDeReg, frequencyScriptCredDeReg} keys scripts dState =
  QC.frequency
    [
      ( frequencyKeyCredDeReg
      , case availableKeys of
          [] -> pure Nothing
          _ -> do
            (_payKey, stakeKey) <- QC.elements availableKeys
            pure $ Just (UnRegTxCert (mkCredential stakeKey), StakeCred stakeKey)
      )
    ,
      ( frequencyScriptCredDeReg
      , case availableScripts of
          [] -> pure Nothing
          _ -> do
            scriptPair@(_, stakeScript) <- QC.elements availableScripts
            pure $
              Just
                ( UnRegTxCert (scriptToCred' stakeScript)
                , ScriptCred scriptPair
                )
      )
    ]
  where
    scriptToCred' = ScriptHashObj . hashScript @era
    registered cred = isAccountRegistered cred (dState ^. accountsL)
    availableKeys =
      filter
        ( \(_, k) ->
            let cred = mkCredential k
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
    zeroRewards cred =
      case lookupAccountState cred (dState ^. accountsL) of
        Nothing -> False
        Just accountState -> accountState ^. balanceAccountStateL == mempty

-- | Generate a new delegation certificate by picking a registered staking
-- credential and pool. The delegation is witnessed by the delegator's
-- credential which we return along with the certificate.
--
-- Returns nothing if there are no registered staking credentials or no
-- registered pools.
genDelegation ::
  forall era.
  (EraScript era, ShelleyEraTxCert era, EraCertState era) =>
  Constants ->
  KeyPairs ->
  [(Script era, Script era)] ->
  CertState era ->
  Gen (Maybe (TxCert era, CertCred era))
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
          cert = DelegStakeTxCert (mkCredential delegatorKey) poolKey
      mkCertFromScript (s, delegatorScript) poolKey =
        Just (scriptCert, ScriptCred (s, delegatorScript))
        where
          scriptCert =
            DelegStakeTxCert (scriptToCred' delegatorScript) poolKey
      registeredDelegate cred = isAccountRegistered cred (dpState ^. certDStateL . accountsL)
      availableDelegates = filter (registeredDelegate . mkCredential . snd) keys
      availableDelegatesScripts =
        filter (registeredDelegate . scriptToCred' . snd) scripts
      registeredPools = psStakePoolParams (dpState ^. certPStateL)
      availablePools = Set.toList $ domain registeredPools

genGenesisDelegation ::
  forall era c.
  (Era era, ShelleyEraTxCert era, ProtVerAtMost era 8, EraCertState era, Crypto c) =>
  -- | Core nodes
  [(GenesisKeyPair c, AllIssuerKeys c 'GenesisDelegate)] ->
  -- | All potential genesis delegate keys
  [AllIssuerKeys c 'GenesisDelegate] ->
  CertState era ->
  Gen (Maybe (TxCert era, CertCred era))
genGenesisDelegation coreNodes delegateKeys dpState =
  if null genesisDelegators || null availableDelegatees
    then pure Nothing
    else do
      gk <- QC.elements genesisDelegators
      AllIssuerKeys {aikCold, aikVrf} <- QC.elements availableDelegatees
      case Map.lookup (hashVKey gk) genDelegs_ of
        Nothing -> pure Nothing
        Just _ -> return $ mkCert gk aikCold (vrfVerKey aikVrf)
  where
    allDelegateKeys = (snd <$> coreNodes) <> delegateKeys
    hashVKey = hashKey . vKey
    mkCert gkey key vrf =
      Just
        ( GenesisDelegTxCert
            (hashVKey gkey)
            (hashVKey key)
            (hashVerKeyVRF @c vrf)
        , CoreKeyCred [gkey]
        )
    GenDelegs genDelegs_ = dpState ^. certDStateL . dsGenDelegsL
    genesisDelegator k = eval (k ∈ dom genDelegs_)
    genesisDelegators = filter (genesisDelegator . hashVKey) (fst <$> coreNodes)
    activeGenDelegsKeyHashSet =
      Set.fromList $ genDelegKeyHash <$> Map.elems genDelegs_
    futureGenDelegsKeyHashSet =
      Set.fromList $ genDelegKeyHash <$> Map.elems (dpState ^. certDStateL . dsFutureGenDelegsL)
    notActiveDelegatee k = coerceKeyRole k `Set.notMember` activeGenDelegsKeyHashSet
    notFutureDelegatee k = coerceKeyRole k `Set.notMember` futureGenDelegsKeyHashSet
    notDelegatee k = notActiveDelegatee k && notFutureDelegatee k
    availableDelegatees = filter (notDelegatee . hashVKey . aikCold) allDelegateKeys

-- | Generate PoolParams and the key witness.
genStakePool ::
  forall c.
  Crypto c =>
  -- | Available keys for stake pool registration
  [AllIssuerKeys c 'StakePool] ->
  -- | KeyPairs containing staking keys to act as owners/reward account
  KeyPairs ->
  -- | Minimum pool cost Protocol Param
  Coin ->
  Gen (PoolParams, KeyPair 'StakePool)
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
    getAnyStakeKey :: KeyPairs -> Gen (VKey 'Staking)
    getAnyStakeKey keys = vKey . snd <$> QC.elements keys
    mkPoolParams ::
      AllIssuerKeys c 'StakePool ->
      Coin ->
      Coin ->
      Natural ->
      VKey 'Staking ->
      (PoolParams, KeyPair 'StakePool)
    mkPoolParams allPoolKeys pledge cost marginPercent acntKey =
      let interval = unsafeBoundRational $ fromIntegral marginPercent % 100
          pps =
            PoolParams
              (hashKey . vKey $ aikCold allPoolKeys)
              (hashVerKeyVRF @c . vrfVerKey $ aikVrf allPoolKeys)
              pledge
              cost
              interval
              (RewardAccount Testnet $ KeyHashObj $ hashKey acntKey)
              Set.empty
              StrictSeq.empty
              SNothing
       in (pps, aikCold allPoolKeys)

-- | Generate `RegPool` and the key witness.
genRegPool ::
  (Era era, EraTxCert era, Crypto c) =>
  [AllIssuerKeys c 'StakePool] ->
  KeyPairs ->
  Coin ->
  Gen (Maybe (TxCert era, CertCred era))
genRegPool poolKeys keyPairs minPoolCost = do
  (pps, poolKey) <- genStakePool poolKeys keyPairs minPoolCost
  pure $ Just (RegPoolTxCert pps, PoolCred poolKey)

-- | Generate a RetirePool along with the keypair which registered it.
--
-- Choose a `KeyHash` to retire, by pulling from the set of registered
-- `KeyHash`s in the `stakePools` mapping. Generate a random epoch within an
-- acceptable range of the current epoch. In addition to the `RetirePool`
-- constructed value, return the keypair which corresponds to the selected
-- `KeyHash`, by doing a lookup in the set of `availableKeys`.
genRetirePool ::
  (EraPParams era, EraTxCert era) =>
  PParams era ->
  [AllIssuerKeys c 'StakePool] ->
  PState era ->
  SlotNo ->
  Gen (Maybe (TxCert era, CertCred era))
genRetirePool _pp poolKeys pState slot =
  if null retireable
    then pure Nothing
    else
      ( \keyHash epoch ->
          Just
            ( RetirePoolTxCert keyHash epoch
            , PoolCred (aikCold $ lookupHash keyHash)
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
        (List.find (\x -> aikColdKeyHash x == hk') poolKeys)
    EpochNo cepoch = epochFromSlotNo slot
    epochLow = cepoch + 1
    -- if epochHigh is more than a few epochs above epochLow, then
    -- because our traces are at most, maybe 6 or so traces long,
    -- we will never reap any pools. Choosing a delta between 1 and 10
    -- should give good mix of sometimes reaping, but mostly not.
    epochHigh = cepoch + 10

-- | Generate an InstantaneousRewards Transfer certificate
genInstantaneousRewardsAccounts ::
  (EraPParams era, EraAccounts era, ShelleyEraTxCert era, ProtVerAtMost era 8) =>
  SlotNo ->
  -- | Index over the cold key hashes of all possible Genesis Delegates
  Map (KeyHash 'GenesisDelegate) (AllIssuerKeys c 'GenesisDelegate) ->
  PParams era ->
  ChainAccountState ->
  DState era ->
  Gen (Maybe (TxCert era, CertCred era))
genInstantaneousRewardsAccounts s genesisDelegatesByHash pparams accountState delegSt = do
  let (GenDelegs genDelegs_) = dsGenDelegs delegSt
      lookupGenDelegate' gk =
        fromMaybe
          (error "genInstantaneousRewardsAccounts: lookupGenDelegate failed")
          (Map.lookup gk genesisDelegatesByHash)
      accountsMap = delegSt ^. accountsL . accountsMapL
  winnerCreds <-
    take
      <$> QC.elements [0 .. (max 0 $ Map.size accountsMap - 1)]
      <*> QC.shuffle (Map.keys accountsMap)
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
          ( MirTxCert (MIRCert pot (StakeAddressesMIR credCoinMap))
          , DelegateCred (aikCold <$> coreSigners)
          )

-- | Generate an InstantaneousRewards Transfer
genInstantaneousRewardsTransfer ::
  (EraPParams era, ShelleyEraTxCert era, ProtVerAtMost era 8) =>
  SlotNo ->
  -- | Index over the cold key hashes of all possible Genesis Delegates
  Map (KeyHash 'GenesisDelegate) (AllIssuerKeys c 'GenesisDelegate) ->
  PParams era ->
  ChainAccountState ->
  DState era ->
  Gen (Maybe (TxCert era, CertCred era))
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
          ( MirTxCert (MIRCert pot (SendToOppositePotMIR $ Coin amount))
          , DelegateCred (aikCold <$> coreSigners)
          )

genInstantaneousRewards ::
  (EraPParams era, EraAccounts era, ShelleyEraTxCert era, ProtVerAtMost era 8) =>
  SlotNo ->
  -- | Index over the cold key hashes of all possible Genesis Delegates
  Map (KeyHash 'GenesisDelegate) (AllIssuerKeys c 'GenesisDelegate) ->
  PParams era ->
  ChainAccountState ->
  DState era ->
  Gen (Maybe (TxCert era, CertCred era))
genInstantaneousRewards slot genesisDelegatesByHash pparams accountState delegSt =
  if hardforkAlonzoAllowMIRTransfer (pparams ^. ppProtocolVersionL)
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
