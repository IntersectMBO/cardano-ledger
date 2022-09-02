{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Strategy for Generic Tests
--   Make the GenState include a Mode of the NewEpochState, modify
--   the ModelNewEpochState to reflect what we generated.
module Test.Cardano.Ledger.Generic.GenState.Generators
  ( genCredential,
    genFreshCredential,
    genFreshRegCred,
    genPool,
    genPoolParams,
    genRewards,
    genNewPool,
    genRetirementHash,
    initStableFields,
  )
where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Alonzo.Scripts hiding (Mint, Script)
import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Era (EraCrypto))
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), coerceKeyRole)
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Shelley.LedgerState (RewardAccounts)
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.Val (Val (..))
import Control.Monad (replicateM, zipWithM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (ask, asks, get, gets, modify)
import Control.SetAlgebra (eval, (⨃))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Generic.Functions (keyPoolDeposits)
import Test.Cardano.Ledger.Generic.GenState.GenScript
import Test.Cardano.Ledger.Generic.GenState.Types
import Test.Cardano.Ledger.Generic.ModelState (ModelNewEpochState (..))
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Tasty.QuickCheck (arbitrary, choose)

-- ========================================================================
-- Tools to get started

-- | Initialize (or overwrite if they are not empty) the Stable fields. It is
--   intended that this be called just once at the beginning of a trace generation.
initStableFields :: forall era. Reflect era => Proof era -> GenRS era ()
initStableFields proof = do
  GenEnv {geSize} <- ask
  let GenSize {..} = geSize
  hashes <- replicateM maxStablePools $ do
    (kh, pp, ips) <- genNewPool
    modify
      ( \gs@GenState {..} ->
          gs
            { gsStablePools = Set.insert kh gsStablePools,
              gsInitialPoolParams = Map.insert kh pp gsInitialPoolParams,
              gsInitialPoolDistr = Map.insert kh ips gsInitialPoolDistr
            }
      )
    modifyModel (\ms -> ms {mPoolParams = Map.insert kh pp $ mPoolParams ms})
    return kh

  -- This incantation gets a list of fresh (not previously generated) Credential
  credentials <- replicateM maxStablePools $ do
    old' <- gets (Map.keysSet . gsInitialRewards)
    prev <- gets gsAvoidCred
    cred <- genFreshCredential 100 Rewrd (Set.union old' prev)
    modify
      ( \gs ->
          gs
            { gsStableDelegators = Set.insert cred $ gsStableDelegators gs,
              gsInitialRewards = Map.insert cred (Coin 0) $ gsInitialRewards gs
            }
      )
    return cred
  let f :: Credential 'Staking (EraCrypto era) -> KeyHash 'StakePool (EraCrypto era) -> GenRS era ()
      f cred kh = do
        pp <- asks gePParams
        let (keydeposit, _) = keyPoolDeposits proof pp
        modifyModel
          ( \ms ->
              ms
                { mDelegations = Map.insert cred kh $ mDelegations ms,
                  mRewards = Map.insert cred (Coin 0) (mRewards @era ms),
                  mDeposited = mDeposited ms <+> keydeposit
                }
          )
        modify (\gs -> gs {gsInitialDelegations = Map.insert cred kh $ gsInitialDelegations gs})
  zipWithM_ f credentials hashes

-- =============================================
-- Generators of inter-related items

-- Adds to the rewards of the ModelNewEpochState. This used exclusively to generate Withdrawals, so
-- we mark these as ones to avoid in the future. Especialy when generating DeRegKey.
genRewards :: Reflect era => GenRS era (RewardAccounts (EraCrypto era))
genRewards = do
  wmax <- gets (withdrawalMax . geSize . gsGenEnv)
  n <- lift $ choose (1, wmax)
  -- we need a fresh credential, one that was not previously
  -- generated here, or one that arose from gsAvoidCred (i.e. prev)
  old <- gets (Map.keysSet . gsInitialRewards)
  prev <- gets gsAvoidCred
  credentials <- genFreshCredentials n 100 Rewrd (Set.union old prev) []
  newRewards <- Map.fromList <$> mapM (\x -> (,) x <$> lift genRewardVal) credentials
  modifyModel (\m -> m {mRewards = eval (mRewards m ⨃ newRewards)}) -- Prefers coins in newrewards
  modify
    ( \st ->
        st
          { gsInitialRewards = eval (gsInitialRewards st ⨃ newRewards),
            gsAvoidCred = Set.union (Set.fromList credentials) (gsAvoidCred st)
          }
    )
  pure newRewards

genRetirementHash :: forall era. Reflect era => GenRS era (KeyHash 'StakePool (EraCrypto era))
genRetirementHash = do
  m <- gets (mPoolParams . gsModel)
  honestKhs <- gets gsStablePools
  avoidKey <- gets gsAvoidKey
  res <- lift . genMapElemWhere m 10 $ \kh _ ->
    kh `Set.notMember` honestKhs && kh `Set.notMember` avoidKey
  case res of
    Just x -> do
      modify (\st -> st {gsAvoidKey = Set.insert (fst x) (gsAvoidKey st)})
      -- if it is retiring, we should probably avoid it in the future
      pure $ fst x
    Nothing -> do
      (kh, pp, ips) <- genNewPool
      addPoolToInitialState kh pp ips
      addPoolToModel kh pp ips
      pure kh

-- | Generate a 'n' fresh credentials (ones not in the set 'old'). We get 'tries' chances,
--   if it doesn't work in 'tries' attempts then quit with an error. Better to raise an error
--   than go into an infinite loop.
genFreshCredentials ::
  forall era kr.
  Reflect era =>
  Int ->
  Int ->
  Tag ->
  Set (Credential kr (EraCrypto era)) ->
  [Credential kr (EraCrypto era)] ->
  GenRS era [Credential kr (EraCrypto era)]
genFreshCredentials _n 0 _tag _old _ans = error "Ran out of tries in genFreshCredentials."
genFreshCredentials n0 tries tag old0 ans0 = go n0 old0 ans0
  where
    go 0 _ ans = pure ans
    go n old ans = do
      c <- genFreshCredential tries tag old
      go (n - 1) (Set.insert c old) (c : ans)

genFreshCredential ::
  forall era kr.
  Reflect era =>
  Int ->
  Tag ->
  Set (Credential kr (EraCrypto era)) ->
  GenRS era (Credential kr (EraCrypto era))
genFreshCredential 0 _tag _old = error "Ran out of tries in genFreshCredential."
genFreshCredential tries0 tag old = go tries0
  where
    go tries = do
      c <- genCredential tag
      if Set.member c old
        then go (tries - 1)
        else pure c

-- Adds to the mPoolParams and the  mPoolDistr of the Model, and the initial set of objects for Traces
genPool :: forall era. Reflect era => GenRS era (KeyHash 'StakePool (EraCrypto era), PoolParams (EraCrypto era))
genPool = frequencyT [(10, genNew), (90, pickExisting)]
  where
    genNew = do
      (kh, pp, ips) <- genNewPool
      addPoolToInitialState kh pp ips
      modifyModel $ \m ->
        m
          { mPoolParams = Map.insert kh pp $ mPoolParams m
          }
      return (kh, pp)
    pickExisting = do
      _pParams <- gets (mPoolParams . gsModel)
      avoidKey <- gets gsAvoidKey
      lift (genMapElemWhere _pParams 10 (\kh _ -> kh `Set.notMember` avoidKey)) >>= \case
        Nothing -> genNew
        Just (kh, pp) -> pure (kh, pp)

-- | Use this function to get a new pool that should not be used in the future transactions
genNewPool :: forall era. Reflect era => GenRS era (KeyHash 'StakePool (EraCrypto era), PoolParams (EraCrypto era), IndividualPoolStake (EraCrypto era))
genNewPool = do
  poolId <- genFreshKeyHash
  poolParam <- genPoolParams poolId
  percent <- lift $ choose (0, 1 :: Float)
  let stake = IndividualPoolStake @(EraCrypto era) (toRational percent) (_poolVrf poolParam)
  modify (\s -> s {gsAvoidKey = Set.insert (coerceKeyRole poolId) $ gsAvoidKey s})
  pure (poolId, poolParam, stake)

genPoolParams ::
  Reflect era =>
  KeyHash 'StakePool (EraCrypto era) ->
  GenRS era (PoolParams (EraCrypto era))
genPoolParams _poolId = do
  _poolVrf <- lift arbitrary
  _poolPledge <- lift genPositiveVal
  _poolCost <- lift genPositiveVal
  _poolMargin <- lift arbitrary
  _poolRAcnt <- RewardAcnt Testnet <$> genFreshRegCred Rewrd
  let _poolOwners = mempty
  let _poolRelays = mempty
  let _poolMD = SNothing
  pure PoolParams {..}

-- Adds to both gsKeys and gsScripts and gsPlutusScript
-- via genKeyHash and genScript

-- | Generate a credential that can be used for supplied purpose (in case of
-- plutus scripts), while occasionally picking out randomly from previously
-- generated set.
-- Returns the credential and True iff the credential is freshly generated
genCredential :: forall era kr. Reflect era => Tag -> GenRS era (Credential kr (EraCrypto era))
genCredential tag =
  frequencyT
    [ (35, KeyHashObj <$> genKeyHash'),
      (35, ScriptHashObj <$> genScript'),
      (10, pickExistingKeyHash),
      (20, pickExistingScript)
    ]
  where
    genKeyHash' = do
      kh <- genFreshKeyHash -- We need to avoid some key credentials
      case tag of
        Rewrd -> modify $ \st ->
          st {gsInitialRewards = Map.insert (KeyHashObj kh) (Coin 0) $ gsInitialRewards st}
        _ -> pure ()
      return $ coerceKeyRole kh
    genScript' = f (100 :: Int)
      where
        f n
          | n <= 0 = error "Failed to generate a fresh script hash"
          | otherwise = do
              sh <- genScript @era reify tag
              initialRewards <- gets gsInitialRewards
              avoidCredentials <- gets gsAvoidCred
              let newcred = ScriptHashObj sh
              if Map.notMember newcred initialRewards && Set.notMember newcred avoidCredentials
                then do
                  case tag of
                    Rewrd -> modify $ \st -> st {gsInitialRewards = Map.insert newcred (Coin 0) $ gsInitialRewards st}
                    _ -> pure ()
                  return sh
                else f $ n - 1
    pickExistingKeyHash =
      KeyHashObj <$> do
        keysMap <- gsKeys <$> get
        lift (genMapElem keysMap) >>= \case
          Just (k, _) -> pure $ coerceKeyRole k
          Nothing -> genKeyHash'
    pickExistingScript =
      ScriptHashObj
        <$> elementsT [pickExistingPlutusScript, pickExistingTimelockScript]
    pickExistingPlutusScript = do
      plutusScriptsMap <-
        Map.filterWithKey (\(_, t) _ -> t == tag) . gsPlutusScripts <$> get
      lift (genMapElem plutusScriptsMap) >>= \case
        Just ((h, _), _) -> pure h
        Nothing -> genScript reify tag
    pickExistingTimelockScript = do
      -- Only pick one if it matches the
      vi <- gets gsValidityInterval -- current ValidityInterval
      vimap <- gets gsVI
      case Map.lookup vi vimap of
        Nothing -> genScript @era reify tag
        Just set ->
          lift (genSetElem set) >>= \case
            Nothing -> genScript reify tag
            Just hash -> pure hash

genFreshRegCred :: forall era. Reflect era => Tag -> GenRS era (Credential 'Staking (EraCrypto era))
genFreshRegCred tag = do
  old <- gets (Map.keysSet . gsInitialRewards)
  avoid <- gets gsAvoidCred
  rewards <- gets $ Map.keysSet . mRewards . gsModel
  cred <- genFreshCredential 100 tag $ old <> avoid <> rewards
  modify (\st -> st {gsAvoidCred = Set.insert cred (gsAvoidCred st)})
  pure cred
