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
module Test.Cardano.Ledger.Generic.GenState
  ( GenEnv (..),
    GenRS,
    GenState (..),
    GenSize (..),
    elementsT, -- TODO move to a utilities module
    frequencyT, -- TODO move to a utilities module
    genCredential,
    genDatumWithHash,
    genFreshCredential,
    genFreshRegCred,
    genKeyHash,
    genPool,
    genPoolParams,
    genPositiveVal,
    genRewards,
    genScript,
    genValidityInterval,
    genNewPool,
    genRetirementHash,
    genGenState,
    getBlocksizeMax,
    getCertificateMax,
    getOldUtxoPercent,
    getRefInputsMax,
    getReserves,
    getSlot,
    getSlotDelta,
    getSpendInputsMax,
    getTreasury,
    getUtxoChoicesMax,
    getUtxoElem,
    getUtxoTest,
    getCollInputsMax,
    getNewPoolTest,
    viewGenState,
    initialLedgerState,
    initStableFields,
    modifyModel,
    runGenRS,
    ioGenRS,
    small,
  )
where

import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Alonzo.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.Scripts hiding (Mint, Script)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj), StakeCredential)
import Cardano.Ledger.Keys
  ( KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    coerceKeyRole,
    hashKey,
  )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Pretty (PDoc, ppInt, ppMap, ppRecord, ppSet, ppString)
import Cardano.Ledger.Pretty.Mary (ppValidityInterval)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    LedgerState (..),
    PState (..),
    RewardAccounts,
    smartUTxOState,
  )
import qualified Cardano.Ledger.Shelley.Scripts as Shelley (MultiSig (..))
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (join, replicateM, when, zipWithM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWST (..), ask, asks, get, gets, modify)
import Control.SetAlgebra (eval, (⨃))
import Data.Default.Class (Default (def))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import qualified Data.Sequence.Strict as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.UMap as UMap
import GHC.Word (Word64)
import Numeric.Natural
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Generic.Fields
import Test.Cardano.Ledger.Generic.Functions
  ( alwaysFalse,
    alwaysTrue,
    keyPoolDeposits,
    obligation',
    primaryLanguage,
    protocolVersion,
    txoutFields,
  )
import Test.Cardano.Ledger.Generic.ModelState
  ( ModelNewEpochState (..),
    genDelegsZero,
    instantaneousRewardsZero,
    mNewEpochStateZero,
    pPUPStateZero,
    pcModelNewEpochState,
  )
import Test.Cardano.Ledger.Generic.PrettyCore
  ( PrettyC (..),
    pcCoin,
    pcCredential,
    pcIndividualPoolStake,
    pcKeyHash,
    pcPoolParams,
    pcTxIn,
    pcTxOut,
  )
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Updaters
import Test.Tasty.QuickCheck
  ( Gen,
    Positive (..),
    arbitrary,
    choose,
    chooseInt,
    elements,
    frequency,
    generate,
  )

-- =================================================

-- | Constants that determine how big a GenState is generated.
data GenSize = GenSize
  { treasury :: !Integer,
    reserves :: !Integer,
    startSlot :: !Word64,
    slotDelta :: !(Word64, Word64),
    blocksizeMax :: !Integer,
    collInputsMax :: !Natural,
    spendInputsMax :: !Int,
    refInputsMax :: !Int,
    utxoChoicesMax :: !Int,
    certificateMax :: !Int,
    withdrawalMax :: !Int,
    oldUtxoPercent :: !Int, -- between 0-100, 10 means pick an old UTxO 10% of the time
    maxStablePools :: !Int
  }
  deriving (Show)

data GenEnv era = GenEnv
  { gePParams :: !(PParams era),
    geSize :: !GenSize
  }

data GenState era = GenState
  { gsValidityInterval :: !ValidityInterval,
    gsKeys :: !(Map (KeyHash 'Witness (Crypto era)) (KeyPair 'Witness (Crypto era))),
    gsScripts :: !(Map (ScriptHash (Crypto era)) (Script era)),
    gsPlutusScripts :: !(Map (ScriptHash (Crypto era), Tag) (IsValid, Script era)),
    gsDatums :: !(Map (DataHash (Crypto era)) (Data era)),
    gsVI :: !(Map ValidityInterval (Set (ScriptHash (Crypto era)))),
    gsModel :: !(ModelNewEpochState era),
    gsInitialUtxo :: !(Map (TxIn (Crypto era)) (TxOut era)),
    gsInitialRewards :: !(Map (Credential 'Staking (Crypto era)) Coin),
    gsInitialDelegations :: !(Map (Credential 'Staking (Crypto era)) (KeyHash 'StakePool (Crypto era))),
    gsInitialPoolParams :: !(Map (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era))),
    gsInitialPoolDistr :: !(Map (KeyHash 'StakePool (Crypto era)) (IndividualPoolStake (Crypto era))),
    -- Stable fields are stable from initialization to the end of the generation process
    gsStablePools :: !(Set (KeyHash 'StakePool (Crypto era))),
    gsStableDelegators :: !(Set (StakeCredential (Crypto era))),
    gsAvoidCred :: !(Set (Credential 'Staking (Crypto era))),
    gsAvoidKey :: !(Set (KeyHash 'StakePool (Crypto era))),
    gsProof :: !(Proof era),
    gsGenEnv :: !(GenEnv era),
    gsSeedIdx :: !Int
  }

emptyGenState :: Reflect era => Proof era -> GenEnv era -> GenState era
emptyGenState proof genv =
  GenState
    (ValidityInterval SNothing SNothing)
    mempty
    mempty
    mempty
    mempty
    mempty
    mNewEpochStateZero
    Map.empty
    Map.empty
    Map.empty
    Map.empty
    Map.empty
    Set.empty
    Set.empty
    Set.empty
    Set.empty
    proof
    genv
    0

instance Default GenSize where
  def =
    GenSize
      { treasury = 1000000,
        reserves = 1000000,
        startSlot = 0,
        slotDelta = (3, 7),
        blocksizeMax = 10,
        collInputsMax = 5,
        oldUtxoPercent = 15,
        spendInputsMax = 10,
        refInputsMax = 6,
        utxoChoicesMax = 30,
        certificateMax = 10,
        withdrawalMax = 10,
        maxStablePools = 5
      }

small :: GenSize
small =
  GenSize
    { treasury = 1000000,
      reserves = 1000000,
      startSlot = 0,
      slotDelta = (2, 5),
      blocksizeMax = 3,
      collInputsMax = 2,
      oldUtxoPercent = 5,
      spendInputsMax = 3,
      refInputsMax = 1,
      utxoChoicesMax = 12,
      certificateMax = 2,
      withdrawalMax = 2,
      maxStablePools = 4
    }

-- ==============================================================
-- The Monad

type GenRS era = RWST (GenEnv era) () (GenState era) Gen

genMapElem :: Map k a -> Gen (Maybe (k, a))
genMapElem m
  | n == 0 = pure Nothing
  | otherwise = do
      i <- choose (0, n - 1)
      pure $ Just $ Map.elemAt i m
  where
    n = Map.size m

genSetElem :: Set a -> Gen (Maybe a)
genSetElem m
  | n == 0 = pure Nothing
  | otherwise = do
      i <- choose (0, n - 1)
      pure $ Just $ Set.elemAt i m
  where
    n = Set.size m

-- | Use up to 'tries' attempts to choose a random (k,a) pair from 'm', that meets predicate 'p'
genMapElemWhere :: Map k a -> Int -> (k -> a -> Bool) -> Gen (Maybe (k, a))
genMapElemWhere m tries p
  | tries <= 0 = pure Nothing
  | n == 0 = pure Nothing
  | otherwise = do
      i <- choose (0, n - 1)
      let (k, a) = Map.elemAt i m
      if p k a
        then pure $ Just $ (k, a)
        else genMapElemWhere m (tries - 1) p
  where
    n = Map.size m

elementsT :: (Monad (t Gen), MonadTrans t) => [t Gen b] -> t Gen b
elementsT = join . lift . elements

frequencyT :: (Monad (t Gen), MonadTrans t) => [(Int, t Gen b)] -> t Gen b
frequencyT [] = error ("frequencyT called with empty list")
frequencyT choices = join . lift . frequency . map (pure <$>) $ choices

-- | Gen a positive single digit Int, on a skewed distribution that
--   favors 2,3,4,5 but occasionally gets others
positiveSingleDigitInt :: Gen Int
positiveSingleDigitInt =
  frequency (map f [(1, 1), (5, 2), (4, 3), (4, 4), (3, 5), (2, 6), (1, 7), (1, 8), (1, 9)])
  where
    f (x, y) = (x, pure y)

-- | Gen a non-negative single digit Int, on a skewed distribution that
--   favors 2,3,4,5 but occasionally gets others
nonNegativeSingleDigitInt :: Gen Int
nonNegativeSingleDigitInt =
  frequency (map f [(1, 0), (2, 1), (5, 2), (4, 3), (3, 4), (2, 5), (2, 6), (1, 7), (1, 8), (1, 9)])
  where
    f (x, y) = (x, pure y)

-- | Generate a non-zero value
genPositiveVal :: Val v => Gen v
genPositiveVal = inject . Coin . getPositive <$> arbitrary

-- | Generate a value (which is occaisionally 0) useful in generating Rewards, where we need a
--   few 0's, because we cannot generate a DeReg certificates, without a 0 Reg value.
--   Also used when generating the CollReturn, where an occasional 0 would be nice
genRewardVal :: Val v => Gen v
genRewardVal = frequency [(3, pure mempty), (97, genPositiveVal)]

modifyModel :: (ModelNewEpochState era -> ModelNewEpochState era) -> GenRS era ()
modifyModel f = modify (\gstate -> gstate {gsModel = f (gsModel gstate)})

-- =====================================================================
-- Accessing information

getSlot :: GenState era -> SlotNo
getSlot = SlotNo . startSlot . geSize . gsGenEnv

getSlotDelta :: GenState era -> (Word64, Word64)
getSlotDelta = slotDelta . geSize . gsGenEnv

getBlocksizeMax :: GenState era -> Integer
getBlocksizeMax = blocksizeMax . geSize . gsGenEnv

getSpendInputsMax :: GenState era -> Int
getSpendInputsMax = spendInputsMax . geSize . gsGenEnv

getRefInputsMax :: GenState era -> Int
getRefInputsMax = refInputsMax . geSize . gsGenEnv

getCertificateMax :: GenState era -> Int
getCertificateMax = certificateMax . geSize . gsGenEnv

getUtxoChoicesMax :: GenState era -> Int
getUtxoChoicesMax = utxoChoicesMax . geSize . gsGenEnv

getCollInputsMax :: GenState era -> Natural
getCollInputsMax = collInputsMax . geSize . gsGenEnv

getOldUtxoPercent :: GenState era -> Int
getOldUtxoPercent x = max 0 (min 100 (oldUtxoPercent (geSize (gsGenEnv x))))

getTreasury :: GenState era -> Coin
getTreasury = Coin . treasury . geSize . gsGenEnv

getReserves :: GenState era -> Coin
getReserves = Coin . reserves . geSize . gsGenEnv

-- | Pick a UTxO element where we can use it in a new Tx. Most of the time we generate new
--   elements for each Tx, but once in a while we choose an existing one. We must be carefull
--   that that the Pay credential of the TxOut can run in the curent ValidityInterval
--   A crude but simple way is to insist Pay credential is either Key locked, or locked
--   with Plutus or MultiSig scripts, and return False for any Timelock scripts.
getUtxoElem :: Reflect era => GenRS era (Maybe (TxIn (Crypto era), TxOut era))
getUtxoElem = do
  x <- gets (mUTxO . gsModel)
  scriptmap <- gets gsScripts
  lift $ genMapElemWhere x 20 (validTxOut reify scriptmap)

-- | Test if the Payment part of the Address in the TxOut
--   is valid in the current ValidityInterval. Using the simple rule allowing
--   only (Key or Plutus or MutiSig) locking. Disallowing all Timelock scripts
validTxOut ::
  Proof era ->
  Map (ScriptHash (Crypto era)) (Script era) ->
  TxIn (Crypto era) ->
  TxOut era ->
  Bool
validTxOut proof m _txin txout = case txoutFields proof txout of
  (Addr _ (KeyHashObj _) _, _, _) -> True
  (Addr _ (ScriptHashObj h) _, _, _) -> case (proof, Map.lookup h m) of
    (Conway _, Just (PlutusScript _ _)) -> True
    (Babbage _, Just (PlutusScript _ _)) -> True
    (Alonzo _, Just (PlutusScript _ _)) -> True
    (Shelley _, Just _msig) -> True
    _ -> False
  _bootstrap -> False

getUtxoTest :: GenRS era (TxIn (Crypto era) -> Bool)
getUtxoTest = do
  x <- gets (mUTxO . gsModel)
  pure (\k -> maybe False (const True) (Map.lookup k x))

-- | To compute deposits we need a function that tells if the KeyHash is a new Pool
--   Compute this function before we do any generation, since such generation
--   may actually add to the mPoolParams, and then the added thing won't appear new.
getNewPoolTest :: GenRS era (KeyHash 'StakePool (Crypto era) -> Bool)
getNewPoolTest = do
  poolparams <- gets (mPoolParams . gsModel)
  pure (`Map.member` poolparams)

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
  let f :: Credential 'Staking (Crypto era) -> KeyHash 'StakePool (Crypto era) -> GenRS era ()
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

runGenRS ::
  Reflect era =>
  Proof era ->
  GenSize ->
  GenRS era a ->
  Gen (a, GenState era)
runGenRS proof gsize action = do
  genenv <- genGenEnv proof gsize
  (ans, state, ()) <- runRWST action genenv (emptyGenState proof genenv)
  pure (ans, state)

-- | Should not be used in tests, this is a helper function to be used in ghci only!
ioGenRS :: Reflect era => Proof era -> GenSize -> GenRS era ans -> IO (ans, GenState era)
ioGenRS proof gsize action = generate $ runGenRS proof gsize action

-- | Generate a random, well-formed, GenEnv
genGenEnv :: Proof era -> GenSize -> Gen (GenEnv era)
genGenEnv proof gsize = do
  maxTxExUnits <- arbitrary :: Gen ExUnits
  maxCollateralInputs <- elements [1 .. collInputsMax gsize]
  collateralPercentage <- fromIntegral <$> chooseInt (1, 10000)
  minfeeA <- fromIntegral <$> chooseInt (0, 1000)
  minfeeB <- fromIntegral <$> chooseInt (0, 10000)
  let pp =
        newPParams
          proof
          [ MinfeeA minfeeA,
            MinfeeB minfeeB,
            defaultCostModels proof,
            MaxValSize 1000,
            MaxTxSize $ fromIntegral (maxBound :: Int),
            MaxTxExUnits maxTxExUnits,
            MaxCollateralInputs maxCollateralInputs,
            CollateralPercentage collateralPercentage,
            ProtocolVersion $ protocolVersion proof,
            PoolDeposit $ Coin 5,
            KeyDeposit $ Coin 2,
            EMax 5
          ]
  pure $
    GenEnv
      { gePParams = pp,
        geSize = gsize
      }

genGenState :: Reflect era => Proof era -> GenSize -> Gen (GenState era)
genGenState proof gsize = do
  let slotNo = startSlot gsize
  minSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (minBound, slotNo))]
  maxSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (slotNo + 1, maxBound))]
  let vi = ValidityInterval (SlotNo <$> minSlotNo) (SlotNo <$> maxSlotNo)
  env <- genGenEnv proof gsize
  pure ((emptyGenState proof env) {gsValidityInterval = vi})

-- | Helper function for development and debugging in ghci
viewGenState :: Reflect era => Proof era -> GenSize -> Bool -> IO ()
viewGenState proof gsize verbose = do
  st <- generate (genGenState proof gsize)
  when verbose $ print (pcGenState proof st)

-- =============================================
-- Generators of inter-related items

-- Adds to the gsKeys
genKeyHash :: Reflect era => GenRS era (KeyHash kr (Crypto era))
genKeyHash = do
  keyPair <- lift arbitrary
  let keyHash = hashKey $ vKey keyPair
  modify $ \ts@GenState {gsKeys} -> ts {gsKeys = Map.insert keyHash keyPair gsKeys}
  pure $ coerceKeyRole keyHash

-- Adds to the gsDatums
genDatumWithHash :: Era era => GenRS era (DataHash (Crypto era), Data era)
genDatumWithHash = do
  datum <- lift arbitrary
  let datumHash = hashData datum
  modify $ \ts@GenState {gsDatums} ->
    ts {gsDatums = Map.insert datumHash datum gsDatums}
  pure (datumHash, datum)

-- Adds to the rewards of the ModelNewEpochState. This used exclusively to generate Withdrawals, so
-- we mark these as ones to avoid in the future. Especialy when generating DeRegKey.
genRewards :: Reflect era => GenRS era (RewardAccounts (Crypto era))
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

genRetirementHash :: forall era. Reflect era => GenRS era (KeyHash 'StakePool (Crypto era))
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
  Set (Credential kr (Crypto era)) ->
  [Credential kr (Crypto era)] ->
  GenRS era [Credential kr (Crypto era)]
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
  Set (Credential kr (Crypto era)) ->
  GenRS era (Credential kr (Crypto era))
genFreshCredential 0 _tag _old = error "Ran out of tries in genFreshCredential."
genFreshCredential tries0 tag old = go tries0
  where
    go tries = do
      c <- genCredential tag
      if Set.member c old
        then go (tries - 1)
        else pure c

-- Adds to the mPoolParams and the  mPoolDistr of the Model, and the initial set of objects for Traces
genPool :: forall era. Reflect era => GenRS era (KeyHash 'StakePool (Crypto era), PoolParams (Crypto era))
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

genFreshKeyHash :: Reflect era => GenRS era (KeyHash kr (Crypto era))
genFreshKeyHash = go (100 :: Int) -- avoid unlikely chance of generated hash collisions.
  where
    go n
      | n <= 0 = error "Something very unlikely happened"
      | otherwise = do
          avoidKeys <- gets gsAvoidKey
          kh <- genKeyHash
          if coerceKeyRole kh `Set.member` avoidKeys
            then go $ n - 1
            else return kh

-- | Use this function to get a new pool that should not be used in the future transactions
genNewPool :: forall era. Reflect era => GenRS era (KeyHash 'StakePool (Crypto era), PoolParams (Crypto era), IndividualPoolStake (Crypto era))
genNewPool = do
  poolId <- genFreshKeyHash
  poolParam <- genPoolParams poolId
  percent <- lift $ choose (0, 1 :: Float)
  let stake = IndividualPoolStake @(Crypto era) (toRational percent) (_poolVrf poolParam)
  modify (\s -> s {gsAvoidKey = Set.insert (coerceKeyRole poolId) $ gsAvoidKey s})
  pure (poolId, poolParam, stake)

addPoolToInitialState ::
  KeyHash 'StakePool (Crypto era) ->
  PoolParams (Crypto era) ->
  IndividualPoolStake (Crypto era) ->
  GenRS era ()
addPoolToInitialState poolId poolparam stake =
  modify
    ( \st ->
        st
          { gsInitialPoolParams = Map.insert poolId poolparam (gsInitialPoolParams st),
            gsInitialPoolDistr = Map.insert poolId stake (gsInitialPoolDistr st)
          }
    )

addPoolToModel :: KeyHash 'StakePool (Crypto era) -> PoolParams (Crypto era) -> IndividualPoolStake (Crypto era) -> GenRS era ()
addPoolToModel poolId poolparam stake =
  modifyModel
    ( \m ->
        m
          { mPoolParams = Map.insert poolId poolparam (mPoolParams m),
            mPoolDistr = Map.insert poolId stake (mPoolDistr m)
          }
    )

genPoolParams ::
  Reflect era =>
  KeyHash 'StakePool (Crypto era) ->
  GenRS era (PoolParams (Crypto era))
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
genCredential :: forall era kr. Reflect era => Tag -> GenRS era (Credential kr (Crypto era))
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

-- | Generate a transaction body validity interval which is close in proximity
--  (less than a stability window) from the current slot.
genValidityInterval :: SlotNo -> Gen ValidityInterval
genValidityInterval (SlotNo s) = do
  let stabilityWindow = 29 -- < 3k/f many slots, where 10k is the epoch length
  start <- frequency [(1, pure SNothing), (4, SJust <$> choose (minBound, s))]
  end <- frequency [(1, pure SNothing), (4, SJust <$> choose (s + 1, s + stabilityWindow))]
  pure $ ValidityInterval (SlotNo <$> start) (SlotNo <$> end)

-- ===========================================================
-- Generate Era agnostic Scripts

-- Adds to gsScripts and gsPlutusScripts
genScript :: forall era. Reflect era => Proof era -> Tag -> GenRS era (ScriptHash (Crypto era))
genScript proof tag = case proof of
  Conway _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Babbage _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Alonzo _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Mary _ -> genTimelockScript proof
  Allegra _ -> genTimelockScript proof
  Shelley _ -> genMultiSigScript proof

-- Adds to gsScripts
genTimelockScript :: forall era. Reflect era => Proof era -> GenRS era (ScriptHash (Crypto era))
genTimelockScript proof = do
  vi@(ValidityInterval mBefore mAfter) <- gets gsValidityInterval
  -- We need to limit how deep these timelocks can go, otherwise this generator will
  -- diverge. It also has to stay very shallow because it grows too fast.
  let genNestedTimelock k
        | k > 0 =
            elementsT $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = elementsT nonRecTimelocks
      nonRecTimelocks :: [GenRS era (Timelock era)]
      nonRecTimelocks =
        [ r
          | SJust r <-
              [ requireTimeStart <$> mBefore,
                requireTimeExpire <$> mAfter,
                SJust requireSignature
              ]
        ]
      requireSignature = RequireSignature <$> genKeyHash
      requireAllOf k = do
        n <- lift nonNegativeSingleDigitInt
        RequireAllOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireAnyOf k = do
        n <- lift positiveSingleDigitInt
        RequireAnyOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireMOf k = do
        n <- lift nonNegativeSingleDigitInt
        m <- lift $ choose (0, n)
        RequireMOf m . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireTimeStart (SlotNo validFrom) = do
        minSlotNo <- lift $ choose (minBound, validFrom)
        pure $ RequireTimeStart (SlotNo minSlotNo)
      requireTimeExpire (SlotNo validTill) = do
        maxSlotNo <- lift $ choose (validTill, maxBound)
        pure $ RequireTimeExpire (SlotNo maxSlotNo)
  tlscript <- genNestedTimelock (2 :: Natural)
  let corescript :: Script era
      corescript = case proof of
        Conway _ -> TimelockScript tlscript
        Babbage _ -> TimelockScript tlscript
        Alonzo _ -> TimelockScript tlscript
        Mary _ -> tlscript
        Allegra _ -> tlscript
        Shelley _ -> error "Shelley does not have TimeLock scripts"
  let scriptHash = hashScript @era corescript
      insertOrCreate x Nothing = Just (Set.singleton x)
      insertOrCreate x (Just s) = Just (Set.insert x s)
  modify $ \ts@GenState {gsScripts, gsVI} ->
    ts
      { gsScripts = Map.insert scriptHash corescript gsScripts,
        gsVI = Map.alter (insertOrCreate scriptHash) vi gsVI
      }
  pure scriptHash

-- Adds to gsScripts
genMultiSigScript :: forall era. Reflect era => Proof era -> GenRS era (ScriptHash (Crypto era))
genMultiSigScript proof = do
  let genNestedMultiSig k
        | k > 0 =
            elementsT $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = elementsT nonRecTimelocks
      nonRecTimelocks = [requireSignature]
      requireSignature = Shelley.RequireSignature @era <$> genKeyHash
      requireAllOf k = do
        n <- lift nonNegativeSingleDigitInt
        Shelley.RequireAllOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireAnyOf k = do
        n <- lift positiveSingleDigitInt
        Shelley.RequireAnyOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireMOf k = do
        n <- lift nonNegativeSingleDigitInt
        m <- lift $ choose (0, n)
        Shelley.RequireMOf m <$> replicateM n (genNestedMultiSig (k - 1))
  msscript <- genNestedMultiSig (2 :: Natural)
  let corescript :: Script era
      corescript = case proof of
        Shelley _ -> msscript
        _ -> error (show proof ++ " does not have MultiSig scripts")
  let scriptHash = hashScript @era corescript
  modify $ \ts@GenState {gsScripts} -> ts {gsScripts = Map.insert scriptHash corescript gsScripts}
  pure scriptHash

-- Adds to gsPlutusScripts
genPlutusScript :: forall era. Reflect era => Proof era -> Tag -> GenRS era (ScriptHash (Crypto era))
genPlutusScript proof tag = do
  isValid <- lift $ frequency [(5, pure False), (95, pure True)]
  -- Plutus scripts alwaysSucceeds needs at least numArgs, while
  -- alwaysFails needs exactly numArgs to have the desired affect.
  -- For reasons unknown, this number differs from Alonzo to Babbage
  -- Perhaps because Babbage is using PlutusV2 scripts?
  let numArgs = case (proof, tag) of
        (Conway _, Spend) -> 2
        (Conway _, _) -> 1
        (Babbage _, Spend) -> 2
        (Babbage _, _) -> 1
        (_, Spend) -> 3
        (_, _) -> 2
  -- While using varying number of arguments for alwaysSucceeds we get
  -- varying script hashes, which helps with the fuzziness
  let mlanguage = primaryLanguage proof
  script <-
    if isValid
      then alwaysTrue proof mlanguage . (+ numArgs) <$> lift (elements [0, 1, 2, 3 :: Natural])
      else pure $ alwaysFalse proof mlanguage numArgs

  let corescript :: Script era
      corescript = case proof of
        Alonzo _ -> script
        Babbage _ -> script
        Conway _ -> script
        _ ->
          error
            ( "PlutusScripts are available starting in the Alonzo era. "
                ++ show proof
                ++ " does not support PlutusScripts."
            )
      scriptHash = hashScript @era corescript
  modify $ \ts@GenState {gsPlutusScripts} ->
    ts {gsPlutusScripts = Map.insert (scriptHash, tag) (IsValid isValid, corescript) gsPlutusScripts}
  pure scriptHash

genFreshRegCred :: forall era. Reflect era => Tag -> GenRS era (Credential 'Staking (Crypto era))
genFreshRegCred tag = do
  old <- gets (Map.keysSet . gsInitialRewards)
  avoid <- gets gsAvoidCred
  rewards <- gets $ Map.keysSet . mRewards . gsModel
  cred <- genFreshCredential 100 tag $ old <> avoid <> rewards
  modify (\st -> st {gsAvoidCred = Set.insert cred (gsAvoidCred st)})
  pure cred

-- =================================================================

pcGenState :: Reflect era => Proof era -> GenState era -> PDoc
pcGenState proof (GenState vi keys scripts plutus dats mvi model iutxo irew idel ipoolp ipoold hp hd avcred _avkey prf _genenv _si) =
  ppRecord
    "GenState Summary"
    [ ("ValidityInterval", ppValidityInterval vi),
      ("Keymap", ppInt (Map.size keys)),
      ("Scriptmap", ppInt (Map.size scripts)),
      ("PlutusScripts", ppInt (Map.size plutus)),
      ("Datums", ppInt (Map.size dats)),
      ("VI-ScriptMap", ppInt (Map.size mvi)),
      ("Model", pcModelNewEpochState proof model),
      ("Initial Utxo", ppMap pcTxIn (pcTxOut proof) iutxo),
      ("Initial Rewards", ppMap pcCredential pcCoin irew),
      ("Initial Delegations", ppMap pcCredential pcKeyHash idel),
      ("Initial PoolParams", ppMap pcKeyHash pcPoolParams ipoolp),
      ("Initial PoolDistr", ppMap pcKeyHash pcIndividualPoolStake ipoold),
      ("Stable PoolParams", ppSet pcKeyHash hp),
      ("Stable Delegators", ppSet pcCredential hd),
      ("Previous RegKey", ppSet pcCredential avcred),
      ("GenEnv", ppString "GenEnv ..."),
      ("Proof", ppString (show prf))
    ]

instance Reflect era => PrettyC (GenState era) era where prettyC = pcGenState

instance era ~ BabbageEra Mock => Show (GenState era) where
  show x = show (pcGenState (Babbage Mock) x)

-- =====================================================================
-- Build an Initial LedgerState for a Trace from a GenState, after
-- generating a coherent Trace (a sequence of Transactions that can
-- logically be applied one after another)

initialLedgerState :: forall era. Reflect era => GenState era -> LedgerState era
initialLedgerState gstate = LedgerState utxostate dpstate
  where
    umap = UMap.unify (gsInitialRewards gstate) (gsInitialDelegations gstate) Map.empty
    utxostate = smartUTxOState (UTxO (gsInitialUtxo gstate)) deposited (Coin 0) (pPUPStateZero @era)
    dpstate = DPState dstate pstate
    dstate =
      DState
        umap
        Map.empty
        genDelegsZero
        instantaneousRewardsZero
    pstate = PState (gsInitialPoolParams gstate) Map.empty Map.empty
    -- In a wellformed LedgerState the deposited equals the obligation
    deposited =
      obligation'
        reify
        (gePParams (gsGenEnv gstate))
        (gsInitialRewards gstate)
        (gsInitialPoolParams gstate)
