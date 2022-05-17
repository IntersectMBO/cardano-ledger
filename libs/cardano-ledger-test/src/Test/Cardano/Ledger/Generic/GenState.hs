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
module Test.Cardano.Ledger.Generic.GenState where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Alonzo.Data (Data (..), DataHash, hashData)
import Cardano.Ledger.Alonzo.Scripts hiding (Mint)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
import Cardano.Ledger.Era (Era (..), ValidateScript (hashScript))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys
  ( KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    coerceKeyRole,
    hashKey,
  )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Pretty (PDoc, ppInt, ppMap, ppRecord, ppSet, ppString)
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
import Control.Monad (join, replicateM, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWST (..), ask, get, gets, modify)
import qualified Control.Monad.Trans.Reader as Reader
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
    obligation',
    primaryLanguage,
    protocolVersion,
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
    blocksizeMax :: !Integer,
    collInputsMax :: !Natural,
    spendInputsMax :: !Int,
    refInputsMax :: !Int,
    utxoChoicesMax :: !Int,
    certificateMax :: !Int,
    withdrawalMax :: !Int,
    oldUtxoPercent :: !Int -- between 0-100, 10 means pick an old UTxO 10% of the time
  }
  deriving (Show)

data GenEnv era = GenEnv
  { geValidityInterval :: !ValidityInterval,
    gePParams :: !(Core.PParams era),
    geSize :: !GenSize
  }

data GenState era = GenState
  { gsKeys :: !(Map (KeyHash 'Witness (Crypto era)) (KeyPair 'Witness (Crypto era))),
    gsScripts :: !(Map (ScriptHash (Crypto era)) (Core.Script era)),
    gsPlutusScripts :: !(Map (ScriptHash (Crypto era), Tag) (IsValid, Core.Script era)),
    gsDatums :: !(Map (DataHash (Crypto era)) (Data era)),
    gsModel :: !(ModelNewEpochState era),
    gsInitialUtxo :: !(Map (TxIn (Crypto era)) (Core.TxOut era)),
    gsInitialRewards :: !(Map (Credential 'Staking (Crypto era)) Coin),
    gsInitialPoolParams :: !(Map (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era))),
    gsInitialPoolDistr :: !(Map (KeyHash 'StakePool (Crypto era)) (IndividualPoolStake (Crypto era))),
    gsRegKey :: !(Set (Credential 'Staking (Crypto era))),
    gsProof :: !(Proof era),
    gsGenEnv :: !(GenEnv era)
  }

emptyGenState :: Reflect era => Proof era -> GenEnv era -> GenState era
emptyGenState proof genv =
  GenState
    mempty
    mempty
    mempty
    mempty
    mNewEpochStateZero
    Map.empty
    Map.empty
    Map.empty
    Map.empty
    Set.empty
    proof
    genv

instance Default GenSize where
  def =
    GenSize
      { treasury = 1000000,
        reserves = 1000000,
        startSlot = 10000000,
        blocksizeMax = 10,
        collInputsMax = 5,
        oldUtxoPercent = 15,
        spendInputsMax = 10,
        refInputsMax = 6,
        utxoChoicesMax = 30,
        certificateMax = 10,
        withdrawalMax = 10
      }

small :: GenSize
small =
  GenSize
    { treasury = 1000000,
      reserves = 1000000,
      startSlot = 100,
      blocksizeMax = 3,
      collInputsMax = 2,
      oldUtxoPercent = 5,
      spendInputsMax = 3,
      refInputsMax = 1,
      utxoChoicesMax = 12,
      certificateMax = 2,
      withdrawalMax = 2
    }

-- ==============================================================
-- The Monad

type GenRS era = RWST (GenEnv era) () (GenState era) Gen

type GenR era = Reader.ReaderT (GenState era) Gen

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

elementsT :: (Monad (t Gen), MonadTrans t) => [t Gen b] -> t Gen b
elementsT = join . lift . elements

frequencyT :: (Monad (t Gen), MonadTrans t) => [(Int, t Gen b)] -> t Gen b
frequencyT = join . lift . frequency . map (pure <$>)

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

getUtxoElem :: GenRS era (Maybe (TxIn (Crypto era), Core.TxOut era))
getUtxoElem = do
  x <- gets (mUTxO . gsModel)
  lift $ genMapElem x

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

runGenRS :: Reflect era => Proof era -> GenSize -> GenRS era ans -> Gen (ans, GenState era)
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
  let slotNo = startSlot gsize
      pp =
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
            KeyDeposit $ Coin 2
          ]
  minSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (minBound, slotNo))]
  maxSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (slotNo + 1, maxBound))]
  pure $
    GenEnv
      { geValidityInterval = ValidityInterval (SlotNo <$> minSlotNo) (SlotNo <$> maxSlotNo),
        gePParams = pp,
        geSize = gsize
      }

genGenState :: Reflect era => Proof era -> GenSize -> Gen (GenState era)
genGenState proof gsize = do
  env <- genGenEnv proof gsize
  pure (emptyGenState proof env)

-- | Helper function for development and debugging in ghci
viewGenState :: Reflect era => Proof era -> GenSize -> Bool -> IO ()
viewGenState proof gsize verbose = do
  st <- generate (genGenState proof gsize)
  when verbose $ putStrLn (show (pcGenState proof st))

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

-- Adds to the rewards of the ModelNewEpochState
genRewards :: Reflect era => GenRS era (RewardAccounts (Crypto era))
genRewards = do
  wmax <- gets (withdrawalMax . geSize . gsGenEnv)
  n <- lift $ choose (1, wmax)
  -- we need a fresh credential, one that was not previously
  -- generated here, or one that arose from RegKey (i.e. prev)
  old <- gets (Map.keysSet . gsInitialRewards)
  prev <- gets gsRegKey
  credentials <- genFreshCredentials n 100 Rewrd (Set.union old prev) []
  newRewards <- Map.fromList <$> mapM (\x -> (,) x <$> lift genRewardVal) credentials
  modifyModel (\m -> m {mRewards = eval (mRewards m ⨃ newRewards)}) -- Prefers coins in newrewards
  pure newRewards

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
genPool :: forall era. Reflect era => GenRS era (KeyHash 'StakePool (Crypto era))
genPool = frequencyT [(10, genNewPool), (90, pickExisting)]
  where
    pickExisting = do
      _pParams <- gets (mPoolParams . gsModel)
      lift (genMapElem _pParams) >>= \case
        Nothing -> genNewPool
        Just poolId -> pure $ fst poolId
    genNewPool = do
      poolId <- genKeyHash
      poolparam <- genPoolParams poolId
      percent <- lift $ choose (0, 1 :: Float)
      let stake = IndividualPoolStake @(Crypto era) (toRational percent) (_poolVrf poolparam)
      modify
        ( \st ->
            st
              { gsInitialPoolParams = Map.insert poolId poolparam (gsInitialPoolParams st),
                gsInitialPoolDistr = Map.insert poolId stake (gsInitialPoolDistr st)
              }
        )
      modifyModel
        ( \m ->
            m
              { mPoolParams = Map.insert poolId poolparam (mPoolParams m),
                mPoolDistr = Map.insert poolId stake (mPoolDistr m)
              }
        )
      pure poolId
    genPoolParams _poolId = do
      _poolVrf <- lift arbitrary
      _poolPledge <- lift genPositiveVal
      _poolCost <- lift genPositiveVal
      _poolMargin <- lift arbitrary
      _poolRAcnt <- RewardAcnt Testnet <$> genCredential Rewrd
      let _poolOwners = mempty
      let _poolRelays = mempty
      let _poolMD = SNothing
      pure PoolParams {..}

-- Adds to both gsKeys and gsScripts and gsPlutusScript
-- via genKeyHash and genScript

-- | Generate a credential that can be used for supplied purpose (in case of
-- plutus scripts), while occasionally picking out randomly from previously
-- generated set.
genCredential :: Reflect era => Tag -> GenRS era (Credential kr (Crypto era))
genCredential tag =
  frequencyT
    [ (35, KeyHashObj <$> genKeyHash),
      (35, ScriptHashObj <$> genScript reify tag),
      (10, pickExistingKeyHash),
      (20, pickExistingScript)
    ]
  where
    pickExistingKeyHash =
      KeyHashObj <$> do
        keysMap <- gsKeys <$> get
        lift (genMapElem keysMap) >>= \case
          Just (k, _) -> pure $ coerceKeyRole k
          Nothing -> genKeyHash
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
      timelockScriptsMap <- gsScripts <$> get
      lift (genMapElem timelockScriptsMap) >>= \case
        Just (h, _) -> pure h
        Nothing -> genScript reify tag

-- ===========================================================
-- Generate Era agnostic Scripts

-- Adds to gsScripts and gsPlutusScripts
genScript :: forall era. Reflect era => Proof era -> Tag -> GenRS era (ScriptHash (Crypto era))
genScript proof tag = case proof of
  Babbage _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Alonzo _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Mary _ -> genTimelockScript proof
  Allegra _ -> genTimelockScript proof
  Shelley _ -> genMultiSigScript proof

-- Adds to gsScripts
genTimelockScript :: forall era. Reflect era => Proof era -> GenRS era (ScriptHash (Crypto era))
genTimelockScript proof = do
  GenEnv {geValidityInterval = ValidityInterval mBefore mAfter} <- ask
  -- We need to limit how deep these timelocks can go, otherwise this generator will
  -- diverge. It also has to stay very shallow because it grows too fast.
  let genNestedTimelock k
        | k > 0 =
            elementsT $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = elementsT nonRecTimelocks
      nonRecTimelocks :: [GenRS era (Timelock (Crypto era))]
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
  let corescript :: Core.Script era
      corescript = case proof of
        Babbage _ -> TimelockScript tlscript
        Alonzo _ -> TimelockScript tlscript
        Mary _ -> tlscript
        Allegra _ -> tlscript
        Shelley _ -> error "Shelley does not have TimeLock scripts"
  let scriptHash = hashScript @era corescript
  modify $ \ts@GenState {gsScripts} -> ts {gsScripts = Map.insert scriptHash corescript gsScripts}
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
      requireSignature = Shelley.RequireSignature <$> genKeyHash
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
  let corescript :: Core.Script era
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

  let corescript :: Core.Script era
      corescript = case proof of
        Alonzo _ -> script
        Babbage _ -> script
        _ -> error ("Only Alonzo and Babbage have PlutusScripts. " ++ show proof ++ " does not.")
      scriptHash = hashScript @era corescript
  modify $ \ts@GenState {gsPlutusScripts} ->
    ts {gsPlutusScripts = Map.insert (scriptHash, tag) (IsValid isValid, corescript) gsPlutusScripts}
  pure scriptHash

-- =================================================================

pcGenState :: Reflect era => Proof era -> GenState era -> PDoc
pcGenState proof (GenState keys scripts plutus dats model iutxo irew ipoolp ipoold irkey prf _genenv) =
  ppRecord
    "GenState Summary"
    [ ("Keymap", ppInt (Map.size keys)),
      ("Scriptmap", ppInt (Map.size scripts)),
      ("PlutusScripts", ppInt (Map.size plutus)),
      ("Datums", ppInt (Map.size dats)),
      ("Model", pcModelNewEpochState proof model),
      ("Initial Utxo", ppMap pcTxIn (pcTxOut proof) iutxo),
      ("Initial Rewards", ppMap pcCredential pcCoin irew),
      ("Initial PoolParams", ppMap pcKeyHash pcPoolParams ipoolp),
      ("Initial PoolDistr", ppMap pcKeyHash pcIndividualPoolStake ipoold),
      ("Previous RegKey", ppSet pcCredential irkey),
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
    utxostate = smartUTxOState (UTxO (gsInitialUtxo gstate)) deposited (Coin 0) (pPUPStateZero @era)
    dpstate = DPState dstate pstate
    dstate =
      DState
        (UMap.unify (gsInitialRewards gstate) Map.empty Map.empty)
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
