{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Strategy for Generic Tests
--   Make the GenState include a Mode of the NewEpochState, modify
--   the ModelNewEpochState to reflect what we generated.
module Test.Cardano.Ledger.Generic.GenState (
  EraGenericGen (..),
  GenEnv (..),
  GenRS,
  GenState (..),
  GenSize (..),
  defaultGenSize,
  PlutusPurposeTag (..),
  plutusPurposeTags,
  elementsT, -- TODO move to a utilities module
  frequencyT, -- TODO move to a utilities module
  positiveSingleDigitInt,
  nonNegativeSingleDigitInt,
  genSetElem,
  genMapElem,
  genMapElemWhere,
  genRewardVal,
  genPositiveVal,
  genGenState,
  genGenEnv,
  genValidityInterval,
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
  initialLedgerState,
  modifyModel,
  runGenRS,
  small,
  genDatumWithHash,
  genKeyHash,
  genScript,
  genFreshKeyHash,
  genCredential,
  genFreshCredential,
  genFreshRegCred,
  genPool,
  genPoolParams,
  genRewards,
  genNewPool,
  genRetirementHash,
  initStableFields,
  modifyGenStateInitialUtxo,
  modifyGenStateInitialAccounts,
  modifyModelCount,
  modifyModelIndex,
  modifyModelUTxO,
  modifyModelMutFee,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  Timelock (..),
  ValidityInterval (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Scripts hiding (Script)
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptIntegrityHash)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (Network (Testnet), inject)
import Cardano.Ledger.Coin (Coin (..), compactCoinOrError)
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj), StakeCredential)
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  smartUTxOState,
  totalObligation,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxId, TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM, join, replicateM, zipWithM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWST (..), ask, asks, get, gets, modify)
import Data.Default (Default (def))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import qualified Data.Sequence.Strict as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff (Expr, ToExpr (toExpr))
import GHC.Generics (Generic)
import GHC.Word (Word64)
import Lens.Micro
import Numeric.Natural
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Examples.STSTestUtils (EraModel (..), PlutusPurposeTag (..))
import Test.Cardano.Ledger.Generic.Functions (
  alwaysFalse,
  alwaysTrue,
 )
import Test.Cardano.Ledger.Generic.ModelState (
  ModelNewEpochState (..),
  genDelegsZero,
  instantaneousRewardsZero,
  mNewEpochStateZero,
 )
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Shelley.Era
import Test.QuickCheck (
  Gen,
  Positive (..),
  arbitrary,
  choose,
  elements,
  frequency,
 )

class (EraTest era, Reflect era, EraModel era) => EraGenericGen era where
  setValidity :: ValidityInterval -> TxBody era -> TxBody era

  setReferenceInputs :: Set TxIn -> TxBody era -> TxBody era

  setCollateralInputs :: Set TxIn -> TxBody era -> TxBody era

  setTotalCollateral :: StrictMaybe Coin -> TxBody era -> TxBody era

  setCollateralReturn :: StrictMaybe (TxOut era) -> TxBody era -> TxBody era

  addRedeemers :: Redeemers era -> TxWits era -> TxWits era

  setScriptIntegrityHash :: StrictMaybe ScriptIntegrityHash -> TxBody era -> TxBody era

  setNetworkIdTxBody :: StrictMaybe Network -> TxBody era -> TxBody era

  genExUnits :: Int -> GenRS era [ExUnits]

  genPParams :: GenSize -> Gen (PParams era)

  -- Era generic "lenses" for testing

  ppMaxCollateralInputsT :: Lens' (PParams era) Natural

  ppCollateralPercentageT :: Lens' (PParams era) Natural

  ppCostModelsT :: Lens' (PParams era) CostModels

  ppMaxTxExUnitsT :: Lens' (PParams era) ExUnits

  ppMaxBlockExUnitsT :: Lens' (PParams era) ExUnits

  ppMaxValSizeT :: Lens' (PParams era) Natural

  -- Utils

  mkScriptIntegrityHash :: PParams era -> [Language] -> TxWits era -> StrictMaybe ScriptIntegrityHash

-- =================================================

-- | Constants that determine how big a GenState is generated.
data GenSize = GenSize
  { treasury :: !Integer
  , reserves :: !Integer
  , startSlot :: !Word64
  , slotDelta :: !(Word64, Word64)
  , blocksizeMax :: !Integer
  , collInputsMax :: !Natural
  , spendInputsMax :: !Int
  , refInputsMax :: !Int
  , utxoChoicesMax :: !Int
  , certificateMax :: !Int
  , withdrawalMax :: !Int
  , oldUtxoPercent :: !Int -- between 0-100, 10 means pick an old UTxO 10% of the time
  , maxStablePools :: !Int
  , invalidScriptFreq :: !Int -- percentage
  , regCertFreq :: !Int
  , delegCertFreq :: !Int
  }
  deriving (Show, Generic)

instance ToExpr GenSize

defaultGenSize :: GenSize
defaultGenSize =
  GenSize
    { treasury = 1000000
    , reserves = 1000000
    , startSlot = 0
    , slotDelta = (3, 7)
    , blocksizeMax = 10
    , collInputsMax = 5
    , oldUtxoPercent = 15
    , spendInputsMax = 10
    , refInputsMax = 6
    , utxoChoicesMax = 30
    , certificateMax = 10
    , withdrawalMax = 10
    , maxStablePools = 5
    , invalidScriptFreq = 5
    , regCertFreq = 75
    , delegCertFreq = 50
    }

data GenEnv era = GenEnv
  { gePParams :: PParams era
  , geSize :: GenSize
  }
  deriving (Generic)

data GenState era = GenState
  { gsValidityInterval :: !ValidityInterval
  , gsKeys :: !(Map (KeyHash 'Witness) (KeyPair 'Witness))
  , gsScripts :: !(Map ScriptHash (Script era))
  , gsPlutusScripts :: !(Map (ScriptHash, PlutusPurposeTag) (IsValid, Script era))
  , gsDatums :: !(Map DataHash (Data era))
  , gsVI :: !(Map ValidityInterval (Set ScriptHash))
  , gsModel :: !(ModelNewEpochState era)
  , gsInitialUtxo :: !(Map TxIn (TxOut era))
  , gsInitialAccounts :: !(Map (Credential 'Staking) (AccountState era))
  , gsInitialPoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  , gsInitialPoolDistr ::
      !(Map (KeyHash 'StakePool) IndividualPoolStake)
  , -- Stable fields are stable from initialization to the end of the generation process
    gsStablePools :: !(Set (KeyHash 'StakePool))
  , gsStableDelegators :: !(Set StakeCredential)
  , gsAvoidCred :: !(Set (Credential 'Staking))
  , gsAvoidKey :: !(Set (KeyHash 'StakePool))
  , gsGenEnv :: !(GenEnv era)
  , gsSeedIdx :: !Int
  }
  deriving (Generic)

instance EraTest era => ToExpr (GenEnv era)

emptyGenState :: Reflect era => GenEnv era -> GenState era
emptyGenState genv =
  GenState
    (ValidityInterval SNothing SNothing)
    mempty
    mempty
    mempty
    mempty
    mempty
    (mNewEpochStateZero {mPParams = gePParams genv})
    Map.empty
    Map.empty
    Map.empty
    Map.empty
    Set.empty
    Set.empty
    Set.empty
    Set.empty
    genv
    0
{-# NOINLINE emptyGenState #-}

small :: GenSize
small =
  GenSize
    { treasury = 1000000
    , reserves = 1000000
    , startSlot = 0
    , slotDelta = (2, 5)
    , blocksizeMax = 3
    , collInputsMax = 2
    , oldUtxoPercent = 5
    , spendInputsMax = 3
    , refInputsMax = 1
    , utxoChoicesMax = 12
    , certificateMax = 2
    , withdrawalMax = 2
    , maxStablePools = 4
    , invalidScriptFreq = 5
    , regCertFreq = 75
    , delegCertFreq = 50
    }

plutusPurposeTags :: Proof era -> [PlutusPurposeTag]
plutusPurposeTags = \case
  Shelley {} -> []
  Allegra {} -> []
  Mary {} -> []
  Alonzo {} -> [Spending .. Rewarding]
  Babbage {} -> [Spending .. Rewarding]
  Conway {} -> [Spending .. Proposing]

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

-- ========================================================
-- Modifying fields of the GenState as side effects in GenRS

setVi :: GenState era -> ValidityInterval -> GenState era
setVi gs vi = gs {gsValidityInterval = vi}
{-# NOINLINE setVi #-}

modifyGenStateKeys ::
  ( Map.Map (KeyHash 'Witness) (KeyPair 'Witness) ->
    Map.Map (KeyHash 'Witness) (KeyPair 'Witness)
  ) ->
  GenRS era ()
modifyGenStateKeys f = modify (\x -> x {gsKeys = f (gsKeys x)})

modifyGenStateDatums ::
  (Map.Map DataHash (Data era) -> Map.Map DataHash (Data era)) ->
  GenRS era ()
modifyGenStateDatums f = modify (\x -> x {gsDatums = f (gsDatums x)})

modifyGenStateVI ::
  ( Map ValidityInterval (Set ScriptHash) ->
    Map ValidityInterval (Set ScriptHash)
  ) ->
  GenRS era ()
modifyGenStateVI f = modify (\x -> x {gsVI = f (gsVI x)})

modifyGenStateInitialAccounts ::
  ( Map.Map (Credential 'Staking) (AccountState era) ->
    Map.Map (Credential 'Staking) (AccountState era)
  ) ->
  GenRS era ()
modifyGenStateInitialAccounts f = modify $ \st -> st {gsInitialAccounts = f (gsInitialAccounts st)}

modifyGenStateInitialUtxo ::
  ( Map TxIn (TxOut era) ->
    Map TxIn (TxOut era)
  ) ->
  GenRS era ()
modifyGenStateInitialUtxo f = modify $ \st -> st {gsInitialUtxo = f (gsInitialUtxo st)}

modifyGenStateAvoidCred ::
  ( Set (Credential 'Staking) ->
    Set (Credential 'Staking)
  ) ->
  GenRS era ()
modifyGenStateAvoidCred f = modify (\st -> st {gsAvoidCred = f (gsAvoidCred st)})

modifyGenStateAvoidKey ::
  (Set (KeyHash 'StakePool) -> Set (KeyHash 'StakePool)) ->
  GenRS era ()
modifyGenStateAvoidKey f = modify (\s -> s {gsAvoidKey = f (gsAvoidKey s)})

modifyGenStateStablePools ::
  (Set (KeyHash 'StakePool) -> Set (KeyHash 'StakePool)) ->
  GenRS era ()
modifyGenStateStablePools f = modify (\gs -> gs {gsStablePools = f (gsStablePools gs)})

modifyGenStateInitialPoolParams ::
  ( Map.Map (KeyHash 'StakePool) PoolParams ->
    Map.Map (KeyHash 'StakePool) PoolParams
  ) ->
  GenRS era ()
modifyGenStateInitialPoolParams f = modify (\gs -> gs {gsInitialPoolParams = f (gsInitialPoolParams gs)})

modifyGenStateInitialPoolDistr ::
  ( Map.Map (KeyHash 'StakePool) IndividualPoolStake ->
    Map.Map (KeyHash 'StakePool) IndividualPoolStake
  ) ->
  GenRS era ()
modifyGenStateInitialPoolDistr f = modify (\gs -> gs {gsInitialPoolDistr = f (gsInitialPoolDistr gs)})

modifyGenStateStableDelegators ::
  (Set StakeCredential -> Set StakeCredential) ->
  GenRS era ()
modifyGenStateStableDelegators f = modify (\gs -> gs {gsStableDelegators = f (gsStableDelegators gs)})

modifyGenStateScripts ::
  ( Map.Map ScriptHash (Script era) ->
    Map.Map ScriptHash (Script era)
  ) ->
  GenRS era ()
modifyGenStateScripts f =
  modify $ \gs -> gs {gsScripts = f (gsScripts gs)}

modifyPlutusScripts ::
  ( Map.Map (ScriptHash, PlutusPurposeTag) (IsValid, Script era) ->
    Map.Map (ScriptHash, PlutusPurposeTag) (IsValid, Script era)
  ) ->
  GenRS era ()
modifyPlutusScripts f = modify (\gs -> gs {gsPlutusScripts = f (gsPlutusScripts gs)})

-- ===================================================
-- functions that modify individual fields of ModelState

modifyModel :: (ModelNewEpochState era -> ModelNewEpochState era) -> GenRS era ()
modifyModel f = modify (\gstate -> gstate {gsModel = f (gsModel gstate)})

modifyModelAccounts :: (Accounts era -> Accounts era) -> GenRS era ()
modifyModelAccounts f = modifyModel (\ms -> ms {mAccounts = f (mAccounts ms)})

modifyModelDeposited :: (Coin -> Coin) -> GenRS era ()
modifyModelDeposited f = modifyModel (\ms -> ms {mDeposited = f (mDeposited ms)})

modifyModelStakePools ::
  ( Map.Map (KeyHash 'StakePool) StakePoolState ->
    Map.Map (KeyHash 'StakePool) StakePoolState
  ) ->
  GenRS era ()
modifyModelStakePools f = modifyModel (\ms -> ms {mStakePools = f (mStakePools ms)})

modifyModelPoolDistr ::
  ( Map (KeyHash 'StakePool) IndividualPoolStake ->
    Map (KeyHash 'StakePool) IndividualPoolStake
  ) ->
  GenRS era ()
modifyModelPoolDistr f = modifyModel (\ms -> ms {mPoolDistr = f (mPoolDistr ms)})

modifyModelCount :: (Int -> Int) -> GenRS era ()
modifyModelCount f = modifyModel (\ms -> ms {mCount = f (mCount ms)})

modifyModelIndex ::
  (Map Int TxId -> Map Int TxId) ->
  GenRS era ()
modifyModelIndex f = modifyModel (\ms -> ms {mIndex = f (mIndex ms)})

modifyModelUTxO ::
  (Map TxIn (TxOut era) -> Map TxIn (TxOut era)) ->
  GenRS era ()
modifyModelUTxO f = modifyModel (\ms -> ms {mUTxO = f (mUTxO ms)})

modifyModelMutFee ::
  ( Map TxIn (TxOut era) ->
    Map TxIn (TxOut era)
  ) ->
  GenRS era ()
modifyModelMutFee f = modifyModel (\m -> m {mMutFee = f (mMutFee m)})

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

-- | Pick a UTxO element where we can use it in a new Tx. Most of the time we generate new
--   elements for each Tx, but once in a while we choose an existing one. We must be carefull
--   that that the Pay credential of the TxOut can run in the curent ValidityInterval
--   A crude but simple way is to insist Pay credential is either Key locked, or locked
--   with Plutus or MultiSig scripts, and return False for any Timelock scripts.
getUtxoElem :: EraModel era => GenRS era (Maybe (TxIn, TxOut era))
getUtxoElem = do
  x <- gets (mUTxO . gsModel)
  scriptmap <- gets gsScripts
  lift $ genMapElemWhere x 20 (\_ -> validTxOut scriptmap)

getUtxoTest :: GenRS era (TxIn -> Bool)
getUtxoTest = do
  x <- gets (mUTxO . gsModel)
  pure (`Map.member` x)

-- | To compute deposits we need a function that tells if the KeyHash is a new Pool
--   Compute this function before we do any generation, since such generation
--   may actually add to the mPoolParams, and then the added thing won't appear new.
getNewPoolTest :: GenRS era (KeyHash 'StakePool -> Bool)
getNewPoolTest = do
  stakePools <- gets (mStakePools . gsModel)
  pure (`Map.member` stakePools)

-- ========================================================================
-- Tools to get started in the Monad

runGenRS ::
  EraGenericGen era =>
  GenSize ->
  GenRS era a ->
  Gen (a, GenState era)
runGenRS gsize action = do
  genenv <- genGenEnv gsize
  (ans, state, ()) <- runRWST action genenv (emptyGenState genenv)
  pure (ans, state)

-- | Generate a random, well-formed, GenEnv
genGenEnv :: forall era. EraGenericGen era => GenSize -> Gen (GenEnv era)
genGenEnv gsize = do
  pp <- genPParams gsize
  pure
    GenEnv
      { geSize = gsize
      , gePParams = pp
      }

genGenState ::
  EraGenericGen era =>
  GenSize ->
  Gen (GenState era)
genGenState gsize = do
  let slotNo = startSlot gsize
  minSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (minBound, slotNo))]
  maxSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (slotNo + 1, maxBound))]
  let vi = ValidityInterval (SlotNo <$> minSlotNo) (SlotNo <$> maxSlotNo)
  env <- genGenEnv gsize
  pure (setVi (emptyGenState env) vi)

-- | Generate a transaction body validity interval which is close in proximity
--  (less than a stability window) from the current slot.
genValidityInterval :: SlotNo -> Gen ValidityInterval
genValidityInterval (SlotNo s) = do
  let stabilityWindow = 29 -- < 3k/f many slots, where 10k is the epoch length
  start <- frequency [(1, pure SNothing), (4, SJust <$> choose (minBound, s))]
  end <- frequency [(1, pure SNothing), (4, SJust <$> choose (s + 1, s + stabilityWindow))]
  pure $ ValidityInterval (SlotNo <$> start) (SlotNo <$> end)

-- =================================================================

pcGenState :: ShelleyEraTest era => GenState era -> Expr
pcGenState = toExpr

instance ShelleyEraTest era => ToExpr (GenState era)

instance (Reflect era, ShelleyEraTest era) => Show (GenState era) where
  show x = show (pcGenState x)

-- =====================================================================
-- Build an Initial LedgerState for a Trace from a GenState, after
-- generating a coherent Trace (a sequence of Transactions that can
-- logically be applied one after another)

initialLedgerState :: forall era. Reflect era => GenState era -> LedgerState era
initialLedgerState gstate = LedgerState utxostate dpstate
  where
    utxostate = smartUTxOState pp (UTxO (gsInitialUtxo gstate)) deposited (Coin 0) emptyGovState mempty
    dpstate =
      def
        & certPStateL .~ pstate
        & certDStateL .~ dstate
    dstate =
      DState
        (accountsFromAccountsMap (gsInitialAccounts gstate))
        Map.empty
        genDelegsZero
        instantaneousRewardsZero
    pstate =
      PState
        Map.empty
        (mkStakePoolState poolDeposit <$> pools)
        Map.empty
        Map.empty
    -- In a wellformed LedgerState the deposited equals the obligation
    deposited = totalObligation dpstate (utxostate ^. utxosGovStateL)
    pools = gsInitialPoolParams gstate
    pp = mPParams (gsModel gstate)
    poolDeposit = pp ^. ppPoolDepositCompactL

-- =============================================
-- Generators of inter-related items

-- Adds to the gsKeys
genKeyHash :: forall kr era. GenRS era (KeyHash kr)
genKeyHash = do
  keyPair <- lift arbitrary
  let keyHash = hashKey $ vKey keyPair
  modifyGenStateKeys (Map.insert keyHash keyPair)
  pure $ coerceKeyRole keyHash

-- Adds to the gsDatums
genDatumWithHash :: Era era => GenRS era (DataHash, Data era)
genDatumWithHash = do
  datum <- lift arbitrary
  let datumHash = hashData datum
  modifyGenStateDatums (Map.insert datumHash datum)
  pure (datumHash, datum)

genFreshKeyHash :: GenRS era (KeyHash kr)
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

-- ===========================================================
-- Generate Era agnostic Scripts

-- Adds to gsScripts and gsPlutusScripts
genScript :: forall era. Reflect era => PlutusPurposeTag -> GenRS era ScriptHash
genScript tag = case reify @era of
  Conway -> elementsT [genTimelockScript, genPlutusScript tag]
  Babbage -> elementsT [genTimelockScript, genPlutusScript tag]
  Alonzo -> elementsT [genTimelockScript, genPlutusScript tag]
  Mary -> genTimelockScript
  Allegra -> genTimelockScript
  Shelley -> genMultiSigScript

-- Adds to gsScripts
genTimelockScript ::
  forall era.
  (AllegraEraScript era, NativeScript era ~ Timelock era) =>
  GenRS era ScriptHash
genTimelockScript = do
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
            [ requireTimeStart <$> mBefore
            , requireTimeExpire <$> mAfter
            , SJust requireSignature
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
  let corescript = fromNativeScript tlscript
  let scriptHash = hashScript @era corescript
      insertOrCreate x Nothing = Just (Set.singleton x)
      insertOrCreate x (Just s) = Just (Set.insert x s)
  modifyGenStateScripts (Map.insert scriptHash corescript)
  modifyGenStateVI (Map.alter (insertOrCreate scriptHash) vi)
  pure scriptHash

-- Adds to gsScripts
genMultiSigScript ::
  forall era.
  (ShelleyEraScript era, NativeScript era ~ MultiSig era) =>
  GenRS era ScriptHash
genMultiSigScript = do
  let genNestedMultiSig k
        | k > 0 =
            elementsT $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = elementsT nonRecTimelocks
      nonRecTimelocks = [requireSignature]
      requireSignature = RequireSignature @era <$> genKeyHash
      requireAllOf k = do
        n <- lift nonNegativeSingleDigitInt
        RequireAllOf . Seq.fromList <$> replicateM n (genNestedMultiSig (k - 1))
      requireAnyOf k = do
        n <- lift positiveSingleDigitInt
        RequireAnyOf . Seq.fromList <$> replicateM n (genNestedMultiSig (k - 1))
      requireMOf k = do
        n <- lift nonNegativeSingleDigitInt
        m <- lift $ choose (0, n)
        RequireMOf m . Seq.fromList <$> replicateM n (genNestedMultiSig (k - 1))
  msscript <- genNestedMultiSig (2 :: Natural)
  let corescript = fromNativeScript msscript
  let scriptHash = hashScript @era corescript
  modifyGenStateScripts (Map.insert scriptHash corescript)
  pure scriptHash

-- Adds to gsPlutusScripts
genPlutusScript ::
  forall era.
  ( Reflect era
  , EraPlutusContext era
  ) =>
  PlutusPurposeTag ->
  GenRS era ScriptHash
genPlutusScript tag = do
  falseFreq <- asks $ invalidScriptFreq . geSize
  isValid <- lift $ frequency [(falseFreq, pure False), (100 - falseFreq, pure True)]
  -- Plutus scripts alwaysSucceeds needs at least numArgs, while
  -- alwaysFails needs exactly numArgs to have the desired affect.
  -- For reasons unknown, this number differs from Alonzo to Babbage
  -- Perhaps because Babbage is using PlutusV2 scripts?
  let numArgs = case (reify @era, tag) of
        (Conway, Spending) -> 2
        (Conway, _) -> 1
        (Babbage, Spending) -> 2
        (Babbage, _) -> 1
        (_, Spending) -> 3
        (_, _) -> 2
  -- While using varying number of arguments for alwaysSucceeds we get
  -- varying script hashes, which helps with the fuzziness
  let mlanguage = case reify @era of
        Conway -> Just PlutusV2
        Babbage -> Just PlutusV2
        Alonzo -> Just PlutusV1
        _ -> Nothing
  script <-
    if isValid
      then alwaysTrue mlanguage . (+ numArgs) <$> lift (elements [0, 1, 2, 3 :: Natural])
      else pure $ alwaysFalse mlanguage numArgs

  let scriptHash = hashScript @era script
  modifyPlutusScripts (Map.insert (scriptHash, tag) (IsValid isValid, script))
  pure scriptHash

-- ======================================================================
-- Generators of Transaction components
-- =======================================================================

-- | Generate a credential that can be used for supplied purpose (in case of
-- plutus scripts), while occasionally picking out randomly from previously
-- generated set. Returns the credential
-- Adds to both gsKeys and gsScripts and gsPlutusScript
-- via genKeyHash and genScript
genCredential ::
  forall kr era. Reflect era => PlutusPurposeTag -> GenRS era (Credential kr)
genCredential tag =
  frequencyT
    [ (35, KeyHashObj <$> genKeyHash')
    , (35, ScriptHashObj <$> genScript')
    , (10, pickExistingKeyHash)
    , (20, pickExistingScript)
    ]
  where
    genNewAccountState = do
      ptr <- lift arbitrary
      deposit <- lift arbitrary
      pure $ mkTestAccountState (Just ptr) deposit Nothing Nothing
    genKeyHash' = do
      kh <- genFreshKeyHash -- We need to avoid some key credentials
      case tag of
        Rewarding -> do
          accountState <- genNewAccountState
          modifyGenStateInitialAccounts (Map.insert (KeyHashObj kh) accountState)
        _ -> pure ()
      return $ coerceKeyRole kh
    genScript' = f (100 :: Int)
      where
        f n
          | n <= 0 = error "Failed to generate a fresh script hash"
          | otherwise = do
              sh <- genScript @era tag
              initialRewards <- gets gsInitialAccounts
              avoidCredentials <- gets gsAvoidCred
              let newcred = ScriptHashObj sh
              if Map.notMember newcred initialRewards && Set.notMember newcred avoidCredentials
                then do
                  case tag of
                    Rewarding -> do
                      accountState <- genNewAccountState
                      modifyGenStateInitialAccounts (Map.insert newcred accountState)
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
        Nothing -> genScript tag
    pickExistingTimelockScript = do
      -- Only pick one if it matches the
      vi <- gets gsValidityInterval -- current ValidityInterval
      vimap <- gets gsVI
      case Map.lookup vi vimap of
        Nothing -> genScript @era tag
        Just s ->
          lift (genSetElem s) >>= \case
            Nothing -> genScript tag
            Just hash -> pure hash

-- Return a fresh credential, one that is not a member of the set 'old'.
-- One gets 'tries' chances to generate a fresh one, before an error is raised.
-- This avoids silent infinite loops.
genFreshCredential ::
  forall era kr.
  Reflect era =>
  Int ->
  PlutusPurposeTag ->
  Set (Credential kr) ->
  GenRS era (Credential kr)
genFreshCredential 0 _tag _old = error "Ran out of tries in genFreshCredential."
genFreshCredential tries0 tag old = go tries0
  where
    go tries = do
      c <- genCredential tag
      if Set.member c old
        then go (tries - 1)
        else pure c

genFreshRegCred ::
  Reflect era => PlutusPurposeTag -> GenRS era (Credential 'Staking)
genFreshRegCred tag = do
  old <- gets (Map.keysSet . gsInitialAccounts)
  avoid <- gets gsAvoidCred
  rewards <- gets $ Map.keysSet . (^. accountsMapL) . mAccounts . gsModel
  cred <- genFreshCredential 100 tag $ old <> avoid <> rewards
  modifyGenStateAvoidCred (Set.insert cred)
  pure cred

genPoolParams ::
  Reflect era =>
  KeyHash 'StakePool ->
  GenRS era PoolParams
genPoolParams ppId = do
  ppVrf <- lift arbitrary
  ppPledge <- lift genPositiveVal
  ppCost <- lift genPositiveVal
  ppMargin <- lift arbitrary
  ppRewardAccount <- RewardAccount Testnet <$> genFreshRegCred Rewarding
  let ppOwners = mempty
  let ppRelays = mempty
  let ppMetadata = SNothing
  let ppDefaultVote = SNothing
  pure PoolParams {..}

-- | Generate a 'n' fresh credentials (ones not in the set 'old'). We get 'tries' chances,
--   if it doesn't work in 'tries' attempts then quit with an error. Better to raise an error
--   than go into an infinite loop.
genFreshCredentials ::
  forall era kr.
  Reflect era =>
  Int ->
  Int ->
  PlutusPurposeTag ->
  Set (Credential kr) ->
  [Credential kr] ->
  GenRS era [Credential kr]
genFreshCredentials _n 0 _tag _old _ans = error "Ran out of tries in genFreshCredentials."
genFreshCredentials n0 tries tag old0 ans0 = go n0 old0 ans0
  where
    go 0 _ ans = pure ans
    go n old ans = do
      c <- genFreshCredential tries tag old
      go (n - 1) (Set.insert c old) (c : ans)

-- | Use this function to get a new pool that should not be used in the future transactions
genNewPool ::
  forall era.
  Reflect era =>
  GenRS
    era
    ( KeyHash 'StakePool
    , PoolParams
    , IndividualPoolStake
    )
genNewPool = do
  poolId <- genFreshKeyHash
  poolParam <- genPoolParams poolId
  percent <- lift $ choose (0, 1 :: Float)
  let stake = IndividualPoolStake (toRational percent) mempty (ppVrf poolParam)
  modifyGenStateAvoidKey (Set.insert (coerceKeyRole poolId))
  pure (poolId, poolParam, stake)

-- | Initialize (or overwrite if they are not empty) the Stable fields. It is
--   intended that this be called just once at the beginning of a trace generation.
initStableFields :: forall era. Reflect era => GenRS era ()
initStableFields = do
  GenEnv {geSize} <- ask
  hashes <- replicateM (maxStablePools geSize) $ do
    pp <- asks gePParams
    (kh, poolParams, ips) <- genNewPool
    modifyGenStateStablePools (Set.insert kh)
    modifyGenStateInitialPoolParams (Map.insert kh poolParams)
    modifyGenStateInitialPoolDistr (Map.insert kh ips)
    modifyModelStakePools (Map.insert kh $ mkStakePoolState (pp ^. ppPoolDepositCompactL) poolParams)
    return kh

  -- This incantation gets a list of fresh (not previously generated) Credential
  credentials <- replicateM (maxStablePools geSize) $ do
    old' <- gets (Map.keysSet . gsInitialAccounts)
    prev <- gets gsAvoidCred
    cred <- genFreshCredential 100 Rewarding (Set.union old' prev)
    return cred
  let registerNewAccount' cred poolId = do
        registerNewAccount cred (Just poolId)
  zipWithM_ registerNewAccount' credentials hashes
  modifyGenStateStableDelegators (Set.union (Set.fromList credentials))

registerNewAccount ::
  EraTest era => Credential 'Staking -> Maybe (KeyHash 'StakePool) -> GenRS era ()
registerNewAccount cred mPoolId = do
  pp <- asks gePParams
  let deposit = pp ^. ppKeyDepositL
  ptr <- lift arbitrary
  modifyModelAccounts $
    registerTestAccount cred (Just ptr) (compactCoinOrError deposit) mPoolId Nothing
  accountState <- fromJust . lookupAccountState cred . mAccounts . gsModel <$> get
  modifyModelDeposited (<+> deposit)
  modifyGenStateInitialAccounts (Map.insert cred accountState)

-- =============================================
-- Generators of inter-related items

-- Adds to the rewards of the ModelNewEpochState. This used exclusively to generate Withdrawals, so
-- we mark these as ones to avoid in the future. Especialy when generating DeRegKey.
genRewards :: Reflect era => GenRS era (Map (Credential 'Staking) Coin)
genRewards = do
  wmax <- gets (withdrawalMax . geSize . gsGenEnv)
  n <- lift $ choose (1, wmax)
  -- we need a fresh credential, one that was not previously
  -- generated here, or one that arose from gsAvoidCred (i.e. prev)
  old <- gets (Map.keysSet . gsInitialAccounts)
  prev <- gets gsAvoidCred
  credentials <- genFreshCredentials n 100 Rewarding (Set.union old prev) []
  balances <- forM credentials $ \cred -> do
    registerNewAccount cred Nothing
    (,) cred <$> lift genRewardVal
  let balanceMap = Map.fromList balances
      compactBalanceMap = Map.map compactCoinOrError balanceMap
      replaceBalances acc =
        Map.foldrWithKey' (\cred b -> Map.adjust (balanceAccountStateL .~ b) cred) acc compactBalanceMap
  modifyModelAccounts (addToBalanceAccounts compactBalanceMap)
  modifyGenStateInitialAccounts replaceBalances
  modifyGenStateAvoidCred (Set.union (Map.keysSet balanceMap))
  pure balanceMap

genRetirementHash :: forall era. Reflect era => GenRS era (KeyHash 'StakePool)
genRetirementHash = do
  m <- gets (mStakePools . gsModel)
  pp <- gets (mPParams . gsModel)
  honestKhs <- gets gsStablePools
  avoidKey <- gets gsAvoidKey
  res <- lift . genMapElemWhere m 10 $ \kh _ ->
    kh `Set.notMember` honestKhs && kh `Set.notMember` avoidKey
  case res of
    Just x -> do
      modifyGenStateAvoidKey (Set.insert (fst x))
      -- if it is retiring, we should probably avoid it in the future
      pure $ fst x
    Nothing -> do
      (poolid, poolparams, stake) <- genNewPool

      -- add the Pool to the initial state
      modifyGenStateInitialPoolParams (Map.insert poolid poolparams)
      modifyGenStateInitialPoolDistr (Map.insert poolid stake)

      -- add the Pool to the Model
      modifyModelStakePools
        (Map.insert poolid $ mkStakePoolState (pp ^. ppPoolDepositCompactL) poolparams)
      modifyModelPoolDistr (Map.insert poolid stake)
      pure poolid

-- Adds to the mPoolParams and the  mPoolDistr of the Model, and the initial set of objects for Traces
genPool ::
  forall era.
  Reflect era =>
  GenRS era (KeyHash 'StakePool, PoolParams)
genPool = frequencyT [(10, genNew), (90, pickExisting)]
  where
    genNew = do
      (kh, pp, ips) <- genNewPool
      pparams <- gets (mPParams . gsModel)
      -- add pool to initial state
      modifyGenStateInitialPoolParams (Map.insert kh pp)
      modifyGenStateInitialPoolDistr (Map.insert kh ips)
      -- update the model
      modifyModelStakePools (Map.insert kh $ mkStakePoolState (pparams ^. ppPoolDepositCompactL) pp)
      return (kh, pp)
    pickExisting = do
      psStakePools <- gets (mStakePools . gsModel)
      avoidKey <- gets gsAvoidKey
      lift (genMapElemWhere psStakePools 10 (\kh _ -> kh `Set.notMember` avoidKey)) >>= \case
        Nothing -> genNew
        Just (kh, pp) -> pure (kh, stakePoolStateToPoolParams kh pp)
