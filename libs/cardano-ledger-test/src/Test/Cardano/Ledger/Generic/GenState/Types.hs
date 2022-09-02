{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Strategy for Generic Tests
--   Make the GenState include a Mode of the NewEpochState, modify
--   the ModelNewEpochState to reflect what we generated.
module Test.Cardano.Ledger.Generic.GenState.Types
  ( GenEnv (..),
    GenRS,
    GenState (..),
    GenSize (..),
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
    viewGenState,
    initialLedgerState,
    modifyModel,
    runGenRS,
    ioGenRS,
    small,
  )
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data (Data (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (PlutusScript), ExUnits, Tag)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj), StakeCredential)
import Cardano.Ledger.Keys
  ( KeyHash (..),
    KeyPair (..),
    KeyRole (..),
  )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Pretty (PDoc, ppInt, ppMap, ppRecord, ppSet, ppString)
import Cardano.Ledger.Pretty.Mary (ppValidityInterval)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    LedgerState (..),
    PState (..),
    smartUTxOState,
  )
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (join, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWST (..), gets, modify)
import Data.Default.Class (Default (def))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.UMap as UMap
import GHC.Word (Word64)
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Generic.Fields
import Test.Cardano.Ledger.Generic.Functions
  ( obligation',
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
import Test.Cardano.Ledger.Generic.Proof
  ( BabbageEra,
    Evidence (Mock),
    Mock,
    Proof (Alonzo, Babbage, Conway, Shelley),
    Reflect (reify),
  )
import Test.Cardano.Ledger.Generic.Updaters (defaultCostModels, newPParams)
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
    gsKeys :: !(Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))),
    gsScripts :: !(Map (ScriptHash (EraCrypto era)) (Script era)),
    gsPlutusScripts :: !(Map (ScriptHash (EraCrypto era), Tag) (IsValid, Script era)),
    gsDatums :: !(Map (DataHash (EraCrypto era)) (Data era)),
    gsVI :: !(Map ValidityInterval (Set (ScriptHash (EraCrypto era)))),
    gsModel :: !(ModelNewEpochState era),
    gsInitialUtxo :: !(Map (TxIn (EraCrypto era)) (TxOut era)),
    gsInitialRewards :: !(Map (Credential 'Staking (EraCrypto era)) Coin),
    gsInitialDelegations :: !(Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))),
    gsInitialPoolParams :: !(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era))),
    gsInitialPoolDistr :: !(Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era))),
    -- Stable fields are stable from initialization to the end of the generation process
    gsStablePools :: !(Set (KeyHash 'StakePool (EraCrypto era))),
    gsStableDelegators :: !(Set (StakeCredential (EraCrypto era))),
    gsAvoidCred :: !(Set (Credential 'Staking (EraCrypto era))),
    gsAvoidKey :: !(Set (KeyHash 'StakePool (EraCrypto era))),
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
getUtxoElem :: Reflect era => GenRS era (Maybe (TxIn (EraCrypto era), TxOut era))
getUtxoElem = do
  x <- gets (mUTxO . gsModel)
  scriptmap <- gets gsScripts
  lift $ genMapElemWhere x 20 (validTxOut reify scriptmap)

-- | Test if the Payment part of the Address in the TxOut
--   is valid in the current ValidityInterval. Using the simple rule allowing
--   only (Key or Plutus or MutiSig) locking. Disallowing all Timelock scripts
validTxOut ::
  Proof era ->
  Map (ScriptHash (EraCrypto era)) (Script era) ->
  TxIn (EraCrypto era) ->
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

getUtxoTest :: GenRS era (TxIn (EraCrypto era) -> Bool)
getUtxoTest = do
  x <- gets (mUTxO . gsModel)
  pure (\k -> maybe False (const True) (Map.lookup k x))

-- | To compute deposits we need a function that tells if the KeyHash is a new Pool
--   Compute this function before we do any generation, since such generation
--   may actually add to the mPoolParams, and then the added thing won't appear new.
getNewPoolTest :: GenRS era (KeyHash 'StakePool (EraCrypto era) -> Bool)
getNewPoolTest = do
  poolparams <- gets (mPoolParams . gsModel)
  pure (`Map.member` poolparams)

-- ========================================================================
-- Tools to get started

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

-- | Generate a transaction body validity interval which is close in proximity
--  (less than a stability window) from the current slot.
genValidityInterval :: SlotNo -> Gen ValidityInterval
genValidityInterval (SlotNo s) = do
  let stabilityWindow = 29 -- < 3k/f many slots, where 10k is the epoch length
  start <- frequency [(1, pure SNothing), (4, SJust <$> choose (minBound, s))]
  end <- frequency [(1, pure SNothing), (4, SJust <$> choose (s + 1, s + stabilityWindow))]
  pure $ ValidityInterval (SlotNo <$> start) (SlotNo <$> end)

-- | Helper function for development and debugging in ghci
viewGenState :: Reflect era => Proof era -> GenSize -> Bool -> IO ()
viewGenState proof gsize verbose = do
  st <- generate (genGenState proof gsize)
  when verbose $ print (pcGenState proof st)

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
