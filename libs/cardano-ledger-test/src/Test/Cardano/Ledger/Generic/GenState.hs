{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Strategy for Generic Tests
--   Pre-generate a bunch of inter-related things using a state Monad over a set of Maps
--   and Sets that remember the inter-relation ships. We then extract this state, and it
--   becomes the environment of the regular generators. We make these Maps and Set big
--   enough that when we need something that has one of these relationships, we randomly
--   choose from the Maps and the Sets in the environment, knowing that we can find the
--   related item(s) when it is needed.
module Test.Cardano.Ledger.Generic.GenState where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data, DataHash, hashData)
import Cardano.Ledger.Alonzo.Scripts hiding (Mint)
import qualified Cardano.Ledger.Alonzo.Scripts as Tag
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (Network (Testnet), ProtVer (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (..), ValidateScript (hashScript))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys
  ( KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    coerceKeyRole,
    hashKey,
  )
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Pretty (PDoc, ppDPState, ppMap, ppPair, ppRecord)
import Cardano.Ledger.Pretty.Alonzo (ppIsValid, ppTag)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    PState (..),
    RewardAccounts,
    rewards,
  )
import qualified Cardano.Ledger.Shelley.Scripts as Shelley (MultiSig (..))
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (join, replicateM, replicateM_)
import qualified Control.Monad.State.Strict as MS (MonadState (..), modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWST (..), ask, get, modify, runRWST)
import Data.Default.Class (Default (def))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import qualified Data.Sequence.Strict as Seq
import qualified Data.UMap as UM
import Numeric.Natural
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Generic.Fields
import Test.Cardano.Ledger.Generic.Functions
import Test.Cardano.Ledger.Generic.PrettyCore
  ( dataHashSummary,
    dataSummary,
    keyHashSummary,
    keyPairSummary,
    scriptHashSummary,
    scriptSummary,
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
  { numKeys :: Int,
    numScripts :: Int,
    numDatums :: Int,
    numRewards :: Int,
    numPools :: Int
  }

data GenEnv era = GenEnv
  { geValidityInterval :: ValidityInterval,
    gePParams :: Core.PParams era
  }

data GenState era = GenState
  { gsKeys :: Map (KeyHash 'Witness (Crypto era)) (KeyPair 'Witness (Crypto era)),
    gsScripts :: Map (ScriptHash (Crypto era)) (Core.Script era),
    gsPlutusScripts :: Map (ScriptHash (Crypto era), Tag) (IsValid, Core.Script era),
    gsDatums :: Map (DataHash (Crypto era)) (Data era),
    gsDPState :: DPState (Crypto era)
  }

emptyGenState :: GenState era
emptyGenState = GenState mempty mempty mempty mempty def

instance Default GenSize where
  def = GenSize {numKeys = 2, numScripts = 2, numDatums = 5, numRewards = 5, numPools = 1}

-- ========================================================================
-- Tools to get started

initState :: Reflect era => Proof era -> GenSize -> GenRS era (GenState era)
initState proof gs = do
  replicateM_ (numKeys gs) genKeyHash
  replicateM_ (numScripts gs) $ sequence_ [genScript proof i | i <- [Spend, Tag.Mint, Cert, Rewrd]]
  replicateM_ (numDatums gs) genDatumWithHash
  replicateM_ (numRewards gs) genRewards
  replicateM_ (numPools gs) genPool
  get

startGen :: Reflect era => Proof era -> GenSize -> Gen (GenEnv era, GenState era)
startGen proof gsize = do
  env <- genGenEnv proof
  (state, _, _) <- runRWST (initState proof gsize) env emptyGenState
  pure (env, state)

startGenRS :: Reflect era => Proof era -> GenSize -> GenRS era (GenEnv era, GenState era)
startGenRS proof gsize = lift $ startGen proof gsize

-- | Helper function for debugging the state generator inside ghci
viewGenState :: Reflect era => Proof era -> IO ()
viewGenState proof = do
  (_, st) <- generate (startGen proof def)
  putStrLn (show (ppGenState proof st))

-- ===========================================================

-- | Generate a random, well-formed, GenEnv
genGenEnv :: Proof era -> Gen (GenEnv era)
genGenEnv proof = do
  maxTxExUnits <- arbitrary :: Gen ExUnits
  Positive maxCollateralInputs <- arbitrary :: Gen (Positive Natural)
  collateralPercentage <- fromIntegral <$> chooseInt (1, 10000) :: Gen Natural
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
            ProtocolVersion $ ProtVer 7 0
          ]
      slotNo = SlotNo 100000000
  minSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (minBound, unSlotNo slotNo))]
  maxSlotNo <- frequency [(1, pure SNothing), (4, SJust <$> choose (unSlotNo slotNo + 1, maxBound))]
  pure $
    GenEnv
      { geValidityInterval = ValidityInterval (SlotNo <$> minSlotNo) (SlotNo <$> maxSlotNo),
        gePParams = pp
      }

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

-- Adds to the gsDPState
genRewards :: Reflect era => GenRS era (RewardAccounts (Crypto era))
genRewards = do
  n <- lift nonNeg1Digit
  newrewards <-
    Map.fromList <$> replicateM n ((,) <$> genCredential Rewrd <*> lift genPositiveVal)
  modifyDState $ \ds -> ds {_unified = rewards ds UM.⨃ newrewards} -- Prefers coins in newrewards
  pure newrewards

-- Adds to PState part of DPSTate
genPool :: Reflect era => GenRS era (KeyHash 'StakePool (Crypto era))
genPool = frequencyT [(10, genNewPool), (90, pickExisting)]
  where
    pickExisting = do
      DPState {_pstate = PState {_pParams}} <- gsDPState <$> get
      lift (genMapElem _pParams) >>= \case
        Nothing -> genNewPool
        Just poolId -> pure $ fst poolId
    genNewPool = do
      poolId <- genKeyHash
      pp <- genPoolParams poolId
      modifyPState $ \ps -> ps {_pParams = Map.insert poolId pp (_pParams ps)}
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
        n <- lift nonNeg1Digit
        RequireAllOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireAnyOf k = do
        n <- lift pos1Digit
        RequireAnyOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireMOf k = do
        n <- lift nonNeg1Digit
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
        n <- lift nonNeg1Digit
        Shelley.RequireAllOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireAnyOf k = do
        n <- lift pos1Digit
        Shelley.RequireAnyOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireMOf k = do
        n <- lift nonNeg1Digit
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

-- ===========================================================
-- The Monad and Era agnostic generators and operators

type GenRS era = RWST (GenEnv era) () (GenState era) Gen

genMapElem :: Map k a -> Gen (Maybe (k, a))
genMapElem m
  | n == 0 = pure Nothing
  | otherwise = do
      i <- choose (0, n - 1)
      pure $ Just $ Map.elemAt i m
  where
    n = Map.size m

elementsT :: (Monad (t Gen), MonadTrans t) => [t Gen b] -> t Gen b
elementsT = join . lift . elements

frequencyT :: (Monad (t Gen), MonadTrans t) => [(Int, t Gen b)] -> t Gen b
frequencyT = join . lift . frequency . map (pure <$>)

-- | Gen a positive single digit Int, on a skewed distribution that
--   favors 2,3,4,5 but occasionally gets others
pos1Digit :: Gen Int
pos1Digit = frequency (map f [(1, 1), (7, 2), (6, 3), (5, 4), (3, 5), (2, 6), (1, 7), (1, 8), (1, 9)])
  where
    f (x, y) = (x, pure y)

-- | Gen a non-negative single digit Int, on a skewed distribution that
--   favors 2,3,4,5 but occasionally gets others
nonNeg1Digit :: Gen Int
nonNeg1Digit = frequency (map f [(1, 0), (2, 1), (7, 2), (6, 3), (5, 4), (3, 5), (2, 6), (1, 7), (1, 8), (1, 9)])
  where
    f (x, y) = (x, pure y)

-- | Generate a non-zero value
genPositiveVal :: Val v => Gen v
genPositiveVal = inject . Coin . getPositive <$> arbitrary

modifyDState :: (MS.MonadState (GenState era) m) => (DState (Crypto era) -> DState (Crypto era)) -> m ()
modifyDState f =
  modifyDPState $ \dp@DPState {_dstate = ds} -> dp {_dstate = f ds}

modifyDPState :: (MS.MonadState (GenState era) m) => (DPState (Crypto era) -> DPState (Crypto era)) -> m ()
modifyDPState f =
  MS.modify $ \s@GenState {gsDPState = dps} -> s {gsDPState = f dps}

modifyPState :: (MS.MonadState (GenState era) m) => (PState (Crypto era) -> PState (Crypto era)) -> m ()
modifyPState f =
  modifyDPState $ \dp@DPState {_pstate = ps} -> dp {_pstate = f ps}

-- ========================================================================

deriving instance CC.Crypto c => Show (GenState (BabbageEra c))

deriving instance CC.Crypto c => Show (GenState (AlonzoEra c))

deriving instance CC.Crypto c => Show (GenState (MaryEra c))

deriving instance CC.Crypto c => Show (GenState (AllegraEra c))

deriving instance CC.Crypto c => Show (GenState (ShelleyEra c))

ppGenState :: CC.Crypto (Crypto era) => Proof era -> GenState era -> PDoc
ppGenState proof (GenState keys scripts plutus dats dp) =
  ppRecord
    "GenState"
    [ ("Keymap", ppMap keyHashSummary keyPairSummary keys),
      ("Scriptmap", ppMap scriptHashSummary (scriptSummary proof) scripts),
      ( "PlutusScripts",
        ppMap
          (ppPair scriptHashSummary ppTag)
          (ppPair ppIsValid (scriptSummary proof))
          plutus
      ),
      ("Datums", ppMap dataHashSummary dataSummary dats),
      ("DPState", ppDPState dp)
    ]
