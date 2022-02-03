{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Cardano.Ledger.Generic.Properties where

import Cardano.Ledger.Alonzo (AlonzoEra, Value)
import Cardano.Ledger.Alonzo.Data (Data, DataHash, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..), nonNativeLanguages)
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeeded)
import Cardano.Ledger.Alonzo.Rules.Ledger (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx
  ( IsValid (..),
    ScriptPurpose (..),
    ValidatedTx (..),
    hashScriptIntegrity,
    ScriptIntegrityHash,
    minfee,
  )
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    Redeemers (..),
    TxDats (..),
    TxWitness (..),
  )
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    strictMaybeToMaybe,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody, ScriptHash (..))
import Cardano.Ledger.Keys
  ( GenDelegs (..),
    KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    coerceKeyRole,
    hashKey,
  )
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.API
  ( Addr (..),
    Credential (..),
    LedgerEnv (LedgerEnv),
    RewardAcnt (..),
    StakeReference (..),
    TxIn (..),
    UTxO (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    PState (..),
    RewardAccounts,
    UTxOState (..),
    rewards,
  )
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.Tx (hashScript)
import Cardano.Ledger.Shelley.TxBody (DCert (..), DelegCert (..), Delegation (..), PoolParams (..))
import Cardano.Ledger.Shelley.UTxO (balance, makeWitnessVKey)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.Val
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM, join, replicateM)
import Control.Monad.State.Strict (MonadState (..), modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWST (..), ask)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Bifunctor (first)
import Data.Coerce
import qualified Data.Compact.SplitMap as SplitMap
import Data.Default.Class (Default (def))
import qualified Data.Foldable as F
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Monoid (All (..))
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UMap (View (Rewards))
import qualified Data.UMap as UM
import GHC.Stack
import Numeric.Natural
import Plutus.V1.Ledger.Api (defaultCostModelParams)
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Generator.Core (genNatural)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- =================================

import Test.Cardano.Ledger.Generic.Proof hiding(lift,Good)
import Test.Cardano.Ledger.Generic.Indexed(Goode)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era(Era(..))
import Test.Cardano.Ledger.Generic.Updaters hiding(Mint)
import qualified Test.Cardano.Ledger.Generic.Updaters as Generic

import qualified Data.List as List
import qualified Cardano.Ledger.Shelley.Scripts as Shelley(MultiSig(..))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage(TxOut(..),Datum(..))
import Cardano.Ledger.Alonzo.Data(binaryDataToData, dataToBinaryData)
import qualified Cardano.Ledger.Shelley.TxBody as Shelley(TxOut(..))
import Cardano.Ledger.TxIn(TxId (..))
import Cardano.Ledger.UnifiedMap(ViewMap)
import qualified Cardano.Ledger.Crypto as CC(Crypto)
import Cardano.Ledger.Babbage(BabbageEra)
import Cardano.Ledger.Mary(MaryEra)
import Cardano.Ledger.Allegra(AllegraEra)
import Cardano.Ledger.Shelley(ShelleyEra)
import qualified Cardano.Ledger.Babbage.PParams as Babbage(PParams, PParams' (..))
import qualified Cardano.Ledger.Shelley.PParams as Shelley(PParams, PParams' (..))

witnesses:: Era era => Proof era -> [WitnessesField era] -> Core.Witnesses era
witnesses era = List.foldl' (updateWitnesses Merge era) (initialWitnesses era)

txouts :: Era era => Proof era -> [TxOutField era] -> Core.TxOut era
txouts era dts = List.foldl' (updateTxOut Override era) (initialTxOut era) dts

coreTxBody :: Era era => Proof era -> [TxBodyField era] -> Core.TxBody era
coreTxBody era dts = List.foldl' (updateTxBody Override era) (initialTxBody era) dts

overrideTxBody :: Era era => Proof era -> Core.TxBody era -> [TxBodyField era] -> Core.TxBody era
overrideTxBody era old dts = List.foldl' (updateTxBody Override era) old dts

coreTx :: Era era => Proof era -> [TxField era] -> Core.Tx era
coreTx era dts = List.foldl' (updateTx Override era) (initialTx era) dts

-- ============================
-- Era agnostic actions on (Core.PParams era)

maxCollateralInputs' :: Proof era -> Core.PParams era -> Natural
maxCollateralInputs' = undefined

maxTxExUnits' :: Proof era -> Core.PParams era -> ExUnits
maxTxExUnits' = undefined

collateralPercentage' :: Proof era -> Core.PParams era -> Natural
collateralPercentage' = undefined

obligation' ::
  Proof era ->
  Core.PParams era ->
  ViewMap (Crypto era) (Credential 'Staking (Crypto era)) Coin ->
  Map (KeyHash 'StakePool crypto) (PoolParams crypto) -> Coin
obligation' = undefined  
  
minfee' :: Proof era -> Core.PParams era -> Core.Tx era -> Coin
minfee' = undefined

hashScriptIntegrity' ::
   Proof era -> 
   Core.PParams era ->
   Set Language ->
   Redeemers era ->
   TxDats era ->  -- (Map.Map (DataHash c) (Data era))
   StrictMaybe (ScriptIntegrityHash (Crypto era))
hashScriptIntegrity' = undefined   


{-
     { _minfeeA = minfeeA,
            _minfeeB = minfeeB,
            _costmdls =
              Map.fromList
                [ (PlutusV1, CostModel $ 0 <$ fromJust defaultCostModelParams),
                  (PlutusV2, CostModel $ 0 <$ fromJust defaultCostModelParams)
                ],
            _maxValSize = 1000,
            _maxTxSize = fromIntegral (maxBound :: Int),
            _maxTxExUnits = maxTxExUnits,
            _collateralPercentage = collateralPercentage,
            _maxCollateralInputs = maxCollateralInput
-}            
-- ============================
-- type A = AlonzoEra C_Crypto

genMapElem :: Map k a -> Gen (Maybe (k, a))
genMapElem m
  | n == 0 = pure Nothing
  | otherwise = do
    i <- choose (0, n - 1)
    pure $ Just $ Map.elemAt i m
  where
    n = Map.size m

-- | Generate a non-zero value
genPositiveVal :: Val v => Gen v
genPositiveVal = inject . Coin . getPositive <$> arbitrary

elementsT :: (Monad (t Gen), MonadTrans t) => [t Gen b] -> t Gen b
elementsT = join . lift . elements

frequencyT :: (Monad (t Gen), MonadTrans t) => [(Int, t Gen b)] -> t Gen b
frequencyT = join . lift . frequency . map (pure <$>)

lookupByKeyM ::
  (MonadState s m, Ord k, Show k) => String -> k -> (s -> Map.Map k v) -> m v
lookupByKeyM name k getMap = do
  m <- getMap <$> get
  case Map.lookup k m of
    Nothing ->
      error $
        "Can't find " ++ name ++ " in the test enviroment: " ++ show k
    Just val -> pure val



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

deriving instance CC.Crypto c => Show (GenState (BabbageEra c))
deriving instance CC.Crypto c => Show (GenState (AlonzoEra c))
deriving instance CC.Crypto c => Show (GenState (MaryEra c))
deriving instance CC.Crypto c => Show (GenState (AllegraEra c))
deriving instance CC.Crypto c => Show (GenState (ShelleyEra c))


-- deriving instance (Era era,Show (Core.Script era)) => Show (GenState era)


modifyDPState :: (MonadState (GenState era) m) => (DPState (Crypto era) -> DPState (Crypto era)) -> m ()
modifyDPState f =
  modify $ \s@GenState {gsDPState = dps} -> s {gsDPState = f dps}

modifyDState :: (MonadState (GenState era) m) => (DState (Crypto era) -> DState (Crypto era)) -> m ()
modifyDState f =
  modifyDPState $ \dp@DPState {_dstate = ds} -> dp {_dstate = f ds}

modifyPState :: (MonadState (GenState era) m) => (PState (Crypto era) -> PState (Crypto era)) -> m ()
modifyPState f =
  modifyDPState $ \dp@DPState {_pstate = ps} -> dp {_pstate = f ps}


emptyGenState :: GenState era
emptyGenState = GenState mempty mempty mempty mempty def


type GenRS era = RWST (GenEnv era) () (GenState era) Gen


-- | Generate a list of specified length with randomish `ExUnit`s where the sum
-- of all values produced will not exceed the maxTxExUnits.
genExUnits :: Proof era -> Int -> GenRS era [ExUnits]
genExUnits era n = do
  GenEnv {gePParams} <- ask
  let ExUnits maxMemUnits maxStepUnits = maxTxExUnits' era gePParams
  memUnits <- lift $ genSequenceSum maxMemUnits
  stepUnits <- lift $ genSequenceSum maxStepUnits
  pure $ zipWith ExUnits memUnits stepUnits
  where
    un = fromIntegral n
    genUpTo maxVal (!totalLeft, !acc) _
      | totalLeft == 0 = pure (0, 0 : acc)
      | otherwise = do
        x <- min totalLeft . round . (% un) <$> genNatural 0 maxVal
        pure (totalLeft - x, x : acc)
    genSequenceSum maxVal
      | maxVal == 0 = pure $ replicate n 0
      | otherwise = snd <$> F.foldlM (genUpTo maxVal) (maxVal, []) [1 .. n]


lookupScript :: forall era m.
  MonadState (GenState era) m =>
  ScriptHash (Crypto era) ->
  Maybe Tag ->
  m (Maybe (Core.Script era))
lookupScript scriptHash mTag = do
  m <- gsScripts <$> get
  case Map.lookup scriptHash m of
    Just script -> pure $ Just script
    Nothing
      | Just tag <- mTag ->
        Just . snd <$> lookupByKeyM "plutusScript" (scriptHash, tag) gsPlutusScripts
    _ -> pure Nothing



-- | Same as `genCredKeyWit`, but for `TxOuts`
genTxOutKeyWitness :: forall era.
  (Reflect era) => 
  Proof era -> Maybe Tag -> Core.TxOut era ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
genTxOutKeyWitness era mTag txout = do
  case (getTxOutAddr txout) of
    AddrBootstrap baddr ->
      error $ "Can't authorize bootstrap address: " ++ show baddr
    Addr _ payCred _ -> (mkWitVKey era mTag payCred)

genCredKeyWit :: forall era k. (Reflect era) => 
  Proof era ->
  Maybe Tag ->
  Credential k (Crypto era) ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
genCredKeyWit era mTag cred = mkWitVKey era mTag cred
     

-- | Same as `genCredTimelockKeyWit`, but for `TxOuts`
genTxOutTimelockKeyWitness :: forall era.
  (Reflect era) => 
  Proof era -> Maybe Tag -> Core.TxOut era ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> Core.Witnesses era)
genTxOutTimelockKeyWitness era mTag txout = do
  case (getTxOutAddr txout) of
    AddrBootstrap baddr ->
      error $ "Can't authorize bootstrap address: " ++ show baddr
    Addr _ payCred _ ->  (witnesses era . ) <$> (mkWitVKey era mTag payCred)

-- | Generator for witnesses necessary for Scripts and Key
-- credentials. Because of the Key credentials generating function requires a body
-- hash for an acutal witness to be constructed. In order to be able to estimate
-- fees and collateral needed we will use produced witness generators twice: one
-- time with bogus body hash for estimation, and the second time with an actual
-- body hash.
genCredTimelockKeyWit :: forall era k. (Reflect era) => 
  Proof era ->
  Maybe Tag ->
  Credential k (Crypto era) ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> Core.Witnesses era)
genCredTimelockKeyWit era mTag cred = -- we could write this: (witnesses era . ) <$> (mkWitVKey era mTag cred)
  do f <- mkWitVKey era mTag cred
     pure (witnesses era . f)
   
-- | Generate a Witnesses producing function. We handle Witnesses come from Keys and Scripts
--   Because scripts vary be Era, we need some Era specific code here: genGenericScriptWitness
mkWitVKey :: forall era kr. (Reflect era) =>
      Proof era -> Maybe Tag -> 
      Credential kr (Crypto era) ->
      GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
mkWitVKey era mTag (KeyHashObj keyHash) = do
      keyPair <- lookupByKeyM "credential" (coerceKeyRole keyHash) gsKeys
      pure $ \bodyHash -> [AddrWits[makeWitnessVKey bodyHash keyPair]]
mkWitVKey era mTag (ScriptHashObj scriptHash) =
      lookupScript @era scriptHash mTag >>= \case
        Nothing ->
          error $ "Impossible: Cannot find script with hash " ++ show scriptHash
        Just script -> do
          let scriptWit = [ScriptWits[script]]        
          otherWit <- genGenericScriptWitness era mTag script
          pure(\ hash -> (scriptWit ++ otherWit hash))

genGenericScriptWitness :: (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Core.Script era ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
genGenericScriptWitness (Shelley c) mTag timelock = mkMultiSigWit (Shelley c) mTag timelock  
genGenericScriptWitness (Allegra c) mTag timelock = mkTimelockWit (Allegra c) mTag timelock    
genGenericScriptWitness (Mary c) mTag timelock = mkTimelockWit (Mary c) mTag timelock  
genGenericScriptWitness (Alonzo c) mTag (TimelockScript timelock) = mkTimelockWit (Alonzo c) mTag timelock
genGenericScriptWitness (Alonzo _) mTag (PlutusScript _ _) = pure(const [])
genGenericScriptWitness (Babbage c) mTag (TimelockScript timelock) = mkTimelockWit (Babbage c) mTag timelock
genGenericScriptWitness (Babbage _) mTag (PlutusScript _ _) = pure(const [])

-- | Used in Aonzo and Babbage and Mary Eras
mkTimelockWit :: forall era. (Reflect era) =>
  Proof era ->
  Maybe Tag -> 
  Timelock (Crypto era) ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
mkTimelockWit era mTag =
      \case
        RequireSignature keyHash -> mkWitVKey era mTag (KeyHashObj keyHash)
        RequireAllOf timelocks -> F.fold <$> mapM (mkTimelockWit era mTag) timelocks
        RequireAnyOf timelocks
          | F.null timelocks ->  pure (const [])
          | otherwise -> mkTimelockWit era mTag =<< lift (elements (F.toList timelocks))
        RequireMOf m timelocks -> do
          ts <- take m <$> lift (shuffle (F.toList timelocks))
          F.fold <$> mapM (mkTimelockWit era mTag) ts
        RequireTimeStart _ -> pure (const [])
        RequireTimeExpire _ -> pure (const [])

-- | Used in Shelley Eras
mkMultiSigWit :: forall era. (Reflect era) =>
  Proof era ->
  Maybe Tag -> 
  Shelley.MultiSig (Crypto era) ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
mkMultiSigWit era mTag (Shelley.RequireSignature keyHash) = mkWitVKey era mTag (KeyHashObj keyHash)
mkMultiSigWit era mTag (Shelley.RequireAllOf timelocks) = F.fold <$> mapM (mkMultiSigWit era mTag) timelocks
mkMultiSigWit era mTag (Shelley.RequireAnyOf timelocks)
          | F.null timelocks =  pure (const [])
          | otherwise = mkMultiSigWit era mTag =<< lift (elements (F.toList timelocks))
mkMultiSigWit era mTag (Shelley.RequireMOf m timelocks) = do
          ts <- take m <$> lift (shuffle (F.toList timelocks))
          F.fold <$> mapM (mkMultiSigWit era mTag) ts

makeDatumWitness :: Proof era -> Core.TxOut era -> GenRS era [WitnessesField era]
makeDatumWitness proof txout = case (proof,txout) of -- (TxOut _ _ mDatum) = mkDatumWit mDatum
      (Babbage _,Babbage.TxOut _ _ (Babbage.DatumHash h)) -> mkDatumWit (SJust h)
      (Babbage _,Babbage.TxOut _ _ (Babbage.Datum bd)) -> pure[DataWits [binaryDataToData bd]]
      (Babbage _,Babbage.TxOut _ _ Babbage.NoDatum) -> pure[]
      (Alonzo _,TxOut _ _ mDatum) -> mkDatumWit mDatum
      other -> pure [] -- No other era has data witnesses
  where
    mkDatumWit SNothing = pure []
    mkDatumWit (SJust datumHash) = do
      datum <- lookupByKeyM "datum" datumHash gsDatums
      pure $ [DataWits [datum]]


genKeyHash :: Reflect era => GenRS era (KeyHash kr (Crypto era))
genKeyHash = do
  keyPair <- lift arbitrary
  let keyHash = hashKey $ vKey keyPair
  modify $ \ts@GenState {gsKeys} -> ts {gsKeys = Map.insert keyHash keyPair gsKeys}
  pure $ coerceKeyRole keyHash


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
      nonRecTimelocks =
        [ r
          | SJust r <-
              [ SJust requireSignature,
                requireTimeStart <$> mBefore,
                requireTimeExpire <$> mAfter
              ]
        ]
      requireSignature = RequireSignature <$> genKeyHash
      requireAllOf k = do
        NonNegative (Small n) <- lift arbitrary
        RequireAllOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireAnyOf k = do
        Positive (Small n) <- lift arbitrary
        RequireAnyOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireMOf k = do
        NonNegative (Small n) <- lift arbitrary
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

genMultiSigScript :: forall era. Reflect era => Proof era -> GenRS era (ScriptHash (Crypto era))
genMultiSigScript proof = do
  let genNestedMultiSig k
        | k > 0 =
          elementsT $
            nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = elementsT nonRecTimelocks
      nonRecTimelocks = [ requireSignature ]
      requireSignature = Shelley.RequireSignature <$> genKeyHash
      requireAllOf k = do
        NonNegative (Small n) <- lift arbitrary
        Shelley.RequireAllOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireAnyOf k = do
        Positive (Small n) <- lift arbitrary
        Shelley.RequireAnyOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireMOf k = do
        NonNegative (Small n) <- lift arbitrary
        m <- lift $ choose (0, n)
        Shelley.RequireMOf m <$> replicateM n (genNestedMultiSig (k - 1))
  msscript <- genNestedMultiSig (2 :: Natural)
  let corescript :: Core.Script era
      corescript = case proof of
        Shelley _ -> msscript
        _ -> error (show proof++" does not have MultiSig scripts")
  let scriptHash = hashScript @era corescript
  modify $ \ts@GenState {gsScripts} -> ts {gsScripts = Map.insert scriptHash corescript gsScripts}
  pure scriptHash


genPlutusScript :: forall era. Reflect era => Proof era -> Tag -> GenRS era (ScriptHash (Crypto era))
genPlutusScript proof tag = do
  isValid <- lift $ frequency [(5, pure False), (95, pure True)]
  -- Plutus scripts alwaysSucceeds needs at least numArgs, while
  -- alwaysFails needs exactly numArgs to have the desired affect.
  let numArgs
        | tag == Spend = 3
        | otherwise = 2
  -- While using varying number of arguments for alwaysSucceeds we get
  -- varying script hashes, which helps with the fuzziness
  language <- lift $ elements (Set.toList nonNativeLanguages)
  script <-
    if isValid
      then alwaysSucceeds @era language . (+ numArgs) . getNonNegative <$> lift arbitrary
      else pure $ alwaysFails @era language numArgs
  let corescript :: Core.Script era
      corescript = case proof of
        Alonzo _ -> script
        Babbage _ -> script
        _ -> error ("Only Alonzo and Babbage have PlutusScripts. "++show proof++" does not.")
      scriptHash = hashScript @era corescript
  modify $ \ts@GenState {gsPlutusScripts} ->
    ts {gsPlutusScripts = Map.insert (scriptHash, tag) (IsValid isValid, corescript) gsPlutusScripts}
  pure scriptHash


genScript ::  forall era. Reflect era => Proof era -> Tag -> GenRS era (ScriptHash (Crypto era))
genScript proof tag = case proof of
  Babbage _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Alonzo _ -> elementsT [genTimelockScript proof, genPlutusScript proof tag]
  Mary _ -> genTimelockScript proof
  Allegra _ -> genTimelockScript proof
  Shelley _ -> genMultiSigScript proof


paymentCredAddr :: Addr c -> Maybe (Credential 'Payment c)
paymentCredAddr (Addr _ cred _) = Just cred
paymentCredAddr _ = Nothing

stakeCredAddr :: Addr c -> Maybe (Credential 'Staking c)
stakeCredAddr (Addr _ _ (StakeRefBase cred)) = Just cred
stakeCredAddr _ = Nothing


lookupPlutusScript :: 
  (MonadState (GenState era) m) =>
  Credential k (Crypto era) ->
  Tag ->
  m (Maybe (IsValid, ScriptHash (Crypto era)))
lookupPlutusScript (KeyHashObj _) _ = pure Nothing
lookupPlutusScript (ScriptHashObj scriptHash) tag =
  fmap (Map.lookup (scriptHash, tag) . gsPlutusScripts) get <&> \case
    Nothing -> Nothing
    Just (isValid, _) -> Just (isValid, scriptHash)


redeemerWitnessMaker :: Era era =>
  Tag ->
  [Maybe (GenRS era (Data era), Credential k (Crypto era))] ->
  GenRS era (IsValid, [ExUnits -> [WitnessesField era]])
redeemerWitnessMaker tag listWithCred =
  let creds =
        [ (ix, genDat, cred)
          | (ix, mCred) <- zip [0 ..] listWithCred,
            Just (genDat, cred) <- [mCred]
        ]
      allValid = IsValid . getAll . foldMap (\(IsValid v) -> All v)
   in fmap (first allValid . unzip . catMaybes) $
        forM creds $ \(ix, genDat, cred) ->
          lookupPlutusScript cred tag >>= \case
            Nothing -> pure Nothing
            Just (isValid, _) -> do
              datum <- genDat
              let rPtr = RdmrPtr tag ix
                  mkWit exUnits = [RdmrWits (Redeemers $ Map.singleton rPtr (datum, exUnits))]
              pure $ Just (isValid, mkWit)


-- | Collaterals can't have scripts, this is where this generator is needed.
genNoScriptRecipient :: Reflect era => GenRS era (Addr (Crypto era))
genNoScriptRecipient = do
  paymentCred <- KeyHashObj <$> genKeyHash
  stakeCred <- StakeRefBase . KeyHashObj <$> genKeyHash
  pure (Addr Testnet paymentCred stakeCred)


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

genRecipient :: Reflect era => GenRS era (Addr (Crypto era))
genRecipient = do
  paymentCred <- genCredential Spend
  stakeCred <- genCredential Cert
  pure (Addr Testnet paymentCred (StakeRefBase stakeCred))


genDatum :: Era era => GenRS era (Data era)
genDatum = snd <$> genDatumWithHash

genDatumWithHash :: Era era => GenRS era (DataHash (Crypto era), Data era)
genDatumWithHash = do
  datum <- lift arbitrary
  let datumHash = hashData datum
  modify $ \ts@GenState {gsDatums} -> ts {gsDatums = Map.insert datumHash datum gsDatums}
  pure (datumHash, datum)


genTxOut :: Reflect era => Proof era -> Core.Value era -> GenRS era [TxOutField era]
genTxOut proof val = do
  addr <- genRecipient
  cred <- maybe (error "BootstrapAddress encountered") pure $ paymentCredAddr addr
  dataHashFields <-
    case cred of
      KeyHashObj _ -> pure []
      ScriptHashObj scriptHash -> do
        maybecorescript <- lookupScript scriptHash (Just Spend)
        case (proof,maybecorescript) of 
           (Babbage _,Just (PlutusScript _ _)) -> do
              (datahash,_data) <- genDatumWithHash
              pure [DHash [datahash]]
           (Alonzo _,Just (PlutusScript _ _)) -> do
              (datahash,_data) <- genDatumWithHash
              pure [DHash [datahash]]              
           (_,_) -> pure []
  pure $ [Address addr,Amount val] ++ dataHashFields


genUTxO ::  Reflect era => GenRS era (UTxO era)
genUTxO = do
  NonEmpty ins <- lift $ resize 10 arbitrary
  UTxO <$> sequence (SplitMap.fromSet (const genOut) (Set.fromList ins))
  where
    genOut = do
       val <- lift genPositiveVal
       fields <- genTxOut reify val
       pure(txouts reify fields)


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


genDCert :: Reflect era => GenRS era (DCert (Crypto era))
genDCert =
  elementsT
    [ DCertDeleg
        <$> elementsT
          [ RegKey <$> genCredential Cert,
            DeRegKey <$> genCredential Cert,
            Delegate <$> genDelegation
          ]
    ]
  where
    genDelegation = do
      rewardAccount <- genCredential Cert
      poolId <- genPool
      pure $ Delegation {_delegator = rewardAccount, _delegatee = poolId}


genDCerts :: Reflect era => GenRS era [DCert (Crypto era)]
genDCerts = do
  let genUniqueScript (!dcs, !ss, !regCreds) _ = do
        dc <- genDCert
        -- Workaround a misfeature where duplicate plutus scripts in DCert are ignored
        let insertIfNotPresent dcs' regCreds' mKey mScriptHash
              | Just (_, scriptHash) <- mScriptHash =
                if (scriptHash, mKey) `Set.member` ss
                  then (dcs, ss, regCreds)
                  else (dc : dcs', Set.insert (scriptHash, mKey) ss, regCreds')
              | otherwise = (dc : dcs', ss, regCreds')
        -- Generate registration and de-registration delegation certificates,
        -- while ensuring the proper registered/unregestered state in DState
        case dc of
          DCertDeleg d
            | RegKey regCred <- d ->
              if regCred `Set.member` regCreds
                then pure (dcs, ss, regCreds)
                else pure (dc : dcs, ss, Set.insert regCred regCreds)
            | DeRegKey deregCred <- d ->
              if deregCred `Set.member` regCreds
                then
                  insertIfNotPresent dcs (Set.delete deregCred regCreds) Nothing
                    <$> lookupPlutusScript deregCred Cert
                else pure (dcs, ss, regCreds)
            | Delegate (Delegation delegCred delegKey) <- d ->
              let (dcs', regCreds')
                    | delegCred `Set.member` regCreds = (dcs, regCreds)
                    | otherwise =
                      (DCertDeleg (RegKey delegCred) : dcs, Set.insert delegCred regCreds)
               in insertIfNotPresent dcs' regCreds' (Just delegKey)
                    <$> lookupPlutusScript delegCred Cert
          _ -> pure (dc : dcs, ss, regCreds)
  NonNegative n <- lift arbitrary
  DPState {_dstate = DState {_unified}} <- gsDPState <$> get
  let initSets = ([], Set.empty, UM.domain (Rewards _unified))
  (dcs, _, _) <- F.foldlM genUniqueScript initSets [1 :: Int .. n]
  pure $ reverse dcs


getTxOutVal :: Proof era -> Core.TxOut era -> Core.Value era
getTxOutVal (Babbage _) (Babbage.TxOut _ v _) = v
getTxOutVal (Alonzo _) (TxOut _ v _) = v
getTxOutVal (Mary _) (Shelley.TxOut _ v) = v
getTxOutVal (Allegra _) (Shelley.TxOut _ v) = v
getTxOutVal (Shelley _) (Shelley.TxOut _ v) = v


genCollateralUTxO ::
  (HasCallStack,Reflect era) =>
  [Addr (Crypto era)] ->
  Coin ->
  UTxO era ->
  GenRS era (UTxO era, Map.Map (TxIn (Crypto era)) (Core.TxOut era))
genCollateralUTxO collateralAddresses (Coin fee) (UTxO utxo) = do
  GenEnv {gePParams} <- ask
  let collPerc = collateralPercentage' reify gePParams
      minCollTotal = Coin (ceiling ((fee * toInteger collPerc) % 100))
      -- Generate a collateral that is neither in UTxO map nor has already been generated
      genNewCollateral addr coll um c = do
        txIn <- lift arbitrary
        if SplitMap.member txIn utxo || Map.member txIn coll
          then genNewCollateral addr coll um c
          else pure (um, Map.insert txIn (txouts reify [Address addr,Amount (inject c)]) coll, c)
      -- Either pick a collateral from a map or generate a completely new one
      genCollateral addr coll um
        | Map.null um = genNewCollateral addr coll um =<< lift genPositiveVal
        | otherwise = do
          i <- lift $ chooseInt (0, Map.size um - 1)
          let (txIn, txOut) = Map.elemAt i um
              val = getTxOutVal reify txOut
          pure (Map.deleteAt i um, Map.insert txIn txOut coll, coin val)
      -- Recursively either pick existing key spend only outputs or generate new ones that
      -- will be later added to the UTxO map
      go ecs !coll !curCollTotal !um
        | curCollTotal >= minCollTotal = pure coll
        | [] <- ecs = error "Impossible: supplied less addresses then `maxCollateralInputs`"
        | ec : ecs' <- ecs = do
          (um', coll', c) <-
            if null ecs'
              then genNewCollateral ec coll um (minCollTotal <-> curCollTotal)
              else elementsT [genCollateral ec coll Map.empty, genCollateral ec coll um]
          go ecs' coll' (curCollTotal <+> c) um'
  collaterals <-
    go collateralAddresses Map.empty (Coin 0) $
      SplitMap.toMap $ SplitMap.filter spendOnly utxo
  pure (UTxO (Map.foldrWithKey' SplitMap.insert utxo collaterals), collaterals)


spendOnly :: Era era => Core.TxOut era -> Bool
spendOnly txout = case getTxOutAddr txout of
  (Addr _ (ScriptHashObj _) _) -> False
  (Addr _ _ (StakeRefBase (ScriptHashObj _))) -> False
  _ -> True
  
emptyPPUPstate :: forall era. Proof era -> State (Core.EraRule "PPUP" era)
emptyPPUPstate (Babbage _) = def
emptyPPUPstate (Alonzo _) = def
emptyPPUPstate (Mary _) = def
emptyPPUPstate (Allegra _) = def
emptyPPUPstate (Shelley _) = def


genUTxOState :: forall era. Reflect era => UTxO era -> GenRS era (UTxOState era)
genUTxOState utxo = do
  GenEnv {gePParams} <- ask
  DPState {_dstate, _pstate} <- gsDPState <$> get
  let deposited = obligation' reify gePParams (rewards _dstate) (_pParams _pstate)
  lift (UTxOState utxo deposited <$> arbitrary <*> pure(emptyPPUPstate @era reify) <*> pure def)

-- | Break a TxOut into its mandatory and optional parts 
txoutFields :: Proof era -> Core.TxOut era -> (Addr (Crypto era),Core.Value era,[TxOutField era])
txoutFields (Alonzo _) (TxOut addr val (SJust dh)) = (addr,val,[ DHash [dh]])
txoutFields (Alonzo _) (TxOut addr val SNothing) = (addr,val,[])
txoutFields (Babbage _) (Babbage.TxOut addr val Babbage.NoDatum) = (addr,val,[])
txoutFields (Babbage _) (Babbage.TxOut addr val (Babbage.DatumHash h)) = (addr,val,[DHash [h]])
txoutFields (Babbage _) (Babbage.TxOut addr val d) = (addr,val,[Datum [d]])
txoutFields (Mary _) (Shelley.TxOut addr val) = (addr,val,[])
txoutFields (Allegra _) (Shelley.TxOut addr val) = (addr,val,[])
txoutFields (Shelley _) (Shelley.TxOut addr val) = (addr,val,[])

injectFee :: Proof era -> Coin -> Core.TxOut era -> Core.TxOut era
injectFee (Babbage _) fee (Babbage.TxOut addr val d) = Babbage.TxOut addr (val <+> inject fee) d
injectFee (Alonzo _) fee (TxOut addr val mdh) = TxOut addr (val <+> inject fee) mdh
injectFee (Mary _) fee (Shelley.TxOut addr val) = Shelley.TxOut addr (val <+> inject fee)
injectFee (Allegra _) fee (Shelley.TxOut addr val) = Shelley.TxOut addr (val <+> inject fee)
injectFee (Shelley _) fee (Shelley.TxOut addr val) = Shelley.TxOut addr (val <+> inject fee)

genRecipientsFrom :: Reflect era => [Core.TxOut era] -> GenRS era [Core.TxOut era]
genRecipientsFrom txOuts = do
  let outCount = length txOuts
  approxCount <- lift $ choose (1, outCount)
  let extra = outCount - approxCount
      avgExtra = ceiling (toInteger extra % toInteger approxCount)
      genExtra e
        | e <= 0 || avgExtra == 0 = pure 0
        | otherwise = lift $ chooseInt (0, avgExtra)
  let goNew _ [] !rs = pure rs
      goNew e (tx : txs) !rs = do
        leftToAdd <- genExtra e
        goExtra (e - leftToAdd) leftToAdd (inject (Coin 0)) tx txs rs
      goExtra _ _ s tx [] !rs = genWithChange s tx rs
      goExtra e 0 s tx txs !rs = goNew e txs =<< genWithChange s tx rs
      goExtra e n s txout (tx : txs) !rs = goExtra e (n - 1) (s <+> v) tx txs rs
        where v = getTxOutVal reify txout
      genWithChange s txout rs = do
        let (!addr,!v,!ds) = txoutFields reify txout
        c <- Coin <$> lift (choose (1, unCoin $ coin v))
        fields <- genTxOut reify (s <+> inject c)
        if c < coin v
          then
            let !change = txouts reify ([Address addr, Amount(v <-> inject c)]++ds)
             in pure (txouts reify fields : change : rs)
          else pure (txouts reify fields : rs)
  goNew extra txOuts []


getDCertCredential :: DCert crypto -> Maybe (Credential 'Staking crypto)
getDCertCredential = \case
  DCertDeleg d ->
    case d of
      RegKey _rk -> Nothing -- we don't require witnesses for RegKey
      DeRegKey drk -> Just drk
      Delegate (Delegation dk _) -> Just dk
  DCertPool _p -> Nothing
  DCertGenesis _g -> Nothing
  DCertMir _m -> Nothing


genRewards :: Reflect era => GenRS era (RewardAccounts (Crypto era))
genRewards = do
  NonNegative n <- lift arbitrary
  newrewards <-
    Map.fromList <$> replicateM n ((,) <$> genCredential Rewrd <*> lift genPositiveVal)
  modifyDState $ \ds -> ds {_unified = rewards ds UM.⨃ newrewards} -- Prefers coins in newrewards
  pure newrewards


genWithdrawals :: Reflect era => GenRS era (Wdrl (Crypto era))
genWithdrawals = do
  let networkId = Testnet
  newrewards <- genRewards
  pure $ Wdrl $ Map.fromList $ map (first (RewardAcnt networkId)) $ Map.toList newrewards



languagesUsed :: forall era. Era era => 
  Proof era ->
  Core.Tx era ->
  UTxO era ->
  Map (ScriptHash (Crypto era), Tag) (IsValid, Core.Script era) ->
  Set Language
languagesUsed proof tx utxo plutusScripts = case proof of
    (Shelley _) -> Set.empty
    (Allegra _) ->  Set.empty
    (Mary _) -> Set.empty
    (Alonzo _) -> Set.fromList [lang | (_, PlutusScript lang _) <- mapMaybe lookupPlutus needed]
       where needed = scriptsNeeded @era utxo tx
    (Babbage _) -> Set.fromList [lang | (_, PlutusScript lang _) <- mapMaybe lookupPlutus needed]
       where needed = scriptsNeeded @era utxo tx  -- TODO FIXME, Not sure this is the right function for Babbage
  where
    lookupPlutus :: (ScriptPurpose (Crypto era), ScriptHash (Crypto era)) -> Maybe (IsValid, Core.Script era)
    lookupPlutus ((Spending _), sh) = Map.lookup (sh, Spend) plutusScripts
    lookupPlutus ((Rewarding _), sh) = Map.lookup (sh, Rewrd) plutusScripts
    lookupPlutus ((Certifying _), sh) = Map.lookup (sh, Cert) plutusScripts
    lookupPlutus ((Minting _), sh) = Map.lookup (sh, Mint) plutusScripts


txoutAddrHash :: Proof era -> Core.TxOut era -> (Addr (Crypto era),Maybe (DataHash (Crypto era)))
txoutAddrHash (Alonzo _) (TxOut addr val (SJust dh)) = (addr,Just dh)
txoutAddrHash (Alonzo _) (TxOut addr val SNothing) = (addr,Nothing)
txoutAddrHash (Babbage _) (Babbage.TxOut addr val Babbage.NoDatum) = (addr,Nothing)
txoutAddrHash (Babbage _) (Babbage.TxOut addr val (Babbage.DatumHash dh)) = (addr,Just dh)
txoutAddrHash (Babbage _) (Babbage.TxOut addr val d) = (addr,Nothing)
txoutAddrHash (Mary _) (Shelley.TxOut addr val) = (addr,Nothing)
txoutAddrHash (Allegra _) (Shelley.TxOut addr val) = (addr,Nothing)
txoutAddrHash (Shelley _) (Shelley.TxOut addr val) = (addr,Nothing)

strictMaybeToList:: StrictMaybe t -> [t]
strictMaybeToList SNothing = []
strictMaybeToList (SJust x) = [x]

valid :: IsValid -> Bool
valid (IsValid b) = b

isValid' :: Proof era -> Core.Tx era -> IsValid
isValid' (Alonzo _) x = isValid x
isValid' (Babbage _) x = isValid x
isValid' _ _ = IsValid True

genValidatedTx :: forall era. Reflect era => Proof era -> GenRS era (UTxO era, Core.Tx era)
genValidatedTx proof = do
  GenEnv {geValidityInterval, gePParams} <- ask
  UTxO utxoNoCollateral <- genUTxO
  -- 1. Produce utxos that will be spent
  n <- lift $ choose (1, length utxoNoCollateral)
  toSpendNoCollateral <-
    Map.fromList . take n <$> lift (shuffle $ SplitMap.toList utxoNoCollateral)
  -- 2. Check if all Plutus scripts are valid
  let toSpendNoCollateralTxOuts :: [Core.TxOut era]
      toSpendNoCollateralTxOuts = Map.elems toSpendNoCollateral
      -- We use maxBound to ensure the serializaed size overestimation
      maxCoin = Coin (toInteger (maxBound :: Int))
  -- 3. Generate all recipients and witnesses needed for spending Plutus scripts
  recipients <- genRecipientsFrom toSpendNoCollateralTxOuts
  (IsValid v1, mkPaymentWits) <- -- mkPaymentWits :: ExUnits -> [WitnessField]
    redeemerWitnessMaker
      Spend
      [ (\dh c -> (lookupByKeyM "datum" dh gsDatums, c))
          <$> mDatumHash
          <*> paymentCredAddr addr
        | (_, coretxout) <- Map.toAscList toSpendNoCollateral
        , let (addr,mDatumHash) = txoutAddrHash proof coretxout
      ]

  wdrls@(Wdrl wdrlMap) <- genWithdrawals
  rewardsWithdrawalTxOut <- txouts proof <$> (genTxOut proof $ inject $ F.fold wdrlMap)
  let wdrlCreds = map (getRwdCred . fst) $ Map.toAscList wdrlMap
  (IsValid v2, mkWdrlWits) <-
    redeemerWitnessMaker Rewrd $ map (Just . (,) genDatum) wdrlCreds
  dcerts <- genDCerts
  let dcertCreds = map getDCertCredential dcerts
  (IsValid v3, mkCertsWits) <-
    redeemerWitnessMaker Cert $ map ((,) genDatum <$>) dcertCreds

  let isValid = IsValid (v1 && v2 && v3)
      mkWits :: [ExUnits -> [WitnessesField era]]
      mkWits = mkPaymentWits ++ mkCertsWits ++ mkWdrlWits
  exUnits <- genExUnits proof (length mkWits)
 
  let redeemerWitsList = concat (zipWith ($) mkWits exUnits)
  datumWitsList <- concat <$> mapM (makeDatumWitness proof) (Map.elems toSpendNoCollateral)
  -- t(SafeHash (Crypto era) EraIndependentTxBody -> Core.Witnesses era)
  keyWitsMakers <- mapM (genTxOutKeyWitness proof (Just Spend)) toSpendNoCollateralTxOuts
  dcertWitsMakers <- mapM (genCredKeyWit proof (Just Cert)) $ catMaybes dcertCreds
  rwdrsWitsMakers <- mapM (genCredKeyWit proof (Just Rewrd)) wdrlCreds
   
  -- 4. Estimate inputs that will be used as collateral
  maxCollateralCount <-
    lift $ chooseInt (1, fromIntegral (maxCollateralInputs' proof gePParams))
  bogusCollateralTxId <- lift (arbitrary :: Gen (TxId (Crypto era)))
  let bogusCollateralTxIns =
        Set.fromList
          [ TxIn bogusCollateralTxId (fromIntegral i)
            | i <- [maxBound, maxBound - 1 .. maxBound - maxCollateralCount - 1]
          ]        
  collateralAddresses <- replicateM maxCollateralCount genNoScriptRecipient
  bogusCollateralKeyWitsMakers <-
    mapM (\a -> genTxOutKeyWitness proof Nothing (txouts proof [Address a,Amount (inject maxCoin)])) collateralAddresses
  networkId <- lift $ elements [SNothing, SJust Testnet]

 
  -- 5. Estimate the fee
  let redeemerDatumWits = (redeemerWitsList ++ datumWitsList)
      bogusIntegrityHash = hashScriptIntegrity' proof gePParams mempty (Redeemers mempty) mempty
      txBodyNoFee =
        coreTxBody proof
          [ Inputs $ Set.toList (Map.keysSet (toSpendNoCollateral)),
            Collateral $ Set.toList bogusCollateralTxIns,
            Outputs (rewardsWithdrawalTxOut : recipients),
            Certs dcerts,
            Wdrls wdrls,
            Txfee maxCoin,
            Vldt geValidityInterval,
            Update [],
            ReqSignerHashes [],
            Generic.Mint mempty,
            WppHash (strictMaybeToList bogusIntegrityHash),
            AdHash [],
            Txnetworkid networkId
          ]
      txBodyNoFeeHash = hashAnnotated txBodyNoFee
      witsMakers :: [SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era]]
      witsMakers = keyWitsMakers ++ dcertWitsMakers ++ rwdrsWitsMakers
      noFeeWits :: [WitnessesField era]
      noFeeWits =
        redeemerDatumWits
          <> foldMap ($ txBodyNoFeeHash) (witsMakers ++ bogusCollateralKeyWitsMakers)
      bogusTxForFeeCalc = coreTx proof [Body txBodyNoFee,Witnesses (witnesses proof noFeeWits), Valid (valid isValid), AuxData []]
      fee = minfee' proof gePParams bogusTxForFeeCalc

  -- 6. Crank up the amount in one of outputs to account for the fee. Note this is
  -- a hack that is not possible in a real life, but in the end it does produce
  -- real life like setup
  feeKey <- lift $ elements $ Map.keys toSpendNoCollateral
  let utxoFeeAdjusted =
        UTxO $ case SplitMap.lookup feeKey utxoNoCollateral of
          Nothing -> utxoNoCollateral
          Just txOut -> SplitMap.insert feeKey (injectFee proof fee txOut) utxoNoCollateral

  -- 7. Generate utxos that will be used as collateral
  (utxo, collMap) <- genCollateralUTxO collateralAddresses fee utxoFeeAdjusted
  collateralKeyWitsMakers <- mapM (genTxOutKeyWitness proof Nothing) $ Map.elems collMap

  -- 8. Construct the correct Tx with valid fee and collaterals
  allPlutusScripts <- gsPlutusScripts <$> get
  let mIntegrityHash =
        hashScriptIntegrity'
          proof
          gePParams
          (languagesUsed proof bogusTxForFeeCalc (UTxO utxoNoCollateral) allPlutusScripts)
          (mkTxrdmrs redeemerDatumWits)
          (mkTxdats redeemerDatumWits)
      txBody = overrideTxBody proof txBodyNoFee
          [ Txfee fee ,
            Collateral (Set.toList (Map.keysSet collMap)),
            WppHash (strictMaybeToList mIntegrityHash)
          ]
      txBodyHash = hashAnnotated txBody
      wits =
        redeemerDatumWits
          <> foldMap ($ txBodyHash) (witsMakers ++ collateralKeyWitsMakers)
      validTx = -- ValidatedTx txBody (witnesses proof wits) isValid SNothing
                coreTx proof [Body txBody,Witnesses (witnesses proof wits), Valid (valid isValid), AuxData []]
  pure (utxo, validTx)


-- | Scan though the fields unioning all the RdrmWits fields into one Redeemer map
mkTxrdmrs :: forall era. Era era => [WitnessesField era] -> Redeemers era
mkTxrdmrs fields = Redeemers (List.foldl' accum Map.empty fields)
  where accum m1 (RdmrWits (Redeemers m2)) = Map.union m1 m2
        accum m1 _ = m1

-- | Scan though the fields unioning all the DataWits fields into one TxDat
mkTxdats :: forall era. Era era => [WitnessesField era] -> TxDats era
mkTxdats fields = TxDats (List.foldl' accum Map.empty fields)
  where accum m (DataWits ds) = (List.foldl' accum2 m ds)
          where accum2 m d = Map.insert (hashData @era d) d m
        accum m _ = m
        


genTxAndUTXOState ::
  ( Reflect era,
    STS (Core.EraRule "UTXOW" era)
  ) => Proof era -> Gen (TRC (Core.EraRule "UTXOW" era), GenState era)
genTxAndUTXOState proof@(Alonzo _) = do
  (TRC (LedgerEnv slotNo _ pp _, (utxoState, _), vtx), genState) <-
    genTxAndLEDGERState proof
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), utxoState, vtx), genState)

genTxAndLEDGERState ::
  ( Reflect era,
    STS (Core.EraRule "LEDGER" era)
  ) => Proof era -> Gen (TRC (Core.EraRule "LEDGER" era), GenState era)
genTxAndLEDGERState proof@(Alonzo _) = do
  txIx <- arbitrary
  maxTxExUnits <- (arbitrary :: Gen ExUnits)
  Positive maxCollateralInputs <- arbitrary
  collateralPercentage <- (fromIntegral <$> chooseInt (1, 10000)) :: Gen Natural
  minfeeA <- fromIntegral <$> chooseInt (0, 1000)
  minfeeB <- fromIntegral <$> chooseInt (0, 10000)
  let genT = do
        (utxo, tx) <- genValidatedTx proof
        utxoState <- genUTxOState utxo
        dpState <- gsDPState <$> get
        pure $ TRC (ledgerEnv, (utxoState, dpState), tx)
      pp :: PParams era
      pp =
        def
          { _minfeeA = minfeeA,
            _minfeeB = minfeeB,
            _costmdls =
              Map.fromList
                [ (PlutusV1, CostModel $ 0 <$ fromJust defaultCostModelParams),
                  (PlutusV2, CostModel $ 0 <$ fromJust defaultCostModelParams)
                ],
            _maxValSize = 1000,
            _maxTxSize = fromIntegral (maxBound :: Int),
            _maxTxExUnits = maxTxExUnits,
            _collateralPercentage = collateralPercentage,
            _maxCollateralInputs = maxCollateralInputs
          }
      slotNo = SlotNo 100000000
      ledgerEnv = LedgerEnv slotNo txIx pp (AccountState (Coin 0) (Coin 0))
  minSlotNo <- oneof [pure SNothing, SJust <$> choose (minBound, unSlotNo slotNo)]
  maxSlotNo <- oneof [pure SNothing, SJust <$> choose (unSlotNo slotNo + 1, maxBound)]
  let env =
        GenEnv
          { geValidityInterval = ValidityInterval (SlotNo <$> minSlotNo) (SlotNo <$> maxSlotNo),
            gePParams = pp
          }
  (trc, s, _) <- runRWST genT env emptyGenState
  pure (trc, s)




totalAda :: Reflect era => UTxOState era -> DPState (Crypto era) -> Coin
totalAda (UTxOState utxo f d _ _) DPState {_dstate} =
  f <> d <> coin (balance utxo) <> F.foldl' (<>) mempty (rewards _dstate)



testTxValidForLEDGER :: Reflect era => Proof era -> (TRC (Core.EraRule "LEDGER" era), GenState era) -> Property
testTxValidForLEDGER proof@(Alonzo _) (trc@(TRC (_, (utxoState, dpstate), vtx)), _) =
  case runShelleyBase $ applySTSTest trc of
    Right (utxoState', dpstate') ->
      classify (coerce (isValid' proof vtx)) "TxValid" $
        totalAda utxoState' dpstate' === totalAda utxoState dpstate                        
testTxValidForLEDGER proof@(Babbage _) (trc@(TRC (_, (utxoState, dpstate), vtx)), _) =
  case runShelleyBase $ applySTSTest trc of
    Right (utxoState', dpstate') ->
      classify (coerce (isValid' proof vtx)) "TxValid" $
        totalAda utxoState' dpstate' === totalAda utxoState dpstate
    Left e -> counterexample (show e) (property False)
testTxValidForLEDGER proof@(Mary _) (trc@(TRC (_, (utxoState, dpstate), vtx)), _) =
  case runShelleyBase $ applySTSTest trc of
    Right (utxoState', dpstate') ->
      classify (coerce (isValid' proof vtx)) "TxValid" $
        totalAda utxoState' dpstate' === totalAda utxoState dpstate
    Left e -> counterexample (show e) (property False)
testTxValidForLEDGER proof@(Allegra _) (trc@(TRC (_, (utxoState, dpstate), vtx)), _) =
  case runShelleyBase $ applySTSTest trc of
    Right (utxoState', dpstate') ->
      classify (coerce (isValid' proof vtx)) "TxValid" $
        totalAda utxoState' dpstate' === totalAda utxoState dpstate
    Left e -> counterexample (show e) (property False)
testTxValidForLEDGER proof@(Shelley _) (trc@(TRC (_, (utxoState, dpstate), vtx)), _) =
  case runShelleyBase $ applySTSTest trc of
    Right (utxoState', dpstate') ->
      classify (coerce (isValid' proof vtx)) "TxValid" $
        totalAda utxoState' dpstate' === totalAda utxoState dpstate
    Left e -> counterexample (show e) (property False)           


alonzoProperties :: TestTree
alonzoProperties =
  testGroup
    "Alonzo UTXOW property tests"
    [ testProperty "test ValidTx" $ forAll (genTxAndLEDGERState (Alonzo Mock)) (testTxValidForLEDGER (Alonzo Mock))
    ]
