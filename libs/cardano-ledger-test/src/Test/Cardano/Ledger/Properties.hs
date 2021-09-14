{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Properties where

import Cardano.Ledger.Alonzo (AlonzoEra, Value)
import Cardano.Ledger.Alonzo.Data (Data, DataHash, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..), nonNativeLanguages)
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeeded)
import Cardano.Ledger.Alonzo.Rules.Ledger (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Scripts hiding (alwaysFails, alwaysSucceeds)
import Cardano.Ledger.Alonzo.Tx
  ( IsValid (..),
    ScriptPurpose (..),
    ValidatedTx (..),
    hashScriptIntegrity,
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

type A = AlonzoEra C_Crypto

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

data GenEnv = GenEnv
  { geValidityInterval :: ValidityInterval,
    gePParams :: PParams A
  }

data GenState = GenState
  { gsKeys :: Map (KeyHash 'Witness C_Crypto) (KeyPair 'Witness C_Crypto),
    gsScripts :: Map (ScriptHash C_Crypto) (Script A),
    gsPlutusScripts :: Map (ScriptHash C_Crypto, Tag) (IsValid, Script A),
    gsDatums :: Map (DataHash C_Crypto) (Data A),
    gsDPState :: DPState C_Crypto
  }
  deriving (Show)

modifyDPState :: MonadState GenState m => (DPState C_Crypto -> DPState C_Crypto) -> m ()
modifyDPState f =
  modify $ \s@GenState {gsDPState = dps} -> s {gsDPState = f dps}

modifyDState :: MonadState GenState m => (DState C_Crypto -> DState C_Crypto) -> m ()
modifyDState f =
  modifyDPState $ \dp@DPState {_dstate = ds} -> dp {_dstate = f ds}

modifyPState :: MonadState GenState m => (PState C_Crypto -> PState C_Crypto) -> m ()
modifyPState f =
  modifyDPState $ \dp@DPState {_pstate = ps} -> dp {_pstate = f ps}

emptyGenState :: GenState
emptyGenState = GenState mempty mempty mempty mempty def

type GenRS = RWST GenEnv () GenState Gen

-- | Generate a list of specified length with randomish `ExUnit`s where the sum
-- of all values produced will not exceed the maxTxExUnits.
genExUnits :: Int -> GenRS [ExUnits]
genExUnits n = do
  GenEnv {gePParams} <- ask
  let ExUnits maxMemUnits maxStepUnits = _maxTxExUnits gePParams
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

-- | Same as `genCredTimelockKeyWit`, but for `TxOuts`
genTxOutTimelockKeyWitness ::
  Maybe Tag -> TxOut A -> GenRS (SafeHash C_Crypto EraIndependentTxBody -> TxWitness A)
genTxOutTimelockKeyWitness mTag (TxOut addr _ _) = do
  case addr of
    AddrBootstrap baddr ->
      error $ "Can't authorize bootstrap address: " ++ show baddr
    Addr _ payCred _ -> genCredTimelockKeyWit mTag payCred

-- | Generator for witnesses necessary for Timelock scripts and Key
-- credentials. Because of the Key credentials produced function requires a body
-- hash for an acutal witness to be constructed. In order to be able to estimate
-- fees and collateral needed we will use produced witness generators twice: one
-- time with bogus body hash for estimation, and the second time with an actual
-- body hash.
genCredTimelockKeyWit ::
  Maybe Tag ->
  Credential k C_Crypto ->
  GenRS (SafeHash C_Crypto EraIndependentTxBody -> TxWitness A)
genCredTimelockKeyWit mTag = mkWitVKey
  where
    mkWitVKey ::
      Credential kr C_Crypto ->
      GenRS (SafeHash C_Crypto EraIndependentTxBody -> TxWitness A)
    mkWitVKey (KeyHashObj keyHash) = do
      keyPair <- lookupByKeyM "credential" (coerceKeyRole keyHash) gsKeys
      pure $ \bodyHash ->
        mempty {txwitsVKey = Set.singleton $ makeWitnessVKey bodyHash keyPair}
    mkWitVKey (ScriptHashObj scriptHash) =
      lookupScript scriptHash mTag >>= \case
        Nothing ->
          error $ "Impossible: Cannot find script with hash " ++ show scriptHash
        Just script -> do
          let scriptWit = mempty {txscripts = Map.singleton scriptHash script}
          case script of
            TimelockScript timelock -> do
              timelockWit <- mkTimelockWit timelock
              pure $ \bodyHash -> timelockWit bodyHash <> scriptWit
            PlutusScript _ _ps -> pure $ const scriptWit
    mkTimelockWit =
      \case
        RequireSignature keyHash -> mkWitVKey (KeyHashObj keyHash)
        RequireAllOf timelocks -> F.fold <$> mapM mkTimelockWit timelocks
        RequireAnyOf timelocks
          | F.null timelocks -> pure mempty
          | otherwise -> mkTimelockWit =<< lift (elements (F.toList timelocks))
        RequireMOf m timelocks -> do
          ts <- take m <$> lift (shuffle (F.toList timelocks))
          F.fold <$> mapM mkTimelockWit ts
        RequireTimeStart _ -> pure mempty
        RequireTimeExpire _ -> pure mempty

makeDatumWitness :: TxOut A -> GenRS (TxWitness A)
makeDatumWitness (TxOut _ _ mDatum) = mkDatumWit mDatum
  where
    mkDatumWit SNothing = pure mempty
    mkDatumWit (SJust datumHash) = do
      datum <- lookupByKeyM "datum" datumHash gsDatums
      pure $ mempty {txdats = TxDats $ Map.singleton datumHash datum}

genKeyHash :: GenRS (KeyHash kr C_Crypto)
genKeyHash = do
  keyPair <- lift arbitrary
  let keyHash = hashKey $ vKey keyPair
  modify $ \ts@GenState {gsKeys} -> ts {gsKeys = Map.insert keyHash keyPair gsKeys}
  pure $ coerceKeyRole keyHash

genTimelockScript :: GenRS (ScriptHash C_Crypto)
genTimelockScript = do
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
  script <- TimelockScript <$> genNestedTimelock (2 :: Natural)
  let scriptHash = hashScript @A script
  modify $ \ts@GenState {gsScripts} -> ts {gsScripts = Map.insert scriptHash script gsScripts}
  pure scriptHash

genPlutusScript :: Tag -> GenRS (ScriptHash C_Crypto)
genPlutusScript tag = do
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
      then alwaysSucceeds language . (+ numArgs) . getNonNegative <$> lift arbitrary
      else pure $ alwaysFails language numArgs
  let scriptHash = hashScript @A script
  modify $ \ts@GenState {gsPlutusScripts} ->
    ts {gsPlutusScripts = Map.insert (scriptHash, tag) (IsValid isValid, script) gsPlutusScripts}
  pure scriptHash

genScript :: Tag -> GenRS (ScriptHash C_Crypto)
genScript tag = elementsT [genTimelockScript, genPlutusScript tag]

paymentCredAddr :: Addr C_Crypto -> Maybe (Credential 'Payment C_Crypto)
paymentCredAddr (Addr _ cred _) = Just cred
paymentCredAddr _ = Nothing

stakeCredAddr :: Addr C_Crypto -> Maybe (Credential 'Staking C_Crypto)
stakeCredAddr (Addr _ _ (StakeRefBase cred)) = Just cred
stakeCredAddr _ = Nothing

lookupScript ::
  MonadState GenState m =>
  ScriptHash C_Crypto ->
  Maybe Tag ->
  m (Maybe (Script A))
lookupScript scriptHash mTag = do
  m <- gsScripts <$> get
  case Map.lookup scriptHash m of
    Just script -> pure $ Just script
    Nothing
      | Just tag <- mTag ->
        Just . snd <$> lookupByKeyM "plutusScript" (scriptHash, tag) gsPlutusScripts
    _ -> pure Nothing

lookupPlutusScript ::
  MonadState GenState m =>
  Credential k C_Crypto ->
  Tag ->
  m (Maybe (IsValid, ScriptHash C_Crypto))
lookupPlutusScript (KeyHashObj _) _ = pure Nothing
lookupPlutusScript (ScriptHashObj scriptHash) tag =
  fmap (Map.lookup (scriptHash, tag) . gsPlutusScripts) get <&> \case
    Nothing -> Nothing
    Just (isValid, _) -> Just (isValid, scriptHash)

redeemerWitnessMaker ::
  Tag ->
  [Maybe (GenRS (Data A), Credential k C_Crypto)] ->
  GenRS (IsValid, [ExUnits -> TxWitness A])
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
                  mkWit exUnits =
                    mempty
                      { txrdmrs = Redeemers $ Map.singleton rPtr (datum, exUnits)
                      }
              pure $ Just (isValid, mkWit)

-- | Collaterals can't have scripts, this is where this generator is needed.
genNoScriptRecipient :: GenRS (Addr C_Crypto)
genNoScriptRecipient = do
  paymentCred <- KeyHashObj <$> genKeyHash
  stakeCred <- StakeRefBase . KeyHashObj <$> genKeyHash
  pure (Addr Testnet paymentCred stakeCred)

-- | Generate a credential that can be used for supplied purpose (in case of
-- plutus scripts), while occasianally picking out randomly from previously
-- generated set.
genCredential :: Tag -> GenRS (Credential kr C_Crypto)
genCredential tag =
  frequencyT
    [ (35, KeyHashObj <$> genKeyHash),
      (35, ScriptHashObj <$> genScript tag),
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
        Nothing -> genScript tag
    pickExistingTimelockScript = do
      timelockScriptsMap <- gsScripts <$> get
      lift (genMapElem timelockScriptsMap) >>= \case
        Just (h, _) -> pure h
        Nothing -> genScript tag

genRecipient :: GenRS (Addr C_Crypto)
genRecipient = do
  paymentCred <- genCredential Spend
  stakeCred <- genCredential Cert
  pure (Addr Testnet paymentCred (StakeRefBase stakeCred))

genDatum :: GenRS (Data A)
genDatum = snd <$> genDatumWithHash

genDatumWithHash :: GenRS (DataHash C_Crypto, Data A)
genDatumWithHash = do
  datum <- lift arbitrary
  let datumHash = hashData datum
  modify $ \ts@GenState {gsDatums} -> ts {gsDatums = Map.insert datumHash datum gsDatums}
  pure (datumHash, datum)

genTxOut :: Value A -> GenRS (TxOut A)
genTxOut val = do
  addr <- genRecipient
  cred <- maybe (error "BootstrapAddress encountered") pure $ paymentCredAddr addr
  mDatumHash <-
    case cred of
      KeyHashObj _ -> pure SNothing
      ScriptHashObj scriptHash ->
        lookupScript scriptHash (Just Spend) >>= \case
          Just (PlutusScript _ _) -> SJust . fst <$> genDatumWithHash
          _ -> pure SNothing
  pure $ TxOut addr val mDatumHash

genUTxO :: GenRS (UTxO A)
genUTxO = do
  NonEmpty ins <- lift $ resize 10 arbitrary
  UTxO <$> sequence (Map.fromSet (const genOut) (Set.fromList ins))
  where
    genOut = genTxOut =<< lift genPositiveVal

genPool :: GenRS (KeyHash 'StakePool C_Crypto)
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

genDCert :: GenRS (DCert C_Crypto)
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

genDCerts :: GenRS [DCert C_Crypto]
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
  DPState {_dstate = DState {_rewards}} <- gsDPState <$> get
  let initSets = ([], Set.empty, Map.keysSet _rewards)
  (dcs, _, _) <- F.foldlM genUniqueScript initSets [1 :: Int .. n]
  pure $ reverse dcs

genCollateralUTxO ::
  HasCallStack =>
  [Addr C_Crypto] ->
  Coin ->
  UTxO A ->
  GenRS (UTxO A, Map.Map (TxIn C_Crypto) (TxOut A))
genCollateralUTxO collateralAddresses (Coin fee) (UTxO utxoMap) = do
  GenEnv {gePParams} <- ask
  let collPerc = _collateralPercentage gePParams
      minCollTotal = Coin (ceiling ((fee * toInteger collPerc) % 100))
      -- Generate a collateral that is neither in UTxO map nor has already been generated
      genNewCollateral addr coll um c = do
        txIn <- lift arbitrary
        if Map.member txIn utxoMap || Map.member txIn coll
          then genNewCollateral addr coll um c
          else pure (um, Map.insert txIn (TxOut addr (inject c) SNothing) coll, c)
      -- Either pick a collateral from a map or generate a completely new one
      genCollateral addr coll um
        | Map.null um = genNewCollateral addr coll um =<< lift genPositiveVal
        | otherwise = do
          i <- lift $ chooseInt (0, Map.size um - 1)
          let (txIn, txOut@(TxOut _ val _)) = Map.elemAt i um
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
  collaterals <- go collateralAddresses Map.empty (Coin 0) $ Map.filter spendOnly utxoMap
  pure (UTxO (Map.union utxoMap collaterals), collaterals)
  where
    spendOnly (TxOut (Addr _ (ScriptHashObj _) _) _ _) = False
    spendOnly (TxOut (Addr _ _ (StakeRefBase (ScriptHashObj _))) _ _) = False
    spendOnly _ = True

genUTxOState :: UTxO A -> GenRS (UTxOState A)
genUTxOState utxo = do
  GenEnv {gePParams} <- ask
  DPState {_dstate, _pstate} <- gsDPState <$> get
  let deposited = obligation gePParams (_rewards _dstate) (_pParams _pstate)
  lift (UTxOState utxo deposited <$> arbitrary <*> pure def)

genRecipientsFrom :: [TxOut A] -> GenRS [TxOut A]
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
      goExtra e n s (TxOut _ v _) (tx : txs) !rs = goExtra e (n - 1) (s <+> v) tx txs rs
      genWithChange s (TxOut addr v d) rs = do
        c <- Coin <$> lift (choose (1, unCoin $ coin v))
        r <- genTxOut (s <+> inject c)
        if c < coin v
          then
            let !change = TxOut addr (v <-> inject c) d
             in pure (r : change : rs)
          else pure (r : rs)
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

genRewards :: GenRS (RewardAccounts C_Crypto)
genRewards = do
  NonNegative n <- lift arbitrary
  rewards <-
    Map.fromList <$> replicateM n ((,) <$> genCredential Rewrd <*> lift genPositiveVal)
  modifyDState $ \ds -> ds {_rewards = rewards `Map.union` _rewards ds}
  pure rewards

genWithdrawals :: GenRS (Wdrl C_Crypto)
genWithdrawals = do
  let networkId = Testnet
  rewards <- genRewards
  pure $ Wdrl $ Map.fromList $ map (first (RewardAcnt networkId)) $ Map.toList rewards

languagesUsed ::
  ValidatedTx A ->
  UTxO A ->
  Map (ScriptHash C_Crypto, Tag) (IsValid, Script A) ->
  Set Language
languagesUsed tx utxo plutusScripts =
  Set.fromList [lang | (_, PlutusScript lang _) <- mapMaybe lookupPlutus needed]
  where
    needed = scriptsNeeded utxo tx
    lookupPlutus ((Spending _), sh) = Map.lookup (sh, Spend) plutusScripts
    lookupPlutus ((Rewarding _), sh) = Map.lookup (sh, Rewrd) plutusScripts
    lookupPlutus ((Certifying _), sh) = Map.lookup (sh, Cert) plutusScripts
    lookupPlutus ((Minting _), sh) = Map.lookup (sh, Mint) plutusScripts

genValidatedTx :: GenRS (UTxO A, ValidatedTx A)
genValidatedTx = do
  GenEnv {geValidityInterval, gePParams} <- ask
  UTxO utxoNoCollateral <- genUTxO
  -- 1. Produce utxos that will be spent
  n <- lift $ choose (1, length utxoNoCollateral)
  toSpendNoCollateral <-
    Map.fromList . take n <$> lift (shuffle $ Map.toList utxoNoCollateral)
  -- 2. Check if all Plutus scripts are valid
  let toSpendNoCollateralTxOuts = Map.elems toSpendNoCollateral
      -- We use maxBound to ensure the serializaed size overestimation
      maxCoin = Coin (toInteger (maxBound :: Int))
  -- 3. Generate all recipients and witnesses needed for spending Plutus scripts
  recipients <- genRecipientsFrom toSpendNoCollateralTxOuts
  (IsValid v1, mkPaymentWits) <-
    redeemerWitnessMaker
      Spend
      [ (\dh c -> (lookupByKeyM "datum" dh gsDatums, c))
          <$> strictMaybeToMaybe mDatumHash
          <*> paymentCredAddr addr
        | (_, TxOut addr _ mDatumHash) <- Map.toAscList toSpendNoCollateral
      ]

  wdrls@(Wdrl wdrlMap) <- genWithdrawals
  rewardsWithdrawalTxOut <- genTxOut $ inject $ F.fold wdrlMap
  let wdrlCreds = map (getRwdCred . fst) $ Map.toAscList wdrlMap
  (IsValid v2, mkWdrlWits) <-
    redeemerWitnessMaker Rewrd $ map (Just . (,) genDatum) wdrlCreds

  dcerts <- genDCerts
  let dcertCreds = map getDCertCredential dcerts
  (IsValid v3, mkCertsWits) <-
    redeemerWitnessMaker Cert $ map ((,) genDatum <$>) dcertCreds

  let isValid = IsValid (v1 && v2 && v3)
      mkWits = mkPaymentWits ++ mkCertsWits ++ mkWdrlWits
  exUnits <- genExUnits (length mkWits)
  let redeemerWitsList = zipWith ($) mkWits exUnits
  datumWitsList <- mapM makeDatumWitness (Map.elems toSpendNoCollateral)
  keyWitsMakers <- mapM (genTxOutTimelockKeyWitness (Just Spend)) toSpendNoCollateralTxOuts
  dcertWitsMakers <- mapM (genCredTimelockKeyWit (Just Cert)) $ catMaybes dcertCreds
  rwdrsWitsMakers <- mapM (genCredTimelockKeyWit (Just Rewrd)) wdrlCreds
  -- 4. Estimate inputs that will be used as collateral
  maxCollateralCount <-
    lift $ chooseInt (1, fromIntegral (_maxCollateralInputs gePParams))
  bogusCollateralTxId <- lift arbitrary
  let bogusCollateralTxIns =
        Set.fromList
          [ TxIn bogusCollateralTxId (fromIntegral i)
            | i <- [maxBound, maxBound - 1 .. maxBound - maxCollateralCount - 1]
          ]
  collateralAddresses <- replicateM maxCollateralCount genNoScriptRecipient
  bogusCollateralKeyWitsMakers <-
    forM collateralAddresses $ \a ->
      genTxOutTimelockKeyWitness Nothing $ TxOut a (inject maxCoin) SNothing
  networkId <- lift $ elements [SNothing, SJust Testnet]
  -- 5. Estimate the fee
  let redeemerDatumWits = mconcat (redeemerWitsList ++ datumWitsList)
      bogusIntegrityHash = hashScriptIntegrity gePParams mempty (Redeemers mempty) mempty
      txBodyNoFee =
        TxBody
          { inputs = Map.keysSet toSpendNoCollateral,
            collateral = bogusCollateralTxIns,
            outputs = Seq.fromList (rewardsWithdrawalTxOut : recipients),
            txcerts = Seq.fromList dcerts,
            txwdrls = wdrls,
            txfee = maxCoin,
            txvldt = geValidityInterval,
            txUpdates = SNothing,
            reqSignerHashes = mempty,
            mint = mempty,
            scriptIntegrityHash = bogusIntegrityHash,
            adHash = SNothing,
            txnetworkid = networkId
          }
      txBodyNoFeeHash = hashAnnotated txBodyNoFee
      witsMakers = keyWitsMakers ++ dcertWitsMakers ++ rwdrsWitsMakers
      noFeeWits =
        redeemerDatumWits
          <> foldMap ($ txBodyNoFeeHash) (witsMakers ++ bogusCollateralKeyWitsMakers)
      bogusTxForFeeCalc = ValidatedTx txBodyNoFee noFeeWits isValid SNothing
      fee = minfee gePParams bogusTxForFeeCalc
  -- 6. Crank up the amount in one of outputs to account for the fee. Note this is
  -- a hack that is not possible in a real life, but in the end it does produce
  -- real life like setup
  feeKey <- lift $ elements $ Map.keys toSpendNoCollateral
  let injectFee (TxOut addr val mdh) = TxOut addr (val <+> inject fee) mdh
      utxoFeeAdjusted =
        UTxO $ Map.update (Just . injectFee) feeKey utxoNoCollateral
  -- 7. Generate utxos that will be used as collateral
  (utxo, collMap) <- genCollateralUTxO collateralAddresses fee utxoFeeAdjusted
  collateralKeyWitsMakers <- mapM (genTxOutTimelockKeyWitness Nothing) $ Map.elems collMap
  -- 8. Construct the correct Tx with valid fee and collaterals
  allPlutusScripts <- gsPlutusScripts <$> get
  let mIntegrityHash =
        hashScriptIntegrity
          gePParams
          (languagesUsed bogusTxForFeeCalc (UTxO utxoNoCollateral) allPlutusScripts)
          (txrdmrs redeemerDatumWits)
          (txdats redeemerDatumWits)
      txBody =
        txBodyNoFee
          { txfee = fee,
            collateral = Map.keysSet collMap,
            scriptIntegrityHash = mIntegrityHash
          }
      txBodyHash = hashAnnotated txBody
      wits =
        redeemerDatumWits
          <> foldMap ($ txBodyHash) (witsMakers ++ collateralKeyWitsMakers)
      validTx = ValidatedTx txBody wits isValid SNothing
  pure (utxo, validTx)

genTxAndUTXOState :: Gen (TRC (AlonzoUTXOW A), GenState)
genTxAndUTXOState = do
  (TRC (LedgerEnv slotNo _ pp _, (utxoState, _), vtx), genState) <-
    genTxAndLEDGERState
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), utxoState, vtx), genState)

genTxAndLEDGERState :: Gen (TRC (AlonzoLEDGER A), GenState)
genTxAndLEDGERState = do
  txIx <- arbitrary
  maxTxExUnits <- arbitrary
  Positive maxCollateralInputs <- arbitrary
  collateralPercentage <- fromIntegral <$> chooseInt (0, 10000)
  minfeeA <- fromIntegral <$> chooseInt (0, 1000)
  minfeeB <- fromIntegral <$> chooseInt (0, 10000)
  let genT = do
        (utxo, tx) <- genValidatedTx
        utxoState <- genUTxOState utxo
        dpState <- gsDPState <$> get
        pure $ TRC (ledgerEnv, (utxoState, dpState), tx)
      pp :: PParams A
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

totalAda :: UTxOState A -> DPState C_Crypto -> Coin
totalAda (UTxOState utxo f d _) DPState {_dstate} =
  f <> d <> coin (balance utxo) <> F.fold (_rewards _dstate)

testTxValidForLEDGER :: (TRC (AlonzoLEDGER A), GenState) -> Property
testTxValidForLEDGER (trc@(TRC (_, (utxoState, dpstate), vtx)), _) =
  case runShelleyBase $ applySTSTest trc of
    Right (utxoState', dpstate') ->
      classify (coerce (isValid vtx)) "TxValid" $
        totalAda utxoState' dpstate' === totalAda utxoState dpstate
    Left e -> counterexample (show e) (property False)

alonzoProperties :: TestTree
alonzoProperties =
  testGroup
    "Alonzo UTXOW property tests"
    [ testProperty "test ValidTx" $ forAll genTxAndLEDGERState testTxValidForLEDGER
    ]
