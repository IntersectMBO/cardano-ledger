{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Properties where

import Cardano.Ledger.Alonzo (AlonzoEra, Value)
import Cardano.Ledger.Alonzo.Data (Data, DataHash, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx
  ( IsValid (..),
    ValidatedTx (..),
    hashScriptIntegrity,
    minfee,
  )
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers (..), TxDats (..), TxWitness (..))
import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (..), strictMaybeToMaybe)
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
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.Val
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM, join, replicateM)
import Control.Monad.State.Strict (MonadState (..), modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWST (..), ask, evalRWST)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Bifunctor (first)
import Data.Coerce
import Data.Default.Class (Default (def))
import qualified Data.Foldable as F
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (All (..))
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import GHC.Stack
import Numeric.Natural
import Plutus.V1.Ledger.Api (defaultCostModelParams)
import Shelley.Spec.Ledger.API
  ( Addr (..),
    Credential (..),
    StakeReference (..),
    TxIn (..),
    UTxO (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.LedgerState (UTxOState (..))
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Shelley.Spec.Ledger.Tx (hashScript)
import Shelley.Spec.Ledger.UTxO (balance, makeWitnessVKey)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Shelley.Spec.Ledger.Utils (applySTSTest, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

type A = AlonzoEra C_Crypto

elementsT :: (Monad (t Gen), MonadTrans t) => [t Gen b] -> t Gen b
elementsT = join . lift . elements

frequencyT :: (Monad (t Gen), MonadTrans t) => [(Int, t Gen b)] -> t Gen b
frequencyT = join . lift . frequency . map (pure <$>)

lookupByHashM ::
  (MonadState s m, Ord k, Show k) => String -> k -> (s -> Map.Map k v) -> m v
lookupByHashM name k getMap = do
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
    gsScripts :: Map (ScriptHash C_Crypto) (StrictMaybe (Tag, IsValid), Script A),
    gsDatums :: Map (DataHash C_Crypto) (Data A)
  }

instance Semigroup GenState where
  ts1 <> ts2 =
    GenState
      (gsKeys ts1 <> gsKeys ts2)
      (gsScripts ts1 <> gsScripts ts2)
      (gsDatums ts1 <> gsDatums ts2)

instance Monoid GenState where
  mempty = GenState mempty mempty mempty

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
        x <- min totalLeft . round . (% un) <$> choose (0, maxVal)
        pure (totalLeft - x, x : acc)
    genSequenceSum maxVal
      | maxVal == 0 = pure $ replicate n 0
      | otherwise = snd <$> F.foldlM (genUpTo maxVal) (maxVal, []) [1 .. n]

-- genTxPlutusWitness :: ExUnits -> Int -> DataHash C_Crypto -> GenRS (TxWitness A)
-- genTxPlutusWitness txIx datumHash exUnits = do
--   (tag, datum) <- lookupByHashM "datum" datumHash gsDatums
--   let rPtr = RdmrPtr tag (fromIntegral txIx)
--   pure $
--     mempty
--       { txrdmrs = Redeemers $ Map.singleton rPtr (datum, exUnits),
--         txdats = TxDats $ Map.singleton datumHash datum
--       }

-- | Generator for witnesses needed for Timelock scripts and Key
-- credentials. Because of the latter produced function requires a body hash for
-- an acutal witness to be constructed.
genTimelockKeyWitness ::
  TxOut A -> GenRS (SafeHash C_Crypto EraIndependentTxBody -> TxWitness A)
genTimelockKeyWitness (TxOut addr _ _) = do
  case addr of
    AddrBootstrap baddr ->
      error $ "Can't authorize bootstrap address: " ++ show baddr
    Addr _ payCred stakeCred -> do
      let mkWitVKey ::
            Credential kr C_Crypto ->
            GenRS (SafeHash C_Crypto EraIndependentTxBody -> TxWitness A)
          mkWitVKey (KeyHashObj keyHash) = do
            cred <- lookupByHashM "credential" (coerceKeyRole keyHash) gsKeys
            pure $ \bodyHash ->
              mempty
                { txwitsVKey = Set.singleton $ makeWitnessVKey bodyHash cred
                }
          mkWitVKey (ScriptHashObj scriptHash) = do
            s@(mDatumHash, script) <-
              lookupByHashM "script" scriptHash gsScripts
            let scriptWit = mempty {txscripts = Map.singleton scriptHash script}
            case script of
              TimelockScript timelock
                | SNothing <- mDatumHash -> do
                  timelockWit <- mkTimelockWit timelock
                  pure $ \bodyHash -> timelockWit bodyHash <> scriptWit
              PlutusScript _ps
                | SJust (_datumHash, _) <- mDatumHash -> pure $ const scriptWit
              _ ->
                error $ "Impossible: Mismatch in generated script: " ++ show s
          mkTimelockWit =
            \case
              RequireSignature keyHash -> mkWitVKey (KeyHashObj keyHash)
              RequireAllOf timelocks -> F.fold <$> mapM mkTimelockWit timelocks
              RequireAnyOf timelocks
                | F.null timelocks -> pure mempty
                | otherwise ->
                  mkTimelockWit =<< lift (elements (F.toList timelocks))
              RequireMOf m timelocks -> do
                ts <- take m <$> lift (shuffle (F.toList timelocks))
                F.fold <$> mapM mkTimelockWit ts
              RequireTimeStart _ -> pure mempty
              RequireTimeExpire _ -> pure mempty
          mkStakeWit (StakeRefBase cred) = mkWitVKey cred
          mkStakeWit _ = pure mempty
      witVKey <- mkWitVKey payCred
      stakeWitVKey <- mkStakeWit stakeCred
      pure $ witVKey <> stakeWitVKey

genKeyHash :: GenRS (KeyHash 'Witness C_Crypto)
genKeyHash = do
  keyPair <- lift arbitrary
  let keyHash = hashKey $ vKey keyPair
  modify $ \ts@GenState {gsKeys} -> ts {gsKeys = Map.insert keyHash keyPair gsKeys}
  pure keyHash

genTimelock :: GenRS (Timelock C_Crypto)
genTimelock = do
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
  genNestedTimelock (2 :: Natural)

genScript :: Tag -> GenRS (ScriptHash C_Crypto)
genScript tag =
  elementsT
    [ toScriptHash . (,) SNothing . TimelockScript =<< genTimelock,
      toScriptHash =<< genPlutusScript
    ]
  where
    genPlutusScript = do
      isValid <- lift $ frequency [(5, pure False), (95, pure True)]
      -- Plutus scripts expect exactly 3 arguments to work properly.
      let script
            | isValid = alwaysSucceeds 3
            | otherwise = alwaysFails 3
      pure (SJust (tag, IsValid isValid), script)
    toScriptHash s@(_, script) = do
      let scriptHash = hashScript @A script
      modify $ \ts@GenState {gsScripts} -> ts {gsScripts = Map.insert scriptHash s gsScripts}
      pure scriptHash

lookupScriptValidity :: MonadState GenState m => Addr C_Crypto -> m (StrictMaybe IsValid)
lookupScriptValidity (Addr _ (ScriptHashObj scriptHash) _) =
  fmap snd . fst <$> lookupByHashM "script" scriptHash gsScripts
lookupScriptValidity _ = pure SNothing

paymentCredAddr :: Addr C_Crypto -> Maybe (Credential 'Payment C_Crypto)
paymentCredAddr (Addr _ cred _) = Just cred
paymentCredAddr _ = Nothing

stakeCredAddr :: Addr C_Crypto -> Maybe (Credential 'Staking C_Crypto)
stakeCredAddr (Addr _ _ (StakeRefBase cred)) = Just cred
stakeCredAddr _ = Nothing

lookupPlutusScript ::
  MonadState GenState m =>
  Credential k C_Crypto ->
  m (Maybe (Tag, IsValid))
lookupPlutusScript (KeyHashObj _) = pure Nothing
lookupPlutusScript (ScriptHashObj scriptHash) =
  lookupByHashM "script" scriptHash gsScripts <&> \case
    (SJust (tag, isValid), _) -> Just (tag, isValid)
    _ -> Nothing

plutusWitnessMaker ::
  [Maybe (GenRS (DataHash C_Crypto, Data A), Credential k C_Crypto)] ->
  GenRS (IsValid, [ExUnits -> TxWitness A])
plutusWitnessMaker listWithCred =
  let creds =
        [ (ix, genDat, cred)
          | (ix, mCred) <- zip [0 ..] listWithCred,
            Just (genDat, cred) <- [mCred]
        ]
      allValid = IsValid . getAll . foldMap (\(IsValid v) -> All v)
   in fmap (first allValid . unzip . catMaybes) $
        forM creds $ \(ix, genDat, cred) ->
          lookupPlutusScript cred >>= \case
            Nothing -> pure Nothing
            Just (tag, isValid) -> do
              (datumHash, datum) <- genDat
              let rPtr = RdmrPtr tag ix
                  mkWit exUnits =
                    mempty
                      { txrdmrs = Redeemers $ Map.singleton rPtr (datum, exUnits),
                        txdats = TxDats $ Map.singleton datumHash datum
                      }
              pure $ Just (isValid, mkWit)

genNoScriptRecipient :: GenRS (Addr C_Crypto)
genNoScriptRecipient = do
  paymentCred <- coerceKeyRole . KeyHashObj <$> genKeyHash
  stakeCred <- StakeRefBase . coerceKeyRole . KeyHashObj <$> genKeyHash
  pure (Addr Testnet paymentCred stakeCred)

genCredential :: Tag -> GenRS (Credential kr C_Crypto)
genCredential tag =
  elementsT
    [ coerceKeyRole . KeyHashObj <$> genKeyHash,
      ScriptHashObj <$> genScript tag
    ]

genRecipient :: GenRS (Addr C_Crypto)
genRecipient = do
  paymentCred <- genCredential Spend
  stakeCred <- coerceKeyRole . KeyHashObj <$> genKeyHash
  --stakeCred <- genCredential Rewrd
  pure (Addr Testnet paymentCred (StakeRefBase stakeCred))

genDatumHash :: GenRS (DataHash C_Crypto)
genDatumHash = fst <$> genDatum

genDatum :: GenRS (DataHash C_Crypto, Data A)
genDatum = do
  datum <- lift arbitrary
  let datumHash = hashData datum
  modify $ \ts@GenState {gsDatums} -> ts {gsDatums = Map.insert datumHash datum gsDatums}
  pure (datumHash, datum)

genTxOut :: Value A -> GenRS (TxOut A)
genTxOut val = do
  addr <- genRecipient --frequencyT [(80, genRecipient), (20, pickExistingRecipient)]
  cred <- maybe (error "BootstrapAddress encountered") pure $ paymentCredAddr addr
  mDatumHash <-
    lookupPlutusScript cred >>= \case
      Nothing -> pure SNothing
      Just _ -> SJust <$> genDatumHash
  pure $ TxOut addr val mDatumHash

-- | Generate a non-zero value
genVal :: Val v => Gen v
genVal = inject . Coin . getPositive <$> arbitrary

genUTxO :: GenRS (UTxO A)
genUTxO = do
  NonEmpty ins <- lift $ resize 10 arbitrary
  UTxO <$> sequence (Map.fromSet (const genOut) (Set.fromList ins))
  where
    genOut = genTxOut =<< lift genVal

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
        | Map.null um = genNewCollateral addr coll um =<< lift genVal
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
genUTxOState utxo =
  lift (UTxOState utxo <$> arbitrary <*> arbitrary <*> pure def)

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
            let change = TxOut addr (v <-> inject c) d
             in pure (r : change : rs)
          else pure (r : rs)
  goNew extra txOuts []

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
    plutusWitnessMaker
      [ (\dh c -> ((,) dh <$> lookupByHashM "datum" dh gsDatums, c))
          <$> strictMaybeToMaybe mDatumHash <*> paymentCredAddr addr
        | (_, TxOut addr _ mDatumHash) <- Map.toAscList toSpendNoCollateral
      ]
  (IsValid v2, mkStakingWits) <-
    plutusWitnessMaker
      [ (,) genDatum <$> stakeCredAddr addr
        | (_, TxOut addr _ _) <- Map.toAscList toSpendNoCollateral
      ]
  let isValid = IsValid (v1 && v2)
  exUnits <- genExUnits (length mkPaymentWits + length mkStakingWits)
  let redeemerWitsList = zipWith ($) (mkPaymentWits ++ mkStakingWits) exUnits

  keyWitsMakers <- mapM genTimelockKeyWitness toSpendNoCollateralTxOuts
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
      genTimelockKeyWitness $ TxOut a (inject maxCoin) SNothing
  networkId <- lift $ elements [SNothing, SJust Testnet]
  -- 5. Estimate the fee
  let redeemerWits = mconcat redeemerWitsList
      mIntegrityHash =
        hashScriptIntegrity
          gePParams
          ( if null redeemerWitsList
              then Set.empty
              else Set.singleton PlutusV1
          )
          (txrdmrs redeemerWits)
          (txdats redeemerWits)
      txBodyNoFee =
        TxBody
          { inputs = Map.keysSet toSpendNoCollateral,
            collateral = bogusCollateralTxIns,
            outputs = Seq.fromList recipients, -- has scripts
            txcerts = mempty, -- has scripts
            txwdrls = Wdrl mempty, -- has scripts
            txfee = maxCoin,
            txvldt = geValidityInterval,
            txUpdates = SNothing,
            reqSignerHashes = mempty,
            mint = mempty,
            scriptIntegrityHash = mIntegrityHash,
            adHash = SNothing,
            txnetworkid = networkId
          }
      txBodyNoFeeHash = hashAnnotated txBodyNoFee
      noFeeWits =
        redeemerWits
          <> foldMap
            ($ txBodyNoFeeHash)
            (keyWitsMakers ++ bogusCollateralKeyWitsMakers)
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
  collateralKeyWitsMakers <- mapM genTimelockKeyWitness $ Map.elems collMap
  -- 8. Construct the correct Tx with valid fee and collaterals
  let txBody = txBodyNoFee {txfee = fee, collateral = Map.keysSet collMap}
      txBodyHash = hashAnnotated txBody
      wits =
        redeemerWits
          <> foldMap ($ txBodyHash) (keyWitsMakers ++ collateralKeyWitsMakers)
      validTx = ValidatedTx txBody wits isValid SNothing
  pure (utxo, validTx)

genTxAndUTXOState :: Gen (TRC (AlonzoUTXOW A))
genTxAndUTXOState = do
  maxTxExUnits <- arbitrary
  Positive maxCollateralInputs <- arbitrary
  collateralPercentage <- fromIntegral <$> chooseInt (0, 10000)
  minfeeA <- fromIntegral <$> chooseInt (0, 1000)
  minfeeB <- fromIntegral <$> chooseInt (0, 10000)
  let genT = do
        (utxo, tx) <- genValidatedTx
        utxoState <- genUTxOState utxo
        pure $ TRC (utxoEnv, utxoState, tx)
      pp :: PParams A
      pp =
        def
          { _minfeeA = minfeeA,
            _minfeeB = minfeeB,
            _costmdls = Map.singleton PlutusV1 $ CostModel $ 0 <$ fromJust defaultCostModelParams,
            _maxValSize = 1000,
            _maxTxSize = fromIntegral (maxBound :: Int),
            _maxTxExUnits = maxTxExUnits,
            _collateralPercentage = collateralPercentage,
            _maxCollateralInputs = maxCollateralInputs
          }
      slotNo = SlotNo 100000000
      utxoEnv = UtxoEnv slotNo pp mempty (GenDelegs mempty)
  minSlotNo <- oneof [pure SNothing, SJust <$> choose (minBound, unSlotNo slotNo)]
  maxSlotNo <- oneof [pure SNothing, SJust <$> choose (unSlotNo slotNo + 1, maxBound)]
  let env =
        GenEnv
          { geValidityInterval = ValidityInterval (SlotNo <$> minSlotNo) (SlotNo <$> maxSlotNo),
            gePParams = pp
          }
  fst <$> evalRWST genT env mempty

totalAda :: UTxOState A -> Coin
totalAda (UTxOState utxo f d _) = f <> d <> coin (balance utxo)

testTxValidForUTXOW :: TRC (AlonzoUTXOW A) -> Property
testTxValidForUTXOW trc@(TRC (UtxoEnv _ _pp _ _, utxoState, vtx)) =
  case runShelleyBase $ applySTSTest trc of
    Right utxoState' ->
      classify (coerce (isValid vtx)) "TxValid" $
        totalAda utxoState' === totalAda utxoState
    Left e -> counterexample (show e) (property False)

testValidTxForUTXOW :: Property
testValidTxForUTXOW = forAll genTxAndUTXOState testTxValidForUTXOW

alonzoProperties :: TestTree
alonzoProperties =
  testGroup
    "Alonzo UTXOW property tests"
    [ testProperty "test ValidTx" testValidTxForUTXOW
    ]
