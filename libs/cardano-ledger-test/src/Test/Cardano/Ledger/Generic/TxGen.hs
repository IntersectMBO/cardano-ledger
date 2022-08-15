{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.TxGen
  ( genAlonzoTx,
    Box (..),
    applySTSByProof,
    assembleWits,
    coreTx,
    coreTxBody,
    coreTxOut,
    genUTxO,
    testTx,
  )
where

import Cardano.Ledger.Alonzo.Data (Data, dataToBinaryData, hashData)
import Cardano.Ledger.Alonzo.PParams (AlonzoPParamsHKD (..))
import Cardano.Ledger.Alonzo.Scripts hiding (Script)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    Redeemers (..),
    TxDats (..),
  )
import Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..), Datum (..))
import Cardano.Ledger.BaseTypes (Network (..), mkTxIxPartial)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
    coerceKeyRole,
  )
import Cardano.Ledger.Pretty (PrettyA (..), ppRecord)
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.API
  ( Addr (..),
    Credential (..),
    PoolCert (RegPool, RetirePool),
    PoolParams (..),
    RewardAcnt (..),
    StakeReference (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.LedgerState (RewardAccounts)
import qualified Cardano.Ledger.Shelley.PParams as Shelley (ShelleyPParamsHKD (..))
import qualified Cardano.Ledger.Shelley.Scripts as Shelley (MultiSig (..))
import Cardano.Ledger.Shelley.TxBody (DCert (..), DelegCert (..), Delegation (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..), makeWitnessVKey)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.Slot (EpochNo (EpochNo))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM, replicateM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (asks, get, gets, modify)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Bifunctor (first)
import Data.Default.Class (Default (def))
import qualified Data.Foldable as F
import Data.Functor
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Monoid (All (..))
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word16)
import GHC.Stack
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Generic.Fields hiding (Mint)
import qualified Test.Cardano.Ledger.Generic.Fields as Generic (TxBodyField (Mint))
import Test.Cardano.Ledger.Generic.Functions
import Test.Cardano.Ledger.Generic.GenState
  ( GenEnv (..),
    GenRS,
    GenState (..),
    elementsT,
    frequencyT,
    genCredential,
    genDatumWithHash,
    genFreshRegCred,
    genKeyHash,
    genNewPool,
    genPool,
    genPositiveVal,
    genRetirementHash,
    genRewards,
    genScript,
    genValidityInterval,
    getCertificateMax,
    getOldUtxoPercent,
    getRefInputsMax,
    getSpendInputsMax,
    getUtxoChoicesMax,
    getUtxoElem,
    getUtxoTest,
    modifyModel,
    runGenRS,
  )
import Test.Cardano.Ledger.Generic.ModelState
  ( MUtxo,
    ModelNewEpochState (..),
    UtxoEntry,
    pcModelNewEpochState,
  )
import Test.Cardano.Ledger.Generic.PrettyCore (pcTx)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Updaters hiding (first)
import Test.Cardano.Ledger.Shelley.Generator.Core (genNatural)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo, runShelleyBase)
import Test.QuickCheck

-- ===================================================
-- Assembing lists of Fields in to (XX era)

-- | This uses merging semantics, it expects duplicate fields, and merges them together
assembleWits :: Era era => Proof era -> [WitnessesField era] -> TxWits era
assembleWits era = List.foldl' (updateWitnesses merge era) (initialWitnesses era)

coreTxOut :: Era era => Proof era -> [TxOutField era] -> TxOut era
coreTxOut era dts = List.foldl' (updateTxOut era) (initialTxOut era) dts

coreTxBody :: EraTxBody era => Proof era -> [TxBodyField era] -> TxBody era
coreTxBody era dts = List.foldl' (updateTxBody era) (initialTxBody era) dts

overrideTxBody :: EraTxBody era => Proof era -> TxBody era -> [TxBodyField era] -> TxBody era
overrideTxBody era old dts = List.foldl' (updateTxBody era) old dts

coreTx :: Proof era -> [TxField era] -> Tx era
coreTx era dts = List.foldl' (updateTx era) (initialTx era) dts

-- ====================================================================

lookupByKeyM ::
  (Ord k, Show k, HasCallStack) => String -> k -> (GenState era -> Map.Map k v) -> GenRS era v
lookupByKeyM name k getMap = do
  m <- gets getMap
  case Map.lookup k m of
    Nothing ->
      error $
        "Can't find " ++ name ++ " in the test enviroment: " ++ show k
    Just val -> pure val

-- | Generate a list of specified length with randomish `ExUnit`s where the sum
--   of all values produced will not exceed the maxTxExUnits.
genExUnits :: Proof era -> Int -> GenRS era [ExUnits]
genExUnits era n = do
  GenEnv {gePParams} <- gets gsGenEnv
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

lookupScript ::
  forall era.
  ScriptHash (Crypto era) ->
  Maybe Tag ->
  GenRS era (Maybe (Script era))
lookupScript scriptHash mTag = do
  m <- gsScripts <$> get
  case Map.lookup scriptHash m of
    Just script -> pure $ Just script
    Nothing
      | Just tag <- mTag ->
          Just . snd <$> lookupByKeyM "plutusScript" (scriptHash, tag) gsPlutusScripts
    _ -> pure Nothing

-- ========================================================================
-- Generating TxWits, here we are not adding anything to the GenState
-- only looking up things already added, and assembling the right pieces to
-- make TxWits.

-- | Used in Shelley Eras
mkMultiSigWit ::
  forall era.
  (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Shelley.MultiSig (Crypto era) ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
mkMultiSigWit era mTag (Shelley.RequireSignature keyHash) = mkWitVKey era mTag (KeyHashObj keyHash)
mkMultiSigWit era mTag (Shelley.RequireAllOf timelocks) = F.fold <$> mapM (mkMultiSigWit era mTag) timelocks
mkMultiSigWit era mTag (Shelley.RequireAnyOf timelocks)
  | F.null timelocks = pure (const [])
  | otherwise = mkMultiSigWit era mTag =<< lift (elements (F.toList timelocks))
mkMultiSigWit era mTag (Shelley.RequireMOf m timelocks) = do
  ts <- take m <$> lift (shuffle (F.toList timelocks))
  F.fold <$> mapM (mkMultiSigWit era mTag) ts

-- | Timeock scripts are used in Mary and subsequent Eras.
mkTimelockWit ::
  forall era.
  (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Timelock (Crypto era) ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
mkTimelockWit era mTag =
  \case
    RequireSignature keyHash -> mkWitVKey era mTag (KeyHashObj keyHash)
    RequireAllOf timelocks -> F.fold <$> mapM (mkTimelockWit era mTag) timelocks
    RequireAnyOf timelocks
      | F.null timelocks -> pure (const [])
      | otherwise -> mkTimelockWit era mTag =<< lift (elements (F.toList timelocks))
    RequireMOf m timelocks -> do
      ts <- take m <$> lift (shuffle (F.toList timelocks))
      F.fold <$> mapM (mkTimelockWit era mTag) ts
    RequireTimeStart _ -> pure (const [])
    RequireTimeExpire _ -> pure (const [])

genGenericScriptWitness ::
  (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Script era ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
genGenericScriptWitness (Shelley c) mTag timelock = mkMultiSigWit (Shelley c) mTag timelock
genGenericScriptWitness (Allegra c) mTag timelock = mkTimelockWit (Allegra c) mTag timelock
genGenericScriptWitness (Mary c) mTag timelock = mkTimelockWit (Mary c) mTag timelock
genGenericScriptWitness (Alonzo c) mTag (TimelockScript timelock) = mkTimelockWit (Alonzo c) mTag timelock
genGenericScriptWitness (Alonzo _) _ (PlutusScript _ _) = pure (const [])
genGenericScriptWitness (Babbage c) mTag (TimelockScript timelock) = mkTimelockWit (Babbage c) mTag timelock
genGenericScriptWitness (Babbage _) _ (PlutusScript _ _) = pure (const [])
genGenericScriptWitness (Conway c) mTag (TimelockScript timelock) = mkTimelockWit (Conway c) mTag timelock
genGenericScriptWitness (Conway _) _ (PlutusScript _ _) = pure (const [])

-- | Generate a TxWits producing function. We handle TxWits come from Keys and Scripts
--   Because scripts vary be Era, we need some Era specific code here: genGenericScriptWitness
mkWitVKey ::
  forall era kr.
  (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Credential kr (Crypto era) ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
mkWitVKey _ _mTag (KeyHashObj keyHash) = do
  keyPair <- lookupByKeyM "credential" (coerceKeyRole keyHash) gsKeys
  pure $ \bodyHash -> [AddrWits' [makeWitnessVKey bodyHash keyPair]]
mkWitVKey era mTag (ScriptHashObj scriptHash) =
  lookupScript @era scriptHash mTag >>= \case
    Nothing ->
      error $ "Impossible: Cannot find script with hash " ++ show scriptHash
    Just script -> do
      let scriptWit = ScriptWits' [script]
      otherWit <- genGenericScriptWitness era mTag script
      pure (\hash -> scriptWit : otherWit hash)

-- | Same as `genCredKeyWit`, but for `TxOuts`
genTxOutKeyWitness ::
  forall era.
  Reflect era =>
  Proof era ->
  Maybe Tag ->
  TxOut era ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
genTxOutKeyWitness era mTag txOut =
  case txOut ^. addrTxOutL of
    AddrBootstrap baddr ->
      error $ "Can't authorize bootstrap address: " ++ show baddr
    Addr _ payCred _ ->
      case getTxOutRefScript reify txOut of
        SNothing -> mkWitVKey era mTag payCred
        SJust script -> do
          f1 <- mkWitVKey era mTag payCred
          f2 <- genGenericScriptWitness reify (Just Spend) script
          pure (\safehash -> f1 safehash ++ f2 safehash)

genCredKeyWit ::
  forall era k.
  (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Credential k (Crypto era) ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
genCredKeyWit era mTag cred = mkWitVKey era mTag cred

makeDatumWitness :: Proof era -> TxOut era -> GenRS era [WitnessesField era]
makeDatumWitness proof txout = case (proof, txout) of
  (Babbage _, BabbageTxOut _ _ (DatumHash h) _) -> mkDatumWit (SJust h)
  (Babbage _, BabbageTxOut _ _ (Datum _) _) -> pure []
  (Babbage _, BabbageTxOut _ _ NoDatum _) -> pure []
  (Conway _, BabbageTxOut _ _ (DatumHash h) _) -> mkDatumWit (SJust h)
  (Conway _, BabbageTxOut _ _ (Datum _) _) -> pure []
  (Conway _, BabbageTxOut _ _ NoDatum _) -> pure []
  (Alonzo _, AlonzoTxOut _ _ mDatum) -> mkDatumWit mDatum
  _ -> pure [] -- No other era has data witnesses
  where
    mkDatumWit SNothing = pure []
    mkDatumWit (SJust datumHash) = do
      datum <- lookupByKeyM "datum" datumHash gsDatums
      pure [DataWits' [datum]]

-- | Does the current Credential point to a PlutusScript? If so return its IsValid and Hash
lookupPlutusScript ::
  Credential k (Crypto era) ->
  Tag ->
  GenRS era (Maybe (IsValid, ScriptHash (Crypto era)))
lookupPlutusScript (KeyHashObj _) _ = pure Nothing
lookupPlutusScript (ScriptHashObj scriptHash) tag =
  fmap (Map.lookup (scriptHash, tag) . gsPlutusScripts) get <&> \case
    Nothing -> Nothing
    Just (isValid, _) -> Just (isValid, scriptHash)

-- | Make RdmrWits WitnessesField only if the Credential is for a Plutus Script
--  And it is in the spending inputs, not the reference inputs
redeemerWitnessMaker ::
  Era era =>
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
                  mkWit3 exUnits = [RdmrWits (Redeemers $ Map.singleton rPtr (datum, exUnits))]
              -- we should not add this if the tx turns out to be in the reference inputs.
              -- we accomplish this by not calling this function on referenceInputs
              pure $ Just (isValid, mkWit3)

-- ===================================================================================
-- Now we start actual generators that create things and enter them into
-- the GenState, and sometimes into the Model.

-- | Collaterals can't have scripts, this is where this generator is needed.
--   As we generate this we add to the gsKeys field of the GenState.
genNoScriptRecipient :: Reflect era => GenRS era (Addr (Crypto era))
genNoScriptRecipient = do
  paymentCred <- KeyHashObj <$> genKeyHash
  stakeCred <- StakeRefBase . KeyHashObj <$> genKeyHash
  pure (Addr Testnet paymentCred stakeCred)

-- | Sometimes generates new Credentials, and some times reuses old ones
genRecipient :: Reflect era => GenRS era (Addr (Crypto era))
genRecipient = do
  paymentCred <- genCredential Spend
  stakeCred <- genCredential Cert
  pure (Addr Testnet paymentCred (StakeRefBase stakeCred))

genDatum :: Era era => GenRS era (Data era)
genDatum = snd <$> genDatumWithHash

-- | Generate a Babbage Datum witness to use as a redeemer for a Plutus Script.
--   TxWits can be a ScriptHash, or an inline Datum
genBabbageDatum :: forall era. Era era => GenRS era (Datum era)
genBabbageDatum =
  frequencyT
    [ (1, (DatumHash . fst) <$> genDatumWithHash),
      (4, (Datum . dataToBinaryData . snd) <$> genDatumWithHash)
    ]

genRefScript :: Reflect era => Proof era -> GenRS era (StrictMaybe (Script era))
genRefScript proof = do
  scripthash <- genScript proof Spend
  mscript <- lookupScript scripthash (Just Spend)
  case mscript of
    Nothing -> pure SNothing
    Just script -> pure (SJust script)

genTxOut :: Reflect era => Proof era -> Value era -> GenRS era [TxOutField era]
genTxOut proof val = do
  addr <- genRecipient
  cred <- maybe (error "BootstrapAddress encountered") pure $ paymentCredAddr addr
  dataHashFields <-
    case cred of
      KeyHashObj _ -> pure []
      ScriptHashObj scriptHash -> do
        maybeCoreScript <- lookupScript scriptHash (Just Spend)
        case proof of
          Conway _ -> case maybeCoreScript of
            Just (PlutusScript _ _) -> do
              datum <- genBabbageDatum
              script <- genRefScript proof
              pure [FDatum datum, RefScript script]
            _ -> pure []
          Babbage _ -> case maybeCoreScript of
            Just (PlutusScript _ _) -> do
              datum <- genBabbageDatum
              script <- genRefScript proof
              pure [FDatum datum, RefScript script]
            _ -> pure []
          Alonzo _ -> case maybeCoreScript of
            Just (PlutusScript _ _) -> do
              (datahash, _data) <- genDatumWithHash
              pure [DHash (SJust datahash)]
            _ -> pure []
          Mary _ -> pure []
          Allegra _ -> pure []
          Shelley _ -> pure []
  pure $ [Address addr, Amount val] ++ dataHashFields

-- ================================================================================

-- | Generate a TxIn whose TxIx will never clash with an Input created from a TxOut
genTxIn :: forall era. Reflect era => Proof era -> Int -> Gen (TxIn (Crypto era))
genTxIn _proof numChoices = do
  txId <- resize 40 arbitrary
  -- The TxIx for Inputs created from outputs of a TxBody, will only range from (1..numChoices)
  -- TxIx for these arbitrary Inputs range from (n+1..n+100). This makes the TxIx ranges
  -- incommensurate.
  txIx <- (mkTxIxPartial . fromIntegral) <$> choose (numChoices + 1, numChoices + 100)
  pure (TxIn txId txIx)

-- | Generate a non-empty List of fresh TxIn. By fresh we mean TxIn's that
--   we have not generated previously. We use the current InitialUtxo to test this.
genFreshTxIn :: forall era. Reflect era => Int -> GenRS era [TxIn (Crypto era)]
genFreshTxIn tries | tries <= 0 = error "Could not generate a fresh TxIn after many tries."
genFreshTxIn tries = do
  entriesInUse <- gets gsInitialUtxo
  -- Max number of choices. So the UTxO will never be larger than this
  numChoicesMax <- gets getUtxoChoicesMax
  n <- lift $ choose (1, numChoicesMax + 3)
  ins <- lift $ vectorOf n (genTxIn @era reify numChoicesMax)
  case filter (`Map.notMember` entriesInUse) ins of
    [] -> genFreshTxIn (tries - 1)
    freshTxIns -> pure (take numChoicesMax freshTxIns)

-- ====================================================================
-- Generating UTxO and associated Inputs (Set (TxIn era))

-- | Generate a somewhat arbitrary MUtxo.  Occasionally add a bit of
--   the MUtxo in the Model to the one generated.  This way the Tx we generate may
--   spend some of the old UTxo. The result has at most 1 entry from the
--   old MUtxo, and If it has only one entry, that entry is not from the old MUtxo
genUTxO :: Reflect era => GenRS era (MUtxo era, Maybe (UtxoEntry era))
genUTxO = do
  ins <- genFreshTxIn 100
  pairs <- sequence (map (\x -> (x,) <$> genOut) ins)
  percent <- gets getOldUtxoPercent
  -- Choose a pair from the oldUTxO
  maybepair <- frequencyT [(percent, getUtxoElem), (100 - percent, pure Nothing)]
  pure (Map.fromList (maybeCons maybepair pairs), maybepair)
  where
    -- Note: Never add an old pair unless there are more than 1 new pairs
    maybeCons (Just pair) xs | length xs > 1 = pair : xs
    maybeCons _ xs = xs
    genOut = do
      val <- lift genPositiveVal
      fields <- genTxOut reify val
      pure (coreTxOut reify fields)

-- | Generate both the spending and reference inputs and a key from the spending
--   inputs we can use to pay the fee. That key is never from the oldUTxO
genSpendReferenceInputs ::
  Map (TxIn (Crypto era)) (TxOut era) ->
  GenRS
    era
    ( UtxoEntry era, -- The fee key, used to pay the fee.
      Map (TxIn (Crypto era)) (TxOut era),
      Map (TxIn (Crypto era)) (TxOut era),
      Map (TxIn (Crypto era)) (TxOut era)
    )
genSpendReferenceInputs newUTxO = do
  let pairs = Map.toList newUTxO
  maxInputs <- gets getSpendInputsMax
  maxRef <- gets getRefInputsMax
  numInputs <- lift $ choose (1, min (Map.size newUTxO) maxInputs)
  numRefInputs <- lift $ choose (0, maxRef)
  badTest <- getUtxoTest
  (feepair@(txin, txout), inputPairs) <- lift $ chooseGood (badTest . fst) numInputs pairs
  refInputPairs <- take numRefInputs <$> lift (shuffle pairs)
  let inputs = Map.fromList inputPairs
      refInputs = Map.fromList refInputPairs
  -- The feepair is added to the mMutFee field
  modifyModel (\m -> m {mMutFee = Map.insert txin txout (mMutFee m)})
  let filtered = Map.restrictKeys newUTxO (Set.union (Map.keysSet inputs) (Map.keysSet refInputs))
  pure (feepair, inputs, refInputs, filtered)

-- | The invariant is:
--
-- * 'xs' is a non empty list.
-- * 'xs' has at most one 'bad' element
-- * if 'xs' has length 1, then the element is guaranteed to be good.
-- * 'n' is a positive `Int`, ranging between (1 .. length xs).
-- Return a pair (good,ys) where
-- > (bad good)==False,
-- > (good `elem` ys),
-- > (length ys) == n, and
-- > all (`elem` xs) ys.
--
-- This is used to generate the spending inputs, which always contain on
-- Input we can use to pay the fee, that does not come from the oldUTxO.
chooseGood :: (a -> Bool) -> Int -> [a] -> Gen (a, [a])
chooseGood bad n xs = do
  let (good, others) =
        case xs of
          [] -> error ("empty list in chooseGood, should never happen. n = " ++ show n ++ ", length xs = " ++ show (length xs))
          [x] -> (x, [])
          (x : y : more) -> if bad x then (y, (x : more)) else (x, y : more)
  tailx <- take (n - 1) <$> shuffle others
  result <- shuffle (good : tailx)
  pure (good, result)

-- ==================================================
-- Generating Certificates, May add to the Model

genDCert :: forall era. Reflect era => Proof era -> SlotNo -> GenRS era (DCert (Crypto era))
genDCert proof slot = do
  res <-
    elementsT
      [ DCertDeleg
          <$> frequencyT
            [ (75, RegKey <$> genRegKey),
              (25, DeRegKey <$> genDeRegKey),
              (50, Delegate <$> genDelegation)
            ],
        DCertPool
          <$> frequencyT
            [ (75, RegPool <$> genFreshPool),
              (25, RetirePool <$> genRetirementHash <*> genEpoch)
            ]
      ]
  return res
  where
    genRegKey = genFreshRegCred @era Cert
    genDeRegKey = genCredential Cert
    genDelegation = do
      rewardAccount <- genFreshRegCred Cert
      (kh, _) <- genPool
      pure $ Delegation {_delegator = rewardAccount, _delegatee = kh}
    genFreshPool = do
      (_kh, pp, _) <- genNewPool
      return pp
    genEpoch = do
      let EpochNo txEpoch = epochFromSlotNo slot
      EpochNo curEpoch <- gets $ mEL . gsModel
      EpochNo maxEpoch <- asks $ epochMax proof . gePParams
      let nextEpoch = 1 + (txEpoch - curEpoch) -- This will be either 1 or 2. It is 2 if the Tx is at the epoch boundary
      delta <- lift $ choose (nextEpoch, maxEpoch)
      return . EpochNo $ (curEpoch + delta)

genDCerts :: forall era. Reflect era => Proof era -> SlotNo -> GenRS era [DCert (Crypto era)]
genDCerts proof slot = do
  let genUniqueScript (!dcs, !ss, !regCreds) _ = do
        honest <- gets gsStableDelegators
        dc <- genDCert proof slot
        -- Workaround a misfeature where duplicate plutus scripts in DCert are ignored
        -- so if a duplicate might be generated, we don't do that generation
        let insertIfNotPresent dcs' regCreds' mKey mScriptHash
              | Just (_, scriptHash) <- mScriptHash =
                  if (scriptHash, mKey) `Set.member` ss
                    then (dcs, ss, regCreds)
                    else (dc : dcs', Set.insert (scriptHash, mKey) ss, regCreds')
              | otherwise = (dc : dcs', ss, regCreds')
        -- Generate registration and de-registration delegation certificates,
        -- while ensuring the proper registered/unregistered state in DState
        case dc of
          DCertDeleg d
            | RegKey regCred <- d ->
                if regCred `Map.member` regCreds -- Can't register if it is already registered
                  then pure (dcs, ss, regCreds)
                  else pure (dc : dcs, ss, Map.insert regCred (Coin 99) regCreds) -- 99 is a NonZero Value
            | DeRegKey deregCred <- d ->
                -- We can't make DeRegKey certificate if deregCred is not already registered
                -- or if the Rewards balance for deregCred is not 0,
                -- or if the credential is one of the StableDelegators (which are never de-registered)
                case Map.lookup deregCred regCreds of
                  Nothing -> pure (dcs, ss, regCreds)
                  -- No credential, skip making certificate
                  Just (Coin 0) ->
                    -- Ok to make certificate, rewards balance is 0
                    if Set.member deregCred honest
                      then pure (dcs, ss, regCreds)
                      else
                        insertIfNotPresent dcs (Map.delete deregCred regCreds) Nothing
                          <$> lookupPlutusScript deregCred Cert
                  Just (Coin _) -> pure (dcs, ss, regCreds)
            -- Either Reward balance is not zero, or no Credential, so skip making certificate
            | Delegate (Delegation delegCred delegKey) <- d ->
                let (dcs', regCreds')
                      | delegCred `Map.member` regCreds = (dcs, regCreds)
                      | otherwise -- In order to Delegate, the delegCred must exist in rewards.
                      -- so if it is not there, we put it there, otherwise we may
                      -- never generate a valid delegation.
                        =
                          (DCertDeleg (RegKey delegCred) : dcs, Map.insert delegCred (Coin 99) regCreds)
                 in insertIfNotPresent dcs' regCreds' (Just delegKey)
                      <$> lookupPlutusScript delegCred Cert
          DCertPool d
            | RegPool _ <- d -> do
                pure (dc : dcs, ss, regCreds)
            | RetirePool kh _ <- d -> do
                -- We need to make sure that the pool is registered before
                -- we try to retire it
                modelPools <- gets $ mPoolParams . gsModel
                case Map.lookup kh modelPools of
                  Just _ -> pure (dc : dcs, ss, regCreds)
                  Nothing -> pure (dcs, ss, regCreds)
          _ -> pure (dc : dcs, ss, regCreds)
  maxcert <- gets getCertificateMax
  n <- lift $ choose (0, maxcert)
  reward <- gets (mRewards . gsModel)
  let initSets ::
        ( [DCert (Crypto era)],
          Set (ScriptHash (Crypto era), Maybe (KeyHash 'StakePool (Crypto era))),
          Map (Credential 'Staking (Crypto era)) Coin
        )
      initSets = ([], Set.empty, reward)
  (dcs, _, _) <- F.foldlM genUniqueScript initSets [1 :: Int .. n]
  pure $ reverse dcs

-- | Generate a set of Collateral inputs sufficient to pay the minimum fee ('minCollTotal') computed
--   from the fee and the collateralPercentage from the PParams. Return the new UTxO, the inputs,
--   and coin of the excess amount included in the inputs, above what is needed to pay the minimum fee.
genCollateralUTxO ::
  forall era.
  (HasCallStack, Reflect era) =>
  [Addr (Crypto era)] ->
  Coin ->
  MUtxo era ->
  GenRS era (MUtxo era, Map.Map (TxIn (Crypto era)) (TxOut era), Coin)
genCollateralUTxO collateralAddresses (Coin fee) utxo = do
  GenEnv {gePParams} <- gets gsGenEnv
  let collPerc = collateralPercentage' reify gePParams
      minCollTotal = Coin (ceiling ((fee * toInteger collPerc) % 100))
      -- Generate a collateral that is neither in UTxO map nor has already been generated
      genNewCollateral addr coll um c = do
        -- The size of the Gen computation is driven down when we generate scripts, so it can be 0 here
        -- that is really bad, because if the happens we get the same TxIn every time, and 'coll' never grows,
        -- so this function doesn't terminate. We want many choices of TxIn, so resize just this arbitrary by 30.
        entriesInUse <- gets gsInitialUtxo
        txIn <- lift (resize 30 (arbitrary :: Gen (TxIn (Crypto era))))
        if Map.member txIn utxo || Map.member txIn coll || txIn `Map.member` entriesInUse
          then genNewCollateral addr coll um c
          else pure (um, Map.insert txIn (coreTxOut reify [Address addr, Amount (inject c)]) coll, c)
      -- Either pick a collateral from a map or generate a completely new one
      genCollateral addr coll um
        | Map.null um = genNewCollateral addr coll um =<< lift genPositiveVal
        | otherwise = do
            i <- lift $ chooseInt (0, Map.size um - 1)
            let (txIn, txOut) = Map.elemAt i um
            pure (Map.deleteAt i um, Map.insert txIn txOut coll, txOut ^. coinTxOutL)
      -- Recursively either pick existing key spend only outputs or generate new ones that
      -- will be later added to the UTxO map
      go ::
        [Addr (Crypto era)] ->
        Map (TxIn (Crypto era)) (TxOut era) ->
        Coin ->
        Map (TxIn (Crypto era)) (TxOut era) ->
        GenRS era (Map (TxIn (Crypto era)) (TxOut era), Coin)
      go ecs !coll !curCollTotal !um
        | curCollTotal >= minCollTotal = pure (coll, curCollTotal <-> minCollTotal)
        | [] <- ecs = error "Impossible: supplied less addresses than `maxCollateralInputs`"
        | ec : ecs' <- ecs = do
            (um', coll', c) <-
              if null ecs'
                then -- This is the last input, so most of the time, put something (val > 0)
                -- extra in it or we will always have a ColReturn with zero in it.
                do
                  excess <- lift genPositiveVal
                  genNewCollateral ec coll um ((minCollTotal <-> curCollTotal) <+> excess)
                else elementsT [genCollateral ec coll Map.empty, genCollateral ec coll um]
            go ecs' coll' (curCollTotal <+> c) um'
  (collaterals, excessColCoin) <-
    go collateralAddresses Map.empty (Coin 0) $ Map.filter spendOnly utxo
  pure (Map.union collaterals utxo, collaterals, excessColCoin)

spendOnly :: EraTxOut era => TxOut era -> Bool
spendOnly txOut = case txOut ^. addrTxOutL of
  Addr _ (ScriptHashObj _) _ -> False
  _ -> True

-- | This function is used to generate the Outputs of a TxBody, It is computed by taking the
--   Outputs of the range of the domain restricted UTxO, resticted by the Inputs of the TxBody,
--   as input to the function, and then making new Outputs, where the sum of the Coin is the same.
--   This way we generate a 'balanced' TxBody (modulo fees, deposits, refunds etc. which are
--   handled separately)
genRecipientsFrom :: Reflect era => [TxOut era] -> GenRS era [TxOut era]
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
      goExtra e n !s txOut (tx : txs) !rs = goExtra e (n - 1) (s <+> v) tx txs rs
        where
          v = txOut ^. valueTxOutL
      -- Potentially split 'txout' into two TxOuts. If the two piece path is used
      -- one of two TxOuts uses the same 'addr' as 'txout' and holds the 'change'
      -- (i.e. difference between the original and the second, non-change, TxOut).
      -- In either case whether it adds 1 or 2 TxOuts to 'rs', the coin value of
      -- the new TxOut(s), is the same as the coin value of 'txout'.
      genWithChange s txout rs = do
        let !(!addr, !v, !ds) = txoutFields reify txout
            vCoin = unCoin (coin v)
        if vCoin == 0 -- If the coin balance is 0, don't add any TxOuts to 'rs'
          then pure rs
          else do
            c <- Coin <$> lift (choose (1, vCoin))
            fields <- genTxOut reify (s <+> inject c)
            pure $
              if c < coin v
                then
                  let !change = coreTxOut reify (Address addr : Amount (v <-> inject c) : ds)
                   in coreTxOut reify fields : change : rs
                else coreTxOut reify fields : rs
  goNew extra txOuts []

getDCertCredential :: DCert crypto -> Maybe (Credential 'Staking crypto)
getDCertCredential = \case
  DCertDeleg d ->
    case d of
      RegKey _rk -> Nothing -- we don't require witnesses for RegKey
      DeRegKey drk -> Just drk
      Delegate (Delegation dk _) -> Just dk
  DCertPool pc ->
    case pc of
      RegPool PoolParams {..} -> Just . coerceKeyRole $ KeyHashObj _poolId
      RetirePool kh _ -> Just . coerceKeyRole $ KeyHashObj kh
  DCertGenesis _g -> Nothing
  DCertMir _m -> Nothing

genWithdrawals :: Reflect era => SlotNo -> GenRS era (Wdrl (Crypto era), RewardAccounts (Crypto era))
genWithdrawals slot =
  if epochFromSlotNo slot == EpochNo 0
    then do
      let networkId = Testnet
      newRewards <- genRewards
      pure (Wdrl $ Map.mapKeys (RewardAcnt networkId) newRewards, newRewards)
    else pure (Wdrl Map.empty, Map.empty)

timeToLive :: ValidityInterval -> SlotNo
timeToLive (ValidityInterval _ (SJust n)) = n
timeToLive (ValidityInterval _ SNothing) = SlotNo maxBound

-- ============================================================================

genAlonzoTx :: forall era. Reflect era => Proof era -> SlotNo -> GenRS era (UTxO era, Tx era)
genAlonzoTx proof slot = do
  (utxo, tx, _fee, _old) <- genAlonzoTxAndInfo proof slot
  pure (utxo, tx)

genAlonzoTxAndInfo ::
  forall era.
  Reflect era =>
  Proof era ->
  SlotNo ->
  GenRS
    era
    ( UTxO era,
      Tx era,
      UtxoEntry era, -- The fee key
      Maybe (UtxoEntry era) -- from oldUtxO
    )
genAlonzoTxAndInfo proof slot = do
  GenEnv {gePParams} <- gets gsGenEnv
  validityInterval <- lift $ genValidityInterval slot
  modify (\gs -> gs {gsValidityInterval = validityInterval})

  -- 1. Produce utxos that will be spent
  (utxoChoices, maybeoldpair) <- genUTxO

  -- 2. Generate UTxO for spending and reference inputs
  --    Note the spending inputs and the reference inputs may overlap.
  --    feeKey is one of the inputs from the spending inputs, safe to pay the fee with.
  ( feepair@(feeKey, _), -- One of the spending inputs, to be used to pay the fee
    toSpendNoCollateral, -- All of the spending inputs
    refInputsUtxo, -- All the reference inputs
    utxoNoCollateral -- Union of all the above
    ) <-
    genSpendReferenceInputs utxoChoices

  -- 3. Check if all Plutus scripts are valid
  let toSpendNoCollateralTxOuts :: [TxOut era]
      toSpendNoCollateralTxOuts = Map.elems toSpendNoCollateral
      -- We use maxBound to ensure the serialized size overestimation
      maxCoin = Coin (toInteger (maxBound :: Int))
  -- 4. Generate all recipients and witnesses needed for spending Plutus scripts
  recipients <- genRecipientsFrom toSpendNoCollateralTxOuts

  --  mkPaymentWits :: ExUnits -> [WitnessesField era]
  (IsValid v1, mkPaymentWits) <-
    redeemerWitnessMaker
      Spend
      [ (\dh cred -> (lookupByKeyM "datum" dh gsDatums, cred))
          <$> mDatumHash
          <*> Just credential
        | (_, coretxout) <- Map.toAscList toSpendNoCollateral,
          let (credentials, mDatumHash) = txoutEvidence proof coretxout,
          credential <- credentials
      ]

  -- generate Withdrawals before DCerts, as Rewards are populated in the Model here,
  -- and we need to avoid certain DCerts if they conflict with existing Rewards
  (wdrls@(Wdrl wdrlMap), newRewards) <- genWithdrawals slot
  let withdrawalAmount = F.fold wdrlMap

  rewardsWithdrawalTxOut <-
    if withdrawalAmount == Coin 0
      then pure Nothing
      else Just . coreTxOut proof <$> genTxOut proof (inject withdrawalAmount)
  let wdrlCreds = map (getRwdCred . fst) $ Map.toAscList wdrlMap
  (IsValid v2, mkWdrlWits) <-
    redeemerWitnessMaker Rewrd $ map (Just . (,) genDatum) wdrlCreds

  dcerts <- genDCerts proof slot
  let dcertCreds = map getDCertCredential dcerts
  (IsValid v3, mkCertsWits) <-
    redeemerWitnessMaker Cert $ map ((,) genDatum <$>) dcertCreds

  let isValid = IsValid (v1 && v2 && v3)
      mkWits :: [ExUnits -> [WitnessesField era]]
      mkWits = mkPaymentWits ++ mkCertsWits ++ mkWdrlWits
  exUnits <- genExUnits proof (length mkWits)

  let redeemerWitsList :: [WitnessesField era]
      redeemerWitsList = concat (zipWith ($) mkWits exUnits)
  datumWitsList <- concat <$> mapM (makeDatumWitness proof) (Map.elems toSpendNoCollateral)
  keyWitsMakers <-
    mapM
      (genTxOutKeyWitness proof (Just Spend))
      (toSpendNoCollateralTxOuts ++ Map.elems refInputsUtxo)
  dcertWitsMakers <- mapM (genCredKeyWit proof (Just Cert)) $ catMaybes dcertCreds
  rwdrsWitsMakers <- mapM (genCredKeyWit proof (Just Rewrd)) wdrlCreds

  -- 5. Estimate inputs that will be used as collateral
  maxCollateralCount <-
    lift $ chooseInt (1, fromIntegral (maxCollateralInputs' proof gePParams))
  bogusCollateralTxId <- lift (arbitrary :: Gen (TxId (Crypto era)))
  let bogusCollateralTxIns =
        Set.fromList
          [ TxIn bogusCollateralTxId (mkTxIxPartial (fromIntegral i))
            | i <- [maxBound, maxBound - 1 .. maxBound - fromIntegral maxCollateralCount - 1] :: [Word16]
          ]
  collateralAddresses <- replicateM maxCollateralCount genNoScriptRecipient
  bogusCollateralKeyWitsMakers <- forM collateralAddresses $ \a ->
    genTxOutKeyWitness proof Nothing (coreTxOut proof [Address a, Amount (inject maxCoin)])
  networkId <- lift $ elements [SNothing, SJust Testnet]

  -- 6. Generate bogus collateral fields, and functions for updating them when we know their real values
  -- Add a stub for the TotalCol field
  bogusTotalCol <- frequencyT [(1, pure SNothing), (9, pure (SJust (Coin 0)))] -- generate a bogus Coin, fill it in later
  let updateTotalColl SNothing _ = SNothing
      updateTotalColl (SJust (Coin n)) (Coin m) = SJust (Coin (n + m))
  -- If Babbage era, or greater, add a stub for a CollateralReturn TxOut
  bogusCollReturn <-
    if Some proof >= Some (Babbage Mock)
      then frequencyT [(1, pure SNothing), (9, (SJust . coreTxOut proof) <$> genTxOut proof (inject (Coin 0)))]
      else pure SNothing
  let updateCollReturn SNothing _ = SNothing
      updateCollReturn (SJust txout) v = SJust (injectFee proof v txout)

  -- 7. Estimate the fee
  let redeemerDatumWits = redeemerWitsList ++ datumWitsList
      bogusIntegrityHash = newScriptIntegrityHash proof gePParams mempty (Redeemers mempty) mempty
      inputSet = Map.keysSet toSpendNoCollateral
      outputList = maybe recipients (: recipients) rewardsWithdrawalTxOut
      txBodyNoFee =
        coreTxBody
          proof
          [ Inputs inputSet,
            Collateral bogusCollateralTxIns,
            RefInputs (Map.keysSet refInputsUtxo),
            TotalCol bogusTotalCol,
            Outputs' outputList,
            CollateralReturn bogusCollReturn,
            Certs' dcerts,
            Wdrls wdrls,
            Txfee maxCoin,
            if Some proof >= Some (Allegra Mock)
              then Vldt validityInterval
              else TTL (timeToLive validityInterval),
            Update' [],
            ReqSignerHashes' [],
            Generic.Mint mempty,
            WppHash bogusIntegrityHash,
            AdHash' [],
            Txnetworkid networkId
          ]
      txBodyNoFeeHash = hashAnnotated txBodyNoFee
      witsMakers :: [SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era]]
      witsMakers = keyWitsMakers ++ dcertWitsMakers ++ rwdrsWitsMakers
      bogusNeededScripts = scriptWitsNeeded' proof utxoNoCollateral txBodyNoFee
      noFeeWits :: [WitnessesField era]
      noFeeWits =
        onlyNecessaryScripts proof bogusNeededScripts $
          redeemerDatumWits
            <> foldMap ($ txBodyNoFeeHash) (witsMakers ++ bogusCollateralKeyWitsMakers)
      bogusTxForFeeCalc =
        coreTx
          proof
          [ Body txBodyNoFee,
            TxWits (assembleWits proof noFeeWits),
            Valid isValid,
            AuxData' []
          ]
      fee = minfee' proof gePParams bogusTxForFeeCalc
      deposits = depositsAndRefunds proof gePParams dcerts

  -- 8. Crank up the amount in one of outputs to account for the fee and deposits. Note
  -- this is a hack that is not possible in a real life, but in the end it does produce
  -- real life like setup. We use the entry with TxIn feeKey, which we can safely overwrite.
  let utxoFeeAdjusted = Map.adjust (injectFee proof (fee <+> deposits)) feeKey utxoNoCollateral

  -- 9. Generate utxos that will be used as collateral
  (utxo, collMap, excessColCoin) <- genCollateralUTxO collateralAddresses fee utxoFeeAdjusted
  collateralKeyWitsMakers <-
    mapM (genTxOutKeyWitness proof Nothing) $ Map.elems collMap

  -- 10. Construct the correct Tx with valid fee and collaterals
  let sNeeded = scriptsNeeded' proof utxo txBodyNoFee
      langs = Set.toList $ languagesUsed proof bogusTxForFeeCalc (UTxO utxoNoCollateral) sNeeded
      mIntegrityHash =
        newScriptIntegrityHash
          proof
          gePParams
          langs
          (mkTxrdmrs redeemerDatumWits)
          (mkTxdats redeemerDatumWits)
      balance =
        case bogusCollReturn of
          SNothing -> txInBalance (Map.keysSet collMap) utxo
          SJust _ -> txInBalance (Map.keysSet collMap) utxo <-> excessColCoin
      txBody =
        overrideTxBody
          proof
          txBodyNoFee
          [ Txfee fee,
            Collateral (Map.keysSet collMap),
            CollateralReturn (updateCollReturn bogusCollReturn excessColCoin),
            TotalCol (updateTotalColl bogusTotalCol balance),
            WppHash mIntegrityHash
          ]
      txBodyHash = hashAnnotated txBody
      neededScripts = scriptWitsNeeded' proof utxo txBody
      wits =
        onlyNecessaryScripts proof neededScripts $
          redeemerDatumWits
            <> foldMap ($ txBodyHash) (witsMakers ++ collateralKeyWitsMakers)
      validTx =
        coreTx
          proof
          [ Body txBody,
            TxWits (assembleWits proof wits),
            Valid isValid,
            AuxData' []
          ]
  count <- gets (mCount . gsModel)

  -- Add the new objects freshly generated for this Tx to the set of Initial objects for a Trace
  modify
    ( \st ->
        st
          { gsInitialUtxo = Map.union (gsInitialUtxo st) (minus utxo maybeoldpair),
            gsInitialRewards = Map.unions [gsInitialRewards st, newRewards]
          }
    )
  -- Modify the model to track what this transaction does.
  modifyModel
    ( \m ->
        m
          { mCount = count + 1,
            mIndex = Map.insert count (TxId txBodyHash) (mIndex m),
            mUTxO = utxo -- This is the UTxO that will run this Tx,
          }
    )
  pure (UTxO utxo, validTx, feepair, maybeoldpair)

minus :: MUtxo era -> Maybe (UtxoEntry era) -> MUtxo era
minus m Nothing = m
minus m (Just (txin, _)) = Map.delete txin m

-- | Keep only Script witnesses that are neccessary in 'era',
onlyNecessaryScripts :: Proof era -> Set (ScriptHash (Crypto era)) -> [WitnessesField era] -> [WitnessesField era]
onlyNecessaryScripts _ _ [] = []
onlyNecessaryScripts proof hashes (ScriptWits m : xs) =
  ScriptWits (Map.restrictKeys m hashes) : onlyNecessaryScripts proof hashes xs
onlyNecessaryScripts proof hashes (x : xs) = x : onlyNecessaryScripts proof hashes xs

-- | Scan though the fields unioning all the RdrmWits fields into one Redeemer map
mkTxrdmrs :: forall era. Era era => [WitnessesField era] -> Redeemers era
mkTxrdmrs fields = Redeemers (List.foldl' accum Map.empty fields)
  where
    accum m1 (RdmrWits (Redeemers m2)) = Map.union m1 m2
    accum m1 _ = m1

-- | Scan though the fields unioning all the DataWits fields into one TxDat
mkTxdats :: forall era. Era era => [WitnessesField era] -> TxDats era
mkTxdats fields = TxDats (List.foldl' accum Map.empty fields)
  where
    accum m (DataWits' ds) = List.foldl' accum2 m ds
      where
        accum2 m2 d = Map.insert (hashData @era d) d m2
    accum m _ = m

-- =======================================================
-- An encapsulation of the Top level types we generate,
-- but that has its own Show instance that we can control.

data Box era = Box (Proof era) (TRC (EraRule "LEDGER" era)) (GenState era)

instance
  ( Era era,
    PrettyA (State (EraRule "LEDGER" era)),
    PrettyA (Script era),
    PrettyA (Signal (EraRule "LEDGER" era)),
    Signal (EraRule "LEDGER" era) ~ Tx era
  ) =>
  Show (Box era)
  where
  show (Box _proof (TRC (_env, _state, _sig)) _gs) =
    show $
      ppRecord
        "Box"
        []

-- Sample things we might use in the Box
-- ("Tx", tcTx proof _sig)
-- ("Tx", prettyA _sig)
-- ("TRC state",prettyA _state)
-- ("GenEnv",pcGenState proof _gs)

testTx :: IO ()
testTx = do
  let proof = Babbage Mock
  ((_utxo, tx, _feepair, _), genstate) <- generate $ runGenRS proof def (genAlonzoTxAndInfo proof (SlotNo 0))
  let m = gsModel genstate
  putStrLn (show (pcTx proof tx))
  putStrLn (show (pcModelNewEpochState proof m))

-- ==============================================================================
-- How we take the generated stuff and put it through the STS rule mechanism
-- in a way that is Era Agnostic

applySTSByProof ::
  forall era.
  (GoodCrypto (Crypto era)) =>
  Proof era ->
  RuleContext 'Transition (EraRule "LEDGER" era) ->
  Either [PredicateFailure (EraRule "LEDGER" era)] (State (EraRule "LEDGER" era))
applySTSByProof (Conway _) _trc = runShelleyBase $ applySTS @(EraRule "LEDGER" (ConwayEra (Crypto era))) _trc
applySTSByProof (Babbage _) _trc = runShelleyBase $ applySTS @(EraRule "LEDGER" (BabbageEra (Crypto era))) _trc
applySTSByProof (Alonzo _) trc = runShelleyBase $ applySTS trc
applySTSByProof (Mary _) trc = runShelleyBase $ applySTS trc
applySTSByProof (Allegra _) trc = runShelleyBase $ applySTS trc
applySTSByProof (Shelley _) trc = runShelleyBase $ applySTS trc
