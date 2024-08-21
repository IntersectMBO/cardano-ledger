{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.TxGen (
  genAlonzoTx,
  Box (..),
  applySTSByProof,
  assembleWits,
  coreTx,
  coreTxBody,
  coreTxOut,
  genUTxO,
)
where

import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  Timelock (..),
  ValidityInterval (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo.Scripts hiding (Script)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (
  Redeemers (..),
  TxDats (..),
  unRedeemers,
 )
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (EpochInterval (..), Network (..), mkTxIxPartial)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..), ConwayTxCert (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
  coerceKeyRole,
 )
import Cardano.Ledger.Plutus.Data (Data, Datum (..), dataToBinaryData, hashData)
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.API (
  Addr (..),
  Credential (..),
  PoolParams (..),
  RewardAccount (..),
  ShelleyDelegCert (..),
  StakeReference (..),
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.LedgerState (RewardAccounts)
import Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.TxCert (
  ShelleyTxCert (..),
  pattern DelegStakeTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
 )
import Cardano.Ledger.Slot (EpochNo (EpochNo))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..))
import Cardano.Ledger.Val
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM, replicateM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (asks, get, gets, modify)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Bifunctor (first)
import qualified Data.Foldable as F
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
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
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Generic.Fields hiding (Mint)
import qualified Test.Cardano.Ledger.Generic.Fields as Generic (TxBodyField (Mint))
import Test.Cardano.Ledger.Generic.Functions
import Test.Cardano.Ledger.Generic.GenState (
  GenEnv (..),
  GenRS,
  GenState (..),
  PlutusPurposeTag (..),
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
  mkRedeemers,
  mkRedeemersFromTags,
  modifyGenStateInitialRewards,
  modifyGenStateInitialUtxo,
  modifyModelCount,
  modifyModelIndex,
  modifyModelMutFee,
  modifyModelUTxO,
 )
import Test.Cardano.Ledger.Generic.ModelState (
  MUtxo,
  ModelNewEpochState (..),
  UtxoEntry,
 )
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (..), ppRecord)
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
coreTxOut era = List.foldl' (updateTxOut era) (initialTxOut era)

coreTxBody :: EraTxBody era => Proof era -> [TxBodyField era] -> TxBody era
coreTxBody era = List.foldl' (updateTxBody era) (initialTxBody era)

overrideTxBody :: EraTxBody era => Proof era -> TxBody era -> [TxBodyField era] -> TxBody era
overrideTxBody era = List.foldl' (updateTxBody era)

coreTx :: Proof era -> [TxField era] -> Tx era
coreTx era = List.foldl' (updateTx era) (initialTx era)

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
  ScriptHash (EraCrypto era) ->
  Maybe PlutusPurposeTag ->
  GenRS era (Maybe (Script era))
lookupScript scriptHash mTag = do
  m <- gsScripts <$> get
  case Map.lookup scriptHash m of
    Just script -> pure $ Just script
    Nothing
      | Just tag <- mTag ->
          Just . snd <$> lookupByKeyM "plutusScript" (scriptHash, tag) gsPlutusScripts
    _ -> pure Nothing

-- =====================================

genGenericScriptWitness ::
  Reflect era =>
  Proof era ->
  Maybe PlutusPurposeTag ->
  Script era ->
  GenRS era (SafeHash (EraCrypto era) EraIndependentTxBody -> [WitnessesField era])
genGenericScriptWitness proof mTag script =
  case proof of
    Shelley -> mkMultiSigWit proof mTag script
    Allegra -> mkTimelockWit proof mTag script
    Mary -> mkTimelockWit proof mTag script
    Alonzo -> case script of
      TimelockScript timelock -> mkTimelockWit proof mTag timelock
      PlutusScript _ -> pure (const [])
    Babbage -> case script of
      TimelockScript timelock -> mkTimelockWit proof mTag timelock
      PlutusScript _ -> pure (const [])
    Conway -> case script of
      TimelockScript timelock -> mkTimelockWit proof mTag timelock
      PlutusScript _ -> pure (const [])

-- | Generate a TxWits producing function. We handle TxWits come from Keys and Scripts
--   Because scripts vary be Era, we need some Era specific code here: genGenericScriptWitness
mkWitVKey ::
  forall era kr.
  Reflect era =>
  Proof era ->
  Maybe PlutusPurposeTag ->
  Credential kr (EraCrypto era) ->
  GenRS era (SafeHash (EraCrypto era) EraIndependentTxBody -> [WitnessesField era])
mkWitVKey _ _mTag (KeyHashObj keyHash) = do
  keyPair <- lookupByKeyM "credential" (coerceKeyRole keyHash) gsKeys
  pure $ \bodyHash -> [AddrWits' [mkWitnessVKey bodyHash keyPair]]
mkWitVKey era mTag (ScriptHashObj scriptHash) =
  lookupScript @era scriptHash mTag >>= \case
    Nothing ->
      error $ "Impossible: Cannot find script with hash " ++ show scriptHash
    Just script -> do
      let scriptWit = ScriptWits' [script]
      otherWit <- genGenericScriptWitness era mTag script
      pure (\hash -> scriptWit : otherWit hash)

-- ========================================================================
-- Generating TxWits, here we are not adding anything to the GenState
-- only looking up things already added, and assembling the right pieces to
-- make TxWits.

-- | Used in Shelley Eras
mkMultiSigWit ::
  forall era.
  (ShelleyEraScript era, NativeScript era ~ MultiSig era, Reflect era) =>
  Proof era ->
  Maybe PlutusPurposeTag ->
  MultiSig era ->
  GenRS era (SafeHash (EraCrypto era) EraIndependentTxBody -> [WitnessesField era])
mkMultiSigWit era mTag (RequireSignature keyHash) = mkWitVKey era mTag (KeyHashObj keyHash)
mkMultiSigWit era mTag (RequireAllOf timelocks) = F.fold <$> mapM (mkMultiSigWit era mTag) timelocks
mkMultiSigWit era mTag (RequireAnyOf timelocks)
  | F.null timelocks = pure (const [])
  | otherwise = mkMultiSigWit era mTag =<< lift (elements (F.toList timelocks))
mkMultiSigWit era mTag (RequireMOf m timelocks) = do
  ts <- take m <$> lift (shuffle (F.toList timelocks))
  F.fold <$> mapM (mkMultiSigWit era mTag) ts
mkMultiSigWit _ _ _ = error "Impossible: All NativeScripts should have been accounted for"

-- | Timeock scripts are used in Mary and subsequent Eras.
mkTimelockWit ::
  forall era.
  (AllegraEraScript era, NativeScript era ~ Timelock era, Reflect era) =>
  Proof era ->
  Maybe PlutusPurposeTag ->
  Timelock era ->
  GenRS era (SafeHash (EraCrypto era) EraIndependentTxBody -> [WitnessesField era])
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

-- | Same as `genCredKeyWit`, but for `TxOuts`
genTxOutKeyWitness ::
  forall era.
  Reflect era =>
  Proof era ->
  Maybe PlutusPurposeTag ->
  TxOut era ->
  GenRS era (SafeHash (EraCrypto era) EraIndependentTxBody -> [WitnessesField era])
genTxOutKeyWitness era mTag txOut =
  case txOut ^. addrTxOutL of
    AddrBootstrap baddr ->
      error $ "Can't authorize bootstrap address: " ++ show baddr
    Addr _ payCred _ ->
      case getTxOutRefScript reify txOut of
        SNothing -> mkWitVKey era mTag payCred
        SJust script -> do
          f1 <- mkWitVKey era mTag payCred
          f2 <- genGenericScriptWitness reify (Just Spending) script
          pure (\safehash -> f1 safehash ++ f2 safehash)

genCredKeyWit ::
  forall era k.
  Reflect era =>
  Proof era ->
  Maybe PlutusPurposeTag ->
  Credential k (EraCrypto era) ->
  GenRS era (SafeHash (EraCrypto era) EraIndependentTxBody -> [WitnessesField era])
genCredKeyWit era mTag cred = mkWitVKey era mTag cred

makeDatumWitness :: Proof era -> TxOut era -> GenRS era [WitnessesField era]
makeDatumWitness proof txout = case (proof, txout) of
  (Babbage, BabbageTxOut _ _ (DatumHash h) _) -> mkDatumWit (SJust h)
  (Babbage, BabbageTxOut _ _ (Datum _) _) -> pure []
  (Babbage, BabbageTxOut _ _ NoDatum _) -> pure []
  (Conway, BabbageTxOut _ _ (DatumHash h) _) -> mkDatumWit (SJust h)
  (Conway, BabbageTxOut _ _ (Datum _) _) -> pure []
  (Conway, BabbageTxOut _ _ NoDatum _) -> pure []
  (Alonzo, AlonzoTxOut _ _ mDatum) -> mkDatumWit mDatum
  _ -> pure [] -- No other era has data witnesses
  where
    mkDatumWit SNothing = pure []
    mkDatumWit (SJust datumHash) = do
      datum <- lookupByKeyM "datum" datumHash gsDatums
      pure [DataWits' [datum]]

-- | Does the current Credential point to a PlutusScript? If so return its IsValid and Hash
plutusScriptHashFromTag ::
  Credential k (EraCrypto era) ->
  PlutusPurposeTag ->
  GenRS era (Maybe (IsValid, ScriptHash (EraCrypto era)))
plutusScriptHashFromTag (KeyHashObj _) _ = pure Nothing
plutusScriptHashFromTag (ScriptHashObj scriptHash) tag =
  fmap (Map.lookup (scriptHash, tag) . gsPlutusScripts) get <&> \case
    Nothing -> Nothing
    Just (isValid, _) -> Just (isValid, scriptHash)

-- | Make RdmrWits WitnessesField only if the Credential is for a Plutus Script
--  And it is in the spending inputs, not the reference inputs
redeemerWitnessMaker ::
  Proof era ->
  PlutusPurposeTag ->
  [Maybe (GenRS era (Data era), Credential k (EraCrypto era))] ->
  GenRS era (IsValid, [ExUnits -> [WitnessesField era]])
redeemerWitnessMaker proof tag listWithCred =
  let creds =
        [ (ix, genDat, cred)
        | (ix, mCred) <- zip [0 ..] listWithCred
        , Just (genDat, cred) <- [mCred]
        ]
      allValid :: [IsValid] -> IsValid
      allValid = IsValid . getAll . foldMap (\(IsValid v) -> All v)
   in fmap (first allValid . unzip . catMaybes) $
        forM creds $ \(ix, genDat, cred) ->
          plutusScriptHashFromTag cred tag >>= \case
            Nothing -> pure Nothing
            Just (isValid, _) -> do
              datum <- genDat
              let mkWit3 exUnits =
                    [RdmrWits (mkRedeemersFromTags proof [((tag, ix), (datum, exUnits))])]
              -- we should not add this if the tx turns out to be in the reference inputs.
              -- we accomplish this by not calling this function on referenceInputs
              pure $ Just (isValid, mkWit3)

-- ===================================================================================
-- Now we start actual generators that create things and enter them into
-- the GenState, and sometimes into the Model.

-- | Collaterals can't have scripts, this is where this generator is needed.
--   As we generate this we add to the gsKeys field of the GenState.
genNoScriptRecipient :: Reflect era => GenRS era (Addr (EraCrypto era))
genNoScriptRecipient = do
  paymentCred <- KeyHashObj <$> genKeyHash
  stakeCred <- StakeRefBase . KeyHashObj <$> genKeyHash
  pure (Addr Testnet paymentCred stakeCred)

-- | Sometimes generates new Credentials, and some times reuses old ones
genRecipient :: Reflect era => GenRS era (Addr (EraCrypto era))
genRecipient = do
  paymentCred <- genCredential Spending
  stakeCred <- genCredential Certifying
  pure (Addr Testnet paymentCred (StakeRefBase stakeCred))

genDatum :: Era era => GenRS era (Data era)
genDatum = snd <$> genDatumWithHash

-- | Generate a Babbage Datum witness to use as a redeemer for a Plutus Script.
--   TxWits can be a ScriptHash, or an inline Datum
genBabbageDatum :: forall era. Era era => GenRS era (Datum era)
genBabbageDatum =
  frequencyT
    [ (1, DatumHash . fst <$> genDatumWithHash)
    , (4, Datum . dataToBinaryData . snd <$> genDatumWithHash)
    ]

genRefScript :: Reflect era => Proof era -> GenRS era (StrictMaybe (Script era))
genRefScript proof = do
  scripthash <- genScript proof Spending
  mscript <- lookupScript scripthash (Just Spending)
  case mscript of
    Nothing -> pure SNothing
    Just script -> pure (SJust script)

-- | Gen the Datum and RefScript fields of a TxOut by Analyzing the payment credential's script
genDataHashField :: Reflect era => Proof era -> Maybe (Script era) -> GenRS era [TxOutField era]
genDataHashField proof maybeCoreScript =
  case proof of
    Conway -> case maybeCoreScript of
      Just (PlutusScript _) -> do
        datum <- genBabbageDatum
        script <- genRefScript proof
        pure [FDatum datum, RefScript script]
      _ -> pure []
    Babbage -> case maybeCoreScript of
      Just (PlutusScript _) -> do
        datum <- genBabbageDatum
        script <- genRefScript proof
        pure [FDatum datum, RefScript script]
      _ -> pure []
    Alonzo -> case maybeCoreScript of
      Just (PlutusScript _) -> do
        (datahash, _data) <- genDatumWithHash
        pure [DHash (SJust datahash)]
      _ -> pure []
    _ -> pure [] -- No other Era has any datum in the TxOut

-- | Generate the list of TxOutField that constitute a TxOut
genTxOut :: Reflect era => Proof era -> Value era -> GenRS era [TxOutField era]
genTxOut proof val = do
  addr <- genRecipient
  cred <- maybe (error "BootstrapAddress encountered") pure $ paymentCredAddr addr
  dataHashFields <-
    case cred of
      KeyHashObj _ -> pure []
      ScriptHashObj scriptHash -> do
        maybeCoreScript <- lookupScript scriptHash (Just Spending)
        genDataHashField proof maybeCoreScript
  pure $ [Address addr, Amount val] ++ dataHashFields

-- ================================================================================

-- | Generate a TxIn whose TxIx will never clash with an Input created from a TxOut
genTxIn :: forall era. Reflect era => Proof era -> Int -> Gen (TxIn (EraCrypto era))
genTxIn _proof numChoices = do
  txId <- resize 40 arbitrary
  -- The TxIx for Inputs created from outputs of a TxBody, will only range from (1..numChoices)
  -- TxIx for these arbitrary Inputs range from (n+1..n+100). This makes the TxIx ranges
  -- incommensurate.
  txIx <- (mkTxIxPartial . fromIntegral) <$> choose (numChoices + 1, numChoices + 100)
  pure (TxIn txId txIx)

-- | Generate a non-empty List of fresh TxIn. By fresh we mean TxIn's that
--   we have not generated previously. We use the current InitialUtxo to test this.
genFreshTxIn :: forall era. Reflect era => Int -> GenRS era [TxIn (EraCrypto era)]
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
  Map (TxIn (EraCrypto era)) (TxOut era) ->
  GenRS
    era
    ( UtxoEntry era -- The fee key, used to pay the fee.
    , Map (TxIn (EraCrypto era)) (TxOut era)
    , Map (TxIn (EraCrypto era)) (TxOut era)
    , Map (TxIn (EraCrypto era)) (TxOut era)
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
  modifyModelMutFee (Map.insert txin txout)
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
          [] ->
            error $
              "empty list in chooseGood, should never happen. n = "
                ++ show n
                ++ ", length xs = "
                ++ show (length xs)
          [x] -> (x, [])
          (x : y : more) -> if bad x then (y, x : more) else (x, y : more)
  tailx <- take (n - 1) <$> shuffle others
  result <- shuffle (good : tailx)
  pure (good, result)

-- ==================================================
-- Generating Certificates, May add to the Model

genShelleyDelegCert :: forall era. Reflect era => GenRS era (TxCert era)
genShelleyDelegCert =
  frequencyT
    [ (75, genShelleyRegCert)
    , (25, genShelleyUnRegCert)
    , (50, genDelegation)
    ]
  where
    genShelleyRegCert = RegTxCert <$> genFreshRegCred @era Certifying
    genShelleyUnRegCert = UnRegTxCert <$> genCredential Certifying
    genDelegation = do
      rewardAccount <- genFreshRegCred Certifying
      (poolId, _) <- genPool
      pure $ DelegStakeTxCert rewardAccount poolId

genTxCertDeleg :: forall era. Reflect era => GenRS era (TxCert era)
genTxCertDeleg = case reify @era of
  Shelley -> genShelleyDelegCert
  Mary -> genShelleyDelegCert
  Allegra -> genShelleyDelegCert
  Alonzo -> genShelleyDelegCert
  Babbage -> genShelleyDelegCert
  Conway -> genShelleyDelegCert

genTxCert :: forall era. Reflect era => SlotNo -> GenRS era (TxCert era)
genTxCert slot =
  elementsT
    [ genTxCertDeleg
    , frequencyT
        [ (75, RegPoolTxCert <$> genFreshPool)
        , (25, RetirePoolTxCert <$> genRetirementHash <*> genEpoch)
        ]
    ]
  where
    genFreshPool = do
      (_kh, pp, _) <- genNewPool
      return pp
    genEpoch = do
      let EpochNo txEpoch = epochFromSlotNo slot
      EpochNo curEpoch <- gets $ mEL . gsModel
      EpochInterval maxEpoch <- asks $ view ppEMaxL . gePParams
      let nextEpoch = 1 + (txEpoch - curEpoch) -- This will be either 1 or 2. It is 2 if the Tx is at the epoch boundary
      delta <- lift $ choose (nextEpoch, fromIntegral maxEpoch)
      return . EpochNo $ (curEpoch + delta)

-- getShelleyTxCertDelegG :: forall era. Reflect era => TxCert era -> Maybe (ShelleyDelegCert (EraCrypto era))
-- getShelleyTxCertDelegG = case reify @era of
--   Shelley -> getShelleyTxCertDeleg
--   Mary -> getShelleyTxCertDeleg
--   Allegra -> getShelleyTxCertDeleg
--   Alonzo -> getShelleyTxCertDeleg
--   Babbage -> getShelleyTxCertDeleg
--   Conway -> getShelleyTxCertDeleg -- TODO write a generator for Conwayerts

-- mkShelleyTxCertDelegG :: forall era. Reflect era => ShelleyDelegCert (EraCrypto era) -> TxCert era
-- mkShelleyTxCertDelegG = case reify @era of
--   Shelley -> mkShelleyTxCertDeleg
--   Mary -> mkShelleyTxCertDeleg
--   Allegra -> mkShelleyTxCertDeleg
--   Alonzo -> mkShelleyTxCertDeleg
--   Babbage -> mkShelleyTxCertDeleg
--   Conway -> mkShelleyTxCertDeleg -- TODO write a generator for Conwayerts

genTxCerts :: forall era. Reflect era => SlotNo -> GenRS era [TxCert era]
genTxCerts slot = do
  let genUniqueScript (!dcs, !ss, !regCreds) _ = do
        honest <- gets gsStableDelegators
        dc <- genTxCert slot
        -- Workaround a misfeature where duplicate plutus scripts in TxCert are ignored
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
          RegPoolTxCert _ -> pure (dc : dcs, ss, regCreds)
          RetirePoolTxCert kh _ -> do
            -- We need to make sure that the pool is registered before
            -- we try to retire it
            modelPools <- gets $ mPoolParams . gsModel
            case Map.lookup kh modelPools of
              Just _ -> pure (dc : dcs, ss, regCreds)
              Nothing -> pure (dcs, ss, regCreds)
          RegTxCert regCred ->
            if regCred `Map.member` regCreds -- Can't register if it is already registered
              then pure (dcs, ss, regCreds)
              else pure (dc : dcs, ss, Map.insert regCred (Coin 99) regCreds) -- 99 is a NonZero Value
          UnRegTxCert deregCred ->
            -- We can't make ShelleyUnRegCert certificate if deregCred is not already registered
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
                      <$> plutusScriptHashFromTag deregCred Certifying
              Just (Coin _) -> pure (dcs, ss, regCreds)
          DelegStakeTxCert delegCred delegKey ->
            let (dcs', regCreds') =
                  if delegCred `Map.member` regCreds
                    then (dcs, regCreds)
                    else -- In order to Delegate, the delegCred must exist in rewards.
                    -- so if it is not there, we put it there, otherwise we may
                    -- never generate a valid delegation.

                      ( (RegTxCert delegCred) : dcs
                      , Map.insert delegCred (Coin 99) regCreds
                      )
             in insertIfNotPresent dcs' regCreds' (Just delegKey)
                  <$> plutusScriptHashFromTag delegCred Certifying
          _ -> pure (dc : dcs, ss, regCreds)
  maxcert <- gets getCertificateMax
  n <- lift $ choose (0, maxcert)
  reward <- gets (mRewards . gsModel)
  let initSets ::
        ( [TxCert era]
        , Set (ScriptHash (EraCrypto era), Maybe (KeyHash 'StakePool (EraCrypto era)))
        , Map (Credential 'Staking (EraCrypto era)) Coin
        )
      initSets = ([], Set.empty, reward)
  (dcs, _, _) <- F.foldlM genUniqueScript initSets [1 :: Int .. n]
  pure $ reverse dcs

spendOnly :: EraTxOut era => TxOut era -> Bool
spendOnly txOut = case txOut ^. addrTxOutL of
  Addr _ (ScriptHashObj _) _ -> False
  _ -> True

-- | Generate a set of Collateral inputs sufficient to pay the minimum fee ('minCollTotal') computed
--   from the fee and the collateralPercentage from the PParams. Return the new UTxO, the inputs,
--   and coin of the excess amount included in the inputs, above what is needed to pay the minimum fee.
genCollateralUTxO ::
  forall era.
  (HasCallStack, Reflect era) =>
  [Addr (EraCrypto era)] ->
  Coin ->
  MUtxo era ->
  GenRS era (MUtxo era, Map.Map (TxIn (EraCrypto era)) (TxOut era), Coin)
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
        txIn <- lift (resize 30 (arbitrary :: Gen (TxIn (EraCrypto era))))
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
        [Addr (EraCrypto era)] ->
        Map (TxIn (EraCrypto era)) (TxOut era) ->
        Coin ->
        Map (TxIn (EraCrypto era)) (TxOut era) ->
        GenRS era (Map (TxIn (EraCrypto era)) (TxOut era), Coin)
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

-- | This function is used to generate the Outputs of a TxBody, It is computed by taking the
--   Outputs of the range of the (UTxO resticted by the Inputs of the TxBody),
--   as input to the function, and then making new Outputs, where the sum of the Coin is the same.
--   This way we generate a 'balanced' TxBody (modulo fees, deposits, refunds etc. which are
--   handled separately). The idea is to make sum(txOuts) == sum(genRecipientsFrom txouts), the
--   sum will be the same, but the size may be different.
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

getTxCertCredential ::
  forall era. Reflect era => TxCert era -> Maybe (Credential 'Staking (EraCrypto era))
getTxCertCredential = case reify @era of
  Shelley -> getShelleyTxCertCredential
  Mary -> getShelleyTxCertCredential
  Allegra -> getShelleyTxCertCredential
  Alonzo -> getShelleyTxCertCredential
  Babbage -> getShelleyTxCertCredential
  Conway -> getConwayTxCertCredential

getShelleyTxCertCredential :: ShelleyTxCert era -> Maybe (Credential 'Staking (EraCrypto era))
getShelleyTxCertCredential = \case
  ShelleyTxCertDelegCert d ->
    case d of
      ShelleyRegCert _rk -> Nothing -- we don't require witnesses for ShelleyRegCert
      ShelleyUnRegCert drk -> Just drk
      ShelleyDelegCert dk _ -> Just dk
  ShelleyTxCertPool pc ->
    case pc of
      RegPool PoolParams {..} -> Just . coerceKeyRole $ KeyHashObj ppId
      RetirePool kh _ -> Just . coerceKeyRole $ KeyHashObj kh
  ShelleyTxCertGenesisDeleg _g -> Nothing
  ShelleyTxCertMir _m -> Nothing

getConwayTxCertCredential :: ConwayTxCert era -> Maybe (Credential 'Staking (EraCrypto era))
getConwayTxCertCredential (ConwayTxCertPool (RegPool PoolParams {..})) = Just . coerceKeyRole $ KeyHashObj ppId
getConwayTxCertCredential (ConwayTxCertPool (RetirePool kh _)) = Just . coerceKeyRole $ KeyHashObj kh
getConwayTxCertCredential (ConwayTxCertDeleg (ConwayRegCert _ _)) = Nothing
getConwayTxCertCredential (ConwayTxCertDeleg (ConwayUnRegCert cred _)) = Just cred
getConwayTxCertCredential (ConwayTxCertDeleg (ConwayDelegCert cred _)) = Just cred
getConwayTxCertCredential (ConwayTxCertDeleg (ConwayRegDelegCert cred _ _)) = Just cred
getConwayTxCertCredential (ConwayTxCertGov _) = Nothing

genWithdrawals ::
  Reflect era => SlotNo -> GenRS era (Withdrawals (EraCrypto era), RewardAccounts (EraCrypto era))
genWithdrawals slot =
  if epochFromSlotNo slot == EpochNo 0
    then do
      let networkId = Testnet
      newRewards <- genRewards
      pure (Withdrawals $ Map.mapKeys (RewardAccount networkId) newRewards, newRewards)
    else pure (Withdrawals Map.empty, Map.empty)

timeToLive :: ValidityInterval -> SlotNo
timeToLive (ValidityInterval _ (SJust n)) = n
timeToLive (ValidityInterval _ SNothing) = SlotNo maxBound

-- ============================================================================

minus :: MUtxo era -> Maybe (UtxoEntry era) -> MUtxo era
minus m Nothing = m
minus m (Just (txin, _)) = Map.delete txin m

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
    ( UTxO era
    , Tx era
    , UtxoEntry era -- The fee key
    , Maybe (UtxoEntry era) -- from oldUtxO
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
  ( feepair@(feeKey, _) -- One of the spending inputs, to be used to pay the fee
    , toSpendNoCollateral -- All of the spending inputs
    , refInputsUtxo -- All the reference inputs
    , utxoNoCollateral -- Union of all the above
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
      proof
      Spending
      [ (\dh cred -> (lookupByKeyM "datum" dh gsDatums, cred))
        <$> mDatumHash
        <*> Just credential
      | (_, coretxout) <- Map.toAscList toSpendNoCollateral
      , let (credentials, mDatumHash) = txoutEvidence proof coretxout
      , credential <- credentials
      ]

  -- generate Withdrawals before TxCerts, as Rewards are populated in the Model here,
  -- and we need to avoid certain TxCerts if they conflict with existing Rewards
  (withdrawals@(Withdrawals wdrlMap), newRewards) <- genWithdrawals slot
  let withdrawalAmount = F.fold wdrlMap

  rewardsWithdrawalTxOut <-
    if withdrawalAmount == Coin 0
      then pure Nothing
      else Just . coreTxOut proof <$> genTxOut proof (inject withdrawalAmount)
  let wdrlCreds = map (raCredential . fst) $ Map.toAscList wdrlMap
  (IsValid v2, mkWithdrawalsWits) <-
    redeemerWitnessMaker proof Rewarding $ map (Just . (,) genDatum) wdrlCreds

  dcerts <- genTxCerts slot
  let dcertCreds = map getTxCertCredential dcerts
  (IsValid v3, mkCertsWits) <-
    redeemerWitnessMaker proof Certifying $ map ((,) genDatum <$>) dcertCreds

  let isValid = IsValid (v1 && v2 && v3)
      mkWits :: [ExUnits -> [WitnessesField era]]
      mkWits = mkPaymentWits ++ mkCertsWits ++ mkWithdrawalsWits
  exUnits <- genExUnits proof (length mkWits)

  let redeemerWitsList :: [WitnessesField era]
      redeemerWitsList = concat (zipWith ($) mkWits exUnits)
  datumWitsList <- concat <$> mapM (makeDatumWitness proof) (Map.elems toSpendNoCollateral)
  keyWitsMakers <-
    mapM
      (genTxOutKeyWitness proof (Just Spending))
      (toSpendNoCollateralTxOuts ++ Map.elems refInputsUtxo)
  dcertWitsMakers <- mapM (genCredKeyWit proof (Just Certifying)) $ catMaybes dcertCreds
  rwdrsWitsMakers <- mapM (genCredKeyWit proof (Just Rewarding)) wdrlCreds

  -- 5. Estimate inputs that will be used as collateral
  maxCollateralCount <-
    lift $ chooseInt (1, fromIntegral (maxCollateralInputs' proof gePParams))
  bogusCollateralTxId <- lift (arbitrary :: Gen (TxId (EraCrypto era)))
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
    if Some proof >= Some Babbage
      then
        frequencyT
          [ (1, pure SNothing)
          , (9, SJust . coreTxOut proof <$> genTxOut proof (inject (Coin 0)))
          ]
      else pure SNothing
  let updateCollReturn SNothing _ = SNothing
      updateCollReturn (SJust txout) v = SJust (injectFee proof v txout)

  -- 7. Estimate the fee
  let redeemerDatumWits = redeemerWitsList ++ datumWitsList
      bogusIntegrityHash = newScriptIntegrityHash proof gePParams mempty (mkRedeemers proof []) mempty
      inputSet = Map.keysSet toSpendNoCollateral
      outputList = maybe recipients (: recipients) rewardsWithdrawalTxOut
      txBodyNoFee =
        coreTxBody
          proof
          [ Inputs inputSet
          , Collateral bogusCollateralTxIns
          , RefInputs (Map.keysSet refInputsUtxo)
          , TotalCol bogusTotalCol
          , Outputs' outputList
          , CollateralReturn bogusCollReturn
          , Certs' dcerts
          , Withdrawals' withdrawals
          , Txfee maxCoin
          , if Some proof >= Some Allegra
              then Vldt validityInterval
              else TTL (timeToLive validityInterval)
          , Update' []
          , ReqSignerHashes' []
          , Generic.Mint mempty
          , WppHash bogusIntegrityHash
          , AdHash' []
          , Txnetworkid networkId
          ]
      txBodyNoFeeHash = hashAnnotated txBodyNoFee
      witsMakers :: [SafeHash (EraCrypto era) EraIndependentTxBody -> [WitnessesField era]]
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
          [ Body txBodyNoFee
          , TxWits (assembleWits proof noFeeWits)
          , Valid isValid
          , AuxData' []
          ]
      fee = getMinFeeTxUtxo gePParams bogusTxForFeeCalc (UTxO refInputsUtxo)

  keyDeposits <- gets (mKeyDeposits . gsModel)
  let deposits = case proof of
        Shelley -> depositsAndRefunds gePParams dcerts keyDeposits
        Mary -> depositsAndRefunds gePParams dcerts keyDeposits
        Allegra -> depositsAndRefunds gePParams dcerts keyDeposits
        Alonzo -> depositsAndRefunds gePParams dcerts keyDeposits
        Babbage -> depositsAndRefunds gePParams dcerts keyDeposits
        Conway -> depositsAndRefunds gePParams dcerts keyDeposits

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
          (mkTxrdmrs proof redeemerDatumWits)
          (mkTxdats redeemerDatumWits)
      balance =
        case bogusCollReturn of
          SNothing -> txInBalance (Map.keysSet collMap) utxo
          SJust _ -> txInBalance (Map.keysSet collMap) utxo <-> excessColCoin
      txBody =
        overrideTxBody
          proof
          txBodyNoFee
          [ Txfee fee
          , Collateral (Map.keysSet collMap)
          , CollateralReturn (updateCollReturn bogusCollReturn excessColCoin)
          , TotalCol (updateTotalColl bogusTotalCol balance)
          , WppHash mIntegrityHash
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
          [ Body txBody
          , TxWits (assembleWits proof wits)
          , Valid isValid
          , AuxData' []
          ]
  count <- gets (mCount . gsModel)
  modifyGenStateInitialRewards (`Map.union` newRewards)
  modifyGenStateInitialUtxo (`Map.union` minus utxo maybeoldpair)
  modifyModelCount (const (count + 1))
  modifyModelIndex (Map.insert count (TxId txBodyHash))
  modifyModelUTxO (const utxo)

  pure (UTxO utxo, validTx, feepair, maybeoldpair)

-- | Keep only Script witnesses that are neccessary in 'era',
onlyNecessaryScripts ::
  Proof era -> Set (ScriptHash (EraCrypto era)) -> [WitnessesField era] -> [WitnessesField era]
onlyNecessaryScripts _ _ [] = []
onlyNecessaryScripts proof hashes (ScriptWits m : xs) =
  ScriptWits (Map.restrictKeys m hashes) : onlyNecessaryScripts proof hashes xs
onlyNecessaryScripts proof hashes (x : xs) = x : onlyNecessaryScripts proof hashes xs

-- | Scan though the fields unioning all the RdrmWits fields into one Redeemer map
mkTxrdmrs :: Proof era -> [WitnessesField era] -> Redeemers era
mkTxrdmrs proof fields = mkRedeemers proof $ List.foldl' accum [] fields
  where
    accum m1 (RdmrWits r2) = m1 ++ Map.toList (unRedeemers r2)
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
  ( Era era
  , PrettyA (State (EraRule "LEDGER" era))
  , PrettyA (Script era)
  , PrettyA (Signal (EraRule "LEDGER" era))
  , Signal (EraRule "LEDGER" era) ~ Tx era
  ) =>
  Show (Box era)
  where
  show (Box _proof (TRC (_env, _state, _sig)) _gs) =
    show $
      ppRecord
        "Box"
        []

-- ==============================================================================
-- How we take the generated stuff and put it through the STS rule mechanism
-- in a way that is Era Agnostic

applySTSByProof ::
  forall era.
  Era era =>
  Proof era ->
  RuleContext 'Transition (EraRule "LEDGER" era) ->
  Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) (State (EraRule "LEDGER" era))
applySTSByProof Conway trc = runShelleyBase $ applySTS trc
applySTSByProof Babbage trc = runShelleyBase $ applySTS trc
applySTSByProof Alonzo trc = runShelleyBase $ applySTS trc
applySTSByProof Mary trc = runShelleyBase $ applySTS trc
applySTSByProof Allegra trc = runShelleyBase $ applySTS trc
applySTSByProof Shelley trc = runShelleyBase $ applySTS trc
