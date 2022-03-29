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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Properties where

import Cardano.Ledger.Alonzo.Data (Data, dataToBinaryData, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (TxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (languages)
import Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    Redeemers (..),
    TxDats (..),
  )
import Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Babbage.PParams as Babbage (PParams' (..))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage (Datum (..), TxOut (..))
import Cardano.Ledger.BaseTypes
  ( Network (..),
    ProtVer (..),
    mkTxIxPartial,
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody, ScriptHash (..))
import Cardano.Ledger.Keys
  ( GenDelegs (..),
    KeyRole (..),
    coerceKeyRole,
  )
import Cardano.Ledger.Pretty (PrettyA (..), ppList, ppRecord)
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.API
  ( Addr (..),
    Credential (..),
    LedgerEnv (LedgerEnv),
    RewardAcnt (..),
    StakeReference (..),
    UTxO (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    LedgerState (..),
    PState (..),
    UTxOState (..),
    rewards,
  )
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams' (..))
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import qualified Cardano.Ledger.Shelley.Scripts as Shelley (MultiSig (..))
import Cardano.Ledger.Shelley.TxBody (DCert (..), DelegCert (..), Delegation (..))
import Cardano.Ledger.Shelley.UTxO (balance, makeWitnessVKey)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM, replicateM)
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWST (..), ask)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Bifunctor (first)
import Data.Coerce
import qualified Data.Compact.SplitMap as SplitMap
import Data.Default.Class (Default (def))
import qualified Data.Foldable as F
import Data.Functor
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Monoid (All (..))
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UMap (View (Rewards))
import qualified Data.UMap as UM
import Debug.Trace (trace)
import GHC.Stack
import Numeric.Natural
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
    emptyGenState,
    frequencyT,
    genCredential,
    genDatumWithHash,
    genGenEnv,
    genKeyHash,
    genPool,
    genPositiveVal,
    genRewards,
    genScript,
  )
import Test.Cardano.Ledger.Generic.PrettyCore (txSummary, utxoString)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Updaters hiding (first)
import Test.Cardano.Ledger.Shelley.Generator.Core (genNatural)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Utils (runShelleyBase)
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- ===================================================
-- Assembing lists of Fields in to (Core.XX era)

-- | This uses merging semantics, it expects duplicate fields, and merges them together
assembleWits :: Era era => Proof era -> [WitnessesField era] -> Core.Witnesses era
assembleWits era = List.foldl' (updateWitnesses merge era) (initialWitnesses era)

coreTxOut :: Era era => Proof era -> [TxOutField era] -> Core.TxOut era
coreTxOut era dts = List.foldl' (updateTxOut era) (initialTxOut era) dts

coreTxBody :: Era era => Proof era -> [TxBodyField era] -> Core.TxBody era
coreTxBody era dts = List.foldl' (updateTxBody era) (initialTxBody era) dts

overrideTxBody :: Proof era -> Core.TxBody era -> [TxBodyField era] -> Core.TxBody era
overrideTxBody era old dts = List.foldl' (updateTxBody era) old dts

coreTx :: Proof era -> [TxField era] -> Core.Tx era
coreTx era dts = List.foldl' (updateTx era) (initialTx era) dts

-- ==================================================
-- Era agnostic generators.

lookupByKeyM ::
  (MonadState s m, Ord k, Show k) => String -> k -> (s -> Map.Map k v) -> m v
lookupByKeyM name k getMap = do
  m <- getMap <$> get
  case Map.lookup k m of
    Nothing ->
      error $
        "Can't find " ++ name ++ " in the test enviroment: " ++ show k
    Just val -> pure val

-- | Generate a list of specified length with randomish `ExUnit`s where the sum
--   of all values produced will not exceed the maxTxExUnits.
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

lookupScript ::
  forall era m.
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
genTxOutKeyWitness ::
  forall era.
  (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Core.TxOut era ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
genTxOutKeyWitness era mTag txout =
  case (getTxOutAddr txout) of
    AddrBootstrap baddr ->
      error $ "Can't authorize bootstrap address: " ++ show baddr
    Addr _ payCred _ ->
      case getTxOutRefScript reify txout of
        SNothing -> (mkWitVKey era mTag payCred)
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

-- | Same as `genCredTimelockKeyWit`, but for `TxOuts`
genTxOutTimelockKeyWitness ::
  forall era.
  (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Core.TxOut era ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> Core.Witnesses era)
genTxOutTimelockKeyWitness era mTag txout = do
  case (getTxOutAddr txout) of
    AddrBootstrap baddr ->
      error $ "Can't authorize bootstrap address: " ++ show baddr
    Addr _ payCred _ -> (assembleWits era .) <$> (mkWitVKey era mTag payCred)

-- | Generator for witnesses necessary for Scripts and Key
-- credentials. Because of the Key credentials generating function requires a body
-- hash for an acutal witness to be constructed. In order to be able to estimate
-- fees and collateral needed we will use produced witness generators twice: one
-- time with bogus body hash for estimation, and the second time with an actual
-- body hash.
genCredTimelockKeyWit ::
  forall era k.
  (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Credential k (Crypto era) ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> Core.Witnesses era)
genCredTimelockKeyWit era mTag cred =
  do
    f <- mkWitVKey era mTag cred
    pure (assembleWits era . f)

-- | Generate a Witnesses producing function. We handle Witnesses come from Keys and Scripts
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
      let scriptWit = [ScriptWits' [script]]
      otherWit <- genGenericScriptWitness era mTag script
      pure (\hash -> (scriptWit ++ otherWit hash))

genGenericScriptWitness ::
  (Reflect era) =>
  Proof era ->
  Maybe Tag ->
  Core.Script era ->
  GenRS era (SafeHash (Crypto era) EraIndependentTxBody -> [WitnessesField era])
genGenericScriptWitness (Shelley c) mTag timelock = mkMultiSigWit (Shelley c) mTag timelock
genGenericScriptWitness (Allegra c) mTag timelock = mkTimelockWit (Allegra c) mTag timelock
genGenericScriptWitness (Mary c) mTag timelock = mkTimelockWit (Mary c) mTag timelock
genGenericScriptWitness (Alonzo c) mTag (TimelockScript timelock) = mkTimelockWit (Alonzo c) mTag timelock
genGenericScriptWitness (Alonzo _) _ (PlutusScript _ _) = pure (const [])
genGenericScriptWitness (Babbage c) mTag (TimelockScript timelock) = mkTimelockWit (Babbage c) mTag timelock
genGenericScriptWitness (Babbage _) _ (PlutusScript _ _) = pure (const [])

-- | Used in Aonzo and Babbage and Mary Eras
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

makeDatumWitness :: Proof era -> Core.TxOut era -> GenRS era [WitnessesField era]
makeDatumWitness proof txout = case (proof, txout) of -- (TxOut _ _ mDatum) = mkDatumWit mDatum
  (Babbage _, Babbage.TxOut _ _ (Babbage.DatumHash h) _) -> mkDatumWit (SJust h)
  (Babbage _, Babbage.TxOut _ _ (Babbage.Datum _) _) -> pure []
  (Babbage _, Babbage.TxOut _ _ Babbage.NoDatum _) -> pure []
  (Alonzo _, TxOut _ _ mDatum) -> mkDatumWit mDatum
  _ -> pure [] -- No other era has data witnesses
  where
    mkDatumWit SNothing = pure []
    mkDatumWit (SJust datumHash) = do
      datum <- lookupByKeyM "datum" datumHash gsDatums
      pure [DataWits' [datum]]

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

-- | Collaterals can't have scripts, this is where this generator is needed.
genNoScriptRecipient :: Reflect era => GenRS era (Addr (Crypto era))
genNoScriptRecipient = do
  paymentCred <- KeyHashObj <$> genKeyHash
  stakeCred <- StakeRefBase . KeyHashObj <$> genKeyHash
  pure (Addr Testnet paymentCred stakeCred)

genRecipient :: Reflect era => GenRS era (Addr (Crypto era))
genRecipient = do
  paymentCred <- genCredential Spend
  stakeCred <- genCredential Cert
  pure (Addr Testnet paymentCred (StakeRefBase stakeCred))

genDatum :: Era era => GenRS era (Data era)
genDatum = snd <$> genDatumWithHash

-- | Generate a Babbage Datum witness to use as a redeemer for a Plutus Script.
--   Witnesses can be a ScriptHash, or an inline Datum
genBabbageDatum :: forall era. Era era => GenRS era (Babbage.Datum era)
genBabbageDatum =
  frequencyT
    [ (1, (Babbage.DatumHash . fst) <$> genDatumWithHash),
      (4, (Babbage.Datum . dataToBinaryData . snd) <$> genDatumWithHash)
    ]

genRefScript :: Reflect era => Proof era -> GenRS era (StrictMaybe (Core.Script era))
genRefScript proof = do
  scripthash <- genScript proof Spend
  mscript <- lookupScript scripthash (Just Spend)
  case mscript of
    Nothing -> pure SNothing
    Just script -> pure (SJust script)

genTxOut :: Reflect era => Proof era -> Core.Value era -> GenRS era [TxOutField era]
genTxOut proof val = do
  addr <- genRecipient
  cred <- maybe (error "BootstrapAddress encountered") pure $ paymentCredAddr addr
  dataHashFields <-
    case cred of
      KeyHashObj _ -> pure []
      ScriptHashObj scriptHash -> do
        maybecorescript <- lookupScript scriptHash (Just Spend)
        case (proof, maybecorescript) of
          (Babbage _, Just (PlutusScript _ _)) -> do
            datum <- genBabbageDatum
            script <- genRefScript proof
            pure $ [Datum datum] ++ [RefScript script]
          (Alonzo _, Just (PlutusScript _ _)) -> do
            (datahash, _data) <- genDatumWithHash
            pure [DHash (SJust datahash)]
          (_, _) -> pure []
  pure $ [Address addr, Amount val] ++ dataHashFields

genUTxO :: Reflect era => GenRS era (UTxO era)
genUTxO = do
  NonEmpty ins <- lift $ resize 20 arbitrary
  UTxO <$> sequence (SplitMap.fromSet (const genOut) (Set.fromList ins))
  where
    genOut = do
      val <- lift genPositiveVal
      fields <- genTxOut reify val
      pure (coreTxOut reify fields)

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
  DPState {dpsDState = DState {_unified}} <- gsDPState <$> get
  let initSets = ([], Set.empty, UM.domain (Rewards _unified))
  (dcs, _, _) <- F.foldlM genUniqueScript initSets [1 :: Int .. n]
  pure $ reverse dcs

genCollateralUTxO ::
  forall era.
  (HasCallStack, Reflect era) =>
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
        -- The size of the Gen computation is driven down when we generate scripts, so it can be 0 here
        -- that is really bad, because if the happens we get the same TxIn every time, and 'coll' never grows,
        -- so this function doesn't terminate. We want many choices of TxIn, so resize just this arbitrary by 10.
        txIn <- lift (resize 20 (arbitrary :: Gen (TxIn (Crypto era))))
        if SplitMap.member txIn utxo || Map.member txIn coll
          then genNewCollateral addr coll um c
          else pure (um, Map.insert txIn (coreTxOut reify [Address addr, Amount (inject c)]) coll, c)
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
      go ::
        [Addr (Crypto era)] ->
        Map (TxIn (Crypto era)) (Core.TxOut era) ->
        Coin ->
        Map (TxIn (Crypto era)) (Core.TxOut era) ->
        GenRS era (Map (TxIn (Crypto era)) (Core.TxOut era))
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

genUTxOState :: forall era. Reflect era => UTxO era -> GenRS era (UTxOState era)
genUTxOState utxo = do
  GenEnv {gePParams} <- ask
  DPState {dpsDState, dpsPState} <- gsDPState <$> get
  let deposited = obligation' reify gePParams (rewards dpsDState) (_pParams dpsPState)
  lift (UTxOState utxo deposited <$> arbitrary <*> pure (emptyPPUPstate @era reify) <*> pure def)

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
        where
          v = getTxOutVal reify txout
      genWithChange s txout rs = do
        let (!addr, !v, !ds) = txoutFields reify txout
        c <- Coin <$> lift (choose (1, unCoin $ coin v))
        fields <- genTxOut reify (s <+> inject c)
        if c < coin v
          then
            let !change = coreTxOut reify ([Address addr, Amount (v <-> inject c)] ++ ds)
             in pure (coreTxOut reify fields : change : rs)
          else pure (coreTxOut reify fields : rs)
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

genWithdrawals :: Reflect era => GenRS era (Wdrl (Crypto era))
genWithdrawals = do
  let networkId = Testnet
  newrewards <- genRewards
  pure $ Wdrl $ Map.fromList $ map (first (RewardAcnt networkId)) $ Map.toList newrewards

languagesUsed ::
  forall era.
  Era era =>
  Proof era ->
  Core.Tx era ->
  UTxO era ->
  Map (ScriptHash (Crypto era), Tag) (IsValid, Core.Script era) ->
  Set Language
languagesUsed proof tx utxo _plutusScripts = case proof of
  (Shelley _) -> Set.empty
  (Allegra _) -> Set.empty
  (Mary _) -> Set.empty
  (Alonzo _) -> Cardano.Ledger.Alonzo.TxInfo.languages tx utxo
  (Babbage _) -> Cardano.Ledger.Alonzo.TxInfo.languages tx utxo

timeToLive :: ValidityInterval -> SlotNo
timeToLive (ValidityInterval _ (SJust n)) = n
timeToLive (ValidityInterval _ SNothing) = SlotNo maxBound

genValidatedTx :: forall era. Reflect era => Proof era -> GenRS era (UTxO era, Core.Tx era)
genValidatedTx proof = do
  GenEnv {geValidityInterval, gePParams} <- ask
  UTxO utxoNoCollateral <- genUTxO
  -- 1. Produce utxos that will be spent
  numInputs <- lift $ choose (1, length utxoNoCollateral)
  toSpendNoCollateral <-
    Map.fromList . take numInputs <$> lift (shuffle $ SplitMap.toList utxoNoCollateral)

  -- 2. Produce some that will not be spent but only referred to (Note this may overlap with the spending)
  numRefInputs <- lift $ choose (0, maxRefInputs proof)
  refInputsUtxo <-
    Map.fromList . take numRefInputs <$> lift (shuffle $ SplitMap.toList utxoNoCollateral)

  -- 3. Check if all Plutus scripts are valid
  let toSpendNoCollateralTxOuts :: [Core.TxOut era]
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
          <*> (Just credential)
        | (_, coretxout) <- Map.toAscList toSpendNoCollateral,
          let (credentials, mDatumHash) = txoutEvidence proof coretxout,
          credential <- credentials
      ]

  wdrls@(Wdrl wdrlMap) <- genWithdrawals
  rewardsWithdrawalTxOut <- coreTxOut proof <$> (genTxOut proof $ inject $ F.fold wdrlMap)
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

  let redeemerWitsList :: [WitnessesField era]
      redeemerWitsList = concat (zipWith ($) mkWits exUnits)
  datumWitsList <- concat <$> mapM (makeDatumWitness proof) (Map.elems toSpendNoCollateral)
  keyWitsMakers <- mapM (genTxOutKeyWitness proof (Just Spend)) (toSpendNoCollateralTxOuts ++ Map.elems refInputsUtxo)
  dcertWitsMakers <- mapM (genCredKeyWit proof (Just Cert)) $ catMaybes dcertCreds
  rwdrsWitsMakers <- mapM (genCredKeyWit proof (Just Rewrd)) wdrlCreds

  -- 5. Estimate inputs that will be used as collateral
  maxCollateralCount <-
    lift $ chooseInt (1, fromIntegral (maxCollateralInputs' proof gePParams))
  bogusCollateralTxId <- lift (arbitrary :: Gen (TxId (Crypto era)))
  let bogusCollateralTxIns =
        Set.fromList
          [ TxIn bogusCollateralTxId (mkTxIxPartial (fromIntegral i))
            | i <- [1 .. 10 :: Int] -- [maxBound, maxBound - 1 .. maxBound - maxCollateralCount - 1]
          ]
  collateralAddresses <- replicateM maxCollateralCount genNoScriptRecipient
  bogusCollateralKeyWitsMakers <-
    mapM (\a -> genTxOutKeyWitness proof Nothing (coreTxOut proof [Address a, Amount (inject maxCoin)])) collateralAddresses
  networkId <- lift $ elements [SNothing, SJust Testnet]

  -- 6. Estimate the fee
  let redeemerDatumWits = (redeemerWitsList ++ datumWitsList)
      bogusIntegrityHash = hashScriptIntegrity' proof gePParams mempty (Redeemers mempty) mempty
      txBodyNoFee =
        coreTxBody
          proof
          [ Inputs (Map.keysSet toSpendNoCollateral),
            Collateral bogusCollateralTxIns,
            RefInputs (Map.keysSet refInputsUtxo),
            TotalCol (Coin 0), -- Add a bogus Coin, fill it in later
            Outputs' (rewardsWithdrawalTxOut : recipients),
            Certs' dcerts,
            Wdrls wdrls,
            Txfee maxCoin,
            if (Some proof) >= (Some (Allegra Mock))
              then (Vldt geValidityInterval)
              else (TTL (timeToLive geValidityInterval)),
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
      noFeeWits :: [WitnessesField era]
      noFeeWits =
        redeemerDatumWits
          <> foldMap ($ txBodyNoFeeHash) (witsMakers ++ bogusCollateralKeyWitsMakers)
      bogusTxForFeeCalc =
        coreTx
          proof
          [ Body txBodyNoFee,
            Witnesses (assembleWits proof noFeeWits),
            Valid isValid,
            AuxData' []
          ]
      -- refAdjustment = txInBalance (Map.keysSet refInputsUtxo) (UTxO (SplitMap.fromMap refInputsUtxo))
      fee = minfee' proof gePParams bogusTxForFeeCalc

  -- 7. Crank up the amount in one of outputs to account for the fee. Note this is
  -- a hack that is not possible in a real life, but in the end it does produce
  -- real life like setup
  feeKey <- lift $ elements $ Map.keys toSpendNoCollateral
  let utxoFeeAdjusted =
        UTxO $ case SplitMap.lookup feeKey utxoNoCollateral of
          Nothing -> utxoNoCollateral -- It is aways there because spend is a subset, s just need the txOut
          Just txOut -> SplitMap.insert feeKey (injectFee proof fee txOut) utxoNoCollateral

  -- 8. Generate utxos that will be used as collateral
  (utxo, collMap) <- genCollateralUTxO collateralAddresses fee utxoFeeAdjusted
  collateralKeyWitsMakers <-
    mapM (genTxOutKeyWitness proof Nothing) $ Map.elems collMap

  -- 9. Construct the correct Tx with valid fee and collaterals
  allPlutusScripts <- gsPlutusScripts <$> get
  let mIntegrityHash =
        hashScriptIntegrity'
          proof
          gePParams
          (languagesUsed proof bogusTxForFeeCalc (UTxO utxoNoCollateral) allPlutusScripts)
          (mkTxrdmrs redeemerDatumWits)
          (mkTxdats redeemerDatumWits)
      txBody =
        overrideTxBody
          proof
          txBodyNoFee
          [ Txfee fee,
            Collateral (Map.keysSet collMap),
            TotalCol (txInBalance (Map.keysSet collMap) utxo),
            WppHash mIntegrityHash
          ]
      txBodyHash = hashAnnotated txBody
      wits =
        redeemerDatumWits
          <> foldMap ($ txBodyHash) (witsMakers ++ collateralKeyWitsMakers)
      validTx =
        coreTx
          proof
          [ Body txBody,
            Witnesses (assembleWits proof wits),
            Valid isValid,
            AuxData' []
          ]
  pure (utxo, validTx)

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
    accum m (DataWits' ds) = (List.foldl' accum2 m ds)
      where
        accum2 m2 d = Map.insert (hashData @era d) d m2
    accum m _ = m

-- =======================================================
-- An encapsulation of the Top level types we generate,
-- but that has its own Show instance that we can control.

data Box era = Box (Proof era) (TRC (Core.EraRule "LEDGER" era)) (GenState era)

instance
  ( Era era,
    PrettyA (State (Core.EraRule "LEDGER" era)),
    PrettyA (Core.Script era),
    PrettyA (Signal (Core.EraRule "LEDGER" era)),
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era
  ) =>
  Show (Box era)
  where
  show (Box _proof (TRC (_env, _state, _sig)) _gs) =
    show $
      ppRecord
        "Box"
        []

-- Sample things we might use in the Box
-- ("Tx", txSummary proof _sig)
-- ("Tx", prettyA _sig)
-- ("TRC state",prettyA _state)
-- ("GenEnv",ppGenState proof _gs)

-- =====================================
-- Now the Top level generators

genTxAndUTXOState :: Reflect era => Proof era -> Gen (TRC (Core.EraRule "UTXOW" era), GenState era)
genTxAndUTXOState proof@(Babbage _) = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@(Alonzo _) = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@(Mary _) = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@(Allegra _) = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)
genTxAndUTXOState proof@(Shelley _) = do
  (Box _ (TRC (LedgerEnv slotNo _ pp _, ledgerState, vtx)) genState) <- genTxAndLEDGERState proof
  pure (TRC (UtxoEnv slotNo pp mempty (GenDelegs mempty), lsUTxOState ledgerState, vtx), genState)

genTxAndLEDGERState ::
  ( Reflect era,
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era,
    State (Core.EraRule "LEDGER" era) ~ LedgerState era,
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era
  ) =>
  Proof era ->
  Gen (Box era)
genTxAndLEDGERState proof = do
  txIx <- arbitrary
  maxTxExUnits <- (arbitrary :: Gen ExUnits)
  maxCollateralInputs <- elements [1 .. 7 :: Natural]
  collateralPercentage <- (fromIntegral <$> chooseInt (1, 10000)) :: Gen Natural
  minfeeA <- fromIntegral <$> chooseInt (0, 1000)
  minfeeB <- fromIntegral <$> chooseInt (0, 10000)
  -- (env,pp) <- setup proof -- Generate a PParams and a GenEnv
  let genT = do
        (utxo, tx) <- genValidatedTx proof
        utxoState <- genUTxOState utxo
        dpState <- gsDPState <$> get
        pure $ TRC (ledgerEnv, LedgerState utxoState dpState, tx)
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
            ProtocolVersion $ ProtVer 7 0
          ]
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
  pure (Box proof trc s)

-- ==============================================================================
-- How we take the generated stuff and put it through the STS rule mechanism
-- in a way that is Era Agnostic

applySTSByProof ::
  forall era.
  (GoodCrypto (Crypto era)) =>
  Proof era ->
  RuleContext 'Transition (Core.EraRule "LEDGER" era) ->
  (Either [PredicateFailure (Core.EraRule "LEDGER" era)] (State (Core.EraRule "LEDGER" era)))
applySTSByProof (Babbage _) _trc = runShelleyBase $ applySTS @(Core.EraRule "LEDGER" (BabbageEra (Crypto era))) _trc
applySTSByProof (Alonzo _) trc = runShelleyBase $ applySTS trc
applySTSByProof (Mary _) trc = runShelleyBase $ applySTS trc
applySTSByProof (Allegra _) trc = runShelleyBase $ applySTS trc
applySTSByProof (Shelley _) trc = runShelleyBase $ applySTS trc

-- =============================================
-- Now a test

totalAda :: Reflect era => LedgerState era -> Coin
totalAda (LedgerState (UTxOState utxo f d _ _) DPState {dpsDState}) =
  f <> d <> coin (balance utxo) <> F.foldl' (<>) mempty (rewards dpsDState)

-- Note we could probably abstract over an arbitray test here with
-- type:: Box era -> Core.Tx era -> UTxOState era -> DPState era -> Property

testTxValidForLEDGER ::
  ( Reflect era,
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era,
    State (Core.EraRule "LEDGER" era) ~ LedgerState era,
    PrettyA (PredicateFailure (Core.EraRule "LEDGER" era)),
    PrettyA (Signal (Core.EraRule "LEDGER" era))
  ) =>
  Proof era ->
  Box era ->
  Property
testTxValidForLEDGER proof (Box _ (trc@(TRC (_, ledgerState, vtx))) _) =
  ( if False
      then trace (show (txSummary proof vtx))
      else id
  )
    $ case applySTSByProof proof trc of -- trc encodes the initial (generated) state, vtx is the transaction
      Right ledgerState' ->
        -- UTxOState and DPState after applying the transaction $$$
        classify (coerce (isValid' proof vtx)) "TxValid" $
          totalAda ledgerState' === totalAda ledgerState
      Left errs ->
        counterexample
          ( utxoString proof (_utxo (lsUTxOState ledgerState)) ++ "\n\n"
              ++ show (prettyA vtx)
              ++ "\n\n"
              ++ show (ppList prettyA errs)
              ++ "\n\n"
              ++ show (txSummary proof vtx)
          )
          (property False)

-- ===============================================================
-- Tools for generating other things from a GenEnv. This way one can
-- test individual functions in this file.

-- | Construct a random (Gen b)
makeGen :: Proof era -> (Proof era -> GenRS era b) -> Gen b
makeGen proof computeWith = do
  env <- genGenEnv proof
  (ans, _state, _written) <- runRWST (computeWith proof) env emptyGenState
  pure ans

runTest :: PrettyA a => (Proof era -> GenRS era a) -> Proof era -> IO ()
runTest computeWith proof = do
  ans <- generate (makeGen proof computeWith)
  putStrLn (show (prettyA ans))

main2 :: IO ()
main2 = runTest (\x -> fst <$> genValidatedTx x) (Alonzo Mock)

-- =============================================
-- Make some property tests

-- =========================================================================
-- The generic types make a roundtrip without adding or losing information

txOutRoundTrip ::
  (Eq (Core.TxOut era), Era era) => Proof era -> Core.TxOut era -> Bool
txOutRoundTrip proof x = coreTxOut proof (abstractTxOut proof x) == x

txRoundTrip ::
  Eq (Core.Tx era) => Proof era -> Core.Tx era -> Bool
txRoundTrip proof x = coreTx proof (abstractTx proof x) == x

txBodyRoundTrip ::
  (Eq (Core.TxBody era), Era era) => Proof era -> Core.TxBody era -> Bool
txBodyRoundTrip proof x = coreTxBody proof (abstractTxBody proof x) == x

txWitRoundTrip ::
  (Eq (Core.Witnesses era), Era era) => Proof era -> Core.Witnesses era -> Bool
txWitRoundTrip proof x = assembleWits proof (abstractWitnesses proof x) == x

coreTypesRoundTrip :: TestTree
coreTypesRoundTrip =
  testGroup
    "Core types make generic roundtrips"
    [ testGroup
        "Witnesses roundtrip"
        [ testProperty "Babbage era" $ txWitRoundTrip (Babbage Mock), -- No Arbitrary instance yet
          testProperty "Alonzo era" $ txWitRoundTrip (Alonzo Mock),
          testProperty "Mary era" $ txWitRoundTrip (Mary Mock),
          testProperty "Allegra era" $ txWitRoundTrip (Allegra Mock),
          testProperty "Shelley era" $ txWitRoundTrip (Shelley Mock)
        ],
      testGroup
        "TxBody roundtrips"
        [ testProperty "Babbage era" $ txBodyRoundTrip (Babbage Mock), -- No Arbitrary instance yet
          testProperty "Alonzo era" $ txBodyRoundTrip (Alonzo Mock),
          testProperty "Mary era" $ txBodyRoundTrip (Mary Mock),
          testProperty "Allegra era" $ txBodyRoundTrip (Allegra Mock),
          testProperty "Shelley era" $ txBodyRoundTrip (Shelley Mock)
        ],
      testGroup
        "TxOut roundtrips"
        [ testProperty "Babbage era" $ txOutRoundTrip (Babbage Mock), -- No Arbitrary instance yet
          testProperty "Alonzo era" $ txOutRoundTrip (Alonzo Mock),
          testProperty "Mary era" $ txOutRoundTrip (Mary Mock),
          testProperty "Allegra era" $ txOutRoundTrip (Allegra Mock),
          testProperty "Shelley era" $ txOutRoundTrip (Shelley Mock)
        ],
      testGroup
        "Tx roundtrips"
        [ testProperty "Babbage era" $ txRoundTrip (Babbage Mock), -- No Arbitrary instance yet
          testProperty "Alonzo era" $ txRoundTrip (Alonzo Mock),
          testProperty "Mary era" $ txRoundTrip (Mary Mock),
          testProperty "Allegra era" $ txRoundTrip (Allegra Mock),
          testProperty "Shelley era" $ txRoundTrip (Shelley Mock)
        ]
    ]

genericProperties :: TestTree
genericProperties =
  testGroup
    "Generic Property tests"
    [ coreTypesRoundTrip,
      testGroup
        "Alonzo UTXOW property tests"
        [ testProperty "Shelley Tx preserves ADA" $
            forAll (genTxAndLEDGERState (Shelley Mock)) (testTxValidForLEDGER (Shelley Mock)),
          testProperty "Mary Tx preserves ADA" $
            forAll (genTxAndLEDGERState (Mary Mock)) (testTxValidForLEDGER (Mary Mock)),
          testProperty "Alonzo ValidTx preserves ADA" $
            forAll (genTxAndLEDGERState (Alonzo Mock)) (testTxValidForLEDGER (Alonzo Mock)),
          testProperty "Babbage ValidTx preserves ADA" $
            forAll (genTxAndLEDGERState (Babbage Mock)) (testTxValidForLEDGER (Babbage Mock))
        ]
    ]

-- ==============================================================
-- Infrastrucure for running individual tests, with easy replay.
-- In ghci just type
-- :main --quickcheck-replay=205148

main :: IO ()
main = test 1 (Babbage Mock)

test :: ReflectC (Crypto era) => Int -> Proof era -> IO ()
test n proof = defaultMain $
  case proof of
    Babbage _ ->
      testProperty "Babbage ValidTx preserves ADA" $
        (withMaxSuccess n (forAll (genTxAndLEDGERState proof) (testTxValidForLEDGER proof)))
    Alonzo _ ->
      testProperty "Babbage ValidTx preserves ADA" $
        (withMaxSuccess n (forAll (genTxAndLEDGERState proof) (testTxValidForLEDGER proof)))
    Shelley _ ->
      testProperty "Babbage ValidTx preserves ADA" $
        (withMaxSuccess n (forAll (genTxAndLEDGERState proof) (testTxValidForLEDGER proof)))
    other -> error ("NO Test in era " ++ show other)
