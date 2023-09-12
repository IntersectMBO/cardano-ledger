{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- ==============================================

module Test.Cardano.Ledger.Constrained.Preds.Tx where

import Cardano.Crypto.Signing (SigningKey)
import Cardano.Ledger.Address (Addr (..), BootstrapAddress, RewardAcnt (..))
import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (..), ScriptIntegrityHash)
import Cardano.Ledger.Alonzo.Language (Language (..), plutusLanguage)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptPurpose (..), rdptr)
import Cardano.Ledger.Alonzo.TxWits (
  RdmrPtr (..),
  Redeemers (..),
  TxDats (..),
 )
import Cardano.Ledger.Alonzo.UTxO (getInputDataHashesTxBody)
import Cardano.Ledger.Api (setMinFeeTx)
import Cardano.Ledger.Babbage.Tx (refScripts)
import Cardano.Ledger.BaseTypes (Network (..), ProtVer (..), StrictMaybe (..), strictMaybeToMaybe)
import Cardano.Ledger.Binary.Decoding (mkSized, sizedSize)
import Cardano.Ledger.Binary.Encoding (EncCBOR)
import Cardano.Ledger.Coin (Coin (..), rationalToCoinViaCeiling)
import Cardano.Ledger.Core (EraRule, EraScript (..), EraTx (..), EraTxBody (..), EraTxOut (..), Value, bodyTxL, coinTxOutL, feeTxBodyL)
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Hashes (DataHash, EraIndependentTxBody, ScriptHash (..))
import Cardano.Ledger.Keys (GenDelegPair (..), GenDelegs (..), Hash, KeyHash, KeyRole (..), asWitness, coerceKeyRole)
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Mary.Core (MaryEraTxBody)
import Cardano.Ledger.Mary.Value (AssetName, MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Pretty (PDoc, PrettyA (..), ppList, ppMap, ppRecord, ppSafeHash)
import Cardano.Ledger.SafeHash (SafeHash, extractHash, hashAnnotated)
import Cardano.Ledger.Shelley.AdaPots (consumedTxBody, producedTxBody)
import Cardano.Ledger.Shelley.LedgerState (LedgerState, keyCertsRefunds, totalCertsDeposits)
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), shelleyWitsVKeyNeeded, witsVKeyNeededNoGov)
import Cardano.Ledger.Shelley.TxBody (WitVKey (..))
import Cardano.Ledger.Shelley.TxCert (isInstantaneousRewards)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (EraUTxO (..))
import Cardano.Ledger.Val (Val (inject, (<+>), (<->)))
import Control.Monad (when)
import Control.State.Transition.Extended (STS (..), TRC (..))
import Data.Default.Class (Default (def))
import Data.Foldable (foldl')
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Examples (checkForSoundness, testIO)
import Test.Cardano.Ledger.Constrained.Monad (Typed, failT, generateWithSeed, monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.CertState (dstateStage, pstateStage, vstateStage)
import Test.Cardano.Ledger.Constrained.Preds.Certs (certsStage)
import Test.Cardano.Ledger.Constrained.Preds.LedgerState (ledgerStateStage)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Repl (ReplMode (..), goRepl, modeRepl)
import Test.Cardano.Ledger.Constrained.Preds.TxOut (txOutPreds)
import Test.Cardano.Ledger.Constrained.Preds.Universes hiding (demo, demoTest, main)
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Scripts (sufficientScript)
import Test.Cardano.Ledger.Constrained.Size (Size (..))
import Test.Cardano.Ledger.Constrained.Solver (toolChainSub)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars hiding (totalAda)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..), TxField (..), abstractTx, abstractTxBody)
import Test.Cardano.Ledger.Generic.Functions (TotalAda (totalAda), protocolVersion)
import Test.Cardano.Ledger.Generic.PrettyCore (
  pcGenDelegPair,
  pcKeyHash,
  pcScript,
  pcScriptHash,
  pcTxBodyField,
  pcTxField,
  pcTxIn,
  pcTxOut,
 )
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.TxGen (applySTSByProof)
import Test.Cardano.Ledger.Generic.Updaters (newScriptIntegrityHash)
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Test.Cardano.Ledger.Constrained.Preds.CertState as CertState
import qualified Test.Cardano.Ledger.Constrained.Preds.Certs as Certs
import qualified Test.Cardano.Ledger.Constrained.Preds.LedgerState as LedgerState
import qualified Test.Cardano.Ledger.Constrained.Preds.PParams as PParams
import qualified Test.Cardano.Ledger.Constrained.Preds.TxOut as TxOut
import qualified Test.Cardano.Ledger.Constrained.Preds.Universes as Universes

predsTests :: TestTree
predsTests =
  testGroup
    "Testing all Stages in the Preds directory"
    [PParams.demoTest, Universes.demoTest, TxOut.demoTest, CertState.demoTest, Certs.demoTest, LedgerState.demoTest]

-- ===========================================================

txBodySize :: forall era. Reflect era => TxBody era -> Int
txBodySize txb = fromIntegral (sizedSize (mkSized (pvMajor (protocolVersion (reify @era))) txb))

byteSize :: forall era a. (EncCBOR a, Reflect era) => Proof era -> a -> Int
byteSize _ x = fromIntegral (sizedSize (mkSized (pvMajor (protocolVersion (reify @era))) x))

byteSizeT :: forall era a. (EncCBOR a, Reflect era) => Term era a -> Target era Int
byteSizeT x = Constr "byteSize" (byteSize (reify @era)) ^$ x

-- ===============================================
-- Helpful Lenses

sndL :: Lens' (a, b) b
sndL = lens snd (\(x, _) y -> (x, y))

fstL :: Lens' (a, b) a
fstL = lens fst (\(_, y) x -> (x, y))

txFL :: Lens' (TxF era) (Tx era)
txFL = lens (\(TxF _ x) -> x) (\(TxF p _) x -> TxF p x)

-- ================================================
-- Auxiliary functions and Targets

computeFinalTx :: EraTx era => PParamsF era -> TxF era -> TxF era
computeFinalTx (PParamsF _ ppV) (TxF p txV) = TxF p newtx
  where
    newtx = setMinFeeTx ppV txV

computeFinalFee :: EraTx era => PParamsF era -> TxF era -> Coin
computeFinalFee (PParamsF _ ppV) (TxF _ txV) = newtx ^. (bodyTxL . feeTxBodyL)
  where
    newtx = setMinFeeTx ppV txV

integrityHash ::
  Era era1 =>
  Proof era1 ->
  Term era2 (PParamsF era1) ->
  Term era2 (Set Language) ->
  Term era2 (Map RdmrPtr (Data era1, ExUnits)) ->
  Term era2 (Map (DataHash (EraCrypto era1)) (Data era1)) ->
  Target era2 (Maybe (ScriptIntegrityHash (EraCrypto era1)))
integrityHash p pp langs rs ds = (Constr "integrityHash" hashfun ^$ pp ^$ langs ^$ rs ^$ ds)
  where
    hashfun (PParamsF _ ppp) ls r d =
      strictMaybeToMaybe $ newScriptIntegrityHash p ppp (Set.toList ls) (Redeemers r) (TxDats d)

-- | "Construct the Scripts Needed to compute the Script Witnesses from the UTxO and the partial TxBody
needT ::
  forall era.
  EraUTxO era =>
  Proof era ->
  Target
    era
    ( TxBodyF era ->
      Map (TxIn (EraCrypto era)) (TxOutF era) ->
      ScriptsNeededF era
    )
needT proof = Constr "neededScripts" needed
  where
    needed :: TxBodyF era -> Map (TxIn (EraCrypto era)) (TxOutF era) -> ScriptsNeededF era
    needed (TxBodyF _ txbodyV) ut = ScriptsNeededF proof (getScriptsNeeded (liftUTxO ut) txbodyV)

rdmrPtrsT ::
  MaryEraTxBody era =>
  Target
    era
    ( TxBodyF era ->
      [((ScriptPurpose era), (ScriptHash (EraCrypto era)))] ->
      Map (ScriptHash (EraCrypto era)) any ->
      Set RdmrPtr
    )
rdmrPtrsT = Constr "getRdmrPtrs" getRdmrPtrs

getRdmrPtrs ::
  MaryEraTxBody era =>
  TxBodyF era ->
  [((ScriptPurpose era), (ScriptHash (EraCrypto era)))] ->
  Map (ScriptHash (EraCrypto era)) any ->
  Set RdmrPtr
getRdmrPtrs (TxBodyF _ txbodyV) xs allplutus = List.foldl' accum Set.empty xs
  where
    accum ans (sp, hash) = case (rdptr txbodyV sp, Map.member hash allplutus) of
      (SJust x, True) -> Set.insert x ans
      _ -> ans

getPlutusDataHashes ::
  (AlonzoEraTxOut era, EraTxBody era, EraScript era) =>
  Map (TxIn (EraCrypto era)) (TxOutF era) ->
  TxBodyF era ->
  Map (ScriptHash (EraCrypto era)) (ScriptF era) ->
  Set (DataHash (EraCrypto era))
getPlutusDataHashes ut (TxBodyF _ txbodyV) m =
  fst $ getInputDataHashesTxBody (liftUTxO ut) txbodyV (Map.map unScriptF m)

bootWitsT ::
  forall era.
  EraTxOut era =>
  Proof era ->
  Map (TxIn (EraCrypto era)) (TxOutF era) ->
  TxBodyF era ->
  Map (KeyHash 'Payment (EraCrypto era)) (Addr (EraCrypto era), SigningKey) ->
  Set (BootstrapWitness (EraCrypto era))
bootWitsT proof spend (TxBodyF _ txb) byronUniv =
  case getCrypto proof of
    Mock -> error "Can only use StandardCrypto in bootWitsT"
    Standard -> bootWitness h boots byronUniv
      where
        boots :: [BootstrapAddress (EraCrypto era)] -- Not every Addr has a BootStrapAddress
        boots = Map.foldl' accum [] spend -- Compute a list of them.
          where
            accum ans (TxOutF _ out) = case out ^. addrTxOutL of
              AddrBootstrap b -> b : ans
              _ -> ans
        h = hashBody proof txb

hashBody :: forall era. Proof era -> TxBody era -> Hash (EraCrypto era) EraIndependentTxBody
hashBody (Shelley _) txb = extractHash @(EraCrypto era) (hashAnnotated txb)
hashBody (Allegra _) txb = extractHash @(EraCrypto era) (hashAnnotated txb)
hashBody (Mary _) txb = extractHash @(EraCrypto era) (hashAnnotated txb)
hashBody (Alonzo _) txb = extractHash @(EraCrypto era) (hashAnnotated txb)
hashBody (Babbage _) txb = extractHash @(EraCrypto era) (hashAnnotated txb)
hashBody (Conway _) txb = extractHash @(EraCrypto era) (hashAnnotated txb)

-- =======================================

-- | Get enough GenDeleg KeyHashes to satisfy the quorum constraint.
sufficientGenDelegs :: Map k (GenDelegPair c) -> Set (KeyHash 'Witness c)
sufficientGenDelegs gendel =
  Set.fromList (take (fromIntegral quorumConstant) (asWitness . genDelegKeyHash <$> Map.elems gendel))

sufficientTxCert ::
  forall era.
  Reflect era =>
  [TxCertF era] ->
  Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  Set (KeyHash 'Witness (EraCrypto era))
sufficientTxCert cs gendel = case whichTxCert (reify @era) of
  TxCertShelleyToBabbage -> foldl' accum Set.empty cs
    where
      accum ans (TxCertF _ cert) =
        if isInstantaneousRewards cert
          then Set.union ans (sufficientGenDelegs gendel)
          else ans
  TxCertConwayToConway -> foldl' accum Set.empty cs
    where
      accum ans (TxCertF _ _) = Set.union ans (sufficientGenDelegs gendel)

-- IS THIS RIGHT, DO WE need the  isInstantaneousRewards  TEST?

-- | Compute sufficient set of keys to make the scripts succeed.
--   The script map is the map of all 'needed' scripts. Some of those
--   scripts may NOT be in the 'scriptWits' (those which are reference scripts)
--   but all needed scripts will need to have their KeyHashes added.
sufficientScriptKeys ::
  Proof era ->
  Map (ScriptHash (EraCrypto era)) (ScriptF era) ->
  Set (KeyHash 'Witness (EraCrypto era))
sufficientScriptKeys proof scriptmap = Map.foldl' accum Set.empty scriptmap
  where
    accum ans (ScriptF _ s) = Set.union ans (sufficientScript proof s)

sufficientKeyHashes ::
  Reflect era =>
  Proof era ->
  Map (ScriptHash (EraCrypto era)) (ScriptF era) ->
  [TxCertF era] ->
  Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  Set (KeyHash 'Witness (EraCrypto era))
sufficientKeyHashes p scriptmap cs gendel =
  Set.union
    (sufficientScriptKeys p scriptmap)
    (sufficientTxCert cs gendel)

-- =======================================

pcUtxo :: Reflect era => Map (TxIn (EraCrypto era)) (TxOutF era) -> String
pcUtxo m = show (pcUtxoDoc m)

pcUtxoDoc :: Reflect era => Map (TxIn (EraCrypto era)) (TxOutF era) -> PDoc
pcUtxoDoc m = ppMap pcTxIn (\(TxOutF p o) -> pcTxOut p o) m

necessaryKeyHashTarget ::
  forall era.
  Reflect era =>
  Term era (TxBodyF era) ->
  Term era (Set (KeyHash 'Witness (EraCrypto era))) ->
  -- Target era (Set (WitVKey 'Witness (EraCrypto era)))
  Target era (Set (KeyHash 'Witness (EraCrypto era)))
necessaryKeyHashTarget txbodyparam reqSignersparam =
  ( Constr "keywits" necessaryKeyHashes
      ^$ txbodyparam
      ^$ (utxo reify)
      ^$ genDelegs
      --  ^$ keymapUniv
      ^$ reqSignersparam
  )

-- | Compute the needed key witnesses from a transaction body.
--   First find all the key hashes from every use of keys in the transaction
--   Then find the KeyPair's associated with those hashes, then
--   using the hash of the TxBody, turn the KeyPair into a witness.
--   In Eras Shelley to Mary, 'reqsigners' should be empty. In Eras Alonzo to Conway
--   we will need to add the witnesses for the required signer hashes, so they are
--   passed in. To compute the witnsses we need the hash of the TxBody. We will call this function
--   twice. Once when we have constructed the 'tempTxBody' used to estimate the fee, and a second time
--   with 'txBodyTerm' where the fee is correct.
--   The underlying function 'shelleyWitsVKeyNeeded' computes the necesary (but not sufficient)
--   key witnesses. The missing ones have to do with MultiSig (and Timelock) scripts and Mir
--   certificates (ones where 'isInstantaneousRewards' predicate is True). So we have to add these as well.
--   A MultiSig (Timelock) scripts needs witnesses for enough Signature scripts to make it True.
--   MIRCert needs enough witnesses from genDelegs to make the quorum constraint true.
necessaryKeyHashes ::
  forall era.
  Reflect era =>
  TxBodyF era ->
  Map (TxIn (EraCrypto era)) (TxOutF era) ->
  Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  Set (KeyHash 'Witness (EraCrypto era)) -> -- Only in Eras Alonzo To Conway,
  Set (KeyHash 'Witness (EraCrypto era))
necessaryKeyHashes (TxBodyF _ txb) u gd reqsigners =
  case reify @era of
    Shelley _ -> Set.union (shelleyWitsVKeyNeeded (liftUTxO u) txb (GenDelegs gd)) reqsigners
    Allegra _ -> Set.union (shelleyWitsVKeyNeeded (liftUTxO u) txb (GenDelegs gd)) reqsigners
    Mary _ -> Set.union (shelleyWitsVKeyNeeded (liftUTxO u) txb (GenDelegs gd)) reqsigners
    Alonzo _ -> Set.union (shelleyWitsVKeyNeeded (liftUTxO u) txb (GenDelegs gd)) reqsigners
    Babbage _ -> Set.union (shelleyWitsVKeyNeeded (liftUTxO u) txb (GenDelegs gd)) reqsigners
    Conway _ -> Set.union (witsVKeyNeededNoGov (liftUTxO u) txb) reqsigners

-- ========================================================

makeKeyWitnessTarget ::
  Reflect era =>
  Term era (TxBodyF era) ->
  Term era (Set (KeyHash 'Witness (EraCrypto era))) ->
  Term era (Set (KeyHash 'Witness (EraCrypto era))) ->
  Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era)) ->
  Term era (Map (KeyHash 'Payment (EraCrypto era)) ((Addr (EraCrypto era)), SigningKey)) ->
  Target era (Set (WitVKey 'Witness (EraCrypto era)))
makeKeyWitnessTarget txbparam necessary sufficient scripts byAdUniv =
  Constr "makeKeyWitness" makeKeyWitness ^$ txbparam ^$ necessary ^$ sufficient ^$ keymapUniv ^$ scripts ^$ genDelegs ^$ byAdUniv

makeKeyWitness ::
  forall era.
  Reflect era =>
  TxBodyF era ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era)) ->
  Map (ScriptHash (EraCrypto era)) (ScriptF era) ->
  Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  Map (KeyHash 'Payment (EraCrypto era)) ((Addr (EraCrypto era)), SigningKey) ->
  Set (WitVKey 'Witness (EraCrypto era))
makeKeyWitness (TxBodyF _ txb) necessary sufficient keyUniv scripts gendel byronAdUniv = keywits
  where
    bodyhash :: SafeHash (EraCrypto era) EraIndependentTxBody
    bodyhash = hashAnnotated txb
    keywits = Set.foldl' accum Set.empty (Set.union necessary sufficient)
      where
        accum ans hash = case Map.lookup hash keyUniv of
          Just keypair -> Set.insert (mkWitnessVKey bodyhash keypair) ans
          Nothing -> case Map.lookup (coerceKeyRole hash) byronAdUniv of
            Just (AddrBootstrap _, _) -> ans -- Bootstrap witnesses are handled in bootWitsT, so we can ignore them here.
            _ ->
              error
                ( "hash not in keymapUniv or byronAddrUniv"
                    ++ show hash
                    ++ "\n member necessary = "
                    ++ show (Set.member hash necessary)
                    ++ "\n member sufficient = "
                    ++ show (Set.member hash sufficient)
                    ++ "\n scripts = "
                    ++ show (ppMap pcScriptHash (\(ScriptF p s) -> pcScript p s) scripts)
                    ++ "\n genDelegs = "
                    ++ show (ppMap pcKeyHash pcGenDelegPair gendel)
                    ++ "\nbyronAddrUniv\n"
                    ++ format @era (MapR PayHashR (PairR AddrR SigningKeyR)) byronAdUniv
                )

-- ===============================================

allValid :: [IsValid] -> IsValid
allValid xs = IsValid (all valid xs)
  where
    valid (IsValid b) = b

scriptWitsLangs :: Map k (ScriptF era) -> Set Language
scriptWitsLangs m = Map.foldl' accum Set.empty m
  where
    accum :: Set Language -> ScriptF era -> Set Language
    accum ans (ScriptF (Alonzo _) (PlutusScript l)) = Set.insert (plutusLanguage l) ans
    accum ans (ScriptF (Babbage _) (PlutusScript l)) = Set.insert (plutusLanguage l) ans
    accum ans (ScriptF (Conway _) (PlutusScript l)) = Set.insert (plutusLanguage l) ans
    accum ans _ = ans

-- | Starting in the Babbage era, we can adjust the script witnesses by not supplying
--   those that appear as a reference script in the UTxO resolved 'spending' inputs.
--   {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
--   This function computes the exact set of hashes that must appear in the witnesses.
adjustNeededByRefScripts ::
  Proof era ->
  (Set (TxIn (EraCrypto era))) ->
  (Set (TxIn (EraCrypto era))) ->
  (Map (TxIn (EraCrypto era)) (TxOutF era)) ->
  (Set (ScriptHash (EraCrypto era))) ->
  (Set (ScriptHash (EraCrypto era)))
adjustNeededByRefScripts proof inps refinps ut neededhashes = case whichTxOut proof of
  TxOutShelleyToMary -> neededhashes
  TxOutAlonzoToAlonzo -> neededhashes
  TxOutBabbageToConway ->
    Set.difference
      neededhashes
      ( Set.union
          (Map.keysSet (refScripts inps (liftUTxO ut)))
          (Map.keysSet (refScripts refinps (liftUTxO ut)))
      )

-- ==============================================================

getUtxoCoinT ::
  Reflect era =>
  Term era (TxIn (EraCrypto era)) ->
  Term era (Map (TxIn (EraCrypto era)) (TxOutF era)) ->
  Target era Coin
getUtxoCoinT feeinput spending = Constr "getUtxoCoin" getUtxoCoin ^$ feeinput ^$ spending
  where
    getUtxoCoin input mp = case Map.lookup input mp of
      Just (TxOutF _ txout) -> txout ^. coinTxOutL
      Nothing -> error "feeinput not in spending"

getNTxOut :: (HasCallStack, Reflect era) => Size -> TxOutF era -> [TxOutF era] -> [TxOutF era]
getNTxOut (SzExact n) feeoutput outputuniv =
  (feeoutput & outputCoinL .~ (Coin 1000000)) : take (n - 1) outputuniv
getNTxOut x _ _ =
  error
    ( "Non (SzExact n): "
        ++ show x
        ++ " in getNTxOut. Use (MetaSize size x) to get a random (SzExact n)."
    )

-- | Compute the sum of all the Values in a List(Set,Map, ...) of TxOut
txoutSum :: forall era t. (Foldable t, Reflect era) => t (TxOutF era) -> Value era
txoutSum xs = foldl' accum mempty xs
  where
    accum ans (TxOutF _ txout) = txout ^. valueTxOutL <+> ans

minusMultiValue :: forall era. Reflect era => Proof era -> Value era -> Value era -> Map (ScriptHash (EraCrypto era)) (Map AssetName Integer)
minusMultiValue p v1 v2 = case whichValue p of
  ValueMaryToConway -> case v1 <-> v2 of MaryValue _ (MultiAsset m) -> Map.mapKeys (\(PolicyID x) -> x) m
  ValueShelleyToAllegra -> mempty

-- ==============================================================
-- Using constraints to generate a TxBody
-- ==============================================================

txBodyPreds :: forall era. (HasCallStack, Reflect era) => UnivSize -> Proof era -> [Pred era]
txBodyPreds sizes p =
  (txOutPreds sizes p balanceCoin (outputs p))
    ++ [ mint :<-: (Constr "sumAssets" (\out spend -> minusMultiValue p (txoutSum out) (txoutSum spend)) ^$ (outputs p) ^$ spending)
       , networkID :<-: justTarget network
       , -- inputs
         Sized (Range 2 10) inputs
       , Member (Left feeTxIn) inputs
       , Subset inputs (Dom (utxo p))
       , -- collateral
         Disjoint inputs collateral
       , Sized (Range 2 3) collateral
       , Subset collateral (Dom colUtxo)
       , --       , Member (Left colInput) collateral
         colRestriction :=: Restrict collateral colUtxo
       , SumsTo (Right (Coin 1)) sumCol EQL [ProjMap CoinR outputCoinL colRestriction]
       , tempTotalCol :=: sumCol
       , -- withdrawals
         Sized (Range 0 2) prewithdrawal
       , Subset prewithdrawal (Dom nonZeroRewards)
       , withdrawals
          :<-: ( Constr "mkRwrdAcnt" (\s r -> Map.fromList (map (\x -> (RewardAcnt Testnet x, r Map.! x)) (Set.toList s)))
                  ^$ prewithdrawal
                  ^$ rewards
               )
       , nonZeroRewards :<-: (Constr "filter (/=0)" (Map.filter (/= (Coin 0))) ^$ rewards)
       , -- refInputs
         Sized (Range 0 1) refInputs
       , Subset refInputs (Dom (utxo p))
       , Sized (Range 1 2) reqSignerHashes
       , Subset reqSignerHashes (Dom keymapUniv)
       , ttl :<-: (Constr "(+5)" (\x -> x + 5) ^$ currentSlot)
       , spending :=: Restrict inputs (utxo p)
       , SumsTo
          (Right (Coin 1))
          balanceCoin
          EQL
          [ProjMap CoinR outputCoinL spending, SumMap withdrawals, One txrefunds, One txdeposits]
       , txrefunds :<-: (Constr "certsRefunds" certsRefunds ^$ pparams p ^$ stakeDeposits ^$ certs)
       , txdeposits :<-: (Constr "certsDeposits" certsDeposits ^$ pparams p ^$ regPools ^$ certs)
       , scriptsNeeded :<-: (needT p ^$ tempTxBody ^$ (utxo p))
       , txisvalid :<-: (Constr "allValid" allValid ^$ valids)
       , Maybe txauxdata (Simple oneAuxdata) [Random oneAuxdata]
       , adHash :<-: (Constr "hashMaybe" (hashTxAuxDataF <$>) ^$ txauxdata)
       , -- Construct a temporary 'Tx' with a size close to the size of the Tx we want.
         -- We will use this to compute the 'txfee' using a fix-point approach.
         Random randomWppHash
       , tempWppHash :<-: justTarget randomWppHash
       , tempTxFee :<-: (constTarget (Coin (fromIntegral (maxBound :: Word64))))
       , tempTxBody :<-: txbodyTarget tempTxFee tempWppHash (Lit CoinR (Coin (fromIntegral (maxBound :: Word64))))
       , tempTx :<-: txTarget tempTxBody tempBootWits tempKeyWits
       , -- Compute the real fee, and then recompute the TxBody and the Tx
         txfee :<-: (Constr "finalFee" computeFinalFee ^$ (pparams p) ^$ tempTx)
         --  , txbodyterm :<-: txbodyTarget txfee wppHash totalCol
         --  , txterm :<-: txTarget txbodyterm bootWits keyWits
       ]
    ++ case whichUTxO p of
      UTxOShelleyToMary ->
        [ -- The following have no effect in ShelleyToMary, but they need to be defined. So we make them random
          Random redeemers
        , Random dataWits
        , Random (collateralReturn p)
        , Random extraCol
        , Random totalCol
        , Random wppHash
        , Restrict (Proj smNeededL (SetR ScriptHashR) scriptsNeeded) (allScriptUniv p) :=: scriptWits
        , Elems (ProjM fstL IsValidR (Restrict (Dom scriptWits) plutusUniv)) :=: valids
        , tempBootWits :<-: (Constr "boots" (bootWitsT p) ^$ spending ^$ tempTxBody ^$ byronAddrUniv)
        , necessaryHashes :<-: necessaryKeyHashTarget @era tempTxBody (Lit (SetR WitHashR) Set.empty)
        , sufficientHashes :<-: (Constr "sufficient" (sufficientKeyHashes p) ^$ scriptWits ^$ certs ^$ genDelegs)
        , tempKeyWits :<-: makeKeyWitnessTarget tempTxBody necessaryHashes sufficientHashes scriptWits byronAddrUniv
        , bootWits :<-: (Constr "boots" (bootWitsT p) ^$ spending ^$ txbodyterm ^$ byronAddrUniv)
        , keyWits :<-: makeKeyWitnessTarget txbodyterm necessaryHashes sufficientHashes scriptWits byronAddrUniv
        , txbodyterm :<-: txbodyTarget txfee tempWppHash (Lit CoinR (Coin 0)) -- The WppHash and totalCol play no role in ShelleyToMary
        , txterm :<-: txTarget txbodyterm bootWits keyWits
        ]
      UTxOAlonzoToConway ->
        [ Proj acNeededL (ListR (PairR (ScriptPurposeR p) ScriptHashR)) scriptsNeeded :=: acNeeded
        , -- Hashes of scripts that will be run. Not all of these will be in the 'scriptWits'
          neededHashSet :<-: (Constr "toSet" (\x -> Set.fromList (map snd x)) ^$ acNeeded)
        , -- Only the refAdjusted scripts need to be in the 'scriptWits' but if the refscripts that are
          -- excluded from 'scriptWits' are Timelock scripts, a sufficient set keys must be added to
          -- the sufficient key hashes.
          refAdjusted :<-: (Constr "adjust" (adjustNeededByRefScripts p) ^$ inputs ^$ refInputs ^$ (utxo p) ^$ neededHashSet)
        , -- , scriptWits :=: Restrict refAdjusted (allScriptUniv p)
          Restrict refAdjusted (allScriptUniv p) :=: scriptWits
        , Elems (ProjM fstL IsValidR (Restrict neededHashSet plutusUniv)) :=: valids
        , rdmrPtrs :<-: (rdmrPtrsT ^$ tempTxBody ^$ acNeeded ^$ plutusUniv)
        , rdmrPtrs :=: Dom redeemers
        , If
            (Constr "null" Set.null ^$ rdmrPtrs)
            (Before (maxTxExUnits p) redeemers)
            (SumsTo (Left (ExUnits 1 1)) (maxTxExUnits p) GTE [ProjMap ExUnitsR sndL redeemers])
        , plutusDataHashes
            :<-: ( Constr "plutusDataHashes" getPlutusDataHashes
                    ^$ utxo p
                    ^$ tempTxBody
                    ^$ allScriptUniv p
                 )
        , Restrict plutusDataHashes dataUniv :=: dataWits
        , tempBootWits :<-: (Constr "boots" (bootWitsT p) ^$ spending ^$ tempTxBody ^$ byronAddrUniv)
        , necessaryHashes :<-: necessaryKeyHashTarget @era tempTxBody reqSignerHashes
        , sufficientHashes
            :<-: ( Constr "sufficient" (sufficientKeyHashes p)
                    ^$ (Restrict neededHashSet (allScriptUniv p))
                    ^$ certs
                    ^$ genDelegs
                 )
        , tempKeyWits :<-: makeKeyWitnessTarget tempTxBody necessaryHashes sufficientHashes scriptWits byronAddrUniv
        , bootWits :<-: (Constr "boots" (bootWitsT p) ^$ spending ^$ txbodyterm ^$ byronAddrUniv)
        , keyWits :<-: makeKeyWitnessTarget txbodyterm necessaryHashes sufficientHashes scriptWits byronAddrUniv
        , -- 'langs' is not computed from the 'scriptWits' because that does not include needed
          -- scripts that are reference scripts, So get the scripts from the neededHashSet
          langs :<-: (Constr "languages" scriptWitsLangs ^$ (Restrict neededHashSet (allScriptUniv p)))
        , wppHash :<-: integrityHash p (pparams p) langs redeemers dataWits
        , owed
            :<-: ( Constr "owed" (\percent (Coin fee) -> rationalToCoinViaCeiling ((fromIntegral percent * fee) % 100))
                    ^$ (collateralPercentage p)
                    ^$ txfee
                 )
        , -- we need to add 'extraCol' to the colUtxo, to pay the collateral fee.
          -- we arrange this so adding the 'extraCol' will make the sum of the all the collateral inputs, one more than 'owed'
          extraCol :<-: (Constr "extraCol" (\(Coin suminputs) (Coin owe) -> (Coin (owe + 1 - suminputs))) ^$ sumCol ^$ owed)
        , totalCol :<-: (Constr "(<+>)" (\x y -> x <+> y <-> Coin 1) ^$ extraCol ^$ sumCol)
        , Member (Right colRetAddr) addrUniv
        , -- This ( collateralReturn) depends on the Coin in the TxOut being (Coin 1).
          -- The computation of 'owed' and 'extraCol" should ensure this.
          collateralReturn p :<-: (Constr "colReturn" (\ad -> TxOutF p (mkBasicTxOut ad (inject (Coin 1)))) ^$ colRetAddr)
        , -- We compute this, so that we can test that the (Coin 1) invariant holds.
          colRetCoin :<-: (Constr "-" (\sumc extra owe -> (sumc <> extra) <-> owe) ^$ sumCol ^$ extraCol ^$ owed)
        , txbodyterm :<-: txbodyTarget txfee wppHash totalCol
        , txterm :<-: txTarget txbodyterm bootWits keyWits
        ]
  where
    spending = Var (V "spending" (MapR TxInR (TxOutR p)) No)
    colUtxo = Var (V "colUtxo" (MapR TxInR (TxOutR p)) No)
    colRestriction = Var (V "colRestriction" (MapR TxInR (TxOutR p)) No)
    nonZeroRewards = var "nonZeroRewards" (MapR CredR CoinR)
    balanceCoin = Var (V "balanceCoin" CoinR No)
    certsRefunds (PParamsF _ pp) depositsx certsx = keyCertsRefunds pp (`Map.lookup` depositsx) (map unTxCertF certsx)
    certsDeposits (PParamsF _ pp) regpools certsx = Coin (-n)
      where
        (Coin n) = totalCertsDeposits pp (`Map.member` regpools) (map unTxCertF certsx)
    txrefunds = Var (pV p "txrefunds" CoinR No)
    txdeposits = Var (pV p "txdeposits" CoinR No)
    acNeeded :: Term era [(ScriptPurpose era, ScriptHash (EraCrypto era))]
    acNeeded = Var $ pV p "acNeeded" (ListR (PairR (ScriptPurposeR p) ScriptHashR)) No
    neededHashSet = Var $ pV p "neededHashSet" (SetR ScriptHashR) No
    rdmrPtrs = Var $ pV p "rdmrPtrs" (SetR RdmrPtrR) No
    plutusDataHashes = Var $ pV p "plutusDataHashes" (SetR DataHashR) No
    oneAuxdata = Var $ pV p "oneAuxdata" (TxAuxDataR reify) No
    tempTxFee = Var $ pV p "tempTxFee" CoinR No
    tempTx = Var $ pV p "tempTx" (TxR p) No
    tempTxBody = Var $ pV p "tempTxBody" (TxBodyR reify) No
    tempWppHash = Var $ pV p "tempWppHash" (MaybeR ScriptIntegrityHashR) No
    randomWppHash = Var $ pV p "randomWppHash" ScriptIntegrityHashR No
    tempBootWits = Var $ pV p "tempBootWits" (SetR (BootstrapWitnessR @era)) No
    tempKeyWits = Var $ pV p "tempKeyWits" (SetR (WitVKeyR reify)) No
    langs = Var $ pV p "langs" (SetR LanguageR) No
    tempTotalCol = Var $ pV p "tempTotalCol" CoinR No
    prewithdrawal = Var $ pV p "preWithdrawal" (SetR CredR) No
    refAdjusted = Var $ pV p "refAdjusted" (SetR ScriptHashR) No
    necessaryHashes = Var $ pV p "necessaryHashes" (SetR WitHashR) No
    sufficientHashes = Var $ pV p "sufficientHashes" (SetR WitHashR) No

txBodyStage ::
  Reflect era =>
  UnivSize ->
  Proof era ->
  Subst era ->
  Gen (Subst era)
txBodyStage sizes proof subst0 = do
  let preds = txBodyPreds sizes proof
  toolChainSub proof standardOrderInfo preds subst0

-- ===============================================================

balanceMap :: Ord k => [(k, Coin)] -> Map k t -> Lens' t Coin -> Map k t
balanceMap pairs m0 l = List.foldl' accum m0 pairs
  where
    accum m (k, thecoin) = Map.adjust (\t -> t & l .~ (thecoin <> t ^. l)) k m

-- | Adjust the Coin part of the TxOut in the 'utxo' map for the TxIn 'feeTxIn' by adding 'txfee'
adjustFeeInput :: (HasCallStack, Reflect era) => Env era -> Typed (Env era)
adjustFeeInput env = case utxo reify of
  u@(Var utxoV) -> do
    feeinput <- runTerm env feeTxIn
    feecoin <- runTerm env txfee
    utxomap <- runTerm env u
    pure (storeVar utxoV (balanceMap [(feeinput, feecoin)] utxomap outputCoinL) env)
  other -> failT ["utxo does not match a Var Term: " ++ show other]

-- | Adjust UTxO image of 'collateral' to pay for the collateral fees. Do this by
--   adding 'extraCol' to the TxOuts associated with col inputs
adjustColInput ::
  (HasCallStack, Reflect era) =>
  -- Term era (TxIn (EraCrypto era)) ->
  -- Term era Coin ->
  -- Term era Coin ->
  Env era ->
  Typed (Env era)
adjustColInput env = do
  let utxoterm = utxo reify
      utxoV = unVar utxoterm
  extracoin <- runTerm env extraCol
  utxomap <- runTerm env utxoterm
  col <- runTerm env collateral
  let newutxo = adjustC (Set.toList col) utxomap extracoin outputCoinL
      env2 = storeVar utxoV newutxo env
  (env5, body) <- updateTarget override txbodyterm (txbodyTarget txfee wppHash totalCol) env2
  (env6, _) <- updateTarget override txterm (txTarget (Lit (TxBodyR reify) body) bootWits keyWits) env5
  pure env6

-- | Adjust the part of the UTxO that maps the 'collateral' inputs, to pay the collateral fee.
--   This will adjust 1 or more of the TxOuts assoicated with the collateral , to make up the difference.
adjustC :: (Ord k, Show k) => [k] -> Map k v -> Coin -> Lens' v Coin -> Map k v
adjustC [] _ extra _ | extra < (Coin 0) = error ("Extra is too negative to adjust Utxo: " ++ show extra)
adjustC [] m _ _ = m
adjustC (i : is) m extra@(Coin n) coinL = case compare n 0 of
  EQ -> m
  GT -> Map.adjust addextra i m
    where
      addextra outf = outf & coinL .~ ((outf ^. coinL) <+> extra)
  LT ->
    if ex >= 0
      then Map.adjust subextra i m
      else adjustC is (Map.adjust subextra i m) amount coinL
    where
      amount@(Coin ex) = case Map.lookup i m of
        Just outf -> (outf ^. coinL) <+> extra <-> (Coin 1)
        Nothing -> error ("Collateral input: " ++ show i ++ " is not found in UTxO in 'adjust'")
      subextra outf = outf & coinL .~ (Coin (max 1 (unCoin ((outf ^. coinL) <+> extra))))

updateVal :: (a -> b -> a) -> Term era a -> b -> Env era -> Typed (Env era)
updateVal adjust term@(Var v) delta env = do
  varV <- runTerm env term
  pure $ storeVar v (adjust varV delta) env
updateVal _ v _ _ = failT ["Non Var in updateVal: " ++ show v]

updateTerm :: (a -> b -> a) -> Term era a -> Term era b -> Env era -> Typed (Env era)
updateTerm adjust term@(Var v) delta env = do
  varV <- runTerm env term
  deltaV <- runTerm env delta
  pure $ storeVar v (adjust varV deltaV) env
updateTerm _ v _ _ = failT ["Non Var in updateTerm: " ++ show v]

updateTarget :: (a -> b -> a) -> Term era a -> Target era b -> Env era -> Typed (Env era, b)
updateTarget adjust term@(Var v) delta env = do
  varV <- runTerm env term
  deltaV <- runTarget env delta
  pure $ (storeVar v (adjust varV deltaV) env, deltaV)
updateTarget _ v _ _ = failT ["Non Var in updateTarget: " ++ show v]

override :: x -> x -> x
override _ y = y

-- ========================================

genTxAndLedger :: Reflect era => UnivSize -> Proof era -> Gen (LedgerState era, Tx era, Env era)
genTxAndLedger sizes proof = do
  subst <-
    ( pure emptySubst
        >>= pParamsStage proof
        >>= universeStage sizes proof
        >>= vstateStage proof
        >>= pstateStage proof
        >>= dstateStage proof
        >>= certsStage proof
        >>= ledgerStateStage sizes proof
        >>= txBodyStage sizes proof
      )
  env0 <- monadTyped $ substToEnv subst emptyEnv
  env1 <- monadTyped $ adjustFeeInput env0
  env2 <- monadTyped $ adjustColInput env1
  ledger <- monadTyped $ runTarget env2 (ledgerStateT proof)
  (TxF _ tx) <- monadTyped (findVar (unVar txterm) env2)
  pure (ledger, tx, env2)

gone :: Gen (IO ())
gone = do
  txIx <- arbitrary
  let proof = Babbage Standard
      sizes = def
  (ledgerstate, tx, env) <- genTxAndLedger sizes proof
  slot <- monadTyped (findVar (unVar currentSlot) env)
  (PParamsF _ pp) <- monadTyped (findVar (unVar (pparams proof)) env)
  accntState <- monadTyped (runTarget env accountStateT)
  utxo1 <- monadTyped (findVar (unVar (utxo proof)) env)
  let lenv = LedgerEnv slot txIx pp accntState
  -- putStrLn (show (pcTx (Babbage Standard) tx))
  pure $ case applySTSByProof proof (TRC (lenv, ledgerstate, tx)) of
    Right _ledgerState' -> do
      -- putStrLn (show (pcTx (Babbage Standard) tx))
      putStrLn "SUCCESS"
    Left errs -> do
      putStrLn "FAIL"
      putStrLn (show (pgenTx proof utxo1 tx))
      putStrLn (show (ppList prettyA errs))
      goRepl (Babbage Standard) env ""

test :: Maybe Int -> IO ()
test (Just seed) = do x <- generateWithSeed seed gone; x
test Nothing = do
  seed <- generate arbitrary
  putStrLn ("SEED = " ++ show seed)
  x <- generateWithSeed seed gone
  x

bad :: [Int]
bad = [] -- [3, 8, 13, 41, 50, 60, 65, 82, 99, 100, 109, 112]
go :: Int -> IO ()
go n = sequence_ [print i >> test (Just i) | i <- [n .. 113], not (elem i bad)]

-- ========================================

-- ==============================

-- | Pretty print the fields of a Randomly generated TxBody, except resolve the
--   inputs with the given Ut map.
pgenTxBodyField ::
  Reflect era =>
  Proof era ->
  Map (TxIn (EraCrypto era)) (TxOutF era) ->
  TxBodyField era ->
  [(Text, PDoc)]
pgenTxBodyField proof ut x = case x of
  Inputs s -> [(pack "spend inputs", pcUtxoDoc (Map.restrictKeys ut s))]
  Collateral s -> [(pack "coll inputs", pcUtxoDoc (Map.restrictKeys ut s))]
  RefInputs s -> [(pack "ref inputs", pcUtxoDoc (Map.restrictKeys ut s))]
  other -> pcTxBodyField proof other

pgenTxBody :: Reflect era => Proof era -> TxBody era -> Map (TxIn (EraCrypto era)) (TxOutF era) -> PDoc
pgenTxBody proof txBody ut = ppRecord (pack "TxBody " <> pack (show proof)) pairs
  where
    fields = abstractTxBody proof txBody
    pairs = concatMap (pgenTxBodyField proof ut) fields

pgenTxField ::
  forall era.
  Reflect era =>
  Proof era ->
  Map (TxIn (EraCrypto era)) (TxOutF era) ->
  TxField era ->
  [(Text, PDoc)]
pgenTxField proof ut x = case x of
  Body b -> [(pack "txbody hash", ppSafeHash (hashAnnotated b)), (pack "body", pgenTxBody proof b ut)]
  BodyI xs -> [(pack "body", ppRecord (pack "TxBody") (concat (map (pgenTxBodyField proof ut) xs)))]
  _other -> pcTxField proof x

pgenTx :: Reflect era => Proof era -> Map (TxIn (EraCrypto era)) (TxOutF era) -> Tx era -> PDoc
pgenTx proof ut tx = ppRecord (pack "Tx") pairs
  where
    fields = abstractTx proof tx
    pairs = concatMap (pgenTxField proof ut) fields

-- =================================================
-- Demos and Tests

oneTest ::
  ( Reflect era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  UnivSize ->
  Proof era ->
  Gen Property
oneTest sizes proof = do
  subst <-
    ( pure emptySubst
        >>= pParamsStage proof
        >>= universeStage sizes proof
        >>= vstateStage proof
        >>= pstateStage proof
        >>= dstateStage proof
        >>= certsStage proof
        >>= ledgerStateStage sizes proof
        >>= txBodyStage sizes proof
      )
  env0 <- monadTyped $ substToEnv subst emptyEnv
  env1 <- monadTyped $ adjustFeeInput env0
  env2 <- monadTyped $ adjustColInput env1
  ledgerstate <- monadTyped $ runTarget env2 (ledgerStateT proof)
  (TxF _ tx) <- monadTyped (runTerm env2 txterm)

  slot <- monadTyped (runTerm env2 currentSlot)
  (PParamsF _ pp) <- monadTyped (runTerm env2 (pparams proof))
  accntState <- monadTyped (runTarget env2 accountStateT)
  txIx <- arbitrary
  let lenv = LedgerEnv slot txIx pp accntState
  case applySTSByProof proof (TRC (lenv, ledgerstate, tx)) of
    Right ledgerState' -> pure (totalAda ledgerState' === totalAda ledgerstate)
    Left errs ->
      pure
        ( whenFail
            (putStrLn ("\napplySTS fails\n" ++ unlines (map show errs)) >> (goRepl proof env2 ""))
            (counterexample ("\napplySTS fails\n" ++ unlines (map show errs)) (True === False))
        )

main1 :: IO ()
main1 = quickCheck (withMaxSuccess 30 (oneTest def (Babbage Standard)))

main2 :: IO ()
main2 = quickCheck (withMaxSuccess 30 (oneTest def (Shelley Standard)))

demo :: ReplMode -> IO ()
demo mode = do
  let proof = Babbage Standard
  -- Conway Standard
  -- Alonzo Standard
  -- Mary Standard
  -- Shelley Standard
  let sizes = def
  subst <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage sizes proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= certsStage proof
          >>= ledgerStateStage sizes proof
          >>= txBodyStage sizes proof
      )
  -- rewritten <- snd <$> generate (rewriteGen (1, txBodyPreds sizes proof))
  -- putStrLn (show rewritten)
  (_, status) <- monadTyped $ checkForSoundness (txBodyPreds sizes proof) subst
  case status of
    Nothing -> pure ()
    Just msg -> error msg
  env0 <- monadTyped $ substToEnv subst emptyEnv
  env1 <- monadTyped $ adjustFeeInput env0
  env2 <- monadTyped $ adjustColInput env1
  when (mode == Interactive) $ displayTerm env2 txfee
  when (mode == Interactive) $ displayTerm env2 txterm
  -- compute Produced and Consumed
  (TxBodyF _ txb) <- monadTyped (findVar (unVar txbodyterm) env2)
  certState <- monadTyped $ runTarget env1 certstateT
  (PParamsF _ ppV) <- monadTyped (findVar (unVar (pparams proof)) env2)
  utxoV <- monadTyped (findVar (unVar (utxo proof)) env2)
  when (mode == Interactive) $ putStrLn (show (producedTxBody txb ppV certState))
  when (mode == Interactive) $ putStrLn (show (consumedTxBody txb ppV certState (liftUTxO utxoV)))
  modeRepl mode proof env2 ""

demoTest :: TestTree
demoTest =
  testGroup
    "Tests for Tx Stage"
    [ testIO "Testing Tx Stage" (demo CI)
    , testProperty "One Tx Test Babbage" $ withMaxSuccess 30 (oneTest def (Babbage Standard))
    , testProperty "One Tx Test Shelley" $ withMaxSuccess 30 (oneTest def (Shelley Standard))
    ]

main :: IO ()
main = defaultMain $ testIO "Testing Tx Stage" (demo Interactive)
