{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Constrained.Preds.Universes
where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Signing as Byron
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..), bootstrapKeyHash)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Scripts (Tag (..))
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..), Datum (..), dataToBinaryData, hashData)
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (
  Network (..),
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
  mkCertIxPartial,
 )
import qualified Cardano.Ledger.BaseTypes as Utils (Globals (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  EraScript,
  EraTxOut (..),
  TxOut,
  Value,
  hashScript,
  isNativeScript,
 )
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto, DSIGN)
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Hashes (DataHash, EraIndependentTxBody, ScriptHash)
import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..), coerceKeyRole, hashKey)
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness, makeBootstrapWitness)
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
  multiAssetFromList,
 )
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Val (Val (inject))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes hiding (genTxOut)
import Test.Cardano.Ledger.Constrained.Combinators (genFromMap, itemFromSet, setSized)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.Repl (goRepl)
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Scripts (allPlutusScripts, genCoreScript, spendPlutusScripts)
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo)
import qualified Test.Cardano.Ledger.Shelley.Utils as Utils
import Test.Tasty.QuickCheck

-- ============================================================
-- Coins

variedCoin :: Gen Coin
variedCoin =
  Coin
    <$> frequency
      [ (12, pure 0)
      , (5, choose (1, 10))
      , (10, choose (11, 100))
      , (8, choose (101, 1000))
      , (7, choose (1001, 10000))
      , (2, choose (10001, 100000))
      ]

noZeroCoin :: Gen Coin
noZeroCoin =
  Coin
    <$> frequency
      [ (5, choose (1, 10))
      , (10, choose (11, 100))
      , (8, choose (101, 1000))
      , (7, choose (1001, 10000))
      , (2, choose (10001, 100000))
      ]

-- ===============================================
-- Generating Byron address and their universe

-- | Generate a pair, A Byron address, and the key that can sign it.
genAddrPair :: Network -> Gen (BootstrapAddress c, Byron.SigningKey)
genAddrPair netwrk = do
  signkey <- genSigningKey
  let verificationKey = Byron.toVerification signkey
      asd = Byron.VerKeyASD verificationKey
      byronNetwork = case netwrk of
        Mainnet -> Byron.NetworkMainOrStage
        Testnet -> Byron.NetworkTestnet 0
      attrs =
        Byron.AddrAttributes
          (Just (Byron.HDAddressPayload "a compressed lenna.png"))
          byronNetwork
  pure (BootstrapAddress (Byron.makeAddress asd attrs), signkey)

-- | Generate a Map, that maps the Hash of a Byron address to a pair of
--   the actual Byron address and the key that can sign it.
genByronUniv :: Crypto c => Network -> Gen (Map (KeyHash 'Payment c) (Addr c, Byron.SigningKey))
genByronUniv netwrk = do
  list <- vectorOf 50 (genAddrPair netwrk)
  pure $ Map.fromList (List.map (\(addr, signkey) -> (bootstrapKeyHash addr, (AddrBootstrap addr, signkey))) list)

-- | Given a list of Byron addresses, compute BootStrap witnesses of all of those addresses
--   Can only be used with StandardCrypto
bootWitness ::
  (Crypto c, DSIGN c ~ DSIGN.Ed25519DSIGN) =>
  Hash c EraIndependentTxBody ->
  [BootstrapAddress c] ->
  Map (KeyHash 'Payment c) (Addr c, Byron.SigningKey) ->
  Set (BootstrapWitness c)
bootWitness hash bootaddrs byronuniv = List.foldl' accum Set.empty bootaddrs
  where
    accum ans bootaddr@(BootstrapAddress a) = case Map.lookup (bootstrapKeyHash bootaddr) byronuniv of
      Just (AddrBootstrap _, signkey) ->
        Set.insert (makeBootstrapWitness hash signkey (Byron.addrAttributes a)) ans
      _ -> ans

-- ==================
-- Datums

-- | The universe of non-empty Datums. i.e. There are no NoDatum Datums in this list
genDatums :: Era era => Int -> Map (DataHash (EraCrypto era)) (Data era) -> Gen [Datum era]
genDatums n datauniv = vectorOf n (genDatum datauniv)

-- | Only generate non-empty Datums. I.e. There are no NoDatum Datums generated.
genDatum :: Era era => Map (DataHash (EraCrypto era)) (Data era) -> Gen (Datum era)
genDatum datauniv =
  oneof
    [ DatumHash . fst <$> genFromMap ["from genDatums DatumHash case"] datauniv
    , Datum . dataToBinaryData . snd <$> genFromMap ["from genDatums Datum case"] datauniv
    ]

-- ==============
-- Values

genValueF :: Proof era -> Coin -> Map (ScriptHash (EraCrypto era)) (ScriptF era) -> Gen (Value era)
genValueF p (Coin c) scripts = case whichValue p of
  ValueShelleyToAllegra -> pure (Coin c)
  ValueMaryToConway -> MaryValue c <$> multiAsset scripts

-- ==============
-- TxOuts

genTxOut ::
  Reflect era =>
  Proof era ->
  Coin ->
  Set (Addr (EraCrypto era)) ->
  Map (ScriptHash (EraCrypto era)) (ScriptF era) ->
  Map (ScriptHash (EraCrypto era)) (ScriptF era) ->
  Map (DataHash (EraCrypto era)) (Data era) ->
  Gen (TxOut era)
genTxOut p c addruniv scriptuniv spendscriptuniv datauniv =
  case whichTxOut p of
    TxOutShelleyToMary ->
      ShelleyTxOut <$> pick1 ["genTxOut ShelleyToMary Addr"] addruniv <*> genValueF p c scriptuniv
    TxOutAlonzoToAlonzo -> do
      addr <- pick1 ["genTxOut AlonzoToAlonzo Addr"] addruniv
      v <- genValueF p c scriptuniv
      case addr of
        AddrBootstrap _ -> pure (AlonzoTxOut addr v SNothing)
        Addr _ paycred _ ->
          if needsDatum paycred spendscriptuniv
            then
              AlonzoTxOut addr v . SJust . fst
                <$> genFromMap ["from genTxOut, AlonzoToAlonzo, needsDatum case"] datauniv
            else pure (AlonzoTxOut addr v SNothing)
    TxOutBabbageToConway -> do
      addr <- pick1 ["genTxOut BabbageToConway Addr"] addruniv
      v <- genValueF p c scriptuniv
      (ScriptF _ refscript) <- snd <$> genFromMap ["genTxOut, BabbageToConway, refscript case"] scriptuniv
      maybescript <- elements [SNothing, SJust refscript]
      case addr of
        AddrBootstrap _ -> pure $ BabbageTxOut addr v NoDatum maybescript
        Addr _ paycred _ ->
          if needsDatum paycred spendscriptuniv
            then BabbageTxOut addr v <$> genDatum datauniv <*> pure maybescript
            else pure $ BabbageTxOut addr v NoDatum maybescript

needsDatum :: EraScript era => Credential 'Payment (EraCrypto era) -> Map (ScriptHash (EraCrypto era)) (ScriptF era) -> Bool
needsDatum (ScriptHashObj hash) spendScriptUniv = case Map.lookup hash spendScriptUniv of
  Nothing -> False
  Just (ScriptF _ script) -> not (isNativeScript script)
needsDatum _ _ = False

genTxOuts ::
  Reflect era =>
  Proof era ->
  Set (Addr (EraCrypto era)) ->
  Map (ScriptHash (EraCrypto era)) (ScriptF era) ->
  Map (ScriptHash (EraCrypto era)) (ScriptF era) ->
  Map (DataHash (EraCrypto era)) (Data era) ->
  Gen [TxOutF era]
genTxOuts p addruniv scriptuniv spendscriptuniv datauniv = do
  let genOne = do
        c <- noZeroCoin
        genTxOut p c addruniv scriptuniv spendscriptuniv datauniv
  vectorOf 30 (TxOutF p <$> genOne)

-- ==================================================================
-- MultiAssets

assets :: Set AssetName
assets = Set.fromList [AssetName (fromString (show (n :: Int) ++ "Asset")) | n <- [0 .. 9]]

genMultiAssetTriple ::
  Map.Map (ScriptHash (EraCrypto era)) (ScriptF era) ->
  Set AssetName ->
  Gen Integer ->
  Gen (PolicyID (EraCrypto era), AssetName, Integer)
genMultiAssetTriple scriptMap assetSet genAmount =
  (,,)
    <$> (PolicyID . fst <$> (genFromMap [] scriptMap))
    <*> (fst <$> (itemFromSet [] assetSet))
    <*> genAmount

multiAsset :: Map.Map (ScriptHash (EraCrypto era)) (ScriptF era) -> Gen (MultiAsset (EraCrypto era))
multiAsset scripts = do
  n <- elements [0, 1, 2]
  if n == 0
    then pure mempty -- About 1/3 of the list will be the empty MA
    else do
      -- So lots of duplicates, but we want to choose the empty MA, 1/3 of the time.
      xs <- vectorOf n (genMultiAssetTriple scripts assets (choose (1, 100)))
      pure $ multiAssetFromList xs

-- ===================================================================
-- Helper functions in the Gen monad.

pick1 :: [String] -> Set t -> Gen t
pick1 msgs s = fst <$> itemFromSet ("from pick1" : msgs) s

makeHashScriptMap ::
  Reflect era =>
  Proof era ->
  Int ->
  Scripts.Tag ->
  Map
    (KeyHash 'Witness (EraCrypto era))
    (KeyPair 'Witness (EraCrypto era)) ->
  ValidityInterval ->
  Gen (Map (ScriptHash (EraCrypto era)) (ScriptF era))
makeHashScriptMap p size tag m vi = do
  let genOne Scripts.Spend =
        -- Make an effort to get as many plutus scripts as possible
        oneof
          [ (snd . snd) <$> genFromMap [] (spendPlutusScripts p)
          , genCoreScript p Scripts.Spend m vi
          ]
      genOne t = genCoreScript p t m vi
  scs <- vectorOf size (genOne tag)
  pure $ Map.fromList $ map (\x -> (hashScript x, ScriptF p x)) scs

genDataWits ::
  Era era =>
  Proof era ->
  Int ->
  Gen (Map (DataHash (EraCrypto era)) (Data era))
genDataWits _p size = do
  scs <- vectorOf size arbitrary
  pure $ Map.fromList $ map (\x -> (hashData x, x)) scs

--  This universe must not use Byron Addresses in Babbage and Conway, as Byron Addresses
--  do not play well with plutusScripts in those eras.
genAddrWith ::
  Proof era ->
  Network ->
  Set (Credential 'Payment (EraCrypto era)) ->
  Set Ptr ->
  Set (Credential 'Staking (EraCrypto era)) ->
  Map (KeyHash 'Payment (EraCrypto era)) (Addr (EraCrypto era), Byron.SigningKey) -> -- The Byron Addresss Universe
  Gen (Addr (EraCrypto era))
genAddrWith proof net ps ptrss cs byronMap =
  case whichTxOut proof of
    TxOutBabbageToConway -> Addr net <$> pick1 ["from genPayCred ScriptHashObj"] ps <*> genStakeRefWith ptrss cs
    _ ->
      frequency
        [ (8, Addr net <$> pick1 ["from genPayCred ScriptHashObj"] ps <*> genStakeRefWith ptrss cs)
        , (2, fst . snd <$> genFromMap ["from byronAddrUniv"] byronMap) -- This generates a known Byron Address
        ]

genPtr :: SlotNo -> Gen Ptr
genPtr (SlotNo n) =
  Ptr
    <$> (SlotNo <$> choose (0, n))
    <*> (TxIx <$> choose (0, 10))
    <*> (mkCertIxPartial <$> choose (1, 20))

genStakeRefWith :: Set Ptr -> Set (Credential 'Staking c) -> Gen (StakeReference c)
genStakeRefWith ps cs =
  frequency
    [ (80, StakeRefBase <$> pick1 ["from genStakeRefWith StakeRefBase"] cs)
    , (5, StakeRefPtr <$> pick1 ["from genStakeRefWith StakeRefPtr"] ps)
    , (15, pure StakeRefNull)
    ]

noScripts :: Proof era -> Addr (EraCrypto era) -> Bool
noScripts _ (Addr _ (ScriptHashObj _) _) = False
noScripts _ (Addr _ _ (StakeRefBase (ScriptHashObj _))) = False
noScripts _ (AddrBootstrap _) = False
noScripts _ _ = True

-- ======================================================================
-- Reusable Targets. First order representations of functions for use in
-- building 'Target's. We will apply these to Term variables,
-- (using  (:$) and (^$)) to indicate how to construct a random values assigned
-- to those variables. By convention we name these "functional" targets by
-- post-fixing their names with a captial "T". These may be a bit more
-- prescriptive rather than descriptive, but you do what you have to do.

txOutT :: Reflect era => Proof era -> Addr (EraCrypto era) -> Coin -> TxOutF era
txOutT p x c = TxOutF p (mkBasicTxOut x (inject c))

-- | The collateral consists only of VKey addresses
--   and the collateral outputs in the UTxO do not contain any non-ADA part
colTxOutT :: EraTxOut era => Proof era -> Set (Addr (EraCrypto era)) -> Gen (TxOutF era)
colTxOutT p noScriptAddr = TxOutF p <$> (mkBasicTxOut <$> pick1 [] noScriptAddr <*> (inject <$> noZeroCoin))

scriptHashObjT :: Term era (ScriptHash (EraCrypto era)) -> Target era (Credential k (EraCrypto era))
scriptHashObjT x = Constr "ScriptHashObj" ScriptHashObj ^$ x

keyHashObjT :: Term era (KeyHash 'Witness (EraCrypto era)) -> Target era (Credential k (EraCrypto era))
keyHashObjT x = Constr "KeyHashObj" (KeyHashObj . coerceKeyRole) ^$ x

makeValidityT :: Term era SlotNo -> Term era SlotNo -> Term era SlotNo -> Target era ValidityInterval
makeValidityT begin current end =
  Constr
    "(-i)x(+j)"
    (\beginD x endD -> ValidityInterval (SJust (x - beginD)) (SJust (x + endD)))
    ^$ begin
    ^$ current
    ^$ end

ptrUnivT :: Term era SlotNo -> Target era (Gen (Set Ptr))
ptrUnivT x = Constr "" (setSized ["From init ptruniv"] 30) :$ (Constr "" genPtr ^$ x)

addrUnivT ::
  Proof era ->
  Term era Network ->
  Term era (Set (Credential 'Payment (EraCrypto era))) ->
  Term era (Set Ptr) ->
  Term era (Set (Credential 'Staking (EraCrypto era))) ->
  Term era (Map (KeyHash 'Payment (EraCrypto era)) (Addr (EraCrypto era), Byron.SigningKey)) ->
  Target era (Gen (Set (Addr (EraCrypto era))))
addrUnivT p net ps pts cs byronAddrUnivT =
  Constr "" (setSized ["From addrUnivT"] 30)
    :$ (Constr "genAddrWith" (genAddrWith p) ^$ net ^$ ps ^$ pts ^$ cs ^$ byronAddrUnivT)

makeHashScriptMapT ::
  Proof era ->
  Int ->
  Scripts.Tag ->
  Term era (Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))) ->
  Term era ValidityInterval ->
  Target era (Gen (Map (ScriptHash (EraCrypto era)) (ScriptF era)))
makeHashScriptMapT p size tag m vi =
  Constr
    "makeHashScriptMap"
    (unReflect makeHashScriptMap p size tag)
    ^$ m
    ^$ vi

cast :: forall c k. Set (KeyHash 'Witness c) -> Set (KeyHash k c)
cast x = Set.map (\kh -> coerceKeyRole @KeyHash @'Witness kh) x

-- =================================================================
-- Using constraints to generate the Universes

universePreds :: Reflect era => Proof era -> [Pred era]
universePreds p =
  [ Sized (Range 100 500) currentSlot
  , Sized (Range 0 30) beginSlotDelta
  , Sized (Range 5 35) endSlotDelta
  , Sized (ExactSize 50) keypairs
  , keymapUniv :<-: (Constr "xx" (\s -> Map.fromList (map (\x -> (hashKey (vKey x), x)) s)) ^$ keypairs)
  , Sized (ExactSize 40) prePoolUniv
  , Subset prePoolUniv (Dom keymapUniv)
  , poolHashUniv :<-: (Constr "WitnessToStakePool" cast ^$ prePoolUniv)
  , Sized (ExactSize 10) preStakeUniv
  , Subset preStakeUniv (Dom keymapUniv)
  , stakeHashUniv :<-: (Constr "WitnessToStaking" cast ^$ preStakeUniv)
  , Sized (ExactSize 20) preGenesisUniv
  , Subset preGenesisUniv (Dom keymapUniv)
  , preGenesisDom :<-: (Constr "WitnessToGenesis" cast ^$ preGenesisUniv)
  , preGenesisDom :=: (Dom genesisHashUniv)
  , Sized (ExactSize 40) preVoteUniv
  , Subset preVoteUniv (Dom keymapUniv)
  , voteHashUniv :<-: (Constr "WitnessToStakePool" cast ^$ preVoteUniv)
  , Sized (ExactSize 40) txinUniv
  , Member (Right feeTxIn) txinUniv
  , validityInterval :<-: makeValidityT beginSlotDelta currentSlot endSlotDelta
  , Choose
      (ExactSize 30)
      credList
      [ (1, scriptHashObjT scripthash, [Member (Left scripthash) (Dom (nonSpendScriptUniv p))])
      , (1, keyHashObjT keyhash, [Member (Left keyhash) (Dom keymapUniv)])
      ]
  , credsUniv :<-: listToSetTarget credList
  , GenFrom (spendscriptUniv p) (makeHashScriptMapT p 25 Scripts.Spend keymapUniv validityInterval)
  , GenFrom (nonSpendScriptUniv p) (makeHashScriptMapT p 25 Scripts.Cert keymapUniv validityInterval)
  , allScriptUniv p :<-: (Constr "union" Map.union ^$ (spendscriptUniv p) ^$ (nonSpendScriptUniv p))
  , Choose
      (ExactSize 50)
      spendcredList
      [ (1, scriptHashObjT scripthash, [Member (Left scripthash) (Dom (spendscriptUniv p))])
      , (1, scriptHashObjT scripthash, [Member (Left scripthash) (Dom (spendscriptUniv p))])
      , (1, scriptHashObjT scripthash, [Member (Left scripthash) (Dom (spendscriptUniv p))])
      , (1, scriptHashObjT scripthash, [Member (Left scripthash) (Dom (spendscriptUniv p))])
      , (1, keyHashObjT keyhash, [Member (Left keyhash) (Dom keymapUniv)])
      ]
  , spendCredsUniv :<-: listToSetTarget spendcredList
  , currentEpoch :<-: (Constr "epochFromSlotNo" epochFromSlotNo ^$ currentSlot)
  , GenFrom dataUniv (Constr "dataWits" (genDataWits p) ^$ (Lit IntR 30))
  , GenFrom datumsUniv (Constr "genDatums" (genDatums 30) ^$ dataUniv)
  , -- 'network' is set by testGlobals which contains 'Testnet'
    network :<-: constTarget (Utils.networkId Utils.testGlobals)
  , GenFrom ptrUniv (ptrUnivT currentSlot)
  , GenFrom byronAddrUniv (Constr "byronUniv" genByronUniv ^$ network)
  , GenFrom addrUniv (addrUnivT p network spendCredsUniv ptrUniv credsUniv byronAddrUniv)
  , GenFrom multiAssetUniv (Constr "multiAsset" (vectorOf 50 . multiAsset) ^$ (nonSpendScriptUniv p))
  , GenFrom
      preTxoutUniv
      ( Constr "genTxOuts" (genTxOuts p)
          ^$ addrUniv
          ^$ (nonSpendScriptUniv p)
          ^$ (spendscriptUniv p)
          ^$ dataUniv
      )
  , GenFrom
      (colTxoutUniv p)
      ( Constr
          "colTxOutUniv"
          (\x -> setSized ["From init colTxoutUniv"] 20 (colTxOutT p (Set.filter (noScripts p) x)))
          ^$ addrUniv
      )
  , payUniv :=: spendCredsUniv
  , voteUniv :<-: (Constr "coerce" (Set.map stakeToVote) ^$ credsUniv)
  , bigCoin :<-: constTarget (Coin 2000000)
  , GenFrom
      feeTxOut
      ( Constr
          "txout"
          ( \a c ->
              txOutT p
                <$> pick1 [] (Set.filter (noScripts p) a)
                <*> pure c
          )
          ^$ addrUniv
          ^$ bigCoin
      )
  , txoutUniv p
      :<-: ( Constr
              "insert"
              (\x y _z -> Set.insert x {- (Set.union z -} (Set.fromList y)) -- )
              ^$ feeTxOut
              ^$ preTxoutUniv
              ^$ (colTxoutUniv p)
           )
  , plutusUniv :<-: constTarget (Map.map (\(x, y) -> (x, ScriptF p y)) (allPlutusScripts p))
  , spendPlutusUniv :<-: constTarget (Map.map (\(x, y) -> (x, ScriptF p y)) (spendPlutusScripts p))
  ]
  where
    credList = Var (V "credList" (ListR CredR) No)
    spendcredList = Var (V "spendcred.list" (ListR PCredR) No)
    keyhash = Var (V "keyhash" WitHashR No)
    scripthash = Var (V "scripthash" ScriptHashR No)
    preTxoutUniv = Var (V "preTxoutUniv" (ListR (TxOutR p)) No)
    keypairs = Var (V "keypairs" (ListR KeyPairR) No)
    prePoolUniv = Var (V "prePoolUniv" (SetR WitHashR) No)
    preStakeUniv = Var (V "preStakeUniv" (SetR WitHashR) No)
    preGenesisUniv = Var (V "preGenesisUniv" (SetR WitHashR) No)
    preGenesisDom = Var (V "preGenesisDom" (SetR GenHashR) No)
    preVoteUniv = Var (V "prePoolUniv" (SetR WitHashR) No)

stakeToVote :: Credential 'Staking c -> Credential 'Voting c
stakeToVote = coerceKeyRole

solveUniv :: Reflect era => Proof era -> Gen (Subst era)
solveUniv proof = do
  toolChainSub proof standardOrderInfo (universePreds proof) emptySubst

universeStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
universeStage proof = toolChainSub proof standardOrderInfo (universePreds proof)

main :: IO ()
main = do
  let proof = Babbage Standard
  subst <- generate (universeStage proof emptySubst)
  putStrLn "\n"
  putStrLn (show subst)
  env <- monadTyped (substToEnv subst emptyEnv)
  goRepl proof env ""
