{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Constrained.Preds.Universes
where

import Cardano.Ledger.Crypto (Crypto)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (monadTyped)
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (Size (..))
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Proof
import Test.Tasty.QuickCheck

import Cardano.Ledger.Address (Addr (..))
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo)

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Scripts (Tag (..))
import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  Network (..),
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
  mkCertIxPartial,
 )
import Cardano.Ledger.Core (EraScript (..), hashScript)

-- import Cardano.Ledger.Credential (Credential (..))

import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), coerceKeyRole)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Constrained.Combinators (genFromMap, itemFromSet, mapSized, setSized)
import Test.Cardano.Ledger.Constrained.Scripts (genCoreScript)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))

-- import Cardano.Ledger.TxIn (TxId(..),TxIn)

-- ===================================================================
-- Generating a solution for the Universes directly in the Gen monad.

item' :: Term era t -> t -> SubItem era
item' (Var (V name rep _)) t = SubItem (V name rep No) (Lit rep t)
item' term _ = error ("item' applied to non Var Term: " ++ show term)

scriptWits ::
  Cardano.Ledger.Core.EraScript era =>
  Proof era ->
  Int ->
  Scripts.Tag ->
  Map
    (KeyHash 'Witness (EraCrypto era))
    (KeyPair 'Witness (EraCrypto era)) ->
  ValidityInterval ->
  Gen (Map (ScriptHash (EraCrypto era)) (ScriptF era))
scriptWits p size tag m vi = do
  scs <- vectorOf size (genCoreScript p tag m vi)
  pure $ Map.fromList $ map (\x -> (hashScript x, ScriptF p x)) scs

genAddrWith :: Crypto c => Network -> Set Ptr -> Set (Credential 'Staking c) -> Gen (Addr c)
genAddrWith net ps cs =
  frequency
    [ (8, Addr net <$> arbitrary <*> genStakeRefWith ps cs)
    , (2, AddrBootstrap <$> arbitrary)
    ]

pick1 :: [String] -> Set t -> Gen t
pick1 msgs s = fst <$> itemFromSet ("from pick1" : msgs) s

genStakeRefWith :: Set Ptr -> Set (Credential 'Staking c) -> Gen (StakeReference c)
genStakeRefWith ps cs =
  frequency
    [ (80, StakeRefBase <$> pick1 ["from genStakeRefWith StakeRefBase"] cs)
    , (5, StakeRefPtr <$> pick1 ["from genStakeRefWith StakeRefPtr"] ps)
    , (15, pure StakeRefNull)
    ]

genPtr :: SlotNo -> Gen Ptr
genPtr (SlotNo n) =
  Ptr
    <$> (SlotNo <$> choose (0, n))
    <*> (TxIx <$> choose (0, 10))
    <*> (mkCertIxPartial <$> choose (1, 20))

initUniv :: forall era. Reflect era => Proof era -> Gen (Subst era)
initUniv p = do
  poolsuniv <- setSized ["From init poolsuniv"] 30 (genRep @era PoolHashR)
  keymapuniv <- mapSized ["From init keymapuniv"] 30 (genRep @era WitHashR) (genRep @era KeyPairR)
  genesisuniv <- mapSized ["From init genesisuniv"] 10 (genRep @era GenHashR) (genRep @era GenDelegPairR)
  txinuniv <- setSized ["From init txinuniv"] 30 (genRep @era TxInR)
  lower <- choose (100, 500)
  let slotno = (lower + 3)
  upper <- choose (slotno + 1, slotno + 6)
  let validityinterval = ValidityInterval (SJust (SlotNo lower)) (SJust (SlotNo upper))
  spendscriptuniv <- scriptWits p 40 Scripts.Spend keymapuniv validityinterval
  creduniv <-
    setSized
      ["From init creduniv"]
      30
      ( oneof
          [ ScriptHashObj . fst <$> genFromMap ["From init, creduniv, ScriptHashObj"] spendscriptuniv
          , KeyHashObj . coerceKeyRole . fst <$> genFromMap ["From init, creduniv, KeyHashObj"] keymapuniv
          ]
      )
  networkv <- arbitrary
  ptruniv <- setSized ["From init ptruniv"] 30 (genPtr (SlotNo slotno))
  addruniv <- setSized ["From init addruniv"] 30 (genAddrWith networkv ptruniv creduniv)

  pure
    [ item' poolsUniv poolsuniv
    , item' keymapUniv keymapuniv
    , item' currentSlot (SlotNo slotno)
    , item' validityInterval validityinterval
    , item' (spendscriptUniv p) spendscriptuniv
    , item' credsUniv creduniv
    , item' payUniv (Set.map coerceKeyRole creduniv)
    , item' voteUniv (Set.map coerceKeyRole creduniv)
    , item' genesisUniv genesisuniv
    , item' txinUniv txinuniv
    , item' currentEpoch (epochFromSlotNo (SlotNo slotno))
    , item' network networkv
    , item' ptrUniv ptruniv
    , item' addrUniv addruniv
    ]

-- ======================================================================
-- Reusable Targets. First order representations of functions for use in
-- building 'Target's. We will apply these to Term variables,
-- (using  (:$) and (^$)) to indicate how to construct a random values assigned
-- to those variables. By convention we name these "functional" targets by
-- post-fixing their names with a captial "T". These may be a bit more
-- prescriptive rather than descriptive, but you do what you have to do.

scriptHashObjT :: Term era (ScriptHash (EraCrypto era)) -> Target era (Credential 'Staking (EraCrypto era))
scriptHashObjT x = Constr "ScriptHashObj" ScriptHashObj ^$ x

keyHashObjT :: Term era (KeyHash 'Witness (EraCrypto era)) -> Target era (Credential 'Staking (EraCrypto era))
keyHashObjT x = Constr "KeyHashObj" (KeyHashObj . coerceKeyRole) ^$ x

listToSetT :: Ord x => Term era [x] -> Target era (Set.Set x)
listToSetT x = Constr "FromList" Set.fromList ^$ x

makeValidityT :: Term era SlotNo -> Term era SlotNo -> Term era SlotNo -> Target era ValidityInterval
makeValidityT begin current end =
  Constr
    "(-i)x(+j)"
    (\i x j -> ValidityInterval (SJust (x - i)) (SJust (x + j)))
    ^$ begin
    ^$ current
    ^$ end

ptrUnivT :: Term era SlotNo -> Target era (Gen (Set Ptr))
ptrUnivT x = Constr "" (setSized ["From init ptruniv"] 30) :$ (Constr "" genPtr ^$ x)

addrUnivT ::
  Crypto c =>
  Term era Network ->
  Term era (Set Ptr) ->
  Term era (Set (Credential 'Staking c)) ->
  Target era (Gen (Set (Addr c)))
addrUnivT net ps cs =
  Constr "" (setSized ["From addrUnivT"] 30)
    :$ (Constr "genAddrWith" genAddrWith ^$ net ^$ ps ^$ cs)

-- =================================================================
-- Using constraints to generate the Universes

scriptWitsT ::
  Proof era ->
  Int ->
  Scripts.Tag ->
  Term era (Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))) ->
  Term era ValidityInterval ->
  Target era (Gen (Map (ScriptHash (EraCrypto era)) (ScriptF era)))
scriptWitsT p size tag m vi =
  Constr "scriptWits" (unReflect scriptWits p size tag) ^$ m ^$ vi

currentSlot :: Term era SlotNo
currentSlot = Var (V "currentSlot" SlotNoR No)

currentEpoch :: Term era EpochNo
currentEpoch = Var (V "currentEpoch" EpochR No)

network :: Term era Network
network = Var (V "network" NetworkR No)

addrUniv :: Term era (Set.Set (Addr (EraCrypto era)))
addrUniv = Var $ V "addrUniv" (SetR AddrR) No

ptrUniv :: Term era (Set.Set Ptr)
ptrUniv = Var $ V "ptrUniv" (SetR PtrR) No

universePreds :: Reflect era => Proof era -> [Pred era]
universePreds p =
  [ Sized (Range 100 500) currentSlot
  , Sized (Range 0 3) beginSlotDelta
  , Sized (Range 1 15) endSlotDelta
  , Sized (ExactSize 30) poolsUniv
  , Sized (ExactSize 30) keymapUniv
  , Sized (ExactSize 10) genesisUniv
  , Sized (ExactSize 40) txinUniv
  , validityInterval :<-: makeValidityT beginSlotDelta currentSlot endSlotDelta
  , Choose
      (SzExact 30)
      credList
      [ (scriptHashObjT scripthash, [Member scripthash (Dom (spendscriptUniv p))])
      , (keyHashObjT keyhash, [Member keyhash (Dom keymapUniv)])
      ]
  , credsUniv :<-: listToSetT credList
  , currentEpoch :<-: (Constr "epochFromSlotNo" epochFromSlotNo ^$ currentSlot)
  , GenFrom (spendscriptUniv p) (scriptWitsT p 40 Scripts.Spend keymapUniv validityInterval)
  , GenFrom network (constTarget arbitrary) -- Choose Testnet or Mainnet
  , GenFrom ptrUniv (ptrUnivT currentSlot)
  , GenFrom addrUniv (addrUnivT network ptrUniv credsUniv)
  , payUniv :<-: (Constr "coerce" (Set.map stakeToPay) ^$ credsUniv)
  , voteUniv :<-: (Constr "coerce" (Set.map stakeToVote) ^$ credsUniv)
  ]
  where
    endSlotDelta = Var (V "endSlot.Delta" SlotNoR No)
    beginSlotDelta = Var (V "beginSlot.Delta" SlotNoR No)
    credList = Var (V "cred.list" (ListR CredR) No)
    keyhash = Var (V "keyhash" WitHashR No)
    scripthash = Var (V "scripthash" ScriptHashR No)

stakeToPay :: Credential 'Staking c -> Credential 'Payment c
stakeToPay = coerceKeyRole

stakeToVote :: Credential 'Staking c -> Credential 'Voting c
stakeToVote = coerceKeyRole

solveUniv :: Reflect era => Proof era -> Gen (Subst era)
solveUniv proof = do
  toolChainSub proof standardOrderInfo (universePreds proof) []

universeStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
universeStage proof =
  if True
    then toolChainSub proof standardOrderInfo (universePreds proof)
    else const (initUniv proof)

mainUniverses :: IO ()
mainUniverses = do
  let proof = Babbage Standard
  subst <- generate (universeStage proof [])
  putStrLn "\n"
  putStrLn (show subst)
  env <- monadTyped (substToEnv subst emptyEnv)
  ptrsx <- monadTyped (findVar (unVar ptrUniv) env)
  putStrLn (show ptrsx)
  pure ()
