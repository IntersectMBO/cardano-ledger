{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.Universes
where

import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (Size (..))
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Proof
import Test.Tasty.QuickCheck

-- import Test.Cardano.Ledger.Constrained.GenTx(otherFromEnv)

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Scripts (Tag (..))
import Cardano.Ledger.BaseTypes (SlotNo (..), StrictMaybe (..))
import Cardano.Ledger.Core (EraScript (..), hashScript)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), coerceKeyRole)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Cardano.Ledger.Constrained.Combinators (genFromMap, mapSized, setSized)
import Test.Cardano.Ledger.Constrained.Scripts (genCoreScript)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))

-- ===================================================================
-- Generating a solution for the Universes directly in the Gen monad.

-- | The universe of Scripts (and their hashes) useable in spending contexts
--  That means if they are Plutus scripts then they will be passed an additional
--  argument (the TxInfo context)
spendscriptUniv :: Proof era -> Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era))
spendscriptUniv p = Var (V "spendscriptUniv" (MapR ScriptHashR (ScriptR p)) No)

item :: String -> Rep era t -> t -> SubItem era
item name rep t = SubItem (V name rep No) (Lit rep t)

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

initUniv :: forall era. Reflect era => Proof era -> Gen (Subst era)
initUniv p = do
  poolsuniv <- setSized ["From init poolsuniv"] 30 (genRep @era PoolHashR)
  keymapuniv <- mapSized ["From init keymapuniv"] 30 (genRep @era WitHashR) (genRep @era KeyPairR)
  genesisuniv <- mapSized ["From init genesisuniv"] 10 (genRep @era GenHashR) (genRep @era GenDelegPairR)
  txinuniv <- setSized ["From init txinuniv"] 30 (genRep @era TxInR)
  lower <- SlotNo <$> choose (100, 120)
  upper <- SlotNo <$> choose (125, 135)
  let slotno = lower + 3
      validityinterval = ValidityInterval (SJust lower) (SJust upper)
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
  pure
    [ item "poolsUniv" (SetR PoolHashR) poolsuniv
    , item "keymapUniv" (MapR WitHashR KeyPairR) keymapuniv
    , item "currentSlot" SlotNoR slotno
    , item "validityInterval" ValidityIntervalR validityinterval
    , item "spendscriptUniv" (MapR ScriptHashR (ScriptR p)) spendscriptuniv
    , item "credsUniv" (SetR CredR) creduniv
    , item "genesisUniv" (MapR GenHashR GenDelegPairR) genesisuniv
    , item "txinUniv" (SetR TxInR) txinuniv
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

universePreds :: Reflect era => Proof era -> [Pred era]
universePreds p =
  [ Sized (Range 100 150) currentSlot
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
  , GenFrom (spendscriptUniv p) (scriptWitsT p 40 Scripts.Spend keymapUniv validityInterval)
  ]
  where
    endSlotDelta = Var (V "endSlot.Delta" SlotNoR No)
    beginSlotDelta = Var (V "beginSlot.Delta" SlotNoR No)
    credList = Var (V "cred.list" (ListR CredR) No)
    keyhash = Var (V "keyhash" WitHashR No)
    scripthash = Var (V "scripthash" ScriptHashR No)

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
