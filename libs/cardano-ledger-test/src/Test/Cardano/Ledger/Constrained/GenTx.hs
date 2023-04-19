{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.GenTx where

import Cardano.Ledger.Address (RewardAcnt (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Scripts (Tag (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), SlotNo (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraScript (..), EraTxBody (..), hashScript)
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), coerceKeyRole)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..), DelegCert (..), Delegation (..), PoolCert (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (ScriptF (..))
import Test.Cardano.Ledger.Constrained.Combinators
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Examples (stoi)
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Scripts
import Test.Cardano.Ledger.Constrained.Size (OrdCond (..), Size (..))
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..))
import Test.Cardano.Ledger.Generic.PrettyCore (pcTxBody)
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters (newTxBody)
import Test.QuickCheck

-- =======================================================================================
-- How to construct an actual TxBody from an (Env era) that stores a
-- variable for each of the 'bodyNames'. If one of these vars is not
-- in the Env them, that component of the TxBody will use the default value
-- stored in Test.Cardano.Ledger.Generic.Fields(initialTxBody)

bodyNames :: [String]
bodyNames =
  [ "inputs"
  , "refInputs"
  , "collateral"
  , "outputs"
  , "collateralReturn"
  , "totalCol"
  , "certs"
  , "withdrawals"
  , "txfee"
  , "ttl"
  , "validityInterval"
  , "mint"
  ]

lookupTxBody :: Env era -> String -> [TxBodyField era]
lookupTxBody (Env m) name = case (name, Map.lookup name m) of
  ("inputs", Just (Payload (SetR TxInR) t _)) -> [Inputs t]
  ("refInputs", Just (Payload (SetR TxInR) t _)) -> [RefInputs t]
  ("collateral", Just (Payload (SetR TxInR) t _)) -> [Collateral t]
  ("outputs", Just (Payload (ListR (TxOutR _)) ts _)) -> [Outputs' (map unTxOut ts)]
  ("collateralReturn", Just (Payload (TxOutR _) x _)) -> [CollateralReturn (SJust (unTxOut x))]
  ("totalCol", Just (Payload CoinR x _)) -> [TotalCol (SJust x)]
  ("certs", Just (Payload (ListR DCertR) xs _)) -> [Certs' xs]
  ("withdrawals", Just (Payload (MapR (RewardAcntR @era) CoinR) mp _)) -> [Withdrawals' (Withdrawals mp)]
  ("txfee", Just (Payload CoinR x _)) -> [Txfee x]
  ("ttl", Just (Payload SlotNoR x _)) -> [TTL x]
  ("validityInterval", Just (Payload ValidityIntervalR x _)) -> [Vldt x]
  ("mint", Just (Payload MultiAssetR x _)) -> [Mint x]
  _ -> []

txBodyFromEnv :: Proof era -> Env era -> TxBody era
txBodyFromEnv proof env = unReflect newTxBody proof (concat (map (lookupTxBody env) bodyNames))

otherFromEnv :: Env era -> [String]
otherFromEnv (Env m) = [n ++ " = " ++ synopsis r t | (n, Payload r t _) <- Map.toList m, not (elem n bodyNames)]

getRwdCredL :: Lens' (RewardAcnt c) (Credential 'Staking c)
getRwdCredL = lens getRwdCred (\r c -> r {getRwdCred = c})

-- ==============================================
-- Show how 'Choose' and 'Maybe' are rewritten
-- to more but simpler constraints.

var :: String -> Rep era t -> Term era t
var s r = Var (V s r No)

showRewrites :: IO ()
showRewrites = do
  let choice =
        Choose
          (SzRng 1 4)
          certs
          [ (regKeyF regkey, [NotMember regkey (Dom rewards)])
          , (deRegKeyF deregkey, [Member deregkey (Dom rewards)])
          ,
            ( delegateF (delegationF stkCred poolHash)
            , [Member poolHash (Dom regPools), Member stkCred (Dom rewards)]
            )
          ,
            ( retirePoolF poolHash epoch
            , [Member poolHash (Dom regPools), CanFollow epoch epochNo]
            )
          ]
      choice2 = Maybe someKey (Simple (var "x" CredR)) [Member (var "x" CredR) (Dom rewards)]
      regkey = var "regkey" CredR
      deregkey = var "deregkey" CredR
      stkCred = var "stkCred" CredR
      poolHash = var "poolHash" PoolHashR
      someKey = var "someKey" (MaybeR CredR)
      epoch = var "epoch" EpochR
  (_, newps) <- generate $ rewriteGen (1, [choice, choice2])
  putStrLn (show [choice, choice2])
  putStrLn (show newps)

-- ===================================================================
-- Generating a solution for the Universes directly in the Gen monad.

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
    ]

-- ======================================================================
-- Using constraints to generate the Universes

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

scriptWitsT ::
  Proof era ->
  Int ->
  Scripts.Tag ->
  Term era (Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))) ->
  Term era ValidityInterval ->
  Target era (Gen (Map (ScriptHash (EraCrypto era)) (ScriptF era)))
scriptWitsT p size tag m vi =
  Constr "scriptWits" (unReflect scriptWits p size tag) ^$ m ^$ vi

spendscriptUniv :: Proof era -> Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era))
spendscriptUniv p = Var (V "spendscriptUniv" (MapR ScriptHashR (ScriptR p)) No)

keymapUniv :: Term era (Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era)))
keymapUniv = Var (V "keymapUniv" (MapR WitHashR KeyPairR) No)

currentSlot :: Term era SlotNo
currentSlot = var "currentSlot" SlotNoR

universePreds :: Era era => Proof era -> [Pred era]
universePreds p =
  [ Sized (Range 100 150) currentSlot
  , Sized (Range 0 3) beginSlotDelta
  , Sized (Range 1 15) endSlotDelta
  , Sized (ExactSize 30) poolsUniv
  , Sized (ExactSize 30) keymapUniv
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
    endSlotDelta = var "endSlot.Delta" SlotNoR
    beginSlotDelta = var "beginSlot.Delta" SlotNoR
    credList = Var (V "cred.list" (ListR CredR) No)
    keyhash = Var (V "keyhash" WitHashR No)
    scripthash = Var (V "scripthash" ScriptHashR No)

solveUniv :: Reflect era => Proof era -> Gen (Subst era)
solveUniv proof = do
  toolChainSub proof stoi (universePreds proof) []

-- ======================================================================
-- Using constraints to generate a TxBody

-- First order representations of functions that construct DCert
-- values. For use in building 'Target's. We will apply these to (Term Era t)
-- variables, (using  (:$) and (^$)) to indicate how to construct a
-- DCert from the random values assigned to those variables.
-- Thus:  delegateF (delegationF stkCred poolHash)
-- builds a random 'delegation' DCert from the random values assigned to
-- 'Term's :  'stkCred' and 'poolHash', which are generated using the 'Pred's that
-- mention those 'Term's. By convention we name these "functional" targets by
-- post-fixing their names with a captial "F"

regKeyF :: Term era (StakeCredential (EraCrypto era)) -> Target era (DCert (EraCrypto era))
regKeyF x = Constr "regKeyF" (DCertDeleg . RegKey) ^$ x

deRegKeyF :: Term era (StakeCredential (EraCrypto era)) -> Target era (DCert (EraCrypto era))
deRegKeyF x = Constr "deRegKeyF" (DCertDeleg . DeRegKey) ^$ x

delegateF :: Target era (Delegation (EraCrypto era)) -> Target era (DCert (EraCrypto era))
delegateF x = Constr "delegateF" (DCertDeleg . Delegate) :$ x

delegationF ::
  Term era (StakeCredential (EraCrypto era)) ->
  Term era (KeyHash 'StakePool (EraCrypto era)) ->
  Target era (Delegation (EraCrypto era))
delegationF x y = Constr "DelegationF" Delegation ^$ x ^$ y

retirePoolF ::
  Term era (KeyHash 'StakePool (EraCrypto era)) ->
  Term era EpochNo ->
  Target era (DCert (EraCrypto era))
retirePoolF x y = Constr "retirePoolF" (\h e -> DCertPool (RetirePool h e)) ^$ x ^$ y

txBodyPreds :: Reflect era => Proof era -> [Pred era]
txBodyPreds p =
  [ Random rewards
  , Random regPools
  , Random mint
  , Sized (Range 1 3) epochNo
  , Sized (ExactSize 5) (utxo p)
  , Sized (Range 1 4) inputs
  , Sized (Range 2 4) (outputs p)
  , Sized (Range 0 2) withdrawals
  , Sized (Range 0 1) refInputs
  , Sized (Range 1 2) collateral
  , Subset inputs (Dom (utxo p))
  , Subset refInputs (Dom (utxo p))
  , Subset collateral (Dom (utxo p))
  , spending :=: Restrict inputs (utxo p)
  , SumsTo (Coin 1) inputsCoin EQL [Project CoinR spending]
  , zeroRewards :<-: (Constr "filter (==0)" (Map.filter (== (Coin 0))) ^$ rewards)
  , Subset (ProjS getRwdCredL CredR (Dom withdrawals)) (Dom zeroRewards)
  , Choose
      (SzRng 1 4)
      certs
      [ (regKeyF regkey, [NotMember regkey (Dom rewards)])
      , (deRegKeyF deregkey, [Member deregkey (Dom rewards)])
      ,
        ( delegateF (delegationF stkCred poolHash)
        , [Member poolHash (Dom regPools), Member stkCred (Dom rewards)]
        )
      ,
        ( retirePoolF poolHash epoch
        , [Member poolHash (Dom regPools), CanFollow epoch epochNo]
        )
      ]
  , Maybe someKey (Simple (var "x" CredR)) [Member (var "x" CredR) (Dom rewards)]
  , ttl :<-: (Constr "(+5)" (\x -> x + 5) ^$ currentSlot)
  , txfee :=: inputsCoin
  ]
  where
    spending = Var (V "spending" (MapR TxInR (TxOutR p)) No)
    inputsCoin = Var (V "inputsCoin" CoinR No)
    regkey = var "regkey" CredR
    deregkey = var "deregkey" CredR
    stkCred = var "stkCred" CredR
    poolHash = var "poolHash" PoolHashR
    epoch = var "epoch" EpochR
    someKey = var "someKey" (MaybeR CredR)
    zeroRewards = var "zeroRewards" (MapR CredR CoinR)

solveTxBody :: Reflect era => Proof era -> [Pred era] -> Gen (Env era, TxBody era)
solveTxBody proof ps = do
  -- we have a choice of how to generate the Universes, directly (initUniv)
  -- or using the constraints in (universePreds proof)
  env <-
    if True
      then toolChainSub proof stoi (universePreds proof) [] >>= toolChain proof stoi ps
      else initUniv proof >>= toolChain proof stoi ps
  pure (env, txBodyFromEnv proof env)

main :: IO ()
main = do
  let proof = Babbage Standard
  (env, txb) <- generate (solveTxBody proof (txBodyPreds proof))
  putStrLn (show (pcTxBody proof txb))
  putStrLn (unlines (otherFromEnv env))
