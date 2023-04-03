{-# LANGUAGE GADTs #-}

module Test.Cardano.Ledger.Constrained.GenTx where

import Cardano.Ledger.Address (RewardAcnt (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), SlotNo (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Core (EraTxBody (..))
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.DPState (FutureGenDeleg (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (GenDelegPair)
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Pretty (ppMap)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..), DelegCert (..), Delegation (..), PoolCert (..))
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Control.Exception (ErrorCall (..))
import Control.Monad (when)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.Universe as Univ (Any (..))
import Debug.Trace (trace)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (Adds (..), Sums (genT), unTxOut)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Examples (stoi)
import Test.Cardano.Ledger.Constrained.Lenses (fGenDelegGenKeyHashL)
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Rewrite (compileGen, rewriteGen)
import Test.Cardano.Ledger.Constrained.Size (OrdCond (..), Size (..), genFromSize)
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.Spec (TT)
import Test.Cardano.Ledger.Constrained.Tests (prop_soundness)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..))
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyC (..), pcTxBody, pcTxIn, pcTxOut)
import Test.Cardano.Ledger.Generic.Proof (Reflect (..), Standard, unReflect)
import Test.Cardano.Ledger.Generic.Updaters (newTxBody)
import Test.QuickCheck

-- =======================================================================================

lookupTxBody :: Env era -> String -> [TxBodyField era]
lookupTxBody (Env m) x = case (x, Map.lookup x m) of
  ("inputs", Just (Payload (SetR TxInR) t _)) -> [Inputs t]
  ("refInputs", Just (Payload (SetR TxInR) t _)) -> [RefInputs t]
  ("collateral", Just (Payload (SetR TxInR) t _)) -> [Collateral t]
  ("outputs", Just (Payload (ListR (TxOutR p)) ts _)) -> [Outputs' (map unTxOut ts)]
  ("collateralReturn", Just (Payload (TxOutR p) x _)) -> [CollateralReturn (SJust (unTxOut x))]
  ("totalCol", Just (Payload CoinR x _)) -> [TotalCol (SJust x)]
  ("certs", Just (Payload (ListR DCertR) xs _)) -> [Certs' xs]
  ("withdrawals", Just (Payload (MapR (RewardAcntR @era) CoinR) m _)) -> [Withdrawals' (Withdrawals m)]
  ("txfee", Just (Payload CoinR x _)) -> [Txfee x]
  ("ttl", Just (Payload SlotNoR x _)) -> [TTL x]
  ("validityInterval", Just (Payload ValidityIntervalR x _)) -> [Vldt x]
  ("mint", Just (Payload MultiAssetR x _)) -> [Mint x]
  _ -> []

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

txBodyFromEnv :: Proof era -> Env era -> TxBody era
txBodyFromEnv proof env = unReflect newTxBody proof (concat (map (lookupTxBody env) bodyNames))

otherFromEnv :: Env era -> [String]
otherFromEnv (Env m) = [n ++ " = " ++ synopsis r t | (n, Payload r t _) <- Map.toList m, not (elem n bodyNames)]

tar :: (a -> b) -> String -> Target era (a -> b)
tar f s = Constr s f

var :: String -> Rep era t -> Term era t
var s r = Var (V s r No)

testbody :: Reflect era => Proof era -> [Pred era]
testbody p =
  [ Random rewards
  , Random regPools
  , Sized (Range 1 3) epochNo
  , Sized (Range 100 150) currentSlot
  , Sized (ExactSize 5) (utxo p)
  , Sized (Range 1 4) inputs
  , Subset inputs (Dom (utxo p))
  , Sized (Range 0 1) refInputs
  , Subset refInputs (Dom (utxo p))
  , Sized (Range 1 2) collateral
  , Subset collateral (Dom (utxo p))
  , spending :=: Restrict inputs (utxo p)
  , SumsTo (Coin 1) inputsCoin EQL [Project CoinR spending]
  , Sized (Range 2 4) (outputs p)
  , Sized (Range 0 2) withdrawals
  , Random mint
  , Choose
      (SzRng 1 4)
      certs
      [ (regT, [NotMember regkey (Dom rewards)])
      , (deregT, [Member deregkey (Dom rewards)])
      , (delT, [Member poolHash (Dom regPools), Member stkCred (Dom rewards)])
      , (retirePoolT, [Member poolHash (Dom regPools), CanFollow epoch epochNo])
      ]
  , Maybe someKey (Simple (var "x" CredR)) [Member (var "x" CredR) (Dom rewards)]
  , ttl :<-: (Constr "(+5)" (\x -> x + 5) ^$ currentSlot)
  , validityInterval :<-: (Constr "(-4)x(+5)" (\x -> ValidityInterval (SJust (x - 4)) (SJust (x + 5))) ^$ currentSlot)
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
    retirePoolT = tar DCertPool "DCertPool" :$ (tar RetirePool "RetirePool" ^$ poolHash ^$ epoch)
    regT = tar (DCertDeleg . RegKey) "(DCertDeleg . RegKey)" ^$ regkey
    deregT = tar (DCertDeleg . DeRegKey) "(DCertDeleg . DeRegKey)" ^$ deregkey
    delT = tar (DCertDeleg . Delegate) "(DCertDeleg . Delegate)" :$ (tar Delegation "Delegation" ^$ stkCred ^$ poolHash)
    someKey = var "someKey" (MaybeR CredR)
    slot = var "slot" SlotNoR
    currentSlot = var "currentSlot" SlotNoR

go :: Reflect era => Proof era -> [Pred era] -> Gen (Env era, TxBody era)
go proof ps = do
  env <- toolChain proof stoi ps
  pure (env, txBodyFromEnv proof env)

main = do
  let proof = Babbage Standard
  (env, txb) <- generate (go proof (testbody proof))
  putStrLn (show (pcTxBody proof txb))
  putStrLn (unlines (otherFromEnv env))

showRewrites = do
  let choice =
        Choose
          (SzRng 1 4)
          certs
          [ (regT, [Member regkey (Dom rewards)])
          , (deregT, [Member deregkey (Dom rewards)])
          , (delT, [Member poolHash (Dom regPools), Member stkCred (Dom rewards)])
          ]
      choice2 = Maybe someKey (Simple (var "x" CredR)) [Member (var "x" CredR) (Dom rewards)]
      regkey = var "regkey" CredR
      deregkey = var "deregkey" CredR
      stkCred = var "stkCred" CredR
      poolHash = var "poolHash" PoolHashR
      regT = tar (DCertDeleg . RegKey) "(DCertDeleg . RegKey)" ^$ regkey
      deregT = tar (DCertDeleg . DeRegKey) "(DCertDeleg . DeRegKey)" ^$ deregkey
      delT = tar (DCertDeleg . Delegate) "(DCertDeleg . Delegate)" :$ (tar Delegation "Delegation" ^$ stkCred ^$ poolHash)
      someKey = var "someKey" (MaybeR CredR)
  (_, newps) <- generate $ rewriteGen (1, [choice2])
  putStrLn (show choice)
  putStrLn (show newps)

{- How does the Choose Constraint operate

   Choose (SzRng 1 4) certs
     [ (regT,[Member regkey (Dom rewards)])
     , (deregT,[Member deregkey (Dom rewards)])
     , (delT,[Member poolHash (Dom regPools),Member stkCred (Dom rewards)])
     ]

intended semantics

Choose (Range 1 4) certs
  (forall regkey. Member regkey (Dom rewards))
  (forall deregkey. Member deregkey (Dom rewards))
  (forall poolHash stkCred. Member poolHash (Dom regPools), Member stkCred (Dom rewards))

The intent is to generate a list with between 1 and 4 elements
and bind it to the variable "certs". Each of the (between 1 and 4)
elements in randomly chosen from the 3 choices, and for each choice
constraints are made true. The bound variables in the forall are freshly
instantiated. The unbound variables (regPools rewards) are global.
It works by rewriting the one Choose constraint to N simpler constraints

Here is an example where we choose the maximum of 4 choices

List certs [certs.6, certs.7, certs.8, certs.9]
Targets certs.9 (? :$ deregkey.1)
Targets certs.8 (? :$ regkey.2)
Targets certs.7 (? :$ (? :$ stkCred.3) :$ poolHash.4)
Targets certs.6 (? :$ regkey.5)
Member deregkey.1 (Dom rewards)
Member regkey.2 (Dom rewards)
Member stkCred.3 (Dom rewards)
Member poolHash.4 (Dom regPools)
Member regkey.5 (Dom rewards)

-}

delegate = tar (DCertDeleg . Delegate) "(DCertDeleg . Delegate)" :$ (tar Delegation "Delegation" ^$ stkCred ^$ poolHash)
  where
    poolHash = var "poolHash" PoolHashR
    stkCred = var "stkCred" CredR
