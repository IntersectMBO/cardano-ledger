{-# LANGUAGE DataKinds #-}
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
import Cardano.Ledger.Keys (GenDelegPair, KeyHash, KeyRole (..))
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
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (Adds (..), Sums (genT), unTxOut)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Examples (stoi)
import Test.Cardano.Ledger.Constrained.Lenses (fGenDelegGenKeyHashL)
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Rewrite (compileGen, rewriteGen)
import Test.Cardano.Ledger.Constrained.Size (OrdCond (..), Size (..), genFromIntRange, genFromSize)
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

getRwdCredL :: Lens' (RewardAcnt c) (Credential 'Staking c)
getRwdCredL = lens getRwdCred (\r c -> r {getRwdCred = c})

-- =====================================================================
-- First order representations of functions that construct DCert
-- values. For use in building 'Target's. We will apply these to (Term Era t)
-- variables, (using  (:$) and (^$)) to indicate how to construct a
-- DCert from the random values assigned to those variables.
-- Thus:  delegateF :$ (delegationF ^$ stkCred ^$ poolHash)
-- builds a random 'delegation' DCert from the random values assigned to
-- 'Term's :  'stkCred' and 'poolHash', which are generated using the 'Pred's that
-- mention those 'Term's. By convention we name these "functional" targets by
-- post-fixing their names with a captial "F"

regKeyF :: Target era (StakeCredential c -> DCert c)
regKeyF = Constr "regKeyF" (DCertDeleg . RegKey)

deRegKeyF :: Target era (StakeCredential c -> DCert c)
deRegKeyF = Constr "deRegKeyF" (DCertDeleg . DeRegKey)

delegateF :: Target era (Delegation c -> DCert c)
delegateF = Constr "delegateF" (DCertDeleg . Delegate)

delegationF :: Target era (StakeCredential c -> KeyHash 'StakePool c -> Delegation c)
delegationF = Constr "DelegationF" Delegation

retirePoolF :: Target era (KeyHash 'StakePool c -> EpochNo -> DCert c)
retirePoolF = Constr "retirePoolF" (\h e -> DCertPool (RetirePool h e))

testbody :: Reflect era => Proof era -> [Pred era]
testbody p =
  [ Random rewards
  , Random regPools
  , Random mint
  , Sized (Range 1 3) epochNo
  , Sized (Range 100 150) currentSlot
  , Sized (Range 0 3) beginDelta
  , Sized (Range 1 15) endDelta
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
      [ (regKeyF ^$ regkey, [NotMember regkey (Dom rewards)])
      , (deRegKeyF ^$ deregkey, [Member deregkey (Dom rewards)])
      ,
        ( delegateF :$ (delegationF ^$ stkCred ^$ poolHash)
        , [Member poolHash (Dom regPools), Member stkCred (Dom rewards)]
        )
      ,
        ( retirePoolF ^$ poolHash ^$ epoch
        , [Member poolHash (Dom regPools), CanFollow epoch epochNo]
        )
      ]
  , Maybe someKey (Simple (var "x" CredR)) [Member (var "x" CredR) (Dom rewards)]
  , ttl :<-: (Constr "(+5)" (\x -> x + 5) ^$ currentSlot)
  , validityInterval
      :<-: ( Constr
              "(-i)x(+j)"
              (\i x j -> ValidityInterval (SJust (x - i)) (SJust (x + j)))
              ^$ beginDelta
              ^$ currentSlot
              ^$ endDelta
           )
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
    slot = var "slot" SlotNoR
    currentSlot = var "currentSlot" SlotNoR
    endDelta = var "endDelta" SlotNoR
    beginDelta = var "beginDelta" SlotNoR
    zeroRewards = var "zeroRewards" (MapR CredR CoinR)

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
          [ (regKeyF ^$ regkey, [NotMember regkey (Dom rewards)])
          , (deRegKeyF ^$ deregkey, [Member deregkey (Dom rewards)])
          ,
            ( delegateF :$ (delegationF ^$ stkCred ^$ poolHash)
            , [Member poolHash (Dom regPools), Member stkCred (Dom rewards)]
            )
          ,
            ( retirePoolF ^$ poolHash ^$ epoch
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

{- How does the Choose Constraint operate

   Choose (SzRng 1 4) certs
        [ (regKeyF ^$ regkey, [NotMember regkey (Dom rewards)])
        , (deRegKeyF ^$ deregkey, [Member deregkey (Dom rewards)])
        , (delegateF :$ (delegationF  ^$ stkCred ^$ poolHash),
             [Member poolHash (Dom regPools), Member stkCred (Dom rewards)])
        , (retirePoolF ^$ poolHash ^$ epoch,
             [Member poolHash (Dom regPools), CanFollow epoch epochNo])
        ]

intended semantics

Choose (Range 1 4) certs
  (forall regkey. (regKeyF regkey) | NotMember regkey (Dom rewards))
  (forall deregkey. (deRegKeyF deregkey) | Member deregkey (Dom rewards))
  (forall poolHash stkCred. (delegateF (DelegationF stkCred) poolHash) | Member poolHash (Dom regPools), Member stkCred (Dom rewards))
  (forall epoch poolHash. ((retirePoolF poolHash) epoch) | Member poolHash (Dom regPools), CanFollow epoch epochNo)

The intent is to generate a list with between 1 and 4 elements
and bind it to the variable "certs". Each of the (between 1 and 4)
elements in randomly chosen from the 3 choices, and for each choice
constraints are made true. The bound variables in the forall are freshly
instantiated. The unbound variables (regPools rewards, epochNo) are global.
It works by rewriting the one Choose constraint to many simpler constraints

Here is an example where we choose 3 of the choices

List certs [certs.6, certs.7, certs.8]
certs.8 :<-: (deRegKeyF deregkey.1)
certs.7 :<-: (delegateF (DelegationF stkCred.2) poolHash.3)
certs.6 :<-: (delegateF (DelegationF stkCred.4) poolHash.5)
Member deregkey.1 (Dom rewards)
Member stkCred.2 (Dom rewards)
Member poolHash.3 (Dom regPools)
Member stkCred.4 (Dom rewards)
Member poolHash.5 (Dom regPools)

-}
