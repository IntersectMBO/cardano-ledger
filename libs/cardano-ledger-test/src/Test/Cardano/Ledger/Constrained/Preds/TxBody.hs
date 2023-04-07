{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Constrained.Preds.TxBody where

import Cardano.Ledger.Address (RewardAcnt (..), Withdrawals (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), StrictMaybe (..), maybeToStrictMaybe)
import Cardano.Ledger.CertState (CertState (..), VState (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraTxBody (..), coinTxOutL)
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Pretty (ppMap)
import Cardano.Ledger.Shelley.AdaPots (consumedTxBody, producedTxBody)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..), DelegCert (..), Delegation (..), PoolCert (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast

-- import Test.Cardano.Ledger.Constrained.Classes (liftUTxO)

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Core (TxOut, Value)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.DPState (dstateStage, pstateStage)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Universes
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Size (OrdCond (..), Size (..))
import Test.Cardano.Ledger.Constrained.Solver (toolChainSub)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..), TxOutField (..))
import Test.Cardano.Ledger.Generic.PrettyCore (pcTxBody, pcTxIn, pcTxOut)
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters (newTxBody, newTxOut)
import Test.QuickCheck

import Cardano.Ledger.Credential (PaymentCredential, StakeReference (..))
import Cardano.Ledger.Keys (coerceKeyRole)

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
  , "reqSignerHashes"
  , "networkID"
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
  ("reqSignerHashes", Just (Payload (SetR WitHashR) x _)) -> [ReqSignerHashes x]
  ("networkID", Just (Payload (MaybeR NetworkR) x _)) -> [Txnetworkid (maybeToStrictMaybe x)]
  _ -> []

txBodyFromEnv :: Proof era -> Env era -> TxBody era
txBodyFromEnv proof env = unReflect newTxBody proof (concat (map (lookupTxBody env) bodyNames))

getRwdCredL :: Lens' (RewardAcnt c) (Credential 'Staking c)
getRwdCredL = lens getRwdCred (\r c -> r {getRwdCred = c})

outNames :: [String]
outNames =
  [ "address"
  , "amount"
  , "dhash"
  , "fdatum"
  , "refscript"
  ]

lookupTxOut :: Env era -> String -> [TxOutField era]
lookupTxOut (Env m) name = case (name, Map.lookup name m) of
  ("address", Just (Payload AddrR t _)) -> [Address t]
  ("amount", Just (Payload (ValueR _) (ValueF _ v) _)) -> [Amount v]
  _ -> []

txOutFromEnv :: Proof era -> Env era -> TxOut era
txOutFromEnv proof env = unReflect newTxOut proof (concat (map (lookupTxOut env) outNames))

address :: Term era (Addr (EraCrypto era))
address = Var $ V "address" AddrR No

amount :: Proof era -> Term era (ValueF era)
amount p = Var $ V "amount" (ValueR p) No

-- ==============================================
-- Show how 'Choose' and 'Maybe' are rewritten
-- to more but simpler constraints.

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

-- ======================================================================
-- Reusable Targets
-- First order representations of functions that construct DCert
-- values. For use in building 'Target's. We will apply these to (Term Era t)
-- variables, (using  (:$) and (^$)) to indicate how to construct a
-- DCert from the random values assigned to those variables.
-- Thus:  delegateF (delegationF stkCred poolHash)
-- builds a random 'delegation' DCert from the random values assigned to
-- 'Term's :  'stkCred' and 'poolHash', which are generated using the 'Pred's that
-- mention those 'Term's. By convention we name these "functional" targets by
-- post-fixing their names with a captial "F" or "T"
-- These may be a bit more prescriptive rather than descriptive, but you do what you have to do.

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

-- ======================================================================
-- Using constraints to generate a TxBody

txBodyPreds :: Reflect era => Proof era -> [Pred era]
txBodyPreds p =
  [ Random mint
  , Random networkID
  , Random (amount p)
  , Member address addrUniv
  , Sized (ExactSize 5) (utxo p)
  , Sized (Range 2 5) inputs
  , Sized (Range 2 4) (outputs p)
  , Sized (Range 0 2) withdrawals
  , Sized (Range 0 1) refInputs
  , Sized (Range 1 2) collateral
  , Sized (Range 1 2) reqSignerHashes
  , Subset reqSignerHashes (Dom keymapUniv)
  , Subset (Dom (utxo p)) txinUniv
  , Member feeInput (Dom (utxo p))
  , Subset inputs (Dom (utxo p))
  , Member feeInput inputs
  , Subset refInputs (Dom (utxo p))
  , Subset collateral (Dom (utxo p))
  , NotMember feeInput collateral
  , spending :=: Restrict inputs (utxo p)
  , colUtxo :=: Restrict collateral (utxo p)
  , SumsTo (Coin 1) totalCol EQL [Project CoinR colUtxo]
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
  , txfee :<-: getUtxoCoinT feeInput spending
  ]
  where
    spending = Var (V "spending" (MapR TxInR (TxOutR p)) No)
    colUtxo = Var (V "colUtxo" (MapR TxInR (TxOutR p)) No)
    inputsCoin = Var (V "inputsCoin" CoinR No)
    regkey = var "regkey" CredR
    deregkey = var "deregkey" CredR
    stkCred = var "stkCred" CredR
    poolHash = var "poolHash" PoolHashR
    epoch = var "epoch" EpochR
    someKey = var "someKey" (MaybeR CredR)
    zeroRewards = var "zeroRewards" (MapR CredR CoinR)
    feeInput = var "feeInput" TxInR

txBodyStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
txBodyStage proof = toolChainSub proof standardOrderInfo (txBodyPreds proof)

main :: IO ()
main = do
  let proof = Babbage Standard
  env <-
    generate
      ( pure []
          >>= pParamsStage proof
          >>= universeStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= txBodyStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  let txb = txBodyFromEnv proof env
      out = txOutFromEnv proof env
  dStateV <- monadTyped $ runTarget env dstateT
  pStateV <- monadTyped $ runTarget env pstateT
  (PParamsF _ ppV) <- monadTyped (findVar (unVar (pparams proof)) env)
  utxoV <- monadTyped (findVar (unVar (utxo proof)) env)
  putStrLn (show (pcTxBody proof txb))
  putStrLn (show (producedTxBody txb ppV (CertState (VState Set.empty Map.empty) pStateV dStateV)))
  putStrLn (show (consumedTxBody txb ppV (CertState (VState Set.empty Map.empty) pStateV dStateV) (liftUTxO utxoV)))
  -- putStrLn "\n"
  -- putStrLn (unlines (otherFromEnv bodyNames env))
  spending <- monadTyped (findVar (V "spending" (MapR TxInR (TxOutR proof)) No) env)
  putStrLn ("spending\n" ++ show (ppMap pcTxIn (pcTxOut proof . unTxOut) spending))
  putStrLn (show (pcTxOut proof out))
