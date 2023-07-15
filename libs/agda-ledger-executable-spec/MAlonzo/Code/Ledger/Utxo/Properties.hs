{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module MAlonzo.Code.Ledger.Utxo.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Morphism
import qualified MAlonzo.Code.Axiom.Set.Properties
import qualified MAlonzo.Code.Axiom.Set.Sum
import qualified MAlonzo.Code.Data.Bool.Properties
import qualified MAlonzo.Code.Data.Integer.Base
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Sign.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.ComputationalRelation
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Ledger.Address
import qualified MAlonzo.Code.Ledger.Crypto
import qualified MAlonzo.Code.Ledger.Deleg
import qualified MAlonzo.Code.Ledger.Epoch
import qualified MAlonzo.Code.Ledger.GovernanceActions
import qualified MAlonzo.Code.Ledger.PParams
import qualified MAlonzo.Code.Ledger.Prelude
import qualified MAlonzo.Code.Ledger.TokenAlgebra
import qualified MAlonzo.Code.Ledger.Transaction
import qualified MAlonzo.Code.Ledger.Utxo
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Ledger.Utxo.Properties._._≈_
d__'8776'__22 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__22 = erased
-- Ledger.Utxo.Properties._.TxBody
d_TxBody_282 a0 = ()
-- Ledger.Utxo.Properties._.UTxO
d_UTxO_292 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_UTxO_292 = erased
-- Ledger.Utxo.Properties._.coin
d_coin_328 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny -> Integer
d_coin_328 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_coin_58
      (coe MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0))
-- Ledger.Utxo.Properties._.PParams
d_PParams_744 a0 = ()
-- Ledger.Utxo.Properties._._⊢_⇀⦇_,UTXO⦈_
d__'8866'_'8640''10631'_'44'UTXO'10632'__858 a0 a1 a2 a3 a4 = ()
-- Ledger.Utxo.Properties._.DepositPurpose
d_DepositPurpose_870 a0 = ()
-- Ledger.Utxo.Properties._.UTxOEnv
d_UTxOEnv_886 a0 = ()
-- Ledger.Utxo.Properties._.UTxOState
d_UTxOState_888 a0 = ()
-- Ledger.Utxo.Properties._.balance
d_balance_892 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_balance_892 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_balance_890 (coe v0)
-- Ledger.Utxo.Properties._.cbalance
d_cbalance_896 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_cbalance_896 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_cbalance_896 (coe v0)
-- Ledger.Utxo.Properties._.consumed
d_consumed_906 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> AgdaAny
d_consumed_906 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_consumed_1312 (coe v0)
-- Ledger.Utxo.Properties._.depositRefunds
d_depositRefunds_908 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_depositRefunds_908 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_depositRefunds_1268 (coe v0)
-- Ledger.Utxo.Properties._.newDeposits
d_newDeposits_920 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_newDeposits_920 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_newDeposits_1290 (coe v0)
-- Ledger.Utxo.Properties._.outs
d_outs_924 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_outs_924 ~v0 = du_outs_924
du_outs_924 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_outs_924 = coe MAlonzo.Code.Ledger.Utxo.du_outs_882
-- Ledger.Utxo.Properties._.produced
d_produced_926 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> AgdaAny
d_produced_926 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_produced_1320 (coe v0)
-- Ledger.Utxo.Properties._.updateDeposits
d_updateDeposits_934 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_updateDeposits_934 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_updateDeposits_1254 (coe v0)
-- Ledger.Utxo.Properties.Computational-UTXO
d_Computational'45'UTXO_1058 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.ComputationalRelation.T_Computational_32
d_Computational'45'UTXO_1058 v0
  = coe
      MAlonzo.Code.Ledger.Utxo.d_Computational'45'UTXO_1348 (coe v0)
-- Ledger.Utxo.Properties.⟦⟧-cong-Coin
d_'10214''10215''45'cong'45'Coin_1060 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'10214''10215''45'cong'45'Coin_1060 = erased
-- Ledger.Utxo.Properties.∙-homo-Coin
d_'8729''45'homo'45'Coin_1062 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.T_IsCommutativeMonoidMorphism_498 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo'45'Coin_1062 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'homo'45'Coin_1062 v8
du_'8729''45'homo'45'Coin_1062 ::
  MAlonzo.Code.Algebra.Morphism.T_IsCommutativeMonoidMorphism_498 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'homo'45'Coin_1062 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.d_'8729''45'homo_158
      (coe
         MAlonzo.Code.Algebra.Morphism.d_sm'45'homo_314
         (coe MAlonzo.Code.Algebra.Morphism.d_mn'45'homo_504 (coe v0)))
-- Ledger.Utxo.Properties.balance-cong
d_balance'45'cong_1064 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_balance'45'cong_1064 v0 v1 v2
  = coe
      MAlonzo.Code.Axiom.Set.Sum.du_indexedSum'7504''45'cong_862
      (coe
         MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
         (coe MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0)))
      (coe
         MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
         (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'TxId_258 (coe v0))
         (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Ix_260 (coe v0)))
      (coe
         MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
         (coe
            MAlonzo.Code.Interface.DecEq.du_DecEq'45'Sum_40
            (coe
               MAlonzo.Code.Ledger.Address.du_DecEq'45'BaseAddr_114
               (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Netw_262 (coe v0))
               (coe
                  MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                  (coe
                     MAlonzo.Code.Ledger.Crypto.d_khs_240
                     (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0))))
               (coe
                  MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                  (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0))))
            (coe
               MAlonzo.Code.Ledger.Address.du_DecEq'45'BootstrapAddr_116
               (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Netw_262 (coe v0))
               (coe
                  MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                  (coe
                     MAlonzo.Code.Ledger.Crypto.d_khs_240
                     (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0))))
               (coe
                  MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                  (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0)))))
         (coe
            MAlonzo.Code.Ledger.TokenAlgebra.d_DecEq'45'Value_72
            (coe MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0))))
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Ledger.Transaction.du_getValue_1074
              (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v3))))
      (coe MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910 (coe v1))
      (coe MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910 (coe v2))
-- Ledger.Utxo.Properties.balance-cong-coin
d_balance'45'cong'45'coin_1070 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_balance'45'cong'45'coin_1070 = erased
-- Ledger.Utxo.Properties.balance-∪
d_balance'45''8746'_1078 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_balance'45''8746'_1078 = erased
-- Ledger.Utxo.Properties.newTxid⇒disj
d_newTxid'8658'disj_1086 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_newTxid'8658'disj_1086 ~v0 ~v1 ~v2 ~v3
  = du_newTxid'8658'disj_1086
du_newTxid'8658'disj_1086 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_newTxid'8658'disj_1086
  = coe
      MAlonzo.Code.Axiom.Set.Properties.du_disjoint'8658'disjoint''_622
-- Ledger.Utxo.Properties.consumedCoinEquality
d_consumedCoinEquality_1102 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_consumedCoinEquality_1102 = erased
-- Ledger.Utxo.Properties.producedCoinEquality
d_producedCoinEquality_1120 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_producedCoinEquality_1120 = erased
-- Ledger.Utxo.Properties.balValueToCoin
d_balValueToCoin_1136 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_balValueToCoin_1136 = erased
-- Ledger.Utxo.Properties.posPart-negPart≡x
d_posPart'45'negPart'8801'x_1150 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_posPart'45'negPart'8801'x_1150 = erased
-- Ledger.Utxo.Properties.DepositHelpers.pp
d_pp_1182 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128
d_pp_1182 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10 ~v11 ~v12
  = du_pp_1182 v10
du_pp_1182 ::
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128
du_pp_1182 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_pparams_1040 (coe v0)
-- Ledger.Utxo.Properties.DepositHelpers.dep
d_dep_1184 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer
d_dep_1184 v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 ~v12
  = du_dep_1184 v0 v5
du_dep_1184 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
du_dep_1184 v0 v1
  = coe
      MAlonzo.Code.Axiom.Set.Sum.du_indexedSum'7504''7515'_856
      MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'commutativeMonoid_3206
      (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026 (coe v0))
      MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30 (\ v2 -> v2)
      (coe MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910 (coe v1))
-- Ledger.Utxo.Properties.DepositHelpers.uDep
d_uDep_1186 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer
d_uDep_1186 v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 v9 v10 ~v11 ~v12
  = du_uDep_1186 v0 v5 v9 v10
du_uDep_1186 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 -> Integer
du_uDep_1186 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Axiom.Set.Sum.du_indexedSum'7504''7515'_856
      MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'commutativeMonoid_3206
      (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026 (coe v0))
      MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30 (\ v4 -> v4)
      (coe
         MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910
         (coe
            MAlonzo.Code.Ledger.Utxo.d_updateDeposits_1254 (coe v0)
            (coe du_pp_1182 (coe v3)) (coe v2) (coe v1)))
-- Ledger.Utxo.Properties.DepositHelpers.Δdep
d_Δdep_1188 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer
d_Δdep_1188 v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 v9 v10 ~v11 ~v12
  = du_Δdep_1188 v0 v5 v9 v10
du_Δdep_1188 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 -> Integer
du_Δdep_1188 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Ledger.Utxo.d_depositsChange_1260 (coe v0)
      (coe du_pp_1182 (coe v3)) (coe v2) (coe v1)
-- Ledger.Utxo.Properties.DepositHelpers.utxoSt
d_utxoSt_1190 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042
d_utxoSt_1190 ~v0 v1 ~v2 v3 ~v4 v5 ~v6 v7 ~v8 ~v9 ~v10 ~v11 ~v12
  = du_utxoSt_1190 v1 v3 v5 v7
du_utxoSt_1190 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer -> MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042
du_utxoSt_1190 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Ledger.Utxo.C_'10214'_'44'_'44'_'44'_'10215''7512'_1060
      (coe v0) (coe v1) (coe v2) (coe v3)
-- Ledger.Utxo.Properties.DepositHelpers.ref
d_ref_1192 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer
d_ref_1192 v0 v1 ~v2 v3 ~v4 v5 ~v6 v7 ~v8 v9 v10 ~v11 ~v12
  = du_ref_1192 v0 v1 v3 v5 v7 v9 v10
du_ref_1192 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 -> Integer
du_ref_1192 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Ledger.Utxo.d_depositRefunds_1268 (coe v0)
      (coe du_pp_1182 (coe v6))
      (coe du_utxoSt_1190 (coe v1) (coe v2) (coe v3) (coe v4)) (coe v5)
-- Ledger.Utxo.Properties.DepositHelpers.tot
d_tot_1194 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer
d_tot_1194 v0 v1 ~v2 v3 ~v4 v5 ~v6 v7 ~v8 v9 v10 ~v11 ~v12
  = du_tot_1194 v0 v1 v3 v5 v7 v9 v10
du_tot_1194 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 -> Integer
du_tot_1194 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Ledger.Utxo.d_newDeposits_1290 (coe v0)
      (coe du_pp_1182 (coe v6))
      (coe du_utxoSt_1190 (coe v1) (coe v2) (coe v3) (coe v4)) (coe v5)
-- Ledger.Utxo.Properties.DepositHelpers.h
d_h_1196 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_h_1196 = erased
-- Ledger.Utxo.Properties.DepositHelpers.newBal'
d_newBal''_1202 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_newBal''_1202 = erased
-- Ledger.Utxo.Properties.DepositHelpers.newBal
d_newBal_1206 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_newBal_1206 = erased
-- Ledger.Utxo.Properties.DepositHelpers.noMintAda'
d_noMintAda''_1208 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_noMintAda''_1208 = erased
-- Ledger.Utxo.Properties.DepositHelpers.noMintAda
d_noMintAda_1212 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_noMintAda_1212 = erased
-- Ledger.Utxo.Properties.DepositHelpers.remDepTot
d_remDepTot_1214 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer
d_remDepTot_1214 v0 v1 ~v2 v3 ~v4 v5 ~v6 v7 ~v8 v9 v10 ~v11 ~v12
  = du_remDepTot_1214 v0 v1 v3 v5 v7 v9 v10
du_remDepTot_1214 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 -> Integer
du_remDepTot_1214 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
      (coe
         MAlonzo.Code.Axiom.Set.Sum.du_indexedSum'7504''7515'_856
         MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'commutativeMonoid_3206
         (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026 (coe v0))
         MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30 (\ v7 -> v7)
         (coe MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910 (coe v3)))
      (coe
         du_ref_1192 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
         (coe v6))
-- Ledger.Utxo.Properties.DepositHelpers.uDep≡
d_uDep'8801'_1216 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_uDep'8801'_1216 = erased
-- Ledger.Utxo.Properties.DepositHelpers.deposits-change'
d_deposits'45'change''_1218 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_deposits'45'change''_1218 = erased
-- Ledger.Utxo.Properties.DepositHelpers.dep-ref
d_dep'45'ref_1220 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_dep'45'ref_1220 = erased
-- Ledger.Utxo.Properties.DepositHelpers.ref-tot-0
d_ref'45'tot'45'0_1226 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_ref'45'tot'45'0_1226 = erased
-- Ledger.Utxo.Properties.DepositHelpers.ref≤dep
d_ref'8804'dep_1242 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_ref'8804'dep_1242 v0 v1 ~v2 v3 ~v4 v5 ~v6 v7 ~v8 v9 v10 ~v11 ~v12
  = du_ref'8804'dep_1242 v0 v1 v3 v5 v7 v9 v10
du_ref'8804'dep_1242 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_ref'8804'dep_1242 v0 v1 v2 v3 v4 v5 v6
  = let v7
          = MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
              (coe
                 du_ref_1192 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                 (coe v6))
              (coe (0 :: Integer)) in
    case coe v7 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v8 v9
        -> if coe v8
             then coe
                    seq (coe v9) (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)
             else coe
                    seq (coe v9)
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_'8804''8243''8658''8804'_6082
                       (let v10
                              = MAlonzo.Code.Data.Integer.Base.d_sign_24
                                  (let v10
                                         = coe
                                             MAlonzo.Code.Data.List.Base.du_foldr_242
                                             (coe
                                                (\ v10 ->
                                                   addInt
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                        (coe v10))))
                                             (coe (0 :: Integer))
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du_deduplicate'7495'_768
                                                (coe
                                                   (\ v10 v11 ->
                                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                                        (let v12
                                                               = coe
                                                                   MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                   (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026
                                                                      (coe v0))
                                                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                      (coe v10))
                                                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                      (coe v11)) in
                                                         let v13
                                                               = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                   (coe v10) in
                                                         let v14
                                                               = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                   (coe v11) in
                                                         case coe v12 of
                                                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v15 v16
                                                             -> if coe v15
                                                                  then coe
                                                                         seq (coe v16)
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                            erased
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                               erased
                                                                               (coe
                                                                                  MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
                                                                                  (coe
                                                                                     eqInt (coe v13)
                                                                                     (coe v14)))))
                                                                  else coe
                                                                         seq (coe v16)
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                            (coe v15)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                                                           _ -> MAlonzo.RTE.mazUnreachableError)))
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                   (coe
                                                      MAlonzo.Code.Ledger.Prelude.du_finiteness_174
                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                         (coe
                                                            MAlonzo.Code.Ledger.Utxo.d_updateCertDeposits_1226
                                                            (coe v0)
                                                            (coe
                                                               MAlonzo.Code.Ledger.Utxo.d_pparams_1040
                                                               (coe v6))
                                                            (coe
                                                               MAlonzo.Code.Ledger.Transaction.d_txcerts_1030
                                                               (coe v5))
                                                            (coe
                                                               MAlonzo.Code.Ledger.Utxo.d_updateProposalDeposits_1238
                                                               (coe v0)
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Utxo.d_pparams_1040
                                                                  (coe v6))
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Transaction.d_txid_1048
                                                                  (coe v5))
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Transaction.d_txprop_1036
                                                                  (coe v5))
                                                               (coe v3))))))) in
                                   let v11
                                         = coe
                                             MAlonzo.Code.Data.List.Base.du_foldr_242
                                             (coe
                                                (\ v11 ->
                                                   addInt
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                        (coe v11))))
                                             (coe (0 :: Integer))
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du_deduplicate'7495'_768
                                                (coe
                                                   (\ v11 v12 ->
                                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                                        (let v13
                                                               = coe
                                                                   MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                   (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026
                                                                      (coe v0))
                                                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                      (coe v11))
                                                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                      (coe v12)) in
                                                         let v14
                                                               = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                   (coe v11) in
                                                         let v15
                                                               = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                   (coe v12) in
                                                         case coe v13 of
                                                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v16 v17
                                                             -> if coe v16
                                                                  then coe
                                                                         seq (coe v17)
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                            erased
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                               erased
                                                                               (coe
                                                                                  MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
                                                                                  (coe
                                                                                     eqInt (coe v14)
                                                                                     (coe v15)))))
                                                                  else coe
                                                                         seq (coe v17)
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                            (coe v16)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                                                           _ -> MAlonzo.RTE.mazUnreachableError)))
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                   (coe
                                                      MAlonzo.Code.Ledger.Prelude.du_finiteness_174
                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                         (coe v3))))) in
                                   let v12
                                         = ltInt
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du_foldr_242
                                                (coe
                                                   (\ v12 ->
                                                      addInt
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                           (coe v12))))
                                                (coe (0 :: Integer))
                                                (coe
                                                   MAlonzo.Code.Data.List.Base.du_deduplicate'7495'_768
                                                   (coe
                                                      (\ v12 v13 ->
                                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                                           (let v14
                                                                  = coe
                                                                      MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                      (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026
                                                                         (coe v0))
                                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                         (coe v12))
                                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                         (coe v13)) in
                                                            let v15
                                                                  = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                      (coe v12) in
                                                            let v16
                                                                  = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                      (coe v13) in
                                                            case coe v14 of
                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v17 v18
                                                                -> if coe v17
                                                                     then coe
                                                                            seq (coe v18)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                               erased
                                                                               (coe
                                                                                  MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                                  erased
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
                                                                                     (coe
                                                                                        eqInt
                                                                                        (coe v15)
                                                                                        (coe
                                                                                           v16)))))
                                                                     else coe
                                                                            seq (coe v18)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                               (coe v17)
                                                                               (coe
                                                                                  MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                                                              _ -> MAlonzo.RTE.mazUnreachableError)))
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                      (coe
                                                         MAlonzo.Code.Ledger.Prelude.du_finiteness_174
                                                         (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                            (coe
                                                               MAlonzo.Code.Ledger.Utxo.d_updateCertDeposits_1226
                                                               (coe v0)
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Utxo.d_pparams_1040
                                                                  (coe v6))
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Transaction.d_txcerts_1030
                                                                  (coe v5))
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Utxo.d_updateProposalDeposits_1238
                                                                  (coe v0)
                                                                  (coe
                                                                     MAlonzo.Code.Ledger.Utxo.d_pparams_1040
                                                                     (coe v6))
                                                                  (coe
                                                                     MAlonzo.Code.Ledger.Transaction.d_txid_1048
                                                                     (coe v5))
                                                                  (coe
                                                                     MAlonzo.Code.Ledger.Transaction.d_txprop_1036
                                                                     (coe v5))
                                                                  (coe v3))))))))
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du_foldr_242
                                                (coe
                                                   (\ v12 ->
                                                      addInt
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                           (coe v12))))
                                                (coe (0 :: Integer))
                                                (coe
                                                   MAlonzo.Code.Data.List.Base.du_deduplicate'7495'_768
                                                   (coe
                                                      (\ v12 v13 ->
                                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                                           (let v14
                                                                  = coe
                                                                      MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                      (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026
                                                                         (coe v0))
                                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                         (coe v12))
                                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                         (coe v13)) in
                                                            let v15
                                                                  = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                      (coe v12) in
                                                            let v16
                                                                  = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                      (coe v13) in
                                                            case coe v14 of
                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v17 v18
                                                                -> if coe v17
                                                                     then coe
                                                                            seq (coe v18)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                               erased
                                                                               (coe
                                                                                  MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                                  erased
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
                                                                                     (coe
                                                                                        eqInt
                                                                                        (coe v15)
                                                                                        (coe
                                                                                           v16)))))
                                                                     else coe
                                                                            seq (coe v18)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                               (coe v17)
                                                                               (coe
                                                                                  MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                                                              _ -> MAlonzo.RTE.mazUnreachableError)))
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                      (coe
                                                         MAlonzo.Code.Ledger.Prelude.du_finiteness_174
                                                         (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                            (coe v3)))))) in
                                   if coe v12
                                     then coe
                                            MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                            (coe subInt (coe v11) (coe v10))
                                     else coe subInt (coe v10) (coe v11)) in
                        let v11
                              = MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18
                                  (let v11
                                         = coe
                                             MAlonzo.Code.Data.List.Base.du_foldr_242
                                             (coe
                                                (\ v11 ->
                                                   addInt
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                        (coe v11))))
                                             (coe (0 :: Integer))
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du_deduplicate'7495'_768
                                                (coe
                                                   (\ v11 v12 ->
                                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                                        (let v13
                                                               = coe
                                                                   MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                   (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026
                                                                      (coe v0))
                                                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                      (coe v11))
                                                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                      (coe v12)) in
                                                         let v14
                                                               = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                   (coe v11) in
                                                         let v15
                                                               = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                   (coe v12) in
                                                         case coe v13 of
                                                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v16 v17
                                                             -> if coe v16
                                                                  then coe
                                                                         seq (coe v17)
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                            erased
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                               erased
                                                                               (coe
                                                                                  MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
                                                                                  (coe
                                                                                     eqInt (coe v14)
                                                                                     (coe v15)))))
                                                                  else coe
                                                                         seq (coe v17)
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                            (coe v16)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                                                           _ -> MAlonzo.RTE.mazUnreachableError)))
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                   (coe
                                                      MAlonzo.Code.Ledger.Prelude.du_finiteness_174
                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                         (coe
                                                            MAlonzo.Code.Ledger.Utxo.d_updateCertDeposits_1226
                                                            (coe v0)
                                                            (coe
                                                               MAlonzo.Code.Ledger.Utxo.d_pparams_1040
                                                               (coe v6))
                                                            (coe
                                                               MAlonzo.Code.Ledger.Transaction.d_txcerts_1030
                                                               (coe v5))
                                                            (coe
                                                               MAlonzo.Code.Ledger.Utxo.d_updateProposalDeposits_1238
                                                               (coe v0)
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Utxo.d_pparams_1040
                                                                  (coe v6))
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Transaction.d_txid_1048
                                                                  (coe v5))
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Transaction.d_txprop_1036
                                                                  (coe v5))
                                                               (coe v3))))))) in
                                   let v12
                                         = coe
                                             MAlonzo.Code.Data.List.Base.du_foldr_242
                                             (coe
                                                (\ v12 ->
                                                   addInt
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                        (coe v12))))
                                             (coe (0 :: Integer))
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du_deduplicate'7495'_768
                                                (coe
                                                   (\ v12 v13 ->
                                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                                        (let v14
                                                               = coe
                                                                   MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                   (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026
                                                                      (coe v0))
                                                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                      (coe v12))
                                                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                      (coe v13)) in
                                                         let v15
                                                               = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                   (coe v12) in
                                                         let v16
                                                               = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                   (coe v13) in
                                                         case coe v14 of
                                                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v17 v18
                                                             -> if coe v17
                                                                  then coe
                                                                         seq (coe v18)
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                            erased
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                               erased
                                                                               (coe
                                                                                  MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
                                                                                  (coe
                                                                                     eqInt (coe v15)
                                                                                     (coe v16)))))
                                                                  else coe
                                                                         seq (coe v18)
                                                                         (coe
                                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                            (coe v17)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                                                           _ -> MAlonzo.RTE.mazUnreachableError)))
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                   (coe
                                                      MAlonzo.Code.Ledger.Prelude.du_finiteness_174
                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                         (coe v3))))) in
                                   let v13
                                         = ltInt
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du_foldr_242
                                                (coe
                                                   (\ v13 ->
                                                      addInt
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                           (coe v13))))
                                                (coe (0 :: Integer))
                                                (coe
                                                   MAlonzo.Code.Data.List.Base.du_deduplicate'7495'_768
                                                   (coe
                                                      (\ v13 v14 ->
                                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                                           (let v15
                                                                  = coe
                                                                      MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                      (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026
                                                                         (coe v0))
                                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                         (coe v13))
                                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                         (coe v14)) in
                                                            let v16
                                                                  = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                      (coe v13) in
                                                            let v17
                                                                  = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                      (coe v14) in
                                                            case coe v15 of
                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v18 v19
                                                                -> if coe v18
                                                                     then coe
                                                                            seq (coe v19)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                               erased
                                                                               (coe
                                                                                  MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                                  erased
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
                                                                                     (coe
                                                                                        eqInt
                                                                                        (coe v16)
                                                                                        (coe
                                                                                           v17)))))
                                                                     else coe
                                                                            seq (coe v19)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                               (coe v18)
                                                                               (coe
                                                                                  MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                                                              _ -> MAlonzo.RTE.mazUnreachableError)))
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                      (coe
                                                         MAlonzo.Code.Ledger.Prelude.du_finiteness_174
                                                         (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                            (coe
                                                               MAlonzo.Code.Ledger.Utxo.d_updateCertDeposits_1226
                                                               (coe v0)
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Utxo.d_pparams_1040
                                                                  (coe v6))
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Transaction.d_txcerts_1030
                                                                  (coe v5))
                                                               (coe
                                                                  MAlonzo.Code.Ledger.Utxo.d_updateProposalDeposits_1238
                                                                  (coe v0)
                                                                  (coe
                                                                     MAlonzo.Code.Ledger.Utxo.d_pparams_1040
                                                                     (coe v6))
                                                                  (coe
                                                                     MAlonzo.Code.Ledger.Transaction.d_txid_1048
                                                                     (coe v5))
                                                                  (coe
                                                                     MAlonzo.Code.Ledger.Transaction.d_txprop_1036
                                                                     (coe v5))
                                                                  (coe v3))))))))
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du_foldr_242
                                                (coe
                                                   (\ v13 ->
                                                      addInt
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                           (coe v13))))
                                                (coe (0 :: Integer))
                                                (coe
                                                   MAlonzo.Code.Data.List.Base.du_deduplicate'7495'_768
                                                   (coe
                                                      (\ v13 v14 ->
                                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                                                           (let v15
                                                                  = coe
                                                                      MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                      (MAlonzo.Code.Ledger.Utxo.d_DecEq'45'DepositPurpose_1026
                                                                         (coe v0))
                                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                         (coe v13))
                                                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                         (coe v14)) in
                                                            let v16
                                                                  = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                      (coe v13) in
                                                            let v17
                                                                  = MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                      (coe v14) in
                                                            case coe v15 of
                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v18 v19
                                                                -> if coe v18
                                                                     then coe
                                                                            seq (coe v19)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                               erased
                                                                               (coe
                                                                                  MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                                                                                  erased
                                                                                  (coe
                                                                                     MAlonzo.Code.Data.Bool.Properties.d_T'63'_3512
                                                                                     (coe
                                                                                        eqInt
                                                                                        (coe v16)
                                                                                        (coe
                                                                                           v17)))))
                                                                     else coe
                                                                            seq (coe v19)
                                                                            (coe
                                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                               (coe v18)
                                                                               (coe
                                                                                  MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                                                              _ -> MAlonzo.RTE.mazUnreachableError)))
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                      (coe
                                                         MAlonzo.Code.Ledger.Prelude.du_finiteness_174
                                                         (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                            (coe v3)))))) in
                                   if coe v13
                                     then coe
                                            MAlonzo.Code.Data.Integer.Base.d_'45'__252
                                            (coe subInt (coe v12) (coe v11))
                                     else coe subInt (coe v11) (coe v12)) in
                        case coe v10 of
                          MAlonzo.Code.Data.Sign.Base.C_'45'_8 -> coe v11
                          _ -> coe (0 :: Integer))
                       (coe
                          MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328
                          (coe du_uDep_1186 (coe v0) (coe v3) (coe v5) (coe v6))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.Properties.DepositHelpers.deposits-change
d_deposits'45'change_1256 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_deposits'45'change_1256 = erased
-- Ledger.Utxo.Properties.DepositHelpers.utxo-ref-prop
d_utxo'45'ref'45'prop_1258 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_utxo'45'ref'45'prop_1258 = erased
-- Ledger.Utxo.Properties.DepositHelpers.rearrange0
d_rearrange0_1284 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rearrange0_1284 = erased
-- Ledger.Utxo.Properties.DepositHelpers.rearrange1
d_rearrange1_1292 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rearrange1_1292 = erased
-- Ledger.Utxo.Properties.UTXO-step
d_UTXO'45'step_1296 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  Maybe MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042
d_UTXO'45'step_1296 v0
  = coe
      MAlonzo.Code.Interface.ComputationalRelation.d_compute_38
      (coe d_Computational'45'UTXO_1058 (coe v0))
-- Ledger.Utxo.Properties.UTXO-step-computes-UTXO
d_UTXO'45'step'45'computes'45'UTXO_1298 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_UTXO'45'step'45'computes'45'UTXO_1298 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.ComputationalRelation.d_'8801''45'just'8660'STS_40
      (d_Computational'45'UTXO_1058 (coe v0)) v1 v2 v3 v4
-- Ledger.Utxo.Properties.pov
d_pov_1300 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pov_1300 = erased
-- Ledger.Utxo.Properties._.dep-ref
d_dep'45'ref_1350 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  MAlonzo.Code.Ledger.Utxo.T_inInterval_1006 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_dep'45'ref_1350 = erased
-- Ledger.Utxo.Properties._.deposits-change
d_deposits'45'change_1352 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  MAlonzo.Code.Ledger.Utxo.T_inInterval_1006 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_deposits'45'change_1352 = erased
-- Ledger.Utxo.Properties._.deposits-change'
d_deposits'45'change''_1354 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  MAlonzo.Code.Ledger.Utxo.T_inInterval_1006 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_deposits'45'change''_1354 = erased
-- Ledger.Utxo.Properties._.rearrange0
d_rearrange0_1356 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  MAlonzo.Code.Ledger.Utxo.T_inInterval_1006 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rearrange0_1356 = erased
-- Ledger.Utxo.Properties._.rearrange1
d_rearrange1_1358 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  MAlonzo.Code.Ledger.Utxo.T_inInterval_1006 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rearrange1_1358 = erased
-- Ledger.Utxo.Properties._.ref-tot-0
d_ref'45'tot'45'0_1360 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  MAlonzo.Code.Ledger.Utxo.T_inInterval_1006 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_ref'45'tot'45'0_1360 = erased
-- Ledger.Utxo.Properties._.ref≤dep
d_ref'8804'dep_1362 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  MAlonzo.Code.Ledger.Utxo.T_inInterval_1006 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_ref'8804'dep_1362 v0 v1 v2 v3 v4 v5 v6 ~v7 ~v8 ~v9 ~v10 ~v11 ~v12
                    ~v13 ~v14
  = du_ref'8804'dep_1362 v0 v1 v2 v3 v4 v5 v6
du_ref'8804'dep_1362 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_ref'8804'dep_1362 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_ref'8804'dep_1242 (coe v0) (coe v2) (coe v4) (coe v5) (coe v6)
      (coe v1) (coe v3)
-- Ledger.Utxo.Properties._.uDep≡
d_uDep'8801'_1364 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  MAlonzo.Code.Ledger.Utxo.T_inInterval_1006 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_uDep'8801'_1364 = erased
-- Ledger.Utxo.Properties._.utxo-ref-prop
d_utxo'45'ref'45'prop_1366 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  (MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
   MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34) ->
  MAlonzo.Code.Ledger.Utxo.T_inInterval_1006 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_utxo'45'ref'45'prop_1366 = erased
