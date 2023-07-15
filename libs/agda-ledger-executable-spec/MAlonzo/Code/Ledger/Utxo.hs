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

module MAlonzo.Code.Ledger.Utxo where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Map
import qualified MAlonzo.Code.Axiom.Set.Map.Dec
import qualified MAlonzo.Code.Axiom.Set.Rel
import qualified MAlonzo.Code.Axiom.Set.Sum
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.Integer.Base
import qualified MAlonzo.Code.Data.Integer.Ext
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Properties
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.Sum.Properties
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.ComputationalRelation
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Interface.Decidable.Instance
import qualified MAlonzo.Code.Ledger.Address
import qualified MAlonzo.Code.Ledger.Crypto
import qualified MAlonzo.Code.Ledger.Deleg
import qualified MAlonzo.Code.Ledger.Epoch
import qualified MAlonzo.Code.Ledger.GovernanceActions
import qualified MAlonzo.Code.Ledger.Interface.HasCoin
import qualified MAlonzo.Code.Ledger.PParams
import qualified MAlonzo.Code.Ledger.Prelude
import qualified MAlonzo.Code.Ledger.TokenAlgebra
import qualified MAlonzo.Code.Ledger.Transaction
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects
import qualified MAlonzo.Code.Tactic.Derive.DecEq

-- Ledger.Utxo._._≤ˢ_
d__'8804''738'__22 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny -> AgdaAny -> ()
d__'8804''738'__22 = erased
-- Ledger.Utxo._.Credential
d_Credential_82 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Credential_82 = erased
-- Ledger.Utxo._.DCert
d_DCert_84 a0 a1 = ()
-- Ledger.Utxo._.GovActionID
d_GovActionID_166 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_GovActionID_166 = erased
-- Ledger.Utxo._.GovProposal
d_GovProposal_170 a0 a1 = ()
-- Ledger.Utxo._.MemoryEstimate
d_MemoryEstimate_194 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_MemoryEstimate_194 = erased
-- Ledger.Utxo._.Network
d_Network_198 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_Network_198 = erased
-- Ledger.Utxo._.Slot
d_Slot_254 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_Slot_254 = erased
-- Ledger.Utxo._.TxBody
d_TxBody_280 a0 = ()
-- Ledger.Utxo._.TxOut
d_TxOut_286 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_TxOut_286 = erased
-- Ledger.Utxo._.UTxO
d_UTxO_290 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_UTxO_290 = erased
-- Ledger.Utxo._.Carrier
d_Carrier_306 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_Carrier_306 = erased
-- Ledger.Utxo._.coin
d_coin_326 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny -> Integer
d_coin_326 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_coin_58
      (coe MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0))
-- Ledger.Utxo._.PParams
d_PParams_742 a0 = ()
-- Ledger.Utxo.HasCoin-Map
d_HasCoin'45'Map_864 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.Interface.HasCoin.T_HasCoin_10
d_HasCoin'45'Map_864 ~v0 ~v1 v2 = du_HasCoin'45'Map_864 v2
du_HasCoin'45'Map_864 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.Interface.HasCoin.T_HasCoin_10
du_HasCoin'45'Map_864 v0
  = coe
      MAlonzo.Code.Ledger.Interface.HasCoin.C_HasCoin'46'constructor_9
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Axiom.Set.Sum.du_indexedSum'7504''7515'_856
              MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'commutativeMonoid_3206
              v0 MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30 (\ v2 -> v2)
              (coe MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910 (coe v1))))
-- Ledger.Utxo.utxoEntrySizeWithoutVal
d_utxoEntrySizeWithoutVal_870 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> Integer
d_utxoEntrySizeWithoutVal_870 ~v0 = du_utxoEntrySizeWithoutVal_870
du_utxoEntrySizeWithoutVal_870 :: Integer
du_utxoEntrySizeWithoutVal_870 = coe (8 :: Integer)
-- Ledger.Utxo.utxoEntrySize
d_utxoEntrySize_872 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_utxoEntrySize_872 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe
             addInt (coe du_utxoEntrySizeWithoutVal_870)
             (coe
                MAlonzo.Code.Ledger.TokenAlgebra.d_size_64
                (MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0)) v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.serSize
d_serSize_878 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny -> Integer
d_serSize_878 ~v0 ~v1 = du_serSize_878
du_serSize_878 :: Integer
du_serSize_878 = coe (0 :: Integer)
-- Ledger.Utxo.outs
d_outs_882 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_outs_882 ~v0 v1 = du_outs_882 v1
du_outs_882 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_outs_882 v0
  = coe
      MAlonzo.Code.Axiom.Set.Map.du_mapKeys_820
      (coe
         MAlonzo.Code.Axiom.Set.d_th_1374
         (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
              (coe MAlonzo.Code.Ledger.Transaction.d_txid_1048 (coe v0))
              (coe v1)))
      (coe MAlonzo.Code.Ledger.Transaction.d_txouts_1022 (coe v0))
-- Ledger.Utxo.balance
d_balance_890 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_balance_890 v0 v1
  = coe
      MAlonzo.Code.Axiom.Set.Sum.du_indexedSum'7504''7515'_856
      (MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
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
      (coe MAlonzo.Code.Ledger.Transaction.du_getValue_1074)
      (coe MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910 (coe v1))
-- Ledger.Utxo.cbalance
d_cbalance_896 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_cbalance_896 v0 v1
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_coin_58
      (MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0))
      (d_balance_890 (coe v0) (coe v1))
-- Ledger.Utxo.minfee
d_minfee_900 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_minfee_900 ~v0 v1 v2 = du_minfee_900 v1 v2
du_minfee_900 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
du_minfee_900 v0 v1
  = coe
      addInt (coe MAlonzo.Code.Ledger.PParams.d_b_180 (coe v0))
      (coe
         mulInt (coe MAlonzo.Code.Ledger.PParams.d_a_178 (coe v0))
         (coe MAlonzo.Code.Ledger.Transaction.d_txsize_1046 (coe v1)))
-- Ledger.Utxo.DepositPurpose
d_DepositPurpose_950 a0 = ()
data T_DepositPurpose_950
  = C_CredentialDeposit_952 MAlonzo.Code.Data.Sum.Base.T__'8846'__30 |
    C_PoolDeposit_954 MAlonzo.Code.Data.Sum.Base.T__'8846'__30 |
    C_DRepDeposit_956 MAlonzo.Code.Data.Sum.Base.T__'8846'__30 |
    C_GovActionDeposit_958 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Ledger.Utxo.certDeposit
d_certDeposit_960 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Deleg.T_DCert_274 ->
  Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_certDeposit_960 ~v0 v1 v2 = du_certDeposit_960 v1 v2
du_certDeposit_960 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Deleg.T_DCert_274 ->
  Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_certDeposit_960 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v1 of
      MAlonzo.Code.Ledger.Deleg.C_delegate_276 v3 v4 v5 v6
        -> coe
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                (coe C_CredentialDeposit_952 (coe v3)) (coe v6))
      MAlonzo.Code.Ledger.Deleg.C_regpool_278 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                (coe C_PoolDeposit_954 (coe v3))
                (coe MAlonzo.Code.Ledger.PParams.d_poolDeposit_184 (coe v0)))
      MAlonzo.Code.Ledger.Deleg.C_regdrep_282 v3 v4 v5
        -> coe
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                (coe C_DRepDeposit_956 (coe v3)) (coe v4))
      _ -> coe v2
-- Ledger.Utxo.certDepositᵐ
d_certDeposit'7504'_974 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Deleg.T_DCert_274 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_certDeposit'7504'_974 ~v0 v1 v2 = du_certDeposit'7504'_974 v1 v2
du_certDeposit'7504'_974 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Deleg.T_DCert_274 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_certDeposit'7504'_974 v0 v1
  = let v2 = coe du_certDeposit_960 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3
        -> coe
             seq (coe v3)
             (coe
                MAlonzo.Code.Axiom.Set.Map.du_'10100'_'10101''7504'_648
                (coe
                   MAlonzo.Code.Axiom.Set.d_th_1374
                   (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                (coe v3))
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
        -> coe
             MAlonzo.Code.Axiom.Set.Map.du_'8709''7504'_488
             (coe
                MAlonzo.Code.Axiom.Set.d_th_1374
                (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.certRefund
d_certRefund_986 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Deleg.T_DCert_274 -> Maybe T_DepositPurpose_950
d_certRefund_986 ~v0 v1 = du_certRefund_986 v1
du_certRefund_986 ::
  MAlonzo.Code.Ledger.Deleg.T_DCert_274 -> Maybe T_DepositPurpose_950
du_certRefund_986 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v0 of
      MAlonzo.Code.Ledger.Deleg.C_delegate_276 v2 v3 v4 v5
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> case coe v4 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                           (coe C_CredentialDeposit_952 (coe v2))
                    _ -> coe v1
             _ -> coe v1
      MAlonzo.Code.Ledger.Deleg.C_deregdrep_284 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
             (coe C_DRepDeposit_956 (coe v2))
      _ -> coe v1
-- Ledger.Utxo.certRefundˢ
d_certRefund'738'_994 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Deleg.T_DCert_274 -> [T_DepositPurpose_950]
d_certRefund'738'_994 ~v0 = du_certRefund'738'_994
du_certRefund'738'_994 ::
  MAlonzo.Code.Ledger.Deleg.T_DCert_274 -> [T_DepositPurpose_950]
du_certRefund'738'_994
  = coe
      MAlonzo.Code.Axiom.Set.du_partialToSet_434
      (coe
         MAlonzo.Code.Axiom.Set.d_th_1374
         (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
      (coe du_certRefund_986)
-- Ledger.Utxo.propDepositᵐ
d_propDeposit'7504'_996 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_propDeposit'7504'_996 ~v0 v1 v2 v3
  = du_propDeposit'7504'_996 v1 v2 v3
du_propDeposit'7504'_996 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_propDeposit'7504'_996 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Ledger.GovernanceActions.C_GovProposal'46'constructor_35551 v3 v4 v5 v6
        -> coe
             seq (coe v3)
             (coe
                MAlonzo.Code.Axiom.Set.Map.du_'10100'_'10101''7504'_648
                (coe
                   MAlonzo.Code.Axiom.Set.d_th_1374
                   (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                (coe
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                   (coe C_GovActionDeposit_958 (coe v1))
                   (coe MAlonzo.Code.Ledger.PParams.d_govDeposit_200 (coe v0))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.inInterval
d_inInterval_1006 a0 a1 a2 = ()
data T_inInterval_1006
  = C_both_1014 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 |
    C_lower_1018 | C_upper_1022 | C_none_1024
-- Ledger.Utxo.DecEq-DepositPurpose
d_DecEq'45'DepositPurpose_1026 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'DepositPurpose_1026 v0
  = coe
      MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63
      (coe
         (\ v1 ->
            case coe v1 of
              C_CredentialDeposit_952 v2
                -> coe
                     (\ v3 ->
                        case coe v3 of
                          C_CredentialDeposit_952 v4
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe
                                       (\ v5 ->
                                          coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8) erased)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                       (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                       (coe
                                          MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                          (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                    (coe
                                       MAlonzo.Code.Data.Sum.Properties.du_'8801''45'dec_54
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                          (coe
                                             MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_khs_240
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                   (coe v0)))))
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                          (coe
                                             MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                             (coe
                                                MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                (coe v0))))
                                       (coe v4) (coe v2)))
                          C_PoolDeposit_954 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_DRepDeposit_956 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_GovActionDeposit_958 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          _ -> MAlonzo.RTE.mazUnreachableError)
              C_PoolDeposit_954 v2
                -> coe
                     (\ v3 ->
                        case coe v3 of
                          C_CredentialDeposit_952 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_PoolDeposit_954 v4
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe
                                       (\ v5 ->
                                          coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8) erased)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                       (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                       (coe
                                          MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                          (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                    (coe
                                       MAlonzo.Code.Data.Sum.Properties.du_'8801''45'dec_54
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                          (coe
                                             MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_khs_240
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                   (coe v0)))))
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                          (coe
                                             MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                             (coe
                                                MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                (coe v0))))
                                       (coe v4) (coe v2)))
                          C_DRepDeposit_956 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_GovActionDeposit_958 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          _ -> MAlonzo.RTE.mazUnreachableError)
              C_DRepDeposit_956 v2
                -> coe
                     (\ v3 ->
                        case coe v3 of
                          C_CredentialDeposit_952 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_PoolDeposit_954 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_DRepDeposit_956 v4
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe
                                       (\ v5 ->
                                          coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8) erased)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                       (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                       (coe
                                          MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                          (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                    (coe
                                       MAlonzo.Code.Data.Sum.Properties.du_'8801''45'dec_54
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                          (coe
                                             MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_khs_240
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                   (coe v0)))))
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                          (coe
                                             MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                             (coe
                                                MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                (coe v0))))
                                       (coe v4) (coe v2)))
                          C_GovActionDeposit_958 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          _ -> MAlonzo.RTE.mazUnreachableError)
              C_GovActionDeposit_958 v2
                -> coe
                     (\ v3 ->
                        case coe v3 of
                          C_CredentialDeposit_952 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_PoolDeposit_954 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_DRepDeposit_956 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_GovActionDeposit_958 v4
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe
                                       (\ v5 ->
                                          coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8) erased)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                       (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                       (coe
                                          MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                          (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                    (coe
                                       MAlonzo.Code.Data.Product.Properties.du_'8801''45'dec_78
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_DecEq'45'TxId_258
                                             (coe v0)))
                                       (coe
                                          (\ v5 ->
                                             MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464))
                                       (coe v4) (coe v2)))
                          _ -> MAlonzo.RTE.mazUnreachableError)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Ledger.Utxo.HasCoin-UTxO
d_HasCoin'45'UTxO_1028 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Interface.HasCoin.T_HasCoin_10
d_HasCoin'45'UTxO_1028 v0
  = coe
      MAlonzo.Code.Ledger.Interface.HasCoin.C_HasCoin'46'constructor_9
      (coe d_cbalance_896 (coe v0))
-- Ledger.Utxo.Deposits
d_Deposits_1030 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_Deposits_1030 = erased
-- Ledger.Utxo.UTxOEnv
d_UTxOEnv_1032 a0 = ()
data T_UTxOEnv_1032
  = C_UTxOEnv'46'constructor_24005 AgdaAny
                                   MAlonzo.Code.Ledger.PParams.T_PParams_128
-- Ledger.Utxo.UTxOEnv.slot
d_slot_1038 :: T_UTxOEnv_1032 -> AgdaAny
d_slot_1038 v0
  = case coe v0 of
      C_UTxOEnv'46'constructor_24005 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.UTxOEnv.pparams
d_pparams_1040 ::
  T_UTxOEnv_1032 -> MAlonzo.Code.Ledger.PParams.T_PParams_128
d_pparams_1040 v0
  = case coe v0 of
      C_UTxOEnv'46'constructor_24005 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.UTxOState
d_UTxOState_1042 a0 = ()
data T_UTxOState_1042
  = C_'10214'_'44'_'44'_'44'_'10215''7512'_1060 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                                Integer MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                                Integer
-- Ledger.Utxo.UTxOState.utxo
d_utxo_1052 ::
  T_UTxOState_1042 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_utxo_1052 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'44'_'10215''7512'_1060 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.UTxOState.fees
d_fees_1054 :: T_UTxOState_1042 -> Integer
d_fees_1054 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'44'_'10215''7512'_1060 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.UTxOState.deposits
d_deposits_1056 ::
  T_UTxOState_1042 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_deposits_1056 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'44'_'10215''7512'_1060 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.UTxOState.donations
d_donations_1058 :: T_UTxOState_1042 -> Integer
d_donations_1058 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'44'_'10215''7512'_1060 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.⟦_⟧
d_'10214'_'10215'_1064 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  () -> AgdaAny -> AgdaAny
d_'10214'_'10215'_1064 ~v0 ~v1 v2 = du_'10214'_'10215'_1064 v2
du_'10214'_'10215'_1064 :: AgdaAny -> AgdaAny
du_'10214'_'10215'_1064 v0 = coe v0
-- Ledger.Utxo.∈-inst
d_'8712''45'inst_1076 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  MAlonzo.Code.Interface.Decidable.Instance.T_Dec'8321'_24
d_'8712''45'inst_1076 ~v0 ~v1 v2 v3 = du_'8712''45'inst_1076 v2 v3
du_'8712''45'inst_1076 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  MAlonzo.Code.Interface.Decidable.Instance.T_Dec'8321'_24
du_'8712''45'inst_1076 v0 v1
  = coe
      MAlonzo.Code.Interface.Decidable.Instance.C_Dec'8321''46'constructor_167
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Axiom.Set.d__'8712''63'__1498
              MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 erased v0 v2
              v1))
-- Ledger.Utxo.all?'
d_all'63'''_1092 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  () ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Interface.Decidable.Instance.T_Dec'8321'_24 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_all'63'''_1092 ~v0 ~v1 ~v2 v3 v4 v5 = du_all'63'''_1092 v3 v4 v5
du_all'63'''_1092 ::
  MAlonzo.Code.Interface.Decidable.Instance.T_Dec'8321'_24 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_all'63'''_1092 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Interface.Decidable.Instance.C_Dec'8321''46'constructor_167 v3
        -> coe
             MAlonzo.Code.Axiom.Set.d_all'63'_1506
             MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 erased v1
             erased v3 v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.netId?
d_netId'63'_1106 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Interface.Decidable.Instance.T_Dec'8321'_24
d_netId'63'_1106 v0 ~v1 v2 v3 = du_netId'63'_1106 v0 v2 v3
du_netId'63'_1106 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Interface.Decidable.Instance.T_Dec'8321'_24
du_netId'63'_1106 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Decidable.Instance.C_Dec'8321''46'constructor_167
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Interface.DecEq.d__'8799'__20
              (MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Netw_262 (coe v0))
              (coe v2 v3) v1))
-- Ledger.Utxo.Dec-inInterval
d_Dec'45'inInterval_1118 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Dec'45'inInterval_1118 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v5
               -> case coe v4 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v6
                      -> let v7
                               = MAlonzo.Code.Ledger.Epoch.d__'8804''738''63'__84
                                   (coe
                                      MAlonzo.Code.Ledger.Transaction.d_epochStructure_186 (coe v0))
                                   (coe v5) (coe v1) in
                         let v8
                               = MAlonzo.Code.Ledger.Epoch.d__'8804''738''63'__84
                                   (coe
                                      MAlonzo.Code.Ledger.Transaction.d_epochStructure_186 (coe v0))
                                   (coe v1) (coe v6) in
                         case coe v7 of
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v9 v10
                             -> if coe v9
                                  then case coe v10 of
                                         MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v11
                                           -> case coe v8 of
                                                MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v12 v13
                                                  -> if coe v12
                                                       then case coe v13 of
                                                              MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v14
                                                                -> coe
                                                                     MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                     (coe v12)
                                                                     (coe
                                                                        MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                                                        (coe
                                                                           C_both_1014
                                                                           (coe
                                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                              (coe v11) (coe v14))))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       else coe
                                                              seq (coe v13)
                                                              (coe
                                                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                 (coe v12)
                                                                 (coe
                                                                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  else coe
                                         seq (coe v10)
                                         (coe
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                            (coe v9)
                                            (coe
                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> let v6
                               = MAlonzo.Code.Ledger.Epoch.d__'8804''738''63'__84
                                   (coe
                                      MAlonzo.Code.Ledger.Transaction.d_epochStructure_186 (coe v0))
                                   (coe v5) (coe v1) in
                         case coe v6 of
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v7 v8
                             -> if coe v7
                                  then coe
                                         seq (coe v8)
                                         (coe
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                            (coe v7)
                                            (coe
                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                               (coe C_lower_1018)))
                                  else coe
                                         seq (coe v8)
                                         (coe
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                            (coe v7)
                                            (coe
                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> case coe v4 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v5
                      -> let v6
                               = MAlonzo.Code.Ledger.Epoch.d__'8804''738''63'__84
                                   (coe
                                      MAlonzo.Code.Ledger.Transaction.d_epochStructure_186 (coe v0))
                                   (coe v1) (coe v5) in
                         case coe v6 of
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v7 v8
                             -> if coe v7
                                  then coe
                                         seq (coe v8)
                                         (coe
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                            (coe v7)
                                            (coe
                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                               (coe C_upper_1022)))
                                  else coe
                                         seq (coe v8)
                                         (coe
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                            (coe v7)
                                            (coe
                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> coe
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                           (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                           (coe
                              MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                              (coe C_none_1024))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.HasCoin-UTxOState
d_HasCoin'45'UTxOState_1220 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Interface.HasCoin.T_HasCoin_10
d_HasCoin'45'UTxOState_1220 v0
  = coe
      MAlonzo.Code.Ledger.Interface.HasCoin.C_HasCoin'46'constructor_9
      (coe
         (\ v1 ->
            addInt
              (coe
                 addInt
                 (coe
                    addInt (coe d_donations_1058 (coe v1)) (coe d_fees_1054 (coe v1)))
                 (coe d_cbalance_896 (coe v0) (coe d_utxo_1052 (coe v1))))
              (coe
                 MAlonzo.Code.Axiom.Set.Sum.du_indexedSum'7504''7515'_856
                 MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'commutativeMonoid_3206
                 (d_DecEq'45'DepositPurpose_1026 (coe v0))
                 MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30 (\ v2 -> v2)
                 (coe
                    MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910
                    (coe d_deposits_1056 (coe v1))))))
-- Ledger.Utxo._⊢_⇀⦇_,UTXO⦈_
d__'8866'_'8640''10631'_'44'UTXO'10632'__1224 a0 a1 a2 a3 a4 = ()
data T__'8866'_'8640''10631'_'44'UTXO'10632'__1224
  = C_UTXO'45'inductive_1346 (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
                              MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
                              MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34)
                             T_inInterval_1006 MAlonzo.Code.Data.Nat.Base.T__'8804'__18
                             MAlonzo.Code.Data.Nat.Base.T__'8804'__18
-- Ledger.Utxo.updateCertDeposits
d_updateCertDeposits_1226 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  [MAlonzo.Code.Ledger.Deleg.T_DCert_274] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_updateCertDeposits_1226 v0 v1 v2 v3
  = case coe v2 of
      [] -> coe v3
      (:) v4 v5
        -> coe
             MAlonzo.Code.Axiom.Set.Map.du__'8739'_'7580'_1118
             (coe
                MAlonzo.Code.Axiom.Set.d_th_1374
                (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
             (coe
                MAlonzo.Code.Axiom.Set.d_'8712''45'sp_1496
                MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 erased
                (d_DecEq'45'DepositPurpose_1026 (coe v0)))
             (coe
                MAlonzo.Code.Axiom.Set.Map.Dec.du__'8746''8314'__604
                MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12
                MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'monoid_3204
                (d_DecEq'45'DepositPurpose_1026 (coe v0))
                (d_updateCertDeposits_1226 (coe v0) (coe v1) (coe v5) (coe v3))
                (coe du_certDeposit'7504'_974 (coe v1) (coe v4)))
             (coe du_certRefund'738'_994 v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.updateProposalDeposits
d_updateProposalDeposits_1238 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  AgdaAny ->
  [MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_updateProposalDeposits_1238 v0 v1 v2 v3 v4
  = case coe v3 of
      [] -> coe v4
      (:) v5 v6
        -> coe
             MAlonzo.Code.Axiom.Set.Map.Dec.du__'8746''8314'__604
             MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12
             MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'monoid_3204
             (d_DecEq'45'DepositPurpose_1026 (coe v0))
             (d_updateProposalDeposits_1238
                (coe v0) (coe v1) (coe v2) (coe v6) (coe v4))
             (coe
                du_propDeposit'7504'_996 (coe v1)
                (coe
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                   (coe MAlonzo.Code.Data.List.Base.du_length_304 v6))
                (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Utxo.updateDeposits
d_updateDeposits_1254 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_updateDeposits_1254 v0 v1 v2 v3
  = coe
      d_updateCertDeposits_1226 (coe v0) (coe v1)
      (coe MAlonzo.Code.Ledger.Transaction.d_txcerts_1030 (coe v2))
      (coe
         d_updateProposalDeposits_1238 (coe v0) (coe v1)
         (coe MAlonzo.Code.Ledger.Transaction.d_txid_1048 (coe v2))
         (coe MAlonzo.Code.Ledger.Transaction.d_txprop_1036 (coe v2))
         (coe v3))
-- Ledger.Utxo.depositsChange
d_depositsChange_1260 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_depositsChange_1260 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Integer.Base.d__'8854'__258
      (coe
         MAlonzo.Code.Axiom.Set.Sum.du_indexedSum'7504''7515'_856
         MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'commutativeMonoid_3206
         (d_DecEq'45'DepositPurpose_1026 (coe v0))
         MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30 (\ v4 -> v4)
         (coe
            MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910
            (coe d_updateDeposits_1254 (coe v0) (coe v1) (coe v2) (coe v3))))
      (coe
         MAlonzo.Code.Axiom.Set.Sum.du_indexedSum'7504''7515'_856
         MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'commutativeMonoid_3206
         (d_DecEq'45'DepositPurpose_1026 (coe v0))
         MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30 (\ v4 -> v4)
         (coe MAlonzo.Code.Ledger.Prelude.du__'7584''7504'_910 (coe v3)))
-- Ledger.Utxo.depositRefunds
d_depositRefunds_1268 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_depositRefunds_1268 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Integer.Ext.d_negPart_24
      (coe
         d_depositsChange_1260 (coe v0) (coe v1) (coe v3)
         (coe d_deposits_1056 (coe v2)))
-- Ledger.Utxo._._.deposits
d_deposits_1282 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_deposits_1282 ~v0 v1 ~v2 = du_deposits_1282 v1
du_deposits_1282 ::
  T_UTxOState_1042 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_deposits_1282 v0 = coe d_deposits_1056 (coe v0)
-- Ledger.Utxo._._.donations
d_donations_1284 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_donations_1284 ~v0 v1 ~v2 = du_donations_1284 v1
du_donations_1284 :: T_UTxOState_1042 -> Integer
du_donations_1284 v0 = coe d_donations_1058 (coe v0)
-- Ledger.Utxo._._.fees
d_fees_1286 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_fees_1286 ~v0 v1 ~v2 = du_fees_1286 v1
du_fees_1286 :: T_UTxOState_1042 -> Integer
du_fees_1286 v0 = coe d_fees_1054 (coe v0)
-- Ledger.Utxo._._.utxo
d_utxo_1288 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_utxo_1288 ~v0 v1 ~v2 = du_utxo_1288 v1
du_utxo_1288 ::
  T_UTxOState_1042 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_utxo_1288 v0 = coe d_utxo_1052 (coe v0)
-- Ledger.Utxo.newDeposits
d_newDeposits_1290 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_newDeposits_1290 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Integer.Ext.d_posPart_10
      (coe
         d_depositsChange_1260 (coe v0) (coe v1) (coe v3)
         (coe d_deposits_1056 (coe v2)))
-- Ledger.Utxo._._.deposits
d_deposits_1304 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_deposits_1304 ~v0 v1 ~v2 = du_deposits_1304 v1
du_deposits_1304 ::
  T_UTxOState_1042 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_deposits_1304 v0 = coe d_deposits_1056 (coe v0)
-- Ledger.Utxo._._.donations
d_donations_1306 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_donations_1306 ~v0 v1 ~v2 = du_donations_1306 v1
du_donations_1306 :: T_UTxOState_1042 -> Integer
du_donations_1306 v0 = coe d_donations_1058 (coe v0)
-- Ledger.Utxo._._.fees
d_fees_1308 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_fees_1308 ~v0 v1 ~v2 = du_fees_1308 v1
du_fees_1308 :: T_UTxOState_1042 -> Integer
du_fees_1308 v0 = coe d_fees_1054 (coe v0)
-- Ledger.Utxo._._.utxo
d_utxo_1310 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_utxo_1310 ~v0 v1 ~v2 = du_utxo_1310 v1
du_utxo_1310 ::
  T_UTxOState_1042 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_utxo_1310 v0 = coe d_utxo_1052 (coe v0)
-- Ledger.Utxo.consumed
d_consumed_1312 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> AgdaAny
d_consumed_1312 v0 v1 v2 v3
  = let v4
          = MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.d__'8729'__840
      (MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
         (coe v4))
      (let v5
             = MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0) in
       coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__840
         (MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
            (coe v5))
         (d_balance_890
            (coe v0)
            (coe
               MAlonzo.Code.Axiom.Set.Map.du__'8739'__1110
               (coe
                  MAlonzo.Code.Axiom.Set.d_th_1374
                  (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
               (coe
                  MAlonzo.Code.Axiom.Set.d_'8712''45'sp_1496
                  MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 erased
                  (coe
                     MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                     (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'TxId_258 (coe v0))
                     (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Ix_260 (coe v0))))
               (coe d_utxo_1052 (coe v2))
               (coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v3))))
         (MAlonzo.Code.Ledger.Transaction.d_mint_1026 (coe v3)))
      (coe
         MAlonzo.Code.Ledger.TokenAlgebra.d_inject_60
         (MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0))
         (d_depositRefunds_1268 (coe v0) (coe v1) (coe v2) (coe v3)))
-- Ledger.Utxo.produced
d_produced_1320 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> AgdaAny
d_produced_1320 v0 v1 v2 v3
  = let v4
          = MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.d__'8729'__840
      (MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
         (coe v4))
      (let v5
             = MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0) in
       coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__840
         (MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
            (coe v5))
         (let v6
                = MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0) in
          coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__840
            (MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
               (coe v6))
            (d_balance_890 (coe v0) (coe du_outs_882 (coe v3)))
            (coe
               MAlonzo.Code.Ledger.TokenAlgebra.d_inject_60
               (MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0))
               (MAlonzo.Code.Ledger.Transaction.d_txfee_1024 (coe v3))))
         (coe
            MAlonzo.Code.Ledger.TokenAlgebra.d_inject_60
            (MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0))
            (d_newDeposits_1290 (coe v0) (coe v1) (coe v2) (coe v3))))
      (coe
         MAlonzo.Code.Ledger.TokenAlgebra.d_inject_60
         (MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0))
         (MAlonzo.Code.Ledger.Transaction.d_txdonation_1038 (coe v3)))
-- Ledger.Utxo.Computational-UTXO
d_Computational'45'UTXO_1348 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.ComputationalRelation.T_Computational_32
d_Computational'45'UTXO_1348 v0
  = coe
      MAlonzo.Code.Interface.ComputationalRelation.C_MkComputational_42
      (coe
         (\ v1 v2 v3 ->
            coe
              MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
              (coe
                 MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104
                 MAlonzo.Code.Agda.Primitive.d_lzero_16 erased
                 (coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'8594''45'dec__82
                       (coe
                          MAlonzo.Code.Ledger.Prelude.du_'8799''45''8709'_220
                          (coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v3)))
                       (coe
                          MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                          (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                          (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)))
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                       (coe
                          du_all'63'''_1092
                          (coe
                             du_'8712''45'inst_1076
                             (coe
                                MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                                (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'TxId_258 (coe v0))
                                (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Ix_260 (coe v0)))
                             (coe
                                MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                                (MAlonzo.Code.Axiom.Set.d_th_1374
                                   (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                (coe
                                   MAlonzo.Code.Axiom.Set.Map.du__'738'_458
                                   (coe d_utxo_1052 (coe v2)))))
                          (coe
                             MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                             (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'TxId_258 (coe v0))
                             (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Ix_260 (coe v0)))
                          (coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v3)))
                       (coe
                          MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                          (coe
                             d_Dec'45'inInterval_1118 (coe v0) (coe d_slot_1038 (coe v1))
                             (coe MAlonzo.Code.Ledger.Transaction.d_txvldt_1028 (coe v3)))
                          (coe
                             MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                             (coe
                                MAlonzo.Code.Data.Nat.Properties.d__'8804''63'__2612
                                (coe du_minfee_900 (coe d_pparams_1040 (coe v1)) (coe v3))
                                (coe MAlonzo.Code.Ledger.Transaction.d_txfee_1024 (coe v3)))
                             (coe
                                MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                (coe
                                   MAlonzo.Code.Interface.Decidable.Instance.du_DecEq'8658'Dec_88
                                   (coe
                                      MAlonzo.Code.Ledger.TokenAlgebra.d_DecEq'45'Value_72
                                      (coe
                                         MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256
                                         (coe v0)))
                                   (coe
                                      d_consumed_1312 (coe v0) (coe d_pparams_1040 (coe v1))
                                      (coe v2) (coe v3))
                                   (coe
                                      d_produced_1320 (coe v0) (coe d_pparams_1040 (coe v1))
                                      (coe v2) (coe v3)))
                                (coe
                                   MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                   (coe
                                      MAlonzo.Code.Interface.Decidable.Instance.du_DecEq'8658'Dec_88
                                      (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                                      (coe
                                         MAlonzo.Code.Ledger.TokenAlgebra.d_coin_58
                                         (MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256
                                            (coe v0))
                                         (MAlonzo.Code.Ledger.Transaction.d_mint_1026 (coe v3)))
                                      (coe (0 :: Integer)))
                                   (coe
                                      MAlonzo.Code.Data.Nat.Properties.d__'8804''63'__2612
                                      (coe MAlonzo.Code.Ledger.Transaction.d_txsize_1046 (coe v3))
                                      (coe
                                         MAlonzo.Code.Ledger.PParams.d_maxTxSize_170
                                         (coe d_pparams_1040 (coe v1)))))))))))
              (coe
                 MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                 (coe
                    C_'10214'_'44'_'44'_'44'_'10215''7512'_1060
                    (coe
                       MAlonzo.Code.Axiom.Set.Map.du__'8746''7504''737'__666
                       (coe
                          MAlonzo.Code.Axiom.Set.d_th_1374
                          (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                       (coe
                          MAlonzo.Code.Axiom.Set.d_'8712''45'sp_1496
                          MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 erased
                          (coe
                             MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                             (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'TxId_258 (coe v0))
                             (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Ix_260 (coe v0))))
                       (coe
                          MAlonzo.Code.Axiom.Set.Map.du__'8739'_'7580'_1118
                          (coe
                             MAlonzo.Code.Axiom.Set.d_th_1374
                             (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                          (coe
                             MAlonzo.Code.Axiom.Set.d_'8712''45'sp_1496
                             MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 erased
                             (coe
                                MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                                (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'TxId_258 (coe v0))
                                (coe MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Ix_260 (coe v0))))
                          (coe d_utxo_1052 (coe v2))
                          (coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v3)))
                       (coe du_outs_882 (coe v3)))
                    (coe
                       addInt (coe d_fees_1054 (coe v2))
                       (coe MAlonzo.Code.Ledger.Transaction.d_txfee_1024 (coe v3)))
                    (coe
                       d_updateDeposits_1254 (coe v0) (coe d_pparams_1040 (coe v1))
                       (coe v3) (coe d_deposits_1056 (coe v2)))
                    (coe
                       addInt (coe d_donations_1058 (coe v2))
                       (coe MAlonzo.Code.Ledger.Transaction.d_txdonation_1038 (coe v3)))))
              (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)))
      (coe
         (\ v1 v2 v3 v4 ->
            coe
              MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
              (coe
                 (\ v5 ->
                    let v6
                          = coe
                              MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                              (coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'8594''45'dec__82
                                 (coe
                                    MAlonzo.Code.Ledger.Prelude.du_'8799''45''8709'_220
                                    (coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v3)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)))
                              (coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                 (coe
                                    du_all'63'''_1092
                                    (coe
                                       du_'8712''45'inst_1076
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_DecEq'45'TxId_258
                                             (coe v0))
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Ix_260
                                             (coe v0)))
                                       (coe
                                          MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                                          (MAlonzo.Code.Axiom.Set.d_th_1374
                                             (coe
                                                MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                          (coe
                                             MAlonzo.Code.Axiom.Set.Map.du__'738'_458
                                             (coe d_utxo_1052 (coe v2)))))
                                    (coe
                                       MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                                       (coe
                                          MAlonzo.Code.Ledger.Transaction.d_DecEq'45'TxId_258
                                          (coe v0))
                                       (coe
                                          MAlonzo.Code.Ledger.Transaction.d_DecEq'45'Ix_260
                                          (coe v0)))
                                    (coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v3)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                    (coe
                                       d_Dec'45'inInterval_1118 (coe v0) (coe d_slot_1038 (coe v1))
                                       (coe MAlonzo.Code.Ledger.Transaction.d_txvldt_1028 (coe v3)))
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                       (coe
                                          MAlonzo.Code.Data.Nat.Properties.d__'8804''63'__2612
                                          (coe du_minfee_900 (coe d_pparams_1040 (coe v1)) (coe v3))
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_txfee_1024 (coe v3)))
                                       (coe
                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                          (coe
                                             MAlonzo.Code.Interface.Decidable.Instance.du_DecEq'8658'Dec_88
                                             (coe
                                                MAlonzo.Code.Ledger.TokenAlgebra.d_DecEq'45'Value_72
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256
                                                   (coe v0)))
                                             (coe
                                                d_consumed_1312 (coe v0)
                                                (coe d_pparams_1040 (coe v1)) (coe v2) (coe v3))
                                             (coe
                                                d_produced_1320 (coe v0)
                                                (coe d_pparams_1040 (coe v1)) (coe v2) (coe v3)))
                                          (coe
                                             MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                             (coe
                                                MAlonzo.Code.Interface.Decidable.Instance.du_DecEq'8658'Dec_88
                                                (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                                                (coe
                                                   MAlonzo.Code.Ledger.TokenAlgebra.d_coin_58
                                                   (MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256
                                                      (coe v0))
                                                   (MAlonzo.Code.Ledger.Transaction.d_mint_1026
                                                      (coe v3)))
                                                (coe (0 :: Integer)))
                                             (coe
                                                MAlonzo.Code.Data.Nat.Properties.d__'8804''63'__2612
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_txsize_1046
                                                   (coe v3))
                                                (coe
                                                   MAlonzo.Code.Ledger.PParams.d_maxTxSize_170
                                                   (coe d_pparams_1040 (coe v1))))))))) in
                    case coe v6 of
                      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v7 v8
                        -> coe
                             seq (coe v7)
                             (case coe v8 of
                                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v9
                                  -> coe
                                       C_UTXO'45'inductive_1346
                                       (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                          (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v9)))
                                       (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v9))))
                                       (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                   (coe v9)))))
                                       (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                      (coe
                                                         MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                         (coe v9)))))))
                                _ -> MAlonzo.RTE.mazUnreachableError)
                      _ -> MAlonzo.RTE.mazUnreachableError))
              erased))
