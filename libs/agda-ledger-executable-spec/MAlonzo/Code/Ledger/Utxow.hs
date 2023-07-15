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

module MAlonzo.Code.Ledger.Utxow where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Map
import qualified MAlonzo.Code.Axiom.Set.Rel
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Sum
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Ledger.Address
import qualified MAlonzo.Code.Ledger.Deleg
import qualified MAlonzo.Code.Ledger.GovernanceActions
import qualified MAlonzo.Code.Ledger.PParams
import qualified MAlonzo.Code.Ledger.Prelude
import qualified MAlonzo.Code.Ledger.Transaction
import qualified MAlonzo.Code.Ledger.Utxo

-- Ledger.Utxow._.Credential
d_Credential_82 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Credential_82 = erased
-- Ledger.Utxow._.THash
d_THash_190 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_THash_190 = erased
-- Ledger.Utxow._.P1Script
d_P1Script_210 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_P1Script_210 = erased
-- Ledger.Utxow._.ScriptHash
d_ScriptHash_248 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_ScriptHash_248 = erased
-- Ledger.Utxow._.Tx
d_Tx_278 a0 = ()
-- Ledger.Utxow._.TxBody
d_TxBody_280 a0 = ()
-- Ledger.Utxow._.TxWitnesses
d_TxWitnesses_288 a0 = ()
-- Ledger.Utxow._.UTxO
d_UTxO_290 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_UTxO_290 = erased
-- Ledger.Utxow._.isSigned
d_isSigned_364 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny -> AgdaAny -> AgdaAny -> ()
d_isSigned_364 = erased
-- Ledger.Utxow._.validP1Script
d_validP1Script_440 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> ()
d_validP1Script_440 = erased
-- Ledger.Utxow._._⊢_⇀⦇_,UTXO⦈_
d__'8866'_'8640''10631'_'44'UTXO'10632'__724 a0 a1 a2 a3 a4 = ()
-- Ledger.Utxow._.UTxOEnv
d_UTxOEnv_752 a0 = ()
-- Ledger.Utxow._.UTxOState
d_UTxOState_754 a0 = ()
-- Ledger.Utxow.getVKeys
d_getVKeys_856 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  [MAlonzo.Code.Data.Sum.Base.T__'8846'__30] -> [AgdaAny]
d_getVKeys_856 ~v0 = du_getVKeys_856
du_getVKeys_856 ::
  [MAlonzo.Code.Data.Sum.Base.T__'8846'__30] -> [AgdaAny]
du_getVKeys_856
  = coe
      MAlonzo.Code.Axiom.Set.du_mapPartial_538
      (coe
         MAlonzo.Code.Axiom.Set.d_th_1374
         (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
      (coe MAlonzo.Code.Data.Sum.du_isInj'8321'_24)
-- Ledger.Utxow.getScripts
d_getScripts_858 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  [MAlonzo.Code.Data.Sum.Base.T__'8846'__30] -> [AgdaAny]
d_getScripts_858 ~v0 = du_getScripts_858
du_getScripts_858 ::
  [MAlonzo.Code.Data.Sum.Base.T__'8846'__30] -> [AgdaAny]
du_getScripts_858
  = coe
      MAlonzo.Code.Axiom.Set.du_mapPartial_538
      (coe
         MAlonzo.Code.Axiom.Set.d_th_1374
         (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
      (coe MAlonzo.Code.Data.Sum.du_isInj'8322'_30)
-- Ledger.Utxow.witsVKeyNeeded
d_witsVKeyNeeded_860 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> [AgdaAny]
d_witsVKeyNeeded_860 v0 v1 v2
  = coe
      du_getVKeys_856
      (coe
         MAlonzo.Code.Axiom.Set.du__'8746'__642
         (coe
            MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
         (coe
            MAlonzo.Code.Axiom.Set.du_map_360
            (MAlonzo.Code.Axiom.Set.d_th_1374
               (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
            (\ v3 ->
               coe
                 MAlonzo.Code.Ledger.Address.du_payCred_90
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3)))
            (coe
               MAlonzo.Code.Axiom.Set.Rel.du__'10218''36''10219'__580
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
               (coe MAlonzo.Code.Axiom.Set.Map.du__'738'_458 (coe v1))
               (coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v2))))
         (coe
            MAlonzo.Code.Axiom.Set.du_map_360
            (MAlonzo.Code.Axiom.Set.d_th_1374
               (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
            (\ v3 ->
               MAlonzo.Code.Ledger.GovernanceActions.d_credential_526 (coe v3))
            (MAlonzo.Code.Ledger.Transaction.d_txvote_1034 (coe v2))))
-- Ledger.Utxow.scriptsNeeded
d_scriptsNeeded_866 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> [AgdaAny]
d_scriptsNeeded_866 v0 v1 v2
  = coe
      du_getScripts_858
      (coe
         MAlonzo.Code.Axiom.Set.du__'8746'__642
         (coe
            MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
         (coe
            MAlonzo.Code.Axiom.Set.du_map_360
            (MAlonzo.Code.Axiom.Set.d_th_1374
               (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
            (\ v3 ->
               coe
                 MAlonzo.Code.Ledger.Address.du_payCred_90
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3)))
            (coe
               MAlonzo.Code.Axiom.Set.Rel.du__'10218''36''10219'__580
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
               (coe MAlonzo.Code.Axiom.Set.Map.du__'738'_458 (coe v1))
               (coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v2))))
         (coe
            MAlonzo.Code.Axiom.Set.du_map_360
            (MAlonzo.Code.Axiom.Set.d_th_1374
               (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
            (\ v3 ->
               MAlonzo.Code.Ledger.GovernanceActions.d_credential_526 (coe v3))
            (MAlonzo.Code.Ledger.Transaction.d_txvote_1034 (coe v2))))
-- Ledger.Utxow.scriptsP1
d_scriptsP1_872 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Transaction.T_TxWitnesses_1050 -> [AgdaAny]
d_scriptsP1_872 ~v0 v1 = du_scriptsP1_872 v1
du_scriptsP1_872 ::
  MAlonzo.Code.Ledger.Transaction.T_TxWitnesses_1050 -> [AgdaAny]
du_scriptsP1_872 v0
  = coe
      MAlonzo.Code.Axiom.Set.du_mapPartial_538
      (MAlonzo.Code.Axiom.Set.d_th_1374
         (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
      (coe MAlonzo.Code.Data.Sum.du_isInj'8321'_24)
      (MAlonzo.Code.Ledger.Transaction.d_scripts_1058 (coe v0))
-- Ledger.Utxow._⊢_⇀⦇_,UTXOW⦈_
d__'8866'_'8640''10631'_'44'UTXOW'10632'__876 a0 a1 a2 a3 a4 = ()
data T__'8866'_'8640''10631'_'44'UTXOW'10632'__876
  = C_UTXOW'45'inductive_906 (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
                              MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 -> AgdaAny)
                             (AgdaAny ->
                              MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 -> AgdaAny)
                             (AgdaAny ->
                              MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
                              MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34)
                             MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                             MAlonzo.Code.Ledger.Utxo.T__'8866'_'8640''10631'_'44'UTXO'10632'__1224
-- Ledger.Utxow.All′
d_All'8242'_878 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  () -> (AgdaAny -> ()) -> [AgdaAny] -> ()
d_All'8242'_878 = erased
