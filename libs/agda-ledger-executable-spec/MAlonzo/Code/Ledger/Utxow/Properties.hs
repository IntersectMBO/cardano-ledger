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

module MAlonzo.Code.Ledger.Utxow.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Map
import qualified MAlonzo.Code.Axiom.Set.Rel
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.ComputationalRelation
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Interface.Decidable.Instance
import qualified MAlonzo.Code.Interface.Hashable
import qualified MAlonzo.Code.Ledger.Crypto
import qualified MAlonzo.Code.Ledger.Prelude
import qualified MAlonzo.Code.Ledger.Script
import qualified MAlonzo.Code.Ledger.Transaction
import qualified MAlonzo.Code.Ledger.Utxo
import qualified MAlonzo.Code.Ledger.Utxow
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Ledger.Utxow.Properties._.Ser
d_Ser_252 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_Ser_252 = erased
-- Ledger.Utxow.Properties._.Sig
d_Sig_254 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_Sig_254 = erased
-- Ledger.Utxow.Properties._.Tx
d_Tx_280 a0 = ()
-- Ledger.Utxow.Properties._.VKey
d_VKey_298 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 -> ()
d_VKey_298 = erased
-- Ledger.Utxow.Properties._.validP1Script
d_validP1Script_442 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> ()
d_validP1Script_442 = erased
-- Ledger.Utxow.Properties._._⊢_⇀⦇_,UTXOW⦈_
d__'8866'_'8640''10631'_'44'UTXOW'10632'__726 a0 a1 a2 a3 a4 = ()
-- Ledger.Utxow.Properties._.UTxOState
d_UTxOState_778 a0 = ()
-- Ledger.Utxow.Properties.sigCheck
d_sigCheck_886 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d_sigCheck_886 = erased
-- Ledger.Utxow.Properties.sig-inst
d_sig'45'inst_898 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  AgdaAny -> MAlonzo.Code.Interface.Decidable.Instance.T_Dec'8321'_24
d_sig'45'inst_898 v0 v1
  = coe
      MAlonzo.Code.Interface.Decidable.Instance.C_Dec'8321''46'constructor_167
      (coe
         (\ v2 ->
            case coe v2 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
                -> let v5
                         = MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0) in
                   coe
                     MAlonzo.Code.Ledger.Crypto.d_isSigned'63'_144
                     (MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v5)) v3 v1 v4
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Ledger.Utxow.Properties.valid-inst
d_valid'45'inst_912 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Interface.Decidable.Instance.T_Dec'8321'_24
d_valid'45'inst_912 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Decidable.Instance.C_Dec'8321''46'constructor_167
      (coe
         MAlonzo.Code.Ledger.Script.d_validP1Script'63'_40
         (MAlonzo.Code.Ledger.Script.d_p1s_126
            (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0)))
         v1 v2)
-- Ledger.Utxow.Properties.Computational-Property
d_Computational'45'Property_918 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_Tx_1060 -> ()
d_Computational'45'Property_918 = erased
-- Ledger.Utxow.Properties.Computational-UTXOW
d_Computational'45'UTXOW_940 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.ComputationalRelation.T_Computational_32
d_Computational'45'UTXOW_940 v0
  = coe
      MAlonzo.Code.Interface.ComputationalRelation.C_MkComputational_42
      (coe
         (\ v1 v2 v3 ->
            coe
              MAlonzo.Code.Interface.Decidable.Instance.du_if'7496'_then_else__44
              (coe ())
              (coe
                 MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                 (coe
                    MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                    (coe
                       d_sig'45'inst_898 (coe v0)
                       (coe
                          MAlonzo.Code.Ledger.Transaction.d_txidBytes_252 v0
                          (MAlonzo.Code.Ledger.Transaction.d_txid_1048
                             (coe MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v3)))))
                    (coe
                       MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                       (coe
                          MAlonzo.Code.Ledger.Crypto.d_decEq'45'VKey_158
                          (coe
                             MAlonzo.Code.Ledger.Crypto.d_pkk_210
                             (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0))))
                       (coe
                          MAlonzo.Code.Ledger.Crypto.d_decEq'45'Sig_160
                          (coe
                             MAlonzo.Code.Ledger.Crypto.d_pkk_210
                             (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0)))))
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                       (coe
                          MAlonzo.Code.Ledger.Transaction.d_vkSigs_1056
                          (coe MAlonzo.Code.Ledger.Transaction.d_wits_1070 (coe v3)))))
                 (coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                    (coe
                       MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                       (coe
                          d_valid'45'inst_912 (coe v0)
                          (coe
                             MAlonzo.Code.Axiom.Set.du_map_360
                             (MAlonzo.Code.Axiom.Set.d_th_1374
                                (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                             (MAlonzo.Code.Interface.Hashable.d_hash_16
                                (coe
                                   MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
                                   (coe
                                      MAlonzo.Code.Ledger.Crypto.d_khs_240
                                      (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0)))))
                             (coe
                                MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                                (MAlonzo.Code.Axiom.Set.d_th_1374
                                   (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                (coe
                                   MAlonzo.Code.Axiom.Set.Map.du__'738'_458
                                   (coe
                                      MAlonzo.Code.Ledger.Transaction.d_vkSigs_1056
                                      (coe MAlonzo.Code.Ledger.Transaction.d_wits_1070 (coe v3))))))
                          (coe
                             MAlonzo.Code.Ledger.Transaction.d_txvldt_1028
                             (coe MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v3))))
                       (coe
                          MAlonzo.Code.Ledger.Script.d_DecEq'45'P1Script_44
                          (coe
                             MAlonzo.Code.Ledger.Script.d_p1s_126
                             (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0))))
                       (coe
                          MAlonzo.Code.Ledger.Utxow.du_scriptsP1_872
                          (coe MAlonzo.Code.Ledger.Transaction.d_wits_1070 (coe v3))))
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                       (coe
                          MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                          (coe
                             MAlonzo.Code.Ledger.Utxo.du_'8712''45'inst_1076
                             (coe
                                MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                (coe
                                   MAlonzo.Code.Ledger.Crypto.d_khs_240
                                   (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0))))
                             (coe
                                MAlonzo.Code.Axiom.Set.du_map_360
                                (MAlonzo.Code.Axiom.Set.d_th_1374
                                   (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                (MAlonzo.Code.Interface.Hashable.d_hash_16
                                   (coe
                                      MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
                                      (coe
                                         MAlonzo.Code.Ledger.Crypto.d_khs_240
                                         (coe
                                            MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                            (coe v0)))))
                                (coe
                                   MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                                   (MAlonzo.Code.Axiom.Set.d_th_1374
                                      (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                   (coe
                                      MAlonzo.Code.Axiom.Set.Map.du__'738'_458
                                      (coe
                                         MAlonzo.Code.Ledger.Transaction.d_vkSigs_1056
                                         (coe
                                            MAlonzo.Code.Ledger.Transaction.d_wits_1070
                                            (coe v3)))))))
                          (coe
                             MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                             (coe
                                MAlonzo.Code.Ledger.Crypto.d_khs_240
                                (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0))))
                          (coe
                             MAlonzo.Code.Ledger.Utxow.d_witsVKeyNeeded_860 (coe v0)
                             (coe MAlonzo.Code.Ledger.Utxo.d_utxo_1052 (coe v2))
                             (coe MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v3))))
                       (coe
                          MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                          (coe
                             MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                             (coe
                                MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                                (coe
                                   MAlonzo.Code.Ledger.Utxo.du_'8712''45'inst_1076
                                   (coe
                                      MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                      (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0)))
                                   (coe
                                      MAlonzo.Code.Axiom.Set.du_map_360
                                      (MAlonzo.Code.Axiom.Set.d_th_1374
                                         (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                      (MAlonzo.Code.Interface.Hashable.d_hash_16
                                         (coe
                                            MAlonzo.Code.Ledger.Script.du_Hashable'45'Script_172
                                            (coe
                                               MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0))))
                                      (MAlonzo.Code.Ledger.Transaction.d_scripts_1058
                                         (coe
                                            MAlonzo.Code.Ledger.Transaction.d_wits_1070 (coe v3)))))
                                (coe
                                   MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                   (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0)))
                                (coe
                                   MAlonzo.Code.Ledger.Utxow.d_scriptsNeeded_866 (coe v0)
                                   (coe MAlonzo.Code.Ledger.Utxo.d_utxo_1052 (coe v2))
                                   (coe MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v3))))
                             (coe
                                MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                                (coe
                                   MAlonzo.Code.Ledger.Utxo.du_'8712''45'inst_1076
                                   (coe
                                      MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                      (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0)))
                                   (coe
                                      MAlonzo.Code.Ledger.Utxow.d_scriptsNeeded_866 (coe v0)
                                      (coe MAlonzo.Code.Ledger.Utxo.d_utxo_1052 (coe v2))
                                      (coe MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v3))))
                                (coe
                                   MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                   (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0)))
                                (coe
                                   MAlonzo.Code.Axiom.Set.du_map_360
                                   (MAlonzo.Code.Axiom.Set.d_th_1374
                                      (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                   (MAlonzo.Code.Interface.Hashable.d_hash_16
                                      (coe
                                         MAlonzo.Code.Ledger.Script.du_Hashable'45'Script_172
                                         (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0))))
                                   (MAlonzo.Code.Ledger.Transaction.d_scripts_1058
                                      (coe MAlonzo.Code.Ledger.Transaction.d_wits_1070 (coe v3))))))
                          (coe
                             MAlonzo.Code.Interface.Decidable.Instance.du_DecEq'8658'Dec_88
                             (coe
                                MAlonzo.Code.Interface.DecEq.du_DecEq'45'Maybe_34
                                (coe
                                   MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                   (coe
                                      MAlonzo.Code.Ledger.Transaction.d_adHashingScheme_246
                                      (coe v0))))
                             (coe
                                MAlonzo.Code.Ledger.Transaction.d_txADhash_1042
                                (coe MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v3)))
                             (coe
                                MAlonzo.Code.Data.Maybe.Base.du_map_68
                                (MAlonzo.Code.Interface.Hashable.d_hash_16
                                   (coe
                                      MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
                                      (coe
                                         MAlonzo.Code.Ledger.Transaction.d_adHashingScheme_246
                                         (coe v0))))
                                (MAlonzo.Code.Ledger.Transaction.d_txAD_1072 (coe v3))))))))
              (coe
                 MAlonzo.Code.Interface.ComputationalRelation.d_compute_38
                 (MAlonzo.Code.Ledger.Utxo.d_Computational'45'UTXO_1348 (coe v0)) v1
                 v2 (MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v3)))
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
                                 MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                                 (coe
                                    d_sig'45'inst_898 (coe v0)
                                    (coe
                                       MAlonzo.Code.Ledger.Transaction.d_txidBytes_252 v0
                                       (MAlonzo.Code.Ledger.Transaction.d_txid_1048
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_body_1068
                                             (coe v3)))))
                                 (coe
                                    MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                                    (coe
                                       MAlonzo.Code.Ledger.Crypto.d_decEq'45'VKey_158
                                       (coe
                                          MAlonzo.Code.Ledger.Crypto.d_pkk_210
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                             (coe v0))))
                                    (coe
                                       MAlonzo.Code.Ledger.Crypto.d_decEq'45'Sig_160
                                       (coe
                                          MAlonzo.Code.Ledger.Crypto.d_pkk_210
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                             (coe v0)))))
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                    (coe
                                       MAlonzo.Code.Ledger.Transaction.d_vkSigs_1056
                                       (coe MAlonzo.Code.Ledger.Transaction.d_wits_1070 (coe v3)))))
                              (coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                 (coe
                                    MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                                    (coe
                                       d_valid'45'inst_912 (coe v0)
                                       (coe
                                          MAlonzo.Code.Axiom.Set.du_map_360
                                          (MAlonzo.Code.Axiom.Set.d_th_1374
                                             (coe
                                                MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                          (MAlonzo.Code.Interface.Hashable.d_hash_16
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
                                                (coe
                                                   MAlonzo.Code.Ledger.Crypto.d_khs_240
                                                   (coe
                                                      MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                      (coe v0)))))
                                          (coe
                                             MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                                             (MAlonzo.Code.Axiom.Set.d_th_1374
                                                (coe
                                                   MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                             (coe
                                                MAlonzo.Code.Axiom.Set.Map.du__'738'_458
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_vkSigs_1056
                                                   (coe
                                                      MAlonzo.Code.Ledger.Transaction.d_wits_1070
                                                      (coe v3))))))
                                       (coe
                                          MAlonzo.Code.Ledger.Transaction.d_txvldt_1028
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v3))))
                                    (coe
                                       MAlonzo.Code.Ledger.Script.d_DecEq'45'P1Script_44
                                       (coe
                                          MAlonzo.Code.Ledger.Script.d_p1s_126
                                          (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0))))
                                    (coe
                                       MAlonzo.Code.Ledger.Utxow.du_scriptsP1_872
                                       (coe MAlonzo.Code.Ledger.Transaction.d_wits_1070 (coe v3))))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                    (coe
                                       MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                                       (coe
                                          MAlonzo.Code.Ledger.Utxo.du_'8712''45'inst_1076
                                          (coe
                                             MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_khs_240
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                   (coe v0))))
                                          (coe
                                             MAlonzo.Code.Axiom.Set.du_map_360
                                             (MAlonzo.Code.Axiom.Set.d_th_1374
                                                (coe
                                                   MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                             (MAlonzo.Code.Interface.Hashable.d_hash_16
                                                (coe
                                                   MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
                                                   (coe
                                                      MAlonzo.Code.Ledger.Crypto.d_khs_240
                                                      (coe
                                                         MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                         (coe v0)))))
                                             (coe
                                                MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                                                (MAlonzo.Code.Axiom.Set.d_th_1374
                                                   (coe
                                                      MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                                (coe
                                                   MAlonzo.Code.Axiom.Set.Map.du__'738'_458
                                                   (coe
                                                      MAlonzo.Code.Ledger.Transaction.d_vkSigs_1056
                                                      (coe
                                                         MAlonzo.Code.Ledger.Transaction.d_wits_1070
                                                         (coe v3)))))))
                                       (coe
                                          MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                          (coe
                                             MAlonzo.Code.Ledger.Crypto.d_khs_240
                                             (coe
                                                MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                (coe v0))))
                                       (coe
                                          MAlonzo.Code.Ledger.Utxow.d_witsVKeyNeeded_860 (coe v0)
                                          (coe MAlonzo.Code.Ledger.Utxo.d_utxo_1052 (coe v2))
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v3))))
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                       (coe
                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                          (coe
                                             MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                                             (coe
                                                MAlonzo.Code.Ledger.Utxo.du_'8712''45'inst_1076
                                                (coe
                                                   MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                                   (coe
                                                      MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                      (coe v0)))
                                                (coe
                                                   MAlonzo.Code.Axiom.Set.du_map_360
                                                   (MAlonzo.Code.Axiom.Set.d_th_1374
                                                      (coe
                                                         MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                                   (MAlonzo.Code.Interface.Hashable.d_hash_16
                                                      (coe
                                                         MAlonzo.Code.Ledger.Script.du_Hashable'45'Script_172
                                                         (coe
                                                            MAlonzo.Code.Ledger.Transaction.d_ss_352
                                                            (coe v0))))
                                                   (MAlonzo.Code.Ledger.Transaction.d_scripts_1058
                                                      (coe
                                                         MAlonzo.Code.Ledger.Transaction.d_wits_1070
                                                         (coe v3)))))
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                   (coe v0)))
                                             (coe
                                                MAlonzo.Code.Ledger.Utxow.d_scriptsNeeded_866
                                                (coe v0)
                                                (coe MAlonzo.Code.Ledger.Utxo.d_utxo_1052 (coe v2))
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_body_1068
                                                   (coe v3))))
                                          (coe
                                             MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                                             (coe
                                                MAlonzo.Code.Ledger.Utxo.du_'8712''45'inst_1076
                                                (coe
                                                   MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                                   (coe
                                                      MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                      (coe v0)))
                                                (coe
                                                   MAlonzo.Code.Ledger.Utxow.d_scriptsNeeded_866
                                                   (coe v0)
                                                   (coe
                                                      MAlonzo.Code.Ledger.Utxo.d_utxo_1052 (coe v2))
                                                   (coe
                                                      MAlonzo.Code.Ledger.Transaction.d_body_1068
                                                      (coe v3))))
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_crypto_244
                                                   (coe v0)))
                                             (coe
                                                MAlonzo.Code.Axiom.Set.du_map_360
                                                (MAlonzo.Code.Axiom.Set.d_th_1374
                                                   (coe
                                                      MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                                (MAlonzo.Code.Interface.Hashable.d_hash_16
                                                   (coe
                                                      MAlonzo.Code.Ledger.Script.du_Hashable'45'Script_172
                                                      (coe
                                                         MAlonzo.Code.Ledger.Transaction.d_ss_352
                                                         (coe v0))))
                                                (MAlonzo.Code.Ledger.Transaction.d_scripts_1058
                                                   (coe
                                                      MAlonzo.Code.Ledger.Transaction.d_wits_1070
                                                      (coe v3))))))
                                       (coe
                                          MAlonzo.Code.Interface.Decidable.Instance.du_DecEq'8658'Dec_88
                                          (coe
                                             MAlonzo.Code.Interface.DecEq.du_DecEq'45'Maybe_34
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                                (coe
                                                   MAlonzo.Code.Ledger.Transaction.d_adHashingScheme_246
                                                   (coe v0))))
                                          (coe
                                             MAlonzo.Code.Ledger.Transaction.d_txADhash_1042
                                             (coe
                                                MAlonzo.Code.Ledger.Transaction.d_body_1068
                                                (coe v3)))
                                          (coe
                                             MAlonzo.Code.Data.Maybe.Base.du_map_68
                                             (MAlonzo.Code.Interface.Hashable.d_hash_16
                                                (coe
                                                   MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
                                                   (coe
                                                      MAlonzo.Code.Ledger.Transaction.d_adHashingScheme_246
                                                      (coe v0))))
                                             (MAlonzo.Code.Ledger.Transaction.d_txAD_1072
                                                (coe v3))))))) in
                    case coe v6 of
                      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v7 v8
                        -> if coe v7
                             then case coe v8 of
                                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v9
                                      -> case coe v9 of
                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v10 v11
                                             -> case coe v11 of
                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v12 v13
                                                    -> case coe v13 of
                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v14 v15
                                                           -> case coe v15 of
                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v16 v17
                                                                  -> coe
                                                                       MAlonzo.Code.Ledger.Utxow.C_UTXOW'45'inductive_906
                                                                       v10 v12 v14 v16
                                                                       (coe
                                                                          MAlonzo.Code.Function.Bundles.d_to_938
                                                                          (coe
                                                                             MAlonzo.Code.Interface.ComputationalRelation.d_'8801''45'just'8660'STS_40
                                                                             (MAlonzo.Code.Ledger.Utxo.d_Computational'45'UTXO_1348
                                                                                (coe v0))
                                                                             v1 v2
                                                                             (MAlonzo.Code.Ledger.Transaction.d_body_1068
                                                                                (coe v3))
                                                                             v4)
                                                                          v5)
                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             else erased
                      _ -> MAlonzo.RTE.mazUnreachableError))
              erased))
