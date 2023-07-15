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

module MAlonzo.Code.Axiom.Set.Factor where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Properties
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Ext.Properties
import qualified MAlonzo.Code.Data.List.Relation.Binary.BagAndSetEquality
import qualified MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional
import qualified MAlonzo.Code.Data.List.Relation.Unary.Unique.DecPropositional.Properties
import qualified MAlonzo.Code.Data.List.Relation.Unary.Unique.Propositional.Properties.WithK
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Equivalence
import qualified MAlonzo.Code.Function.Related
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Interface.DecEq

-- Axiom.Set.Factor._._∪_
d__'8746'__18 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__18 v0 v1 v2 v3
  = coe MAlonzo.Code.Axiom.Set.du__'8746'__642 (coe v0) v2 v3
-- Axiom.Set.Factor._._≡ᵉ_
d__'8801''7497'__20 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__20 = erased
-- Axiom.Set.Factor._.FinSet
d_FinSet_32 :: MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> ()
d_FinSet_32 = erased
-- Axiom.Set.Factor._.disjoint
d_disjoint_44 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d_disjoint_44 = erased
-- Axiom.Set.Factor._.finite
d_finite_48 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> AgdaAny -> ()
d_finite_48 = erased
-- Axiom.Set.Factor._ᶠ
d__'7584'_264 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d__'7584'_264 ~v0 ~v1 v2 v3 = du__'7584'_264 v2 v3
du__'7584'_264 ::
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du__'7584'_264 v0 v1
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0) (coe v1)
-- Axiom.Set.Factor.∪-preserves-finite'
d_'8746''45'preserves'45'finite''_274 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8746''45'preserves'45'finite''_274 v0 ~v1 v2 v3 v4 v5
  = du_'8746''45'preserves'45'finite''_274 v0 v2 v3 v4 v5
du_'8746''45'preserves'45'finite''_274 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8746''45'preserves'45'finite''_274 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Axiom.Set.Properties.du_'8746''45'preserves'45'finite_578
      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
-- Axiom.Set.Factor.Factor.factor
d_factor_296 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  ([AgdaAny] -> AgdaAny) ->
  ([AgdaAny] ->
   [AgdaAny] ->
   (AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16) ->
   AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_factor_296 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 = du_factor_296 v4 v6
du_factor_296 ::
  ([AgdaAny] -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_factor_296 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5 -> coe v0 v4
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Factor.Factor.factor-cong
d_factor'45'cong_300 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  ([AgdaAny] -> AgdaAny) ->
  ([AgdaAny] ->
   [AgdaAny] ->
   (AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16) ->
   AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_factor'45'cong_300 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8
  = du_factor'45'cong_300 v5 v6 v7 v8
du_factor'45'cong_300 ::
  ([AgdaAny] ->
   [AgdaAny] ->
   (AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16) ->
   AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_factor'45'cong_300 v0 v1 v2 v3
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
        -> case coe v5 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                      -> case coe v9 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v10 v11
                             -> coe
                                  v0 v6 v10
                                  (\ v12 ->
                                     coe
                                       MAlonzo.Code.Function.Related.du_toRelated_100
                                       (coe
                                          MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                                       (coe
                                          MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                                          (coe
                                             MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                                          (coe
                                             MAlonzo.Code.Function.Related.Propositional.du_SK'45'sym_168
                                             (coe
                                                MAlonzo.Code.Function.Related.Propositional.C_equivalence_88)
                                             (coe v7 v12))
                                          (coe
                                             MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                                             (coe
                                                MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                                             (coe
                                                MAlonzo.Code.Function.Bundles.d_to_938
                                                (coe
                                                   MAlonzo.Code.Axiom.Set.Properties.du_'8801''7497''8660''8801''7497'''_240)
                                                v3 v12)
                                             (coe
                                                MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                                                (coe
                                                   MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                                                (coe v11 v12)
                                                (coe
                                                   MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
                                                   (coe
                                                      MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))))))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Factor.Factor.factor-∪
d_factor'45''8746'_336 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  ([AgdaAny] -> AgdaAny) ->
  ([AgdaAny] ->
   [AgdaAny] ->
   (AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16) ->
   AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  ([AgdaAny] -> [AgdaAny] -> AgdaAny) -> AgdaAny
d_factor'45''8746'_336 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9 v10 v11
  = du_factor'45''8746'_336 v7 v8 v9 v10 v11
du_factor'45''8746'_336 ::
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  ([AgdaAny] -> [AgdaAny] -> AgdaAny) -> AgdaAny
du_factor'45''8746'_336 v0 v1 v2 v3 v4
  = coe
      v4
      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
            (coe du__'7584'_264 (coe v0) (coe v2))))
      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
            (coe du__'7584'_264 (coe v1) (coe v3))))
-- Axiom.Set.Factor.FactorUnique.f-cong'
d_f'45'cong''_364 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928) ->
  AgdaAny
d_f'45'cong''_364 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9
  = du_f'45'cong''_364 v6 v7 v8 v9
du_f'45'cong''_364 ::
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928) ->
  AgdaAny
du_f'45'cong''_364 v0 v1 v2 v3
  = coe
      v0 v1 v2
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.BagAndSetEquality.du_'8764'bag'8658''8621'_1500
         (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v1))
         (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v2))
         (coe
            MAlonzo.Code.Data.List.Relation.Unary.Unique.Propositional.Properties.WithK.du_unique'8743'set'8658'bag_64
            (coe
               (\ v4 ->
                  coe
                    MAlonzo.Code.Function.Related.du_toRelated_100
                    (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                    (coe v3 v4)))))
-- Axiom.Set.Factor.FactorUnique.deduplicate-Σ
d_deduplicate'45'Σ_372 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_deduplicate'45'Σ_372 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7
  = du_deduplicate'45'Σ_372 v3 v7
du_deduplicate'45'Σ_372 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_deduplicate'45'Σ_372 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         MAlonzo.Code.Data.List.Base.du_deduplicate_834
         (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v1)
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.Unique.DecPropositional.Properties.du_deduplicate'45''33'_42
         (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v1)
-- Axiom.Set.Factor.FactorUnique.ext
d_ext_376 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  [AgdaAny] -> AgdaAny
d_ext_376 ~v0 ~v1 ~v2 v3 ~v4 v5 ~v6 v7 = du_ext_376 v3 v5 v7
du_ext_376 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  [AgdaAny] -> AgdaAny
du_ext_376 v0 v1 v2
  = coe v1 (coe du_deduplicate'45'Σ_372 (coe v0) (coe v2))
-- Axiom.Set.Factor.FactorUnique.ext-cong
d_ext'45'cong_382 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16) ->
  AgdaAny
d_ext'45'cong_382 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7 v8 v9
  = du_ext'45'cong_382 v3 v6 v7 v8 v9
du_ext'45'cong_382 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16) ->
  AgdaAny
du_ext'45'cong_382 v0 v1 v2 v3 v4
  = coe
      du_f'45'cong''_364 (coe v1)
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            MAlonzo.Code.Data.List.Base.du_deduplicate_834
            (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v2)
         (coe
            MAlonzo.Code.Data.List.Relation.Unary.Unique.DecPropositional.Properties.du_deduplicate'45''33'_42
            (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v2))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            MAlonzo.Code.Data.List.Base.du_deduplicate_834
            (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v3)
         (coe
            MAlonzo.Code.Data.List.Relation.Unary.Unique.DecPropositional.Properties.du_deduplicate'45''33'_42
            (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v3))
      (coe
         (\ v5 ->
            coe
              MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
              (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
              (coe
                 MAlonzo.Code.Function.Related.Propositional.du_SK'45'sym_168
                 (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_88)
                 (coe
                    MAlonzo.Code.Data.List.Ext.Properties.du_'8712''45'dedup_146
                    (coe v0) (coe v2) (coe v5)))
              (coe
                 MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                 (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                 (coe
                    MAlonzo.Code.Function.Related.du_fromRelated_138
                    (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                    (coe v4 v5))
                 (coe
                    MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                    (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                    (coe
                       MAlonzo.Code.Data.List.Ext.Properties.du_'8712''45'dedup_146
                       (coe v0) (coe v3) (coe v5))
                    (coe
                       MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
                       (coe
                          MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))))))
-- Axiom.Set.Factor.FactorUnique._.factor
d_factor_398 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_factor_398 ~v0 ~v1 ~v2 v3 ~v4 v5 ~v6 = du_factor_398 v3 v5
du_factor_398 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_factor_398 v0 v1
  = coe du_factor_296 (coe du_ext_376 (coe v0) (coe v1))
-- Axiom.Set.Factor.FactorUnique._.factor-cong
d_factor'45'cong_400 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_factor'45'cong_400 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6
  = du_factor'45'cong_400 v3 v6
du_factor'45'cong_400 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_factor'45'cong_400 v0 v1
  = coe
      du_factor'45'cong_300 (coe du_ext'45'cong_382 (coe v0) (coe v1))
-- Axiom.Set.Factor.FactorUnique._.factor-∪
d_factor'45''8746'_402 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  ([AgdaAny] -> [AgdaAny] -> AgdaAny) -> AgdaAny
d_factor'45''8746'_402 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_factor'45''8746'_402
du_factor'45''8746'_402 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  ([AgdaAny] -> [AgdaAny] -> AgdaAny) -> AgdaAny
du_factor'45''8746'_402 v0 v1 v2 v3 v4 v5
  = coe du_factor'45''8746'_336 v1 v2 v3 v4 v5
-- Axiom.Set.Factor.FactorUnique.factor-∪'
d_factor'45''8746'''_418 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16 ->
   AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  ([AgdaAny] ->
   [AgdaAny] ->
   (AgdaAny ->
    MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
    MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
   AgdaAny) ->
  AgdaAny
d_factor'45''8746'''_418 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                         v10 v11 ~v12 v13
  = du_factor'45''8746'''_418 v10 v11 v13
du_factor'45''8746'''_418 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  ([AgdaAny] ->
   [AgdaAny] ->
   (AgdaAny ->
    MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
    MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
   AgdaAny) ->
  AgdaAny
du_factor'45''8746'''_418 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
               -> coe v2 v3 v5 erased
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
