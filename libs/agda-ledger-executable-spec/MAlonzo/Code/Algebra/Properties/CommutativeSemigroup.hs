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

module MAlonzo.Code.Algebra.Properties.CommutativeSemigroup where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Properties.CommutativeSemigroup._.Interchangable
d_Interchangable_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Interchangable_106 = erased
-- Algebra.Properties.CommutativeSemigroup._.LeftSemimedial
d_LeftSemimedial_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftSemimedial_136 = erased
-- Algebra.Properties.CommutativeSemigroup._.RightSemimedial
d_RightSemimedial_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightSemimedial_166 = erased
-- Algebra.Properties.CommutativeSemigroup._.Semimedial
d_Semimedial_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Semimedial_172 = erased
-- Algebra.Properties.CommutativeSemigroup._.alternative
d_alternative_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_alternative_214 ~v0 ~v1 v2 = du_alternative_214 v2
du_alternative_214 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_alternative_214 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Structures.d_assoc_446
              (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                 (coe
                    MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0)))
              v1 v1 v2))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_sym_36
              (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                 (coe
                    MAlonzo.Code.Algebra.Structures.d_isMagma_444
                    (coe
                       MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                       (coe
                          MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v2)
              (coe
                 MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v2))
              (coe
                 MAlonzo.Code.Algebra.Structures.d_assoc_446
                 (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                    (coe
                       MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0)))
                 v1 v2 v2)))
-- Algebra.Properties.CommutativeSemigroup._.alternativeʳ
d_alternative'691'_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_alternative'691'_216 ~v0 ~v1 v2 v3 v4
  = du_alternative'691'_216 v2 v3 v4
du_alternative'691'_216 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_alternative'691'_216 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v2)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v2))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
            (coe
               MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
               (coe v0)))
         v1 v2 v2)
-- Algebra.Properties.CommutativeSemigroup._.alternativeˡ
d_alternative'737'_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_alternative'737'_218 ~v0 ~v1 v2 v3 v4
  = du_alternative'737'_218 v2 v3 v4
du_alternative'737'_218 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_alternative'737'_218 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
         (coe
            MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
            (coe v0)))
      v1 v1 v2
-- Algebra.Properties.CommutativeSemigroup._.flexible
d_flexible_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_flexible_220 ~v0 ~v1 v2 v3 v4 = du_flexible_220 v2 v3 v4
du_flexible_220 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_flexible_220 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
         (coe
            MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
            (coe v0)))
      v1 v2 v1
-- Algebra.Properties.CommutativeSemigroup._.x∙yz≈xy∙z
d_x'8729'yz'8776'xy'8729'z_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'xy'8729'z_222 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'xy'8729'z_222 v2 v3 v4 v5
du_x'8729'yz'8776'xy'8729'z_222 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'xy'8729'z_222 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
            (coe
               MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
               (coe v0)))
         v1 v2 v3)
-- Algebra.Properties.CommutativeSemigroup.interchange
d_interchange_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_interchange_224 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_interchange_224 v2 v3 v4 v5 v6
du_interchange_224 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_interchange_224 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v5
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0) in
          let v6
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v5) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v4))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v4)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v5
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v6
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v5) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v4)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v4))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v5
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v6
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v5) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v5
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v6
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v5) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (let v5
                            = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                (coe v0) in
                      let v6
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v5) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe
                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                              (let v5
                                     = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                         (coe v0) in
                               let v6
                                     = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v5) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_setoid_164
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4)))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_assoc_446
                        (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                              (coe v0)))
                        v1 v3 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4)))
                  (let v5
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v6
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v5) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                     (coe v1)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v4)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v4))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_assoc_446
                        (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                              (coe v0)))
                        v3 v2 v4)))
               (let v5
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v6
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v5) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                  (coe v1)
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (\ v7 -> coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v7 v4)
                     (\ v7 v8 -> v7)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v7 v8 -> v8)
                     (\ v7 -> coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v7 v4)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
                  (let v7
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v8
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v7) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8))
                     (coe v4) (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2)
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_comm_522
                        (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                           (coe v0))
                        v2 v3))))
            (let v5
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v6
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v5) in
             coe
               MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
               (coe v1)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v4)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v4))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_assoc_446
                  (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                        (coe v0)))
                  v2 v3 v4)))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v1 v2 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v4)))
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈y∙xz
d_x'8729'yz'8776'y'8729'xz_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'y'8729'xz_240 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'y'8729'xz_240 v2 v3 v4 v5
du_x'8729'yz'8776'y'8729'xz_240 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'y'8729'xz_240 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1) v3)
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1) v3)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4
                               = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                   (coe v0) in
                         let v5
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_assoc_446
                  (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                        (coe v0)))
                  v2 v1 v3))
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
               (coe v3) (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
               (coe
                  MAlonzo.Code.Algebra.Structures.d_comm_522
                  (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0))
                  v1 v2)))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMagma_444
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                        (coe v0)))))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
            (coe
               MAlonzo.Code.Algebra.Structures.d_assoc_446
               (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))
               v1 v2 v3)))
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈z∙yx
d_x'8729'yz'8776'z'8729'yx_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'z'8729'yx_254 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'z'8729'yx_254 v2 v3 v4 v5
du_x'8729'yz'8776'z'8729'yx_254 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'z'8729'yx_254 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4
                               = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                   (coe v0) in
                         let v5
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)))
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
                  (coe v3) (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_comm_522
                     (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                        (coe v0))
                     v1 v2)))
            (coe
               du_x'8729'yz'8776'y'8729'xz_240 (coe v0) (coe v1) (coe v3)
               (coe v2)))
         (let v4
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
            (coe v1) (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2)
            (coe
               MAlonzo.Code.Algebra.Structures.d_comm_522
               (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0))
               v2 v3)))
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈x∙zy
d_x'8729'yz'8776'x'8729'zy_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'x'8729'zy_268 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'x'8729'zy_268 v2 v3 v4 v5
du_x'8729'yz'8776'x'8729'zy_268 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'x'8729'zy_268 v0 v1 v2 v3
  = let v4
          = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
              (coe v0) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
      (coe v1) (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)
      (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2)
      (coe
         MAlonzo.Code.Algebra.Structures.d_comm_522
         (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
            (coe v0))
         v2 v3)
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈y∙zx
d_x'8729'yz'8776'y'8729'zx_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'y'8729'zx_280 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'y'8729'zx_280 v2 v3 v4 v5
du_x'8729'yz'8776'y'8729'zx_280 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'y'8729'zx_280 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1)
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v4
                            = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                (coe v0) in
                      let v5
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)))
            (coe
               MAlonzo.Code.Algebra.Structures.d_assoc_446
               (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))
               v2 v3 v1))
         (coe
            MAlonzo.Code.Algebra.Structures.d_comm_522
            (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
               (coe v0))
            v1 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)))
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈z∙xy
d_x'8729'yz'8776'z'8729'xy_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'z'8729'xy_294 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'z'8729'xy_294 v2 v3 v4 v5
du_x'8729'yz'8776'z'8729'xy_294 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'z'8729'xy_294 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v4
                            = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                (coe v0) in
                      let v5
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)))
            (coe
               MAlonzo.Code.Algebra.Structures.d_comm_522
               (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0))
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMagma_444
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                        (coe v0)))))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
            (coe
               MAlonzo.Code.Algebra.Structures.d_assoc_446
               (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))
               v1 v2 v3)))
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈yx∙z
d_x'8729'yz'8776'yx'8729'z_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'yx'8729'z_308 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'yx'8729'z_308 v2 v3 v4 v5
du_x'8729'yz'8776'yx'8729'z_308 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'yx'8729'z_308 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1) v3)
      (coe
         du_x'8729'yz'8776'y'8729'xz_240 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1) v3)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v2 v1 v3))
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈zy∙x
d_x'8729'yz'8776'zy'8729'x_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'zy'8729'x_322 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'zy'8729'x_322 v2 v3 v4 v5
du_x'8729'yz'8776'zy'8729'x_322 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'zy'8729'x_322 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v1)
      (coe
         du_x'8729'yz'8776'z'8729'yx_254 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v1)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v3 v2 v1))
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈xz∙y
d_x'8729'yz'8776'xz'8729'y_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'xz'8729'y_336 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'xz'8729'y_336 v2 v3 v4 v5
du_x'8729'yz'8776'xz'8729'y_336 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'xz'8729'y_336 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3) v2)
      (coe
         du_x'8729'yz'8776'x'8729'zy_268 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3) v2)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v1 v3 v2))
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈yz∙x
d_x'8729'yz'8776'yz'8729'x_350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'yz'8729'x_350 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'yz'8729'x_350 v2 v3 v4 v5
du_x'8729'yz'8776'yz'8729'x_350 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'yz'8729'x_350 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1)
      (coe
         du_x'8729'yz'8776'y'8729'zx_280 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v2 v3 v1))
-- Algebra.Properties.CommutativeSemigroup.x∙yz≈zx∙y
d_x'8729'yz'8776'zx'8729'y_364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8729'yz'8776'zx'8729'y_364 ~v0 ~v1 v2 v3 v4 v5
  = du_x'8729'yz'8776'zx'8729'y_364 v2 v3 v4 v5
du_x'8729'yz'8776'zx'8729'y_364 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8729'yz'8776'zx'8729'y_364 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1) v2)
      (coe
         du_x'8729'yz'8776'z'8729'xy_294 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1) v2)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v3 v1 v2))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈y∙xz
d_xy'8729'z'8776'y'8729'xz_378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'y'8729'xz_378 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'y'8729'xz_378 v2 v3 v4 v5
du_xy'8729'z'8776'y'8729'xz_378 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'y'8729'xz_378 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
            (coe
               MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
               (coe v0)))
         v1 v2 v3)
      (coe
         du_x'8729'yz'8776'y'8729'xz_240 (coe v0) (coe v1) (coe v2)
         (coe v3))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈z∙yx
d_xy'8729'z'8776'z'8729'yx_392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'z'8729'yx_392 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'z'8729'yx_392 v2 v3 v4 v5
du_xy'8729'z'8776'z'8729'yx_392 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'z'8729'yx_392 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
            (coe
               MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
               (coe v0)))
         v1 v2 v3)
      (coe
         du_x'8729'yz'8776'z'8729'yx_254 (coe v0) (coe v1) (coe v2)
         (coe v3))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈x∙zy
d_xy'8729'z'8776'x'8729'zy_406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'x'8729'zy_406 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'x'8729'zy_406 v2 v3 v4 v5
du_xy'8729'z'8776'x'8729'zy_406 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'x'8729'zy_406 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
            (coe
               MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
               (coe v0)))
         v1 v2 v3)
      (coe
         du_x'8729'yz'8776'x'8729'zy_268 (coe v0) (coe v1) (coe v2)
         (coe v3))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈y∙zx
d_xy'8729'z'8776'y'8729'zx_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'y'8729'zx_420 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'y'8729'zx_420 v2 v3 v4 v5
du_xy'8729'z'8776'y'8729'zx_420 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'y'8729'zx_420 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
            (coe
               MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
               (coe v0)))
         v1 v2 v3)
      (coe
         du_x'8729'yz'8776'y'8729'zx_280 (coe v0) (coe v1) (coe v2)
         (coe v3))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈z∙xy
d_xy'8729'z'8776'z'8729'xy_434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'z'8729'xy_434 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'z'8729'xy_434 v2 v3 v4 v5
du_xy'8729'z'8776'z'8729'xy_434 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'z'8729'xy_434 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
            (coe
               MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
               (coe v0)))
         v1 v2 v3)
      (coe
         du_x'8729'yz'8776'z'8729'xy_294 (coe v0) (coe v1) (coe v2)
         (coe v3))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈yx∙z
d_xy'8729'z'8776'yx'8729'z_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'yx'8729'z_448 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'yx'8729'z_448 v2 v3 v4 v5
du_xy'8729'z'8776'yx'8729'z_448 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'yx'8729'z_448 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1) v3)
      (coe
         du_xy'8729'z'8776'y'8729'xz_378 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1) v3)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v2 v1 v3))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈zy∙x
d_xy'8729'z'8776'zy'8729'x_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'zy'8729'x_462 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'zy'8729'x_462 v2 v3 v4 v5
du_xy'8729'z'8776'zy'8729'x_462 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'zy'8729'x_462 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v1)
      (coe
         du_xy'8729'z'8776'z'8729'yx_392 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v1)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v3 v2 v1))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈xz∙y
d_xy'8729'z'8776'xz'8729'y_476 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'xz'8729'y_476 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'xz'8729'y_476 v2 v3 v4 v5
du_xy'8729'z'8776'xz'8729'y_476 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'xz'8729'y_476 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3) v2)
      (coe
         du_xy'8729'z'8776'x'8729'zy_406 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3) v2)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v1 v3 v2))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈yz∙x
d_xy'8729'z'8776'yz'8729'x_490 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'yz'8729'x_490 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'yz'8729'x_490 v2 v3 v4 v5
du_xy'8729'z'8776'yz'8729'x_490 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'yz'8729'x_490 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1)
      (coe
         du_xy'8729'z'8776'y'8729'zx_420 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v2 v3 v1))
-- Algebra.Properties.CommutativeSemigroup.xy∙z≈zx∙y
d_xy'8729'z'8776'zx'8729'y_504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'z'8776'zx'8729'y_504 ~v0 ~v1 v2 v3 v4 v5
  = du_xy'8729'z'8776'zx'8729'y_504 v2 v3 v4 v5
du_xy'8729'z'8776'zx'8729'y_504 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_xy'8729'z'8776'zx'8729'y_504 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1) v2)
      (coe
         du_xy'8729'z'8776'z'8729'xy_434 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMagma_444
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1) v2)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v3 v1 v2))
-- Algebra.Properties.CommutativeSemigroup.xy∙xx≈x∙yxx
d_xy'8729'xx'8776'x'8729'yxx_516 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_xy'8729'xx'8776'x'8729'yxx_516 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
         (coe
            MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
            (coe v0)))
      v1 v2 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v1)
-- Algebra.Properties.CommutativeSemigroup.semimedialˡ
d_semimedial'737'_522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_semimedial'737'_522 ~v0 ~v1 v2 v3 v4 v5
  = du_semimedial'737'_522 v2 v3 v4 v5
du_semimedial'737'_522 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_semimedial'737'_522 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v1)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1) v3))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v4
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v5
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1) v3))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v4
                            = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                (coe v0) in
                      let v5
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe
                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                              (let v4
                                     = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                         (coe v0) in
                               let v5
                                     = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_setoid_164
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                        (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isMagma_444
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                    (coe v0)))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                              (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)))
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_assoc_446
                           (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                 (coe v0)))
                           v1 v2 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))))
                  (let v4
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v5
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
                     (coe v1)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1) v3)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_assoc_446
                        (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                              (coe v0)))
                        v2 v1 v3)))
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
                  (coe v1)
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (\ v6 -> coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v6 v3)
                     (\ v6 v7 -> v6)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v6 v7 -> v7)
                     (\ v6 -> coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v6 v3)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
                  (let v6
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v7
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v6) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
                     (coe v3) (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_comm_522
                        (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                           (coe v0))
                        v1 v2))))
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
               (coe v1)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                  (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isMagma_444
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                              (coe v0)))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2) v3)
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3))
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_assoc_446
                     (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                           (coe v0)))
                     v1 v2 v3))))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v1 v1 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)))
-- Algebra.Properties.CommutativeSemigroup.semimedialʳ
d_semimedial'691'_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_semimedial'691'_530 ~v0 ~v1 v2 v3 v4 v5
  = du_semimedial'691'_530 v2 v3 v4 v5
du_semimedial'691'_530 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_semimedial'691'_530 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v1))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v1)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v1)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1) v1))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1) v1))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3) v1))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v4
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v5
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3) v1))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v4
                            = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                (coe v0) in
                      let v5
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe
                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                              (let v4
                                     = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                         (coe v0) in
                               let v5
                                     = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_setoid_164
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                        (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isMagma_444
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                    (coe v0)))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                              (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)))
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_assoc_446
                           (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                 (coe v0)))
                           v2 v1 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))))
                  (let v4
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v5
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
                     (coe v2)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3) v1)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_assoc_446
                        (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                              (coe v0)))
                        v1 v3 v1)))
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
                  (coe v2)
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (\ v6 -> coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v6 v1)
                     (\ v6 v7 -> v6)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v6 v7 -> v7)
                     (\ v6 -> coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v6 v1)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3))
                  (let v6
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v7
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v6) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
                     (coe v1) (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_comm_522
                        (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                           (coe v0))
                        v3 v1))))
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
               (coe v2)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v1))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1) v1)
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                  (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isMagma_444
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                              (coe v0)))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1) v1)
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v1))
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_assoc_446
                     (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                           (coe v0)))
                     v3 v1 v1))))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v2 v3 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v1)))
-- Algebra.Properties.CommutativeSemigroup.middleSemimedial
d_middleSemimedial_544 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_middleSemimedial_544 ~v0 ~v1 v2 v3 v4 v5
  = du_middleSemimedial_544 v2 v3 v4 v5
du_middleSemimedial_544 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_middleSemimedial_544 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                    (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v2)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v1))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v4
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v5
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v1))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v4
                            = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                (coe v0) in
                      let v5
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe
                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                              (let v4
                                     = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                         (coe v0) in
                               let v5
                                     = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_setoid_164
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                        (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isMagma_444
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                    (coe v0)))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1 v3)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v1
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                              (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1)))
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_assoc_446
                           (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                                 (coe v0)))
                           v1 v3 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))))
                  (let v4
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v5
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
                     (coe v1)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2) v1)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3
                        (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v1))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_assoc_446
                        (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                              (coe v0)))
                        v3 v2 v1)))
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
                  (coe v1)
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (\ v6 -> coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v6 v1)
                     (\ v6 v7 -> v6)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v6 v7 -> v7)
                     (\ v6 -> coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v6 v1)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2))
                  (let v6
                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                             (coe v0) in
                   let v7
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v6) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
                     (coe v1) (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3)
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v2)
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_comm_522
                        (MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                           (coe v0))
                        v2 v3))))
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
               (coe v1)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1)
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                  (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isMagma_444
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                              (coe v0)))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2 v3) v1)
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v2
                     (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1))
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_assoc_446
                     (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                           (coe v0)))
                     v2 v3 v1))))
         (coe
            MAlonzo.Code.Algebra.Structures.d_assoc_446
            (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemigroup_622
                  (coe v0)))
            v1 v2 (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__620 v0 v3 v1)))
-- Algebra.Properties.CommutativeSemigroup.semimedial
d_semimedial_552 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_semimedial_552 ~v0 ~v1 v2 = du_semimedial_552 v2
du_semimedial_552 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_semimedial_552 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_semimedial'737'_522 (coe v0))
      (coe du_semimedial'691'_530 (coe v0))
