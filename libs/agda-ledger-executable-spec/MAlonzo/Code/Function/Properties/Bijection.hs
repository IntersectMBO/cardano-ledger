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

module MAlonzo.Code.Function.Properties.Bijection where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Construct.Composition
import qualified MAlonzo.Code.Function.Construct.Identity
import qualified MAlonzo.Code.Function.Construct.Symmetry
import qualified MAlonzo.Code.Function.Properties.Inverse
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Properties.Bijection.refl
d_refl_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_refl_28 ~v0 ~v1 v2 = du_refl_28 v2
du_refl_28 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_refl_28 v0
  = coe
      MAlonzo.Code.Function.Construct.Identity.du_bijection_730 (coe v0)
-- Function.Properties.Bijection.sym-≡
d_sym'45''8801'_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_sym'45''8801'_30 ~v0 ~v1 v2 ~v3 ~v4 = du_sym'45''8801'_30 v2
du_sym'45''8801'_30 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_sym'45''8801'_30 v0
  = coe
      MAlonzo.Code.Function.Construct.Symmetry.du_bijection'45''8801'_696
      (coe v0)
-- Function.Properties.Bijection.trans
d_trans_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_trans_32 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_trans_32 v8
du_trans_32 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_trans_32 v0
  = coe
      MAlonzo.Code.Function.Construct.Composition.du_bijection_1614
      (coe v0)
-- Function.Properties.Bijection.⤖-isEquivalence
d_'10518''45'isEquivalence_34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'10518''45'isEquivalence_34 ~v0 = du_'10518''45'isEquivalence_34
du_'10518''45'isEquivalence_34 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_'10518''45'isEquivalence_34
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (coe
         (\ v0 ->
            coe
              du_refl_28
              (coe
                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)))
      (coe
         (\ v0 v1 ->
            coe
              du_sym'45''8801'_30
              (coe
                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)))
      (coe
         (\ v0 v1 v2 ->
            coe
              du_trans_32
              (coe
                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)))
-- Function.Properties.Bijection.Bijection⇒Inverse
d_Bijection'8658'Inverse_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_Bijection'8658'Inverse_36 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_Bijection'8658'Inverse_36 v5 v6
du_Bijection'8658'Inverse_36 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_Bijection'8658'Inverse_36 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.C_Inverse'46'constructor_24077
      (coe MAlonzo.Code.Function.Bundles.d_to_852 (coe v1))
      (coe
         MAlonzo.Code.Function.Bundles.du_to'8315'_788
         (coe MAlonzo.Code.Function.Bundles.du_surjection_864 (coe v1)))
      (coe MAlonzo.Code.Function.Bundles.d_cong_854 (coe v1))
      (coe
         (\ v2 v3 v4 ->
            coe
              MAlonzo.Code.Function.Bundles.du_injective_858 v1
              (coe
                 MAlonzo.Code.Function.Bundles.du_to'8315'_788
                 (coe MAlonzo.Code.Function.Bundles.du_surjection_864 (coe v1))
                 (coe v2))
              (coe
                 MAlonzo.Code.Function.Bundles.du_to'8315'_788
                 (coe MAlonzo.Code.Function.Bundles.du_surjection_864 (coe v1))
                 (coe v3))
              (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                 (coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                    v0
                    (coe
                       MAlonzo.Code.Function.Bundles.d_to_852 v1
                       (coe
                          MAlonzo.Code.Function.Bundles.du_to'8315'_788
                          (coe MAlonzo.Code.Function.Bundles.du_surjection_864 (coe v1))
                          (coe v2)))
                    v2
                    (coe
                       MAlonzo.Code.Function.Bundles.d_to_852 v1
                       (coe
                          MAlonzo.Code.Function.Bundles.du_to'8315'_788
                          (coe MAlonzo.Code.Function.Bundles.du_surjection_864 (coe v1))
                          (coe v3)))
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       v0 v2 v3
                       (coe
                          MAlonzo.Code.Function.Bundles.d_to_852 v1
                          (coe
                             MAlonzo.Code.Function.Bundles.du_to'8315'_788
                             (coe MAlonzo.Code.Function.Bundles.du_surjection_864 (coe v1))
                             (coe v3)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                          (coe v0) (coe v3)
                          (coe
                             MAlonzo.Code.Function.Bundles.d_to_852 v1
                             (coe
                                MAlonzo.Code.Function.Bundles.du_to'8315'_788
                                (coe MAlonzo.Code.Function.Bundles.du_surjection_864 (coe v1))
                                (coe v3)))
                          (coe
                             MAlonzo.Code.Function.Bundles.d_to_852 v1
                             (coe
                                MAlonzo.Code.Function.Bundles.du_to'8315'_788
                                (coe MAlonzo.Code.Function.Bundles.du_surjection_864 (coe v1))
                                (coe v3)))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (coe v0)))
                             (coe
                                MAlonzo.Code.Function.Bundles.d_to_852 v1
                                (coe
                                   MAlonzo.Code.Function.Bundles.du_to'8315'_788
                                   (coe MAlonzo.Code.Function.Bundles.du_surjection_864 (coe v1))
                                   (coe v3))))
                          (coe du_to'8728'to'8315'_144 (coe v1) (coe v3)))
                       v4)
                    (coe du_to'8728'to'8315'_144 (coe v1) (coe v2))))))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe du_to'8728'to'8315'_144 (coe v1))
         (coe
            (\ v2 ->
               coe
                 MAlonzo.Code.Function.Bundles.du_injective_858 v1
                 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                    (coe
                       MAlonzo.Code.Function.Bundles.du_surjective_860 v1
                       (coe MAlonzo.Code.Function.Bundles.d_to_852 v1 v2)))
                 v2
                 (coe
                    du_to'8728'to'8315'_144 (coe v1)
                    (coe MAlonzo.Code.Function.Bundles.d_to_852 v1 v2)))))
-- Function.Properties.Bijection._.to∘to⁻
d_to'8728'to'8315'_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 -> AgdaAny -> AgdaAny
d_to'8728'to'8315'_144 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_to'8728'to'8315'_144 v6 v7
du_to'8728'to'8315'_144 ::
  MAlonzo.Code.Function.Bundles.T_Bijection_844 -> AgdaAny -> AgdaAny
du_to'8728'to'8315'_144 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe MAlonzo.Code.Function.Bundles.du_surjective_860 v0 v1)
-- Function.Properties.Bijection.Bijection⇒Equivalence
d_Bijection'8658'Equivalence_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_Bijection'8658'Equivalence_152 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_Bijection'8658'Equivalence_152 v5 v6
du_Bijection'8658'Equivalence_152 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_Bijection'8658'Equivalence_152 v0 v1
  = coe
      MAlonzo.Code.Function.Properties.Inverse.du_Inverse'8658'Equivalence_218
      (coe du_Bijection'8658'Inverse_36 (coe v0) (coe v1))
-- Function.Properties.Bijection.⤖⇒↔
d_'10518''8658''8596'_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'10518''8658''8596'_154 ~v0 ~v1 ~v2 ~v3
  = du_'10518''8658''8596'_154
du_'10518''8658''8596'_154 ::
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'10518''8658''8596'_154
  = coe
      du_Bijection'8658'Inverse_36
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Properties.Bijection.⤖⇒⇔
d_'10518''8658''8660'_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'10518''8658''8660'_156 ~v0 ~v1 ~v2 ~v3
  = du_'10518''8658''8660'_156
du_'10518''8658''8660'_156 ::
  MAlonzo.Code.Function.Bundles.T_Bijection_844 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'10518''8658''8660'_156
  = coe
      du_Bijection'8658'Equivalence_152
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
