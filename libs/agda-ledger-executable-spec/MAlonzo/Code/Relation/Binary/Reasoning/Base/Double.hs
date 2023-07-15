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

module MAlonzo.Code.Relation.Binary.Reasoning.Base.Double where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Relation.Binary.Reasoning.Base.Double._IsRelatedTo_
d__IsRelatedTo__56 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T__IsRelatedTo__56
  = C_nonstrict_64 AgdaAny | C_equals_68 AgdaAny
-- Relation.Binary.Reasoning.Base.Double.IsEquality
d_IsEquality_74 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 = ()
data T_IsEquality_74 = C_isEquality_82
-- Relation.Binary.Reasoning.Base.Double.IsEquality?
d_IsEquality'63'_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  T__IsRelatedTo__56 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsEquality'63'_90 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_IsEquality'63'_90 v9
du_IsEquality'63'_90 ::
  T__IsRelatedTo__56 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_IsEquality'63'_90 v0
  = case coe v0 of
      C_nonstrict_64 v1
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      C_equals_68 v1
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe
                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                (coe C_isEquality_82))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Double.extractEquality
d_extractEquality_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__56 -> T_IsEquality_74 -> AgdaAny
d_extractEquality_100 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du_extractEquality_100 v9 v10
du_extractEquality_100 ::
  T__IsRelatedTo__56 -> T_IsEquality_74 -> AgdaAny
du_extractEquality_100 v0 v1
  = coe
      seq (coe v1)
      (case coe v0 of
         C_equals_68 v2 -> coe v2
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Relation.Binary.Reasoning.Base.Double.begin_
d_begin__110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny -> AgdaAny -> T__IsRelatedTo__56 -> AgdaAny
d_begin__110 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9
  = du_begin__110 v6 v7 v8 v9
du_begin__110 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny -> AgdaAny -> T__IsRelatedTo__56 -> AgdaAny
du_begin__110 v0 v1 v2 v3
  = case coe v3 of
      C_nonstrict_64 v4 -> coe v4
      C_equals_68 v4
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82 v0 v1 v2 v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Double.begin-equality_
d_begin'45'equality__124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny -> AgdaAny -> T__IsRelatedTo__56 -> AgdaAny -> AgdaAny
d_begin'45'equality__124 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
                         ~v10
  = du_begin'45'equality__124 v9
du_begin'45'equality__124 :: T__IsRelatedTo__56 -> AgdaAny
du_begin'45'equality__124 v0
  = coe
      du_extractEquality_100 (coe v0)
      (coe
         MAlonzo.Code.Relation.Nullary.Decidable.Core.du_toWitness_108
         (coe du_IsEquality'63'_90 (coe v0)))
-- Relation.Binary.Reasoning.Base.Double.step-∼
d_step'45''8764'_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__56 -> AgdaAny -> T__IsRelatedTo__56
d_step'45''8764'_136 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10 v11
  = du_step'45''8764'_136 v6 v7 v8 v9 v10 v11
du_step'45''8764'_136 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__56 -> AgdaAny -> T__IsRelatedTo__56
du_step'45''8764'_136 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      C_nonstrict_64 v6
        -> coe
             C_nonstrict_64
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_trans_84 v0 v1 v2 v3 v5
                v6)
      C_equals_68 v6
        -> coe
             C_nonstrict_64
             (coe
                MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Double.step-≈
d_step'45''8776'_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__56 -> AgdaAny -> T__IsRelatedTo__56
d_step'45''8776'_156 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10 v11
  = du_step'45''8776'_156 v6 v7 v8 v9 v10 v11
du_step'45''8776'_156 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__56 -> AgdaAny -> T__IsRelatedTo__56
du_step'45''8776'_156 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      C_nonstrict_64 v6
        -> coe
             C_nonstrict_64
             (coe
                MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
                (coe v0) (coe v3) (coe v2) (coe v1)
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                   (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                      (coe v0))
                   v1 v2 v5)
                (coe v6))
      C_equals_68 v6
        -> coe
             C_equals_68
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe v0))
                v1 v2 v3 v5 v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Double.step-≈˘
d_step'45''8776''728'_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__56 -> AgdaAny -> T__IsRelatedTo__56
d_step'45''8776''728'_176 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10
                          v11
  = du_step'45''8776''728'_176 v6 v7 v8 v9 v10 v11
du_step'45''8776''728'_176 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__56 -> AgdaAny -> T__IsRelatedTo__56
du_step'45''8776''728'_176 v0 v1 v2 v3 v4 v5
  = coe
      du_step'45''8776'_156 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe v0))
         v2 v1 v5)
-- Relation.Binary.Reasoning.Base.Double.step-≡
d_step'45''8801'_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  T__IsRelatedTo__56 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T__IsRelatedTo__56
d_step'45''8801'_190 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
                     ~v11
  = du_step'45''8801'_190 v10
du_step'45''8801'_190 :: T__IsRelatedTo__56 -> T__IsRelatedTo__56
du_step'45''8801'_190 v0 = coe v0
-- Relation.Binary.Reasoning.Base.Double.step-≡˘
d_step'45''8801''728'_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  T__IsRelatedTo__56 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T__IsRelatedTo__56
d_step'45''8801''728'_214 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                          v10 ~v11
  = du_step'45''8801''728'_214 v10
du_step'45''8801''728'_214 ::
  T__IsRelatedTo__56 -> T__IsRelatedTo__56
du_step'45''8801''728'_214 v0 = coe v0
-- Relation.Binary.Reasoning.Base.Double._≡⟨⟩_
d__'8801''10216''10217'__226 ::
  T__IsRelatedTo__56 -> T__IsRelatedTo__56
d__'8801''10216''10217'__226 v0 = coe v0
-- Relation.Binary.Reasoning.Base.Double._∎
d__'8718'_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny -> T__IsRelatedTo__56
d__'8718'_234 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 = du__'8718'_234 v6 v7
du__'8718'_234 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny -> T__IsRelatedTo__56
du__'8718'_234 v0 v1
  = coe
      C_equals_68
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe v0))
         v1)
