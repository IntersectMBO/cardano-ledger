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

module MAlonzo.Code.Function.HalfAdjointEquivalence where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Function.Inverse

-- Function.HalfAdjointEquivalence._≃_
d__'8771'__12 a0 a1 a2 a3 = ()
data T__'8771'__12
  = C__'8771'_'46'constructor_909 (AgdaAny -> AgdaAny)
                                  (AgdaAny -> AgdaAny)
-- Function.HalfAdjointEquivalence._≃_.to
d_to_38 :: T__'8771'__12 -> AgdaAny -> AgdaAny
d_to_38 v0
  = case coe v0 of
      C__'8771'_'46'constructor_909 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.HalfAdjointEquivalence._≃_.from
d_from_40 :: T__'8771'__12 -> AgdaAny -> AgdaAny
d_from_40 v0
  = case coe v0 of
      C__'8771'_'46'constructor_909 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.HalfAdjointEquivalence._≃_.left-inverse-of
d_left'45'inverse'45'of_44 ::
  T__'8771'__12 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'inverse'45'of_44 = erased
-- Function.HalfAdjointEquivalence._≃_.right-inverse-of
d_right'45'inverse'45'of_48 ::
  T__'8771'__12 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'inverse'45'of_48 = erased
-- Function.HalfAdjointEquivalence._≃_.left-right
d_left'45'right_52 ::
  T__'8771'__12 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'right_52 = erased
-- Function.HalfAdjointEquivalence._≃_.inverse
d_inverse_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () -> T__'8771'__12 -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_inverse_54 ~v0 ~v1 ~v2 ~v3 v4 = du_inverse_54 v4
du_inverse_54 ::
  T__'8771'__12 -> MAlonzo.Code.Function.Inverse.T_Inverse_58
du_inverse_54 v0
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156 (coe d_to_38 (coe v0))
      (coe d_from_40 (coe v0)) erased erased
-- Function.HalfAdjointEquivalence._≃_.injective
d_injective_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T__'8771'__12 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_injective_60 = erased
-- Function.HalfAdjointEquivalence.↔→≃
d_'8596''8594''8771'_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () -> MAlonzo.Code.Function.Inverse.T_Inverse_58 -> T__'8771'__12
d_'8596''8594''8771'_80 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8596''8594''8771'_80 v4
du_'8596''8594''8771'_80 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58 -> T__'8771'__12
du_'8596''8594''8771'_80 v0
  = coe
      C__'8771'_'46'constructor_909
      (MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
         (coe MAlonzo.Code.Function.Inverse.d_to_78 (coe v0)))
      (MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
         (coe MAlonzo.Code.Function.Inverse.d_from_80 (coe v0)))
-- Function.HalfAdjointEquivalence._.right-inverse-of
d_right'45'inverse'45'of_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'inverse'45'of_122 = erased
-- Function.HalfAdjointEquivalence._.left-right
d_left'45'right_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'right_132 = erased
-- Function.HalfAdjointEquivalence._._.lemma
d_lemma_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lemma_140 = erased
