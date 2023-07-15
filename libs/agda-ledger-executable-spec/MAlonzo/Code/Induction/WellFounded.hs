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

module MAlonzo.Code.Induction.WellFounded where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Induction

-- Induction.WellFounded.WfRec
d_WfRec_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> AgdaAny -> ()
d_WfRec_22 = erased
-- Induction.WellFounded.Acc
d_Acc_42 a0 a1 a2 a3 a4 = ()
data T_Acc_42 = C_acc_52
-- Induction.WellFounded.WellFounded
d_WellFounded_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) -> ()
d_WellFounded_54 = erased
-- Induction.WellFounded.acc-inverse
d_acc'45'inverse_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> T_Acc_42 -> AgdaAny -> AgdaAny -> T_Acc_42
d_acc'45'inverse_68 = erased
-- Induction.WellFounded.Acc-resp-≈
d_Acc'45'resp'45''8776'_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> T_Acc_42 -> T_Acc_42
d_Acc'45'resp'45''8776'_80 = erased
-- Induction.WellFounded.Some.wfRecBuilder
d_wfRecBuilder_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> T_Acc_42 -> AgdaAny -> AgdaAny -> AgdaAny
d_wfRecBuilder_104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9 ~v10
  = du_wfRecBuilder_104 v6 v9
du_wfRecBuilder_104 ::
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny
du_wfRecBuilder_104 v0 v1
  = coe v0 v1 (\ v2 v3 -> coe du_wfRecBuilder_104 (coe v0) v2)
-- Induction.WellFounded.Some.wfRec
d_wfRec_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> T_Acc_42 -> AgdaAny
d_wfRec_118 ~v0 ~v1 ~v2 ~v3 ~v4 = du_wfRec_118
du_wfRec_118 ::
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> T_Acc_42 -> AgdaAny
du_wfRec_118
  = coe
      MAlonzo.Code.Induction.du_subsetBuild_114
      (\ v0 v1 v2 v3 v4 v5 -> coe du_wfRecBuilder_104 v1 v4)
-- Induction.WellFounded.Some.unfold-wfRec
d_unfold'45'wfRec_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny ->
  T_Acc_42 -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unfold'45'wfRec_132 = erased
-- Induction.WellFounded.All.wfRecBuilder
d_wfRecBuilder_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_wfRecBuilder_152 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 ~v8
  = du_wfRecBuilder_152 v7
du_wfRecBuilder_152 ::
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
du_wfRecBuilder_152 v0 v1 v2 = coe du_wfRecBuilder_104 (coe v0) v1
-- Induction.WellFounded.All.wfRec
d_wfRec_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny
d_wfRec_160 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 = du_wfRec_160
du_wfRec_160 ::
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny
du_wfRec_160
  = coe
      MAlonzo.Code.Induction.du_build_54
      (\ v0 v1 v2 -> coe du_wfRecBuilder_152 v1)
-- Induction.WellFounded.All.wfRec-builder
d_wfRec'45'builder_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_wfRec'45'builder_162 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_wfRec'45'builder_162
du_wfRec'45'builder_162 ::
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_wfRec'45'builder_162 v0 v1 v2 = coe du_wfRecBuilder_152 v1
-- Induction.WellFounded.FixPoint.some-wfRec-irrelevant
d_some'45'wfRec'45'irrelevant_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  (AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny ->
    AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny ->
  T_Acc_42 ->
  T_Acc_42 -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_some'45'wfRec'45'irrelevant_198 = erased
-- Induction.WellFounded.FixPoint._.wfRec
d_wfRec_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  (AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny ->
    AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny
d_wfRec_220 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 = du_wfRec_220
du_wfRec_220 ::
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny
du_wfRec_220 = coe du_wfRec_160
-- Induction.WellFounded.FixPoint._.wfRec-builder
d_wfRec'45'builder_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  (AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny ->
    AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_wfRec'45'builder_222 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
  = du_wfRec'45'builder_222
du_wfRec'45'builder_222 ::
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_wfRec'45'builder_222 = coe du_wfRec'45'builder_162
-- Induction.WellFounded.FixPoint._.wfRecBuilder
d_wfRecBuilder_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  (AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny ->
    AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_wfRecBuilder_224 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
  = du_wfRecBuilder_224
du_wfRecBuilder_224 ::
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_wfRecBuilder_224 v0 v1 v2 = coe du_wfRecBuilder_152 v1
-- Induction.WellFounded.FixPoint.wfRecBuilder-wfRec
d_wfRecBuilder'45'wfRec_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  (AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny ->
    AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_wfRecBuilder'45'wfRec_232 = erased
-- Induction.WellFounded.FixPoint.unfold-wfRec
d_unfold'45'wfRec_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny) ->
  (AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny ->
    AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unfold'45'wfRec_258 = erased
-- Induction.WellFounded.Subrelation.accessible
d_accessible_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_Acc_42 -> T_Acc_42
d_accessible_280 = erased
-- Induction.WellFounded.Subrelation.wellFounded
d_wellFounded_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> T_Acc_42) -> AgdaAny -> T_Acc_42
d_wellFounded_288 = erased
-- Induction.WellFounded.InverseImage.accessible
d_accessible_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> T_Acc_42 -> T_Acc_42
d_accessible_308 = erased
-- Induction.WellFounded.InverseImage.wellFounded
d_wellFounded_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> T_Acc_42) -> AgdaAny -> T_Acc_42
d_wellFounded_316 = erased
-- Induction.WellFounded.InverseImage.well-founded
d_well'45'founded_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> T_Acc_42) -> AgdaAny -> T_Acc_42
d_well'45'founded_322 = erased
-- Induction.WellFounded.TransitiveClosure._<⁺_
d__'60''8314'__334 a0 a1 a2 a3 a4 a5 = ()
data T__'60''8314'__334
  = C_'91'_'93'_342 AgdaAny |
    C_trans_354 AgdaAny T__'60''8314'__334 T__'60''8314'__334
-- Induction.WellFounded.TransitiveClosure.downwardsClosed
d_downwardsClosed_360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> T_Acc_42 -> T__'60''8314'__334 -> T_Acc_42
d_downwardsClosed_360 = erased
-- Induction.WellFounded.TransitiveClosure.accessible
d_accessible_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> ()) -> AgdaAny -> T_Acc_42 -> T_Acc_42
d_accessible_372 = erased
-- Induction.WellFounded.TransitiveClosure.accessible′
d_accessible'8242'_376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> T_Acc_42 -> AgdaAny -> T__'60''8314'__334 -> T_Acc_42
d_accessible'8242'_376 = erased
-- Induction.WellFounded.TransitiveClosure.wellFounded
d_wellFounded_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) -> AgdaAny -> T_Acc_42
d_wellFounded_394 = erased
-- Induction.WellFounded.Lexicographic._<_
d__'60'__420 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 = ()
data T__'60'__420 = C_left_432 AgdaAny | C_right_442 AgdaAny
-- Induction.WellFounded.Lexicographic.accessible
d_accessible_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny -> T_Acc_42 -> (AgdaAny -> AgdaAny -> T_Acc_42) -> T_Acc_42
d_accessible_450 = erased
-- Induction.WellFounded.Lexicographic.accessible′
d_accessible'8242'_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  T_Acc_42 ->
  T_Acc_42 ->
  (AgdaAny -> AgdaAny -> T_Acc_42) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T__'60'__420 -> T_Acc_42
d_accessible'8242'_458 = erased
-- Induction.WellFounded.Lexicographic.wellFounded
d_wellFounded_480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  (AgdaAny -> AgdaAny -> T_Acc_42) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Acc_42
d_wellFounded_480 = erased
-- Induction.WellFounded.Lexicographic.well-founded
d_well'45'founded_488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) ->
  (AgdaAny -> AgdaAny -> T_Acc_42) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Acc_42
d_well'45'founded_488 = erased
-- Induction.WellFounded.Inverse-image.accessible
d_accessible_492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> T_Acc_42 -> T_Acc_42
d_accessible_492 = erased
-- Induction.WellFounded.Inverse-image.well-founded
d_well'45'founded_494 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> T_Acc_42) -> AgdaAny -> T_Acc_42
d_well'45'founded_494 = erased
-- Induction.WellFounded.Inverse-image.wellFounded
d_wellFounded_496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> T_Acc_42) -> AgdaAny -> T_Acc_42
d_wellFounded_496 = erased
-- Induction.WellFounded.Transitive-closure._<⁺_
d__'60''8314'__500 a0 a1 a2 a3 a4 a5 = ()
-- Induction.WellFounded.Transitive-closure.accessible
d_accessible_504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> ()) -> AgdaAny -> T_Acc_42 -> T_Acc_42
d_accessible_504 = erased
-- Induction.WellFounded.Transitive-closure.accessible′
d_accessible'8242'_506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> T_Acc_42 -> AgdaAny -> T__'60''8314'__334 -> T_Acc_42
d_accessible'8242'_506 = erased
-- Induction.WellFounded.Transitive-closure.downwardsClosed
d_downwardsClosed_508 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> T_Acc_42 -> T__'60''8314'__334 -> T_Acc_42
d_downwardsClosed_508 = erased
-- Induction.WellFounded.Transitive-closure.wellFounded
d_wellFounded_512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> T_Acc_42) -> AgdaAny -> T_Acc_42
d_wellFounded_512 = erased
