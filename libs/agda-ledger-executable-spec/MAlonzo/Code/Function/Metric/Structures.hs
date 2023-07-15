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

module MAlonzo.Code.Function.Metric.Structures where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Metric.Structures.IsProtoMetric
d_IsProtoMetric_30 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = ()
data T_IsProtoMetric_30
  = C_IsProtoMetric'46'constructor_2109 MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
                                        MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
                                        (AgdaAny ->
                                         AgdaAny ->
                                         AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Metric.Structures.IsProtoMetric.isPartialOrder
d_isPartialOrder_42 ::
  T_IsProtoMetric_30 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_42 v0
  = case coe v0 of
      C_IsProtoMetric'46'constructor_2109 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsProtoMetric.≈-isEquivalence
d_'8776''45'isEquivalence_44 ::
  T_IsProtoMetric_30 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_44 v0
  = case coe v0 of
      C_IsProtoMetric'46'constructor_2109 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsProtoMetric.cong
d_cong_46 ::
  T_IsProtoMetric_30 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_46 v0
  = case coe v0 of
      C_IsProtoMetric'46'constructor_2109 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsProtoMetric.nonNegative
d_nonNegative_48 ::
  T_IsProtoMetric_30 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_48 v0
  = case coe v0 of
      C_IsProtoMetric'46'constructor_2109 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsProtoMetric._.antisym
d_antisym_52 ::
  T_IsProtoMetric_30 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_52 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_42 (coe v0))
-- Function.Metric.Structures.IsProtoMetric._.isEquivalence
d_isEquivalence_54 ::
  T_IsProtoMetric_30 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_54 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_42 (coe v0)))
-- Function.Metric.Structures.IsProtoMetric._.isPreorder
d_isPreorder_56 ::
  T_IsProtoMetric_30 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_56 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_42 (coe v0))
-- Function.Metric.Structures.IsProtoMetric._.refl
d_refl_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsProtoMetric_30 -> AgdaAny -> AgdaAny
d_refl_58 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12
  = du_refl_58 v12
du_refl_58 :: T_IsProtoMetric_30 -> AgdaAny -> AgdaAny
du_refl_58 v0
  = let v1 = d_isPartialOrder_42 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Function.Metric.Structures.IsProtoMetric._.reflexive
d_reflexive_60 ::
  T_IsProtoMetric_30 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_60 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_42 (coe v0)))
-- Function.Metric.Structures.IsProtoMetric._.trans
d_trans_62 ::
  T_IsProtoMetric_30 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_62 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_42 (coe v0)))
-- Function.Metric.Structures.IsProtoMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsProtoMetric_30 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_64 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                              ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'45''8776'_64 v12
du_'8764''45'resp'45''8776'_64 ::
  T_IsProtoMetric_30 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_64 v0
  = let v1 = d_isPartialOrder_42 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Function.Metric.Structures.IsProtoMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsProtoMetric_30 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_66 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                   ~v8 ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'691''45''8776'_66 v12
du_'8764''45'resp'691''45''8776'_66 ::
  T_IsProtoMetric_30 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_66 v0
  = let v1 = d_isPartialOrder_42 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Function.Metric.Structures.IsProtoMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsProtoMetric_30 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_68 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                   ~v8 ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'737''45''8776'_68 v12
du_'8764''45'resp'737''45''8776'_68 ::
  T_IsProtoMetric_30 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_68 v0
  = let v1 = d_isPartialOrder_42 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Function.Metric.Structures.IsProtoMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsProtoMetric_30 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_72 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                          ~v10 ~v11 v12
  = du_isPartialEquivalence_72 v12
du_isPartialEquivalence_72 ::
  T_IsProtoMetric_30 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_72 v0
  = let v1 = d_isPartialOrder_42 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v2))
-- Function.Metric.Structures.IsProtoMetric._.Eq.refl
d_refl_74 :: T_IsProtoMetric_30 -> AgdaAny -> AgdaAny
d_refl_74 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_42 (coe v0))))
-- Function.Metric.Structures.IsProtoMetric._.Eq.reflexive
d_reflexive_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsProtoMetric_30 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_76 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
               v12
  = du_reflexive_76 v12
du_reflexive_76 ::
  T_IsProtoMetric_30 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_76 v0
  = let v1 = d_isPartialOrder_42 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
           (coe v2))
        v3
-- Function.Metric.Structures.IsProtoMetric._.Eq.sym
d_sym_78 ::
  T_IsProtoMetric_30 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_78 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_42 (coe v0))))
-- Function.Metric.Structures.IsProtoMetric._.Eq.trans
d_trans_80 ::
  T_IsProtoMetric_30 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_80 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_42 (coe v0))))
-- Function.Metric.Structures.IsProtoMetric.EqC.isPartialEquivalence
d_isPartialEquivalence_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsProtoMetric_30 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_84 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                          ~v10 ~v11 v12
  = du_isPartialEquivalence_84 v12
du_isPartialEquivalence_84 ::
  T_IsProtoMetric_30 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_84 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_'8776''45'isEquivalence_44 (coe v0))
-- Function.Metric.Structures.IsProtoMetric.EqC.refl
d_refl_86 :: T_IsProtoMetric_30 -> AgdaAny -> AgdaAny
d_refl_86 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_'8776''45'isEquivalence_44 (coe v0))
-- Function.Metric.Structures.IsProtoMetric.EqC.reflexive
d_reflexive_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsProtoMetric_30 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_88 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
               v12
  = du_reflexive_88 v12
du_reflexive_88 ::
  T_IsProtoMetric_30 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_88 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
      (coe d_'8776''45'isEquivalence_44 (coe v0)) v1
-- Function.Metric.Structures.IsProtoMetric.EqC.sym
d_sym_90 ::
  T_IsProtoMetric_30 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_90 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_'8776''45'isEquivalence_44 (coe v0))
-- Function.Metric.Structures.IsProtoMetric.EqC.trans
d_trans_92 ::
  T_IsProtoMetric_30 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_92 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_'8776''45'isEquivalence_44 (coe v0))
-- Function.Metric.Structures.IsPreMetric
d_IsPreMetric_96 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = ()
data T_IsPreMetric_96
  = C_IsPreMetric'46'constructor_6061 T_IsProtoMetric_30
                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Metric.Structures.IsPreMetric.isProtoMetric
d_isProtoMetric_104 :: T_IsPreMetric_96 -> T_IsProtoMetric_30
d_isProtoMetric_104 v0
  = case coe v0 of
      C_IsPreMetric'46'constructor_6061 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsPreMetric.≈⇒0
d_'8776''8658'0_106 ::
  T_IsPreMetric_96 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8776''8658'0_106 v0
  = case coe v0 of
      C_IsPreMetric'46'constructor_6061 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsPreMetric._.antisym
d_antisym_110 ::
  T_IsPreMetric_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_110 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_42 (coe d_isProtoMetric_104 (coe v0)))
-- Function.Metric.Structures.IsPreMetric._.cong
d_cong_112 ::
  T_IsPreMetric_96 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_112 v0 = coe d_cong_46 (coe d_isProtoMetric_104 (coe v0))
-- Function.Metric.Structures.IsPreMetric._.isEquivalence
d_isEquivalence_114 ::
  T_IsPreMetric_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_114 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_42 (coe d_isProtoMetric_104 (coe v0))))
-- Function.Metric.Structures.IsPreMetric._.isPartialOrder
d_isPartialOrder_116 ::
  T_IsPreMetric_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_116 v0
  = coe d_isPartialOrder_42 (coe d_isProtoMetric_104 (coe v0))
-- Function.Metric.Structures.IsPreMetric._.isPreorder
d_isPreorder_118 ::
  T_IsPreMetric_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_118 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_42 (coe d_isProtoMetric_104 (coe v0)))
-- Function.Metric.Structures.IsPreMetric._.nonNegative
d_nonNegative_120 ::
  T_IsPreMetric_96 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_120 v0
  = coe d_nonNegative_48 (coe d_isProtoMetric_104 (coe v0))
-- Function.Metric.Structures.IsPreMetric._.refl
d_refl_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsPreMetric_96 -> AgdaAny -> AgdaAny
d_refl_122 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12
  = du_refl_122 v12
du_refl_122 :: T_IsPreMetric_96 -> AgdaAny -> AgdaAny
du_refl_122 v0
  = let v1 = d_isProtoMetric_104 (coe v0) in
    let v2 = d_isPartialOrder_42 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Structures.IsPreMetric._.reflexive
d_reflexive_124 ::
  T_IsPreMetric_96 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_124 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_42 (coe d_isProtoMetric_104 (coe v0))))
-- Function.Metric.Structures.IsPreMetric._.trans
d_trans_126 ::
  T_IsPreMetric_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_126 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_42 (coe d_isProtoMetric_104 (coe v0))))
-- Function.Metric.Structures.IsPreMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_128 ::
  T_IsPreMetric_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_128 v0
  = coe
      d_'8776''45'isEquivalence_44 (coe d_isProtoMetric_104 (coe v0))
-- Function.Metric.Structures.IsPreMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsPreMetric_96 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_130 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'45''8776'_130 v12
du_'8764''45'resp'45''8776'_130 ::
  T_IsPreMetric_96 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_130 v0
  = let v1 = d_isProtoMetric_104 (coe v0) in
    let v2 = d_isPartialOrder_42 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Structures.IsPreMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsPreMetric_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_132 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'691''45''8776'_132 v12
du_'8764''45'resp'691''45''8776'_132 ::
  T_IsPreMetric_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_132 v0
  = let v1 = d_isProtoMetric_104 (coe v0) in
    let v2 = d_isPartialOrder_42 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Structures.IsPreMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsPreMetric_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'737''45''8776'_134 v12
du_'8764''45'resp'737''45''8776'_134 ::
  T_IsPreMetric_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_134 v0
  = let v1 = d_isProtoMetric_104 (coe v0) in
    let v2 = d_isPartialOrder_42 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Structures.IsPreMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsPreMetric_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_138 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 ~v11 v12
  = du_isPartialEquivalence_138 v12
du_isPartialEquivalence_138 ::
  T_IsPreMetric_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_138 v0
  = let v1 = d_isProtoMetric_104 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_'8776''45'isEquivalence_44 (coe v1))
-- Function.Metric.Structures.IsPreMetric._.EqC.refl
d_refl_140 :: T_IsPreMetric_96 -> AgdaAny -> AgdaAny
d_refl_140 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_'8776''45'isEquivalence_44 (coe d_isProtoMetric_104 (coe v0)))
-- Function.Metric.Structures.IsPreMetric._.EqC.reflexive
d_reflexive_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsPreMetric_96 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_142 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12
  = du_reflexive_142 v12
du_reflexive_142 ::
  T_IsPreMetric_96 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_142 v0
  = let v1 = d_isProtoMetric_104 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_'8776''45'isEquivalence_44 (coe v1)) v2
-- Function.Metric.Structures.IsPreMetric._.EqC.sym
d_sym_144 ::
  T_IsPreMetric_96 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_144 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_'8776''45'isEquivalence_44 (coe d_isProtoMetric_104 (coe v0)))
-- Function.Metric.Structures.IsPreMetric._.EqC.trans
d_trans_146 ::
  T_IsPreMetric_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_146 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_'8776''45'isEquivalence_44 (coe d_isProtoMetric_104 (coe v0)))
-- Function.Metric.Structures.IsPreMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsPreMetric_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_150 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 ~v11 v12
  = du_isPartialEquivalence_150 v12
du_isPartialEquivalence_150 ::
  T_IsPreMetric_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_150 v0
  = let v1 = d_isProtoMetric_104 (coe v0) in
    let v2 = d_isPartialOrder_42 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v3))
-- Function.Metric.Structures.IsPreMetric._.Eq.refl
d_refl_152 :: T_IsPreMetric_96 -> AgdaAny -> AgdaAny
d_refl_152 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_42 (coe d_isProtoMetric_104 (coe v0)))))
-- Function.Metric.Structures.IsPreMetric._.Eq.reflexive
d_reflexive_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsPreMetric_96 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_154 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12
  = du_reflexive_154 v12
du_reflexive_154 ::
  T_IsPreMetric_96 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_154 v0
  = let v1 = d_isProtoMetric_104 (coe v0) in
    let v2 = d_isPartialOrder_42 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
           (coe v3))
        v4
-- Function.Metric.Structures.IsPreMetric._.Eq.sym
d_sym_156 ::
  T_IsPreMetric_96 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_156 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_42 (coe d_isProtoMetric_104 (coe v0)))))
-- Function.Metric.Structures.IsPreMetric._.Eq.trans
d_trans_158 ::
  T_IsPreMetric_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_158 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_42 (coe d_isProtoMetric_104 (coe v0)))))
-- Function.Metric.Structures.IsQuasiSemiMetric
d_IsQuasiSemiMetric_162 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = ()
data T_IsQuasiSemiMetric_162
  = C_IsQuasiSemiMetric'46'constructor_9549 T_IsPreMetric_96
                                            (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Metric.Structures.IsQuasiSemiMetric.isPreMetric
d_isPreMetric_170 :: T_IsQuasiSemiMetric_162 -> T_IsPreMetric_96
d_isPreMetric_170 v0
  = case coe v0 of
      C_IsQuasiSemiMetric'46'constructor_9549 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsQuasiSemiMetric.0⇒≈
d_0'8658''8776'_172 ::
  T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_0'8658''8776'_172 v0
  = case coe v0 of
      C_IsQuasiSemiMetric'46'constructor_9549 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsQuasiSemiMetric._.antisym
d_antisym_176 ::
  T_IsQuasiSemiMetric_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_176 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         d_isPartialOrder_42
         (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.cong
d_cong_178 ::
  T_IsQuasiSemiMetric_162 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_178 v0
  = coe
      d_cong_46
      (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0)))
-- Function.Metric.Structures.IsQuasiSemiMetric._.isEquivalence
d_isEquivalence_180 ::
  T_IsQuasiSemiMetric_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_180 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_42
            (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0)))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.isPartialOrder
d_isPartialOrder_182 ::
  T_IsQuasiSemiMetric_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_182 v0
  = coe
      d_isPartialOrder_42
      (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0)))
-- Function.Metric.Structures.IsQuasiSemiMetric._.isPreorder
d_isPreorder_184 ::
  T_IsQuasiSemiMetric_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_184 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         d_isPartialOrder_42
         (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.isProtoMetric
d_isProtoMetric_186 ::
  T_IsQuasiSemiMetric_162 -> T_IsProtoMetric_30
d_isProtoMetric_186 v0
  = coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0))
-- Function.Metric.Structures.IsQuasiSemiMetric._.nonNegative
d_nonNegative_188 ::
  T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_188 v0
  = coe
      d_nonNegative_48
      (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0)))
-- Function.Metric.Structures.IsQuasiSemiMetric._.refl
d_refl_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny
d_refl_190 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12
  = du_refl_190 v12
du_refl_190 :: T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny
du_refl_190 v0
  = let v1 = d_isPreMetric_170 (coe v0) in
    let v2 = d_isProtoMetric_104 (coe v1) in
    let v3 = d_isPartialOrder_42 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Function.Metric.Structures.IsQuasiSemiMetric._.reflexive
d_reflexive_192 ::
  T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_192 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_42
            (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0)))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.trans
d_trans_194 ::
  T_IsQuasiSemiMetric_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_194 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_42
            (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0)))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_196 ::
  T_IsQuasiSemiMetric_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_196 v0
  = coe
      d_'8776''45'isEquivalence_44
      (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0)))
-- Function.Metric.Structures.IsQuasiSemiMetric._.≈⇒0
d_'8776''8658'0_198 ::
  T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8776''8658'0_198 v0
  = coe d_'8776''8658'0_106 (coe d_isPreMetric_170 (coe v0))
-- Function.Metric.Structures.IsQuasiSemiMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasiSemiMetric_162 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_200 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'45''8776'_200 v12
du_'8764''45'resp'45''8776'_200 ::
  T_IsQuasiSemiMetric_162 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_200 v0
  = let v1 = d_isPreMetric_170 (coe v0) in
    let v2 = d_isProtoMetric_104 (coe v1) in
    let v3 = d_isPartialOrder_42 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Function.Metric.Structures.IsQuasiSemiMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasiSemiMetric_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_202 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'691''45''8776'_202 v12
du_'8764''45'resp'691''45''8776'_202 ::
  T_IsQuasiSemiMetric_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_202 v0
  = let v1 = d_isPreMetric_170 (coe v0) in
    let v2 = d_isProtoMetric_104 (coe v1) in
    let v3 = d_isPartialOrder_42 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Function.Metric.Structures.IsQuasiSemiMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasiSemiMetric_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_204 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'737''45''8776'_204 v12
du_'8764''45'resp'737''45''8776'_204 ::
  T_IsQuasiSemiMetric_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_204 v0
  = let v1 = d_isPreMetric_170 (coe v0) in
    let v2 = d_isProtoMetric_104 (coe v1) in
    let v3 = d_isPartialOrder_42 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Function.Metric.Structures.IsQuasiSemiMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasiSemiMetric_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_208 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 ~v11 v12
  = du_isPartialEquivalence_208 v12
du_isPartialEquivalence_208 ::
  T_IsQuasiSemiMetric_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_208 v0
  = let v1 = d_isPreMetric_170 (coe v0) in
    let v2 = d_isProtoMetric_104 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_'8776''45'isEquivalence_44 (coe v2))
-- Function.Metric.Structures.IsQuasiSemiMetric._.EqC.refl
d_refl_210 :: T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny
d_refl_210 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_'8776''45'isEquivalence_44
         (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.EqC.reflexive
d_reflexive_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasiSemiMetric_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_212 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12
  = du_reflexive_212 v12
du_reflexive_212 ::
  T_IsQuasiSemiMetric_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_212 v0
  = let v1 = d_isPreMetric_170 (coe v0) in
    let v2 = d_isProtoMetric_104 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_'8776''45'isEquivalence_44 (coe v2)) v3
-- Function.Metric.Structures.IsQuasiSemiMetric._.EqC.sym
d_sym_214 ::
  T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_214 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_'8776''45'isEquivalence_44
         (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.EqC.trans
d_trans_216 ::
  T_IsQuasiSemiMetric_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_216 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_'8776''45'isEquivalence_44
         (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasiSemiMetric_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_220 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 ~v11 v12
  = du_isPartialEquivalence_220 v12
du_isPartialEquivalence_220 ::
  T_IsQuasiSemiMetric_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_220 v0
  = let v1 = d_isPreMetric_170 (coe v0) in
    let v2 = d_isProtoMetric_104 (coe v1) in
    let v3 = d_isPartialOrder_42 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v4))
-- Function.Metric.Structures.IsQuasiSemiMetric._.Eq.refl
d_refl_222 :: T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny
d_refl_222 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_42
               (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0))))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.Eq.reflexive
d_reflexive_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasiSemiMetric_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_224 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12
  = du_reflexive_224 v12
du_reflexive_224 ::
  T_IsQuasiSemiMetric_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_224 v0
  = let v1 = d_isPreMetric_170 (coe v0) in
    let v2 = d_isProtoMetric_104 (coe v1) in
    let v3 = d_isPartialOrder_42 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
           (coe v4))
        v5
-- Function.Metric.Structures.IsQuasiSemiMetric._.Eq.sym
d_sym_226 ::
  T_IsQuasiSemiMetric_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_226 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_42
               (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0))))))
-- Function.Metric.Structures.IsQuasiSemiMetric._.Eq.trans
d_trans_228 ::
  T_IsQuasiSemiMetric_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_228 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_42
               (coe d_isProtoMetric_104 (coe d_isPreMetric_170 (coe v0))))))
-- Function.Metric.Structures.IsSemiMetric
d_IsSemiMetric_232 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = ()
data T_IsSemiMetric_232
  = C_IsSemiMetric'46'constructor_13167 T_IsQuasiSemiMetric_162
                                        (AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Metric.Structures.IsSemiMetric.isQuasiSemiMetric
d_isQuasiSemiMetric_240 ::
  T_IsSemiMetric_232 -> T_IsQuasiSemiMetric_162
d_isQuasiSemiMetric_240 v0
  = case coe v0 of
      C_IsSemiMetric'46'constructor_13167 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsSemiMetric.sym
d_sym_242 :: T_IsSemiMetric_232 -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_242 v0
  = case coe v0 of
      C_IsSemiMetric'46'constructor_13167 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsSemiMetric._.0⇒≈
d_0'8658''8776'_246 ::
  T_IsSemiMetric_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_0'8658''8776'_246 v0
  = coe d_0'8658''8776'_172 (coe d_isQuasiSemiMetric_240 (coe v0))
-- Function.Metric.Structures.IsSemiMetric._.antisym
d_antisym_248 ::
  T_IsSemiMetric_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_248 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         d_isPartialOrder_42
         (coe
            d_isProtoMetric_104
            (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))))
-- Function.Metric.Structures.IsSemiMetric._.cong
d_cong_250 ::
  T_IsSemiMetric_232 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_250 v0
  = coe
      d_cong_46
      (coe
         d_isProtoMetric_104
         (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0))))
-- Function.Metric.Structures.IsSemiMetric._.isEquivalence
d_isEquivalence_252 ::
  T_IsSemiMetric_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_252 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_42
            (coe
               d_isProtoMetric_104
               (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0))))))
-- Function.Metric.Structures.IsSemiMetric._.isPartialOrder
d_isPartialOrder_254 ::
  T_IsSemiMetric_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_254 v0
  = coe
      d_isPartialOrder_42
      (coe
         d_isProtoMetric_104
         (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0))))
-- Function.Metric.Structures.IsSemiMetric._.isPreMetric
d_isPreMetric_256 :: T_IsSemiMetric_232 -> T_IsPreMetric_96
d_isPreMetric_256 v0
  = coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0))
-- Function.Metric.Structures.IsSemiMetric._.isPreorder
d_isPreorder_258 ::
  T_IsSemiMetric_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_258 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         d_isPartialOrder_42
         (coe
            d_isProtoMetric_104
            (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))))
-- Function.Metric.Structures.IsSemiMetric._.isProtoMetric
d_isProtoMetric_260 :: T_IsSemiMetric_232 -> T_IsProtoMetric_30
d_isProtoMetric_260 v0
  = coe
      d_isProtoMetric_104
      (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))
-- Function.Metric.Structures.IsSemiMetric._.nonNegative
d_nonNegative_262 ::
  T_IsSemiMetric_232 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_262 v0
  = coe
      d_nonNegative_48
      (coe
         d_isProtoMetric_104
         (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0))))
-- Function.Metric.Structures.IsSemiMetric._.refl
d_refl_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemiMetric_232 -> AgdaAny -> AgdaAny
d_refl_264 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12
  = du_refl_264 v12
du_refl_264 :: T_IsSemiMetric_232 -> AgdaAny -> AgdaAny
du_refl_264 v0
  = let v1 = d_isQuasiSemiMetric_240 (coe v0) in
    let v2 = d_isPreMetric_170 (coe v1) in
    let v3 = d_isProtoMetric_104 (coe v2) in
    let v4 = d_isPartialOrder_42 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Function.Metric.Structures.IsSemiMetric._.reflexive
d_reflexive_266 ::
  T_IsSemiMetric_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_266 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_42
            (coe
               d_isProtoMetric_104
               (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0))))))
-- Function.Metric.Structures.IsSemiMetric._.trans
d_trans_268 ::
  T_IsSemiMetric_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_268 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_42
            (coe
               d_isProtoMetric_104
               (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0))))))
-- Function.Metric.Structures.IsSemiMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_270 ::
  T_IsSemiMetric_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_270 v0
  = coe
      d_'8776''45'isEquivalence_44
      (coe
         d_isProtoMetric_104
         (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0))))
-- Function.Metric.Structures.IsSemiMetric._.≈⇒0
d_'8776''8658'0_272 ::
  T_IsSemiMetric_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8776''8658'0_272 v0
  = coe
      d_'8776''8658'0_106
      (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))
-- Function.Metric.Structures.IsSemiMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemiMetric_232 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_274 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'45''8776'_274 v12
du_'8764''45'resp'45''8776'_274 ::
  T_IsSemiMetric_232 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_274 v0
  = let v1 = d_isQuasiSemiMetric_240 (coe v0) in
    let v2 = d_isPreMetric_170 (coe v1) in
    let v3 = d_isProtoMetric_104 (coe v2) in
    let v4 = d_isPartialOrder_42 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Function.Metric.Structures.IsSemiMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemiMetric_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_276 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'691''45''8776'_276 v12
du_'8764''45'resp'691''45''8776'_276 ::
  T_IsSemiMetric_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_276 v0
  = let v1 = d_isQuasiSemiMetric_240 (coe v0) in
    let v2 = d_isPreMetric_170 (coe v1) in
    let v3 = d_isProtoMetric_104 (coe v2) in
    let v4 = d_isPartialOrder_42 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Function.Metric.Structures.IsSemiMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemiMetric_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_278 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 ~v11 v12
  = du_'8764''45'resp'737''45''8776'_278 v12
du_'8764''45'resp'737''45''8776'_278 ::
  T_IsSemiMetric_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_278 v0
  = let v1 = d_isQuasiSemiMetric_240 (coe v0) in
    let v2 = d_isPreMetric_170 (coe v1) in
    let v3 = d_isProtoMetric_104 (coe v2) in
    let v4 = d_isPartialOrder_42 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Function.Metric.Structures.IsSemiMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemiMetric_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_282 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 ~v11 v12
  = du_isPartialEquivalence_282 v12
du_isPartialEquivalence_282 ::
  T_IsSemiMetric_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_282 v0
  = let v1 = d_isQuasiSemiMetric_240 (coe v0) in
    let v2 = d_isPreMetric_170 (coe v1) in
    let v3 = d_isProtoMetric_104 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_'8776''45'isEquivalence_44 (coe v3))
-- Function.Metric.Structures.IsSemiMetric._.EqC.refl
d_refl_284 :: T_IsSemiMetric_232 -> AgdaAny -> AgdaAny
d_refl_284 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_'8776''45'isEquivalence_44
         (coe
            d_isProtoMetric_104
            (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))))
-- Function.Metric.Structures.IsSemiMetric._.EqC.reflexive
d_reflexive_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemiMetric_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_286 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12
  = du_reflexive_286 v12
du_reflexive_286 ::
  T_IsSemiMetric_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_286 v0
  = let v1 = d_isQuasiSemiMetric_240 (coe v0) in
    let v2 = d_isPreMetric_170 (coe v1) in
    let v3 = d_isProtoMetric_104 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_'8776''45'isEquivalence_44 (coe v3)) v4
-- Function.Metric.Structures.IsSemiMetric._.EqC.sym
d_sym_288 ::
  T_IsSemiMetric_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_288 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_'8776''45'isEquivalence_44
         (coe
            d_isProtoMetric_104
            (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))))
-- Function.Metric.Structures.IsSemiMetric._.EqC.trans
d_trans_290 ::
  T_IsSemiMetric_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_290 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_'8776''45'isEquivalence_44
         (coe
            d_isProtoMetric_104
            (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))))
-- Function.Metric.Structures.IsSemiMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemiMetric_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_294 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 ~v11 v12
  = du_isPartialEquivalence_294 v12
du_isPartialEquivalence_294 ::
  T_IsSemiMetric_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_294 v0
  = let v1 = d_isQuasiSemiMetric_240 (coe v0) in
    let v2 = d_isPreMetric_170 (coe v1) in
    let v3 = d_isProtoMetric_104 (coe v2) in
    let v4 = d_isPartialOrder_42 (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v5))
-- Function.Metric.Structures.IsSemiMetric._.Eq.refl
d_refl_296 :: T_IsSemiMetric_232 -> AgdaAny -> AgdaAny
d_refl_296 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_42
               (coe
                  d_isProtoMetric_104
                  (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))))))
-- Function.Metric.Structures.IsSemiMetric._.Eq.reflexive
d_reflexive_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemiMetric_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_298 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12
  = du_reflexive_298 v12
du_reflexive_298 ::
  T_IsSemiMetric_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_298 v0
  = let v1 = d_isQuasiSemiMetric_240 (coe v0) in
    let v2 = d_isPreMetric_170 (coe v1) in
    let v3 = d_isProtoMetric_104 (coe v2) in
    let v4 = d_isPartialOrder_42 (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
           (coe v5))
        v6
-- Function.Metric.Structures.IsSemiMetric._.Eq.sym
d_sym_300 ::
  T_IsSemiMetric_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_300 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_42
               (coe
                  d_isProtoMetric_104
                  (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))))))
-- Function.Metric.Structures.IsSemiMetric._.Eq.trans
d_trans_302 ::
  T_IsSemiMetric_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_302 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_42
               (coe
                  d_isProtoMetric_104
                  (coe d_isPreMetric_170 (coe d_isQuasiSemiMetric_240 (coe v0)))))))
-- Function.Metric.Structures.IsGeneralMetric
d_IsGeneralMetric_308 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
  = ()
data T_IsGeneralMetric_308
  = C_IsGeneralMetric'46'constructor_17141 T_IsSemiMetric_232
                                           (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Metric.Structures.IsGeneralMetric.isSemiMetric
d_isSemiMetric_318 :: T_IsGeneralMetric_308 -> T_IsSemiMetric_232
d_isSemiMetric_318 v0
  = case coe v0 of
      C_IsGeneralMetric'46'constructor_17141 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsGeneralMetric.triangle
d_triangle_320 ::
  T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_triangle_320 v0
  = case coe v0 of
      C_IsGeneralMetric'46'constructor_17141 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Structures.IsGeneralMetric._.0⇒≈
d_0'8658''8776'_324 ::
  T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_0'8658''8776'_324 v0
  = coe
      d_0'8658''8776'_172
      (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0)))
-- Function.Metric.Structures.IsGeneralMetric._.antisym
d_antisym_326 ::
  T_IsGeneralMetric_308 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_326 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         d_isPartialOrder_42
         (coe
            d_isProtoMetric_104
            (coe
               d_isPreMetric_170
               (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))))
-- Function.Metric.Structures.IsGeneralMetric._.cong
d_cong_328 ::
  T_IsGeneralMetric_308 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_328 v0
  = coe
      d_cong_46
      (coe
         d_isProtoMetric_104
         (coe
            d_isPreMetric_170
            (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0)))))
-- Function.Metric.Structures.IsGeneralMetric._.isEquivalence
d_isEquivalence_330 ::
  T_IsGeneralMetric_308 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_330 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_42
            (coe
               d_isProtoMetric_104
               (coe
                  d_isPreMetric_170
                  (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0)))))))
-- Function.Metric.Structures.IsGeneralMetric._.isPartialOrder
d_isPartialOrder_332 ::
  T_IsGeneralMetric_308 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_332 v0
  = coe
      d_isPartialOrder_42
      (coe
         d_isProtoMetric_104
         (coe
            d_isPreMetric_170
            (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0)))))
-- Function.Metric.Structures.IsGeneralMetric._.isPreMetric
d_isPreMetric_334 :: T_IsGeneralMetric_308 -> T_IsPreMetric_96
d_isPreMetric_334 v0
  = coe
      d_isPreMetric_170
      (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0)))
-- Function.Metric.Structures.IsGeneralMetric._.isPreorder
d_isPreorder_336 ::
  T_IsGeneralMetric_308 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_336 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         d_isPartialOrder_42
         (coe
            d_isProtoMetric_104
            (coe
               d_isPreMetric_170
               (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))))
-- Function.Metric.Structures.IsGeneralMetric._.isProtoMetric
d_isProtoMetric_338 :: T_IsGeneralMetric_308 -> T_IsProtoMetric_30
d_isProtoMetric_338 v0
  = coe
      d_isProtoMetric_104
      (coe
         d_isPreMetric_170
         (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))
-- Function.Metric.Structures.IsGeneralMetric._.isQuasiSemiMetric
d_isQuasiSemiMetric_340 ::
  T_IsGeneralMetric_308 -> T_IsQuasiSemiMetric_162
d_isQuasiSemiMetric_340 v0
  = coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))
-- Function.Metric.Structures.IsGeneralMetric._.nonNegative
d_nonNegative_342 ::
  T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_342 v0
  = coe
      d_nonNegative_48
      (coe
         d_isProtoMetric_104
         (coe
            d_isPreMetric_170
            (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0)))))
-- Function.Metric.Structures.IsGeneralMetric._.refl
d_refl_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny
d_refl_344 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 ~v12
           v13
  = du_refl_344 v13
du_refl_344 :: T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny
du_refl_344 v0
  = let v1 = d_isSemiMetric_318 (coe v0) in
    let v2 = d_isQuasiSemiMetric_240 (coe v1) in
    let v3 = d_isPreMetric_170 (coe v2) in
    let v4 = d_isProtoMetric_104 (coe v3) in
    let v5 = d_isPartialOrder_42 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Function.Metric.Structures.IsGeneralMetric._.reflexive
d_reflexive_346 ::
  T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_346 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_42
            (coe
               d_isProtoMetric_104
               (coe
                  d_isPreMetric_170
                  (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0)))))))
-- Function.Metric.Structures.IsGeneralMetric._.sym
d_sym_348 :: T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_348 v0 = coe d_sym_242 (coe d_isSemiMetric_318 (coe v0))
-- Function.Metric.Structures.IsGeneralMetric._.trans
d_trans_350 ::
  T_IsGeneralMetric_308 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_350 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_42
            (coe
               d_isProtoMetric_104
               (coe
                  d_isPreMetric_170
                  (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0)))))))
-- Function.Metric.Structures.IsGeneralMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_352 ::
  T_IsGeneralMetric_308 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_352 v0
  = coe
      d_'8776''45'isEquivalence_44
      (coe
         d_isProtoMetric_104
         (coe
            d_isPreMetric_170
            (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0)))))
-- Function.Metric.Structures.IsGeneralMetric._.≈⇒0
d_'8776''8658'0_354 ::
  T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8776''8658'0_354 v0
  = coe
      d_'8776''8658'0_106
      (coe
         d_isPreMetric_170
         (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))
-- Function.Metric.Structures.IsGeneralMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsGeneralMetric_308 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_356 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 ~v11 ~v12 v13
  = du_'8764''45'resp'45''8776'_356 v13
du_'8764''45'resp'45''8776'_356 ::
  T_IsGeneralMetric_308 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_356 v0
  = let v1 = d_isSemiMetric_318 (coe v0) in
    let v2 = d_isQuasiSemiMetric_240 (coe v1) in
    let v3 = d_isPreMetric_170 (coe v2) in
    let v4 = d_isProtoMetric_104 (coe v3) in
    let v5 = d_isPartialOrder_42 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Function.Metric.Structures.IsGeneralMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsGeneralMetric_308 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_358 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 ~v11 ~v12 v13
  = du_'8764''45'resp'691''45''8776'_358 v13
du_'8764''45'resp'691''45''8776'_358 ::
  T_IsGeneralMetric_308 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_358 v0
  = let v1 = d_isSemiMetric_318 (coe v0) in
    let v2 = d_isQuasiSemiMetric_240 (coe v1) in
    let v3 = d_isPreMetric_170 (coe v2) in
    let v4 = d_isProtoMetric_104 (coe v3) in
    let v5 = d_isPartialOrder_42 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Function.Metric.Structures.IsGeneralMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsGeneralMetric_308 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_360 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 ~v11 ~v12 v13
  = du_'8764''45'resp'737''45''8776'_360 v13
du_'8764''45'resp'737''45''8776'_360 ::
  T_IsGeneralMetric_308 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_360 v0
  = let v1 = d_isSemiMetric_318 (coe v0) in
    let v2 = d_isQuasiSemiMetric_240 (coe v1) in
    let v3 = d_isPreMetric_170 (coe v2) in
    let v4 = d_isProtoMetric_104 (coe v3) in
    let v5 = d_isPartialOrder_42 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Function.Metric.Structures.IsGeneralMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsGeneralMetric_308 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_364 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 ~v11 ~v12 v13
  = du_isPartialEquivalence_364 v13
du_isPartialEquivalence_364 ::
  T_IsGeneralMetric_308 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_364 v0
  = let v1 = d_isSemiMetric_318 (coe v0) in
    let v2 = d_isQuasiSemiMetric_240 (coe v1) in
    let v3 = d_isPreMetric_170 (coe v2) in
    let v4 = d_isProtoMetric_104 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_'8776''45'isEquivalence_44 (coe v4))
-- Function.Metric.Structures.IsGeneralMetric._.EqC.refl
d_refl_366 :: T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny
d_refl_366 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_'8776''45'isEquivalence_44
         (coe
            d_isProtoMetric_104
            (coe
               d_isPreMetric_170
               (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))))
-- Function.Metric.Structures.IsGeneralMetric._.EqC.reflexive
d_reflexive_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsGeneralMetric_308 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_368 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                ~v12 v13
  = du_reflexive_368 v13
du_reflexive_368 ::
  T_IsGeneralMetric_308 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_368 v0
  = let v1 = d_isSemiMetric_318 (coe v0) in
    let v2 = d_isQuasiSemiMetric_240 (coe v1) in
    let v3 = d_isPreMetric_170 (coe v2) in
    let v4 = d_isProtoMetric_104 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_'8776''45'isEquivalence_44 (coe v4)) v5
-- Function.Metric.Structures.IsGeneralMetric._.EqC.sym
d_sym_370 ::
  T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_370 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_'8776''45'isEquivalence_44
         (coe
            d_isProtoMetric_104
            (coe
               d_isPreMetric_170
               (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))))
-- Function.Metric.Structures.IsGeneralMetric._.EqC.trans
d_trans_372 ::
  T_IsGeneralMetric_308 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_372 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_'8776''45'isEquivalence_44
         (coe
            d_isProtoMetric_104
            (coe
               d_isPreMetric_170
               (coe d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))))
-- Function.Metric.Structures.IsGeneralMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsGeneralMetric_308 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_376 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 ~v11 ~v12 v13
  = du_isPartialEquivalence_376 v13
du_isPartialEquivalence_376 ::
  T_IsGeneralMetric_308 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_376 v0
  = let v1 = d_isSemiMetric_318 (coe v0) in
    let v2 = d_isQuasiSemiMetric_240 (coe v1) in
    let v3 = d_isPreMetric_170 (coe v2) in
    let v4 = d_isProtoMetric_104 (coe v3) in
    let v5 = d_isPartialOrder_42 (coe v4) in
    let v6
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v6))
-- Function.Metric.Structures.IsGeneralMetric._.Eq.refl
d_refl_378 :: T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny
d_refl_378 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_42
               (coe
                  d_isProtoMetric_104
                  (coe
                     d_isPreMetric_170
                     (coe
                        d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))))))
-- Function.Metric.Structures.IsGeneralMetric._.Eq.reflexive
d_reflexive_380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsGeneralMetric_308 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_380 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                ~v12 v13
  = du_reflexive_380 v13
du_reflexive_380 ::
  T_IsGeneralMetric_308 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_380 v0
  = let v1 = d_isSemiMetric_318 (coe v0) in
    let v2 = d_isQuasiSemiMetric_240 (coe v1) in
    let v3 = d_isPreMetric_170 (coe v2) in
    let v4 = d_isProtoMetric_104 (coe v3) in
    let v5 = d_isPartialOrder_42 (coe v4) in
    let v6
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
           (coe v6))
        v7
-- Function.Metric.Structures.IsGeneralMetric._.Eq.sym
d_sym_382 ::
  T_IsGeneralMetric_308 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_382 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_42
               (coe
                  d_isProtoMetric_104
                  (coe
                     d_isPreMetric_170
                     (coe
                        d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))))))
-- Function.Metric.Structures.IsGeneralMetric._.Eq.trans
d_trans_384 ::
  T_IsGeneralMetric_308 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_384 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_42
               (coe
                  d_isProtoMetric_104
                  (coe
                     d_isPreMetric_170
                     (coe
                        d_isQuasiSemiMetric_240 (coe d_isSemiMetric_318 (coe v0))))))))
