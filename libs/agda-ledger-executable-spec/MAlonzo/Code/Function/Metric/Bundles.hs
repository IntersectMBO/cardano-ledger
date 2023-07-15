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

module MAlonzo.Code.Function.Metric.Bundles where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Metric.Structures
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Metric.Bundles.ProtoMetric
d_ProtoMetric_16 a0 a1 a2 a3 a4 = ()
data T_ProtoMetric_16
  = C_ProtoMetric'46'constructor_953 AgdaAny
                                     (AgdaAny -> AgdaAny -> AgdaAny)
                                     MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
-- Function.Metric.Bundles.ProtoMetric.Carrier
d_Carrier_44 :: T_ProtoMetric_16 -> ()
d_Carrier_44 = erased
-- Function.Metric.Bundles.ProtoMetric.Image
d_Image_46 :: T_ProtoMetric_16 -> ()
d_Image_46 = erased
-- Function.Metric.Bundles.ProtoMetric._≈_
d__'8776'__48 :: T_ProtoMetric_16 -> AgdaAny -> AgdaAny -> ()
d__'8776'__48 = erased
-- Function.Metric.Bundles.ProtoMetric._≈ᵢ_
d__'8776''7522'__50 :: T_ProtoMetric_16 -> AgdaAny -> AgdaAny -> ()
d__'8776''7522'__50 = erased
-- Function.Metric.Bundles.ProtoMetric._≤_
d__'8804'__52 :: T_ProtoMetric_16 -> AgdaAny -> AgdaAny -> ()
d__'8804'__52 = erased
-- Function.Metric.Bundles.ProtoMetric.0#
d_0'35'_54 :: T_ProtoMetric_16 -> AgdaAny
d_0'35'_54 v0
  = case coe v0 of
      C_ProtoMetric'46'constructor_953 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.ProtoMetric.d
d_d_56 :: T_ProtoMetric_16 -> AgdaAny -> AgdaAny -> AgdaAny
d_d_56 v0
  = case coe v0 of
      C_ProtoMetric'46'constructor_953 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.ProtoMetric.isProtoMetric
d_isProtoMetric_58 ::
  T_ProtoMetric_16 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_58 v0
  = case coe v0 of
      C_ProtoMetric'46'constructor_953 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.ProtoMetric._.antisym
d_antisym_62 ::
  T_ProtoMetric_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_62 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe d_isProtoMetric_58 (coe v0)))
-- Function.Metric.Bundles.ProtoMetric._.cong
d_cong_64 ::
  T_ProtoMetric_16 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_64 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_cong_46
      (coe d_isProtoMetric_58 (coe v0))
-- Function.Metric.Bundles.ProtoMetric._.isEquivalence
d_isEquivalence_66 ::
  T_ProtoMetric_16 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_66 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe d_isProtoMetric_58 (coe v0))))
-- Function.Metric.Bundles.ProtoMetric._.isPartialOrder
d_isPartialOrder_68 ::
  T_ProtoMetric_16 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_68 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
      (coe d_isProtoMetric_58 (coe v0))
-- Function.Metric.Bundles.ProtoMetric._.isPreorder
d_isPreorder_70 ::
  T_ProtoMetric_16 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_70 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe d_isProtoMetric_58 (coe v0)))
-- Function.Metric.Bundles.ProtoMetric._.nonNegative
d_nonNegative_72 ::
  T_ProtoMetric_16 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_72 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_nonNegative_48
      (coe d_isProtoMetric_58 (coe v0))
-- Function.Metric.Bundles.ProtoMetric._.refl
d_refl_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_16 -> AgdaAny -> AgdaAny
d_refl_74 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_refl_74 v5
du_refl_74 :: T_ProtoMetric_16 -> AgdaAny -> AgdaAny
du_refl_74 v0
  = let v1 = d_isProtoMetric_58 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Bundles.ProtoMetric._.reflexive
d_reflexive_76 ::
  T_ProtoMetric_16 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_76 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe d_isProtoMetric_58 (coe v0))))
-- Function.Metric.Bundles.ProtoMetric._.trans
d_trans_78 ::
  T_ProtoMetric_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_78 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe d_isProtoMetric_58 (coe v0))))
-- Function.Metric.Bundles.ProtoMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_80 ::
  T_ProtoMetric_16 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_80 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
      (coe d_isProtoMetric_58 (coe v0))
-- Function.Metric.Bundles.ProtoMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_16 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_82 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'45''8776'_82 v5
du_'8764''45'resp'45''8776'_82 ::
  T_ProtoMetric_16 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_82 v0
  = let v1 = d_isProtoMetric_58 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Bundles.ProtoMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_84 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'691''45''8776'_84 v5
du_'8764''45'resp'691''45''8776'_84 ::
  T_ProtoMetric_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_84 v0
  = let v1 = d_isProtoMetric_58 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Bundles.ProtoMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_86 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'737''45''8776'_86 v5
du_'8764''45'resp'737''45''8776'_86 ::
  T_ProtoMetric_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_86 v0
  = let v1 = d_isProtoMetric_58 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Bundles.ProtoMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_16 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_90 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_90 v5
du_isPartialEquivalence_90 ::
  T_ProtoMetric_16 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_90 v0
  = let v1 = d_isProtoMetric_58 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe v1))
-- Function.Metric.Bundles.ProtoMetric._.EqC.refl
d_refl_92 :: T_ProtoMetric_16 -> AgdaAny -> AgdaAny
d_refl_92 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe d_isProtoMetric_58 (coe v0)))
-- Function.Metric.Bundles.ProtoMetric._.EqC.reflexive
d_reflexive_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_16 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_94 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_94 v5
du_reflexive_94 ::
  T_ProtoMetric_16 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_94 v0
  = let v1 = d_isProtoMetric_58 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
           (coe v1))
        v2
-- Function.Metric.Bundles.ProtoMetric._.EqC.sym
d_sym_96 ::
  T_ProtoMetric_16 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_96 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe d_isProtoMetric_58 (coe v0)))
-- Function.Metric.Bundles.ProtoMetric._.EqC.trans
d_trans_98 ::
  T_ProtoMetric_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_98 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe d_isProtoMetric_58 (coe v0)))
-- Function.Metric.Bundles.ProtoMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_16 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_102 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_102 v5
du_isPartialEquivalence_102 ::
  T_ProtoMetric_16 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_102 v0
  = let v1 = d_isProtoMetric_58 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v3))
-- Function.Metric.Bundles.ProtoMetric._.Eq.refl
d_refl_104 :: T_ProtoMetric_16 -> AgdaAny -> AgdaAny
d_refl_104 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe d_isProtoMetric_58 (coe v0)))))
-- Function.Metric.Bundles.ProtoMetric._.Eq.reflexive
d_reflexive_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_16 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_106 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_106 v5
du_reflexive_106 ::
  T_ProtoMetric_16 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_106 v0
  = let v1 = d_isProtoMetric_58 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
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
-- Function.Metric.Bundles.ProtoMetric._.Eq.sym
d_sym_108 ::
  T_ProtoMetric_16 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_108 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe d_isProtoMetric_58 (coe v0)))))
-- Function.Metric.Bundles.ProtoMetric._.Eq.trans
d_trans_110 ::
  T_ProtoMetric_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_110 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe d_isProtoMetric_58 (coe v0)))))
-- Function.Metric.Bundles.PreMetric
d_PreMetric_122 a0 a1 a2 a3 a4 = ()
data T_PreMetric_122
  = C_PreMetric'46'constructor_4141 AgdaAny
                                    (AgdaAny -> AgdaAny -> AgdaAny)
                                    MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
-- Function.Metric.Bundles.PreMetric.Carrier
d_Carrier_150 :: T_PreMetric_122 -> ()
d_Carrier_150 = erased
-- Function.Metric.Bundles.PreMetric.Image
d_Image_152 :: T_PreMetric_122 -> ()
d_Image_152 = erased
-- Function.Metric.Bundles.PreMetric._≈_
d__'8776'__154 :: T_PreMetric_122 -> AgdaAny -> AgdaAny -> ()
d__'8776'__154 = erased
-- Function.Metric.Bundles.PreMetric._≈ᵢ_
d__'8776''7522'__156 :: T_PreMetric_122 -> AgdaAny -> AgdaAny -> ()
d__'8776''7522'__156 = erased
-- Function.Metric.Bundles.PreMetric._≤_
d__'8804'__158 :: T_PreMetric_122 -> AgdaAny -> AgdaAny -> ()
d__'8804'__158 = erased
-- Function.Metric.Bundles.PreMetric.0#
d_0'35'_160 :: T_PreMetric_122 -> AgdaAny
d_0'35'_160 v0
  = case coe v0 of
      C_PreMetric'46'constructor_4141 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.PreMetric.d
d_d_162 :: T_PreMetric_122 -> AgdaAny -> AgdaAny -> AgdaAny
d_d_162 v0
  = case coe v0 of
      C_PreMetric'46'constructor_4141 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.PreMetric.isPreMetric
d_isPreMetric_164 ::
  T_PreMetric_122 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_isPreMetric_164 v0
  = case coe v0 of
      C_PreMetric'46'constructor_4141 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.PreMetric._.antisym
d_antisym_168 ::
  T_PreMetric_122 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_168 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe d_isPreMetric_164 (coe v0))))
-- Function.Metric.Bundles.PreMetric._.cong
d_cong_170 ::
  T_PreMetric_122 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_170 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_cong_46
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe d_isPreMetric_164 (coe v0)))
-- Function.Metric.Bundles.PreMetric._.isEquivalence
d_isEquivalence_172 ::
  T_PreMetric_122 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_172 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe d_isPreMetric_164 (coe v0)))))
-- Function.Metric.Bundles.PreMetric._.isPartialOrder
d_isPartialOrder_174 ::
  T_PreMetric_122 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_174 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe d_isPreMetric_164 (coe v0)))
-- Function.Metric.Bundles.PreMetric._.isPreorder
d_isPreorder_176 ::
  T_PreMetric_122 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_176 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe d_isPreMetric_164 (coe v0))))
-- Function.Metric.Bundles.PreMetric._.isProtoMetric
d_isProtoMetric_178 ::
  T_PreMetric_122 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_178 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
      (coe d_isPreMetric_164 (coe v0))
-- Function.Metric.Bundles.PreMetric._.nonNegative
d_nonNegative_180 ::
  T_PreMetric_122 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_180 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_nonNegative_48
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe d_isPreMetric_164 (coe v0)))
-- Function.Metric.Bundles.PreMetric._.refl
d_refl_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_122 -> AgdaAny -> AgdaAny
d_refl_182 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_refl_182 v5
du_refl_182 :: T_PreMetric_122 -> AgdaAny -> AgdaAny
du_refl_182 v0
  = let v1 = d_isPreMetric_164 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Function.Metric.Bundles.PreMetric._.reflexive
d_reflexive_184 ::
  T_PreMetric_122 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_184 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe d_isPreMetric_164 (coe v0)))))
-- Function.Metric.Bundles.PreMetric._.trans
d_trans_186 ::
  T_PreMetric_122 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_186 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe d_isPreMetric_164 (coe v0)))))
-- Function.Metric.Bundles.PreMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_188 ::
  T_PreMetric_122 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_188 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe d_isPreMetric_164 (coe v0)))
-- Function.Metric.Bundles.PreMetric._.≈⇒0
d_'8776''8658'0_190 ::
  T_PreMetric_122 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8776''8658'0_190 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''8658'0_106
      (coe d_isPreMetric_164 (coe v0))
-- Function.Metric.Bundles.PreMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_122 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_192 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'45''8776'_192 v5
du_'8764''45'resp'45''8776'_192 ::
  T_PreMetric_122 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_192 v0
  = let v1 = d_isPreMetric_164 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Function.Metric.Bundles.PreMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_122 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_194 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'691''45''8776'_194 v5
du_'8764''45'resp'691''45''8776'_194 ::
  T_PreMetric_122 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_194 v0
  = let v1 = d_isPreMetric_164 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Function.Metric.Bundles.PreMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_122 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_196 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'737''45''8776'_196 v5
du_'8764''45'resp'737''45''8776'_196 ::
  T_PreMetric_122 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_196 v0
  = let v1 = d_isPreMetric_164 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Function.Metric.Bundles.PreMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_122 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_200 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_200 v5
du_isPartialEquivalence_200 ::
  T_PreMetric_122 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_200 v0
  = let v1 = d_isPreMetric_164 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe v2))
-- Function.Metric.Bundles.PreMetric._.EqC.refl
d_refl_202 :: T_PreMetric_122 -> AgdaAny -> AgdaAny
d_refl_202 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe d_isPreMetric_164 (coe v0))))
-- Function.Metric.Bundles.PreMetric._.EqC.reflexive
d_reflexive_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_122 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_204 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_204 v5
du_reflexive_204 ::
  T_PreMetric_122 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_204 v0
  = let v1 = d_isPreMetric_164 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
           (coe v2))
        v3
-- Function.Metric.Bundles.PreMetric._.EqC.sym
d_sym_206 ::
  T_PreMetric_122 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_206 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe d_isPreMetric_164 (coe v0))))
-- Function.Metric.Bundles.PreMetric._.EqC.trans
d_trans_208 ::
  T_PreMetric_122 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_208 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe d_isPreMetric_164 (coe v0))))
-- Function.Metric.Bundles.PreMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_122 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_212 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_212 v5
du_isPartialEquivalence_212 ::
  T_PreMetric_122 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_212 v0
  = let v1 = d_isPreMetric_164 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v4))
-- Function.Metric.Bundles.PreMetric._.Eq.refl
d_refl_214 :: T_PreMetric_122 -> AgdaAny -> AgdaAny
d_refl_214 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe d_isPreMetric_164 (coe v0))))))
-- Function.Metric.Bundles.PreMetric._.Eq.reflexive
d_reflexive_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_122 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_216 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_216 v5
du_reflexive_216 ::
  T_PreMetric_122 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_216 v0
  = let v1 = d_isPreMetric_164 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v2) in
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
-- Function.Metric.Bundles.PreMetric._.Eq.sym
d_sym_218 ::
  T_PreMetric_122 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_218 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe d_isPreMetric_164 (coe v0))))))
-- Function.Metric.Bundles.PreMetric._.Eq.trans
d_trans_220 ::
  T_PreMetric_122 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_220 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe d_isPreMetric_164 (coe v0))))))
-- Function.Metric.Bundles.PreMetric.protoMetric
d_protoMetric_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_122 -> T_ProtoMetric_16
d_protoMetric_222 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_protoMetric_222 v5
du_protoMetric_222 :: T_PreMetric_122 -> T_ProtoMetric_16
du_protoMetric_222 v0
  = coe
      C_ProtoMetric'46'constructor_953 (d_0'35'_160 (coe v0))
      (d_d_162 (coe v0))
      (MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe d_isPreMetric_164 (coe v0)))
-- Function.Metric.Bundles.QuasiSemiMetric
d_QuasiSemiMetric_234 a0 a1 a2 a3 a4 = ()
data T_QuasiSemiMetric_234
  = C_QuasiSemiMetric'46'constructor_7903 AgdaAny
                                          (AgdaAny -> AgdaAny -> AgdaAny)
                                          MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
-- Function.Metric.Bundles.QuasiSemiMetric.Carrier
d_Carrier_262 :: T_QuasiSemiMetric_234 -> ()
d_Carrier_262 = erased
-- Function.Metric.Bundles.QuasiSemiMetric.Image
d_Image_264 :: T_QuasiSemiMetric_234 -> ()
d_Image_264 = erased
-- Function.Metric.Bundles.QuasiSemiMetric._≈_
d__'8776'__266 :: T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> ()
d__'8776'__266 = erased
-- Function.Metric.Bundles.QuasiSemiMetric._≈ᵢ_
d__'8776''7522'__268 ::
  T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> ()
d__'8776''7522'__268 = erased
-- Function.Metric.Bundles.QuasiSemiMetric._≤_
d__'8804'__270 :: T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> ()
d__'8804'__270 = erased
-- Function.Metric.Bundles.QuasiSemiMetric.0#
d_0'35'_272 :: T_QuasiSemiMetric_234 -> AgdaAny
d_0'35'_272 v0
  = case coe v0 of
      C_QuasiSemiMetric'46'constructor_7903 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.QuasiSemiMetric.d
d_d_274 :: T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> AgdaAny
d_d_274 v0
  = case coe v0 of
      C_QuasiSemiMetric'46'constructor_7903 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.QuasiSemiMetric.isQuasiSemiMetric
d_isQuasiSemiMetric_276 ::
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
d_isQuasiSemiMetric_276 v0
  = case coe v0 of
      C_QuasiSemiMetric'46'constructor_7903 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.QuasiSemiMetric._.0⇒≈
d_0'8658''8776'_280 ::
  T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_0'8658''8776'_280 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_0'8658''8776'_172
      (coe d_isQuasiSemiMetric_276 (coe v0))
-- Function.Metric.Bundles.QuasiSemiMetric._.antisym
d_antisym_282 ::
  T_QuasiSemiMetric_234 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_282 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe d_isQuasiSemiMetric_276 (coe v0)))))
-- Function.Metric.Bundles.QuasiSemiMetric._.cong
d_cong_284 ::
  T_QuasiSemiMetric_234 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_284 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_cong_46
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe d_isQuasiSemiMetric_276 (coe v0))))
-- Function.Metric.Bundles.QuasiSemiMetric._.isEquivalence
d_isEquivalence_286 ::
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_286 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                  (coe d_isQuasiSemiMetric_276 (coe v0))))))
-- Function.Metric.Bundles.QuasiSemiMetric._.isPartialOrder
d_isPartialOrder_288 ::
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_288 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe d_isQuasiSemiMetric_276 (coe v0))))
-- Function.Metric.Bundles.QuasiSemiMetric._.isPreMetric
d_isPreMetric_290 ::
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_isPreMetric_290 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
      (coe d_isQuasiSemiMetric_276 (coe v0))
-- Function.Metric.Bundles.QuasiSemiMetric._.isPreorder
d_isPreorder_292 ::
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_292 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe d_isQuasiSemiMetric_276 (coe v0)))))
-- Function.Metric.Bundles.QuasiSemiMetric._.isProtoMetric
d_isProtoMetric_294 ::
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_294 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe d_isQuasiSemiMetric_276 (coe v0)))
-- Function.Metric.Bundles.QuasiSemiMetric._.nonNegative
d_nonNegative_296 ::
  T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_296 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_nonNegative_48
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe d_isQuasiSemiMetric_276 (coe v0))))
-- Function.Metric.Bundles.QuasiSemiMetric._.refl
d_refl_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny
d_refl_298 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_refl_298 v5
du_refl_298 :: T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny
du_refl_298 v0
  = let v1 = d_isQuasiSemiMetric_276 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Function.Metric.Bundles.QuasiSemiMetric._.reflexive
d_reflexive_300 ::
  T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_300 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                  (coe d_isQuasiSemiMetric_276 (coe v0))))))
-- Function.Metric.Bundles.QuasiSemiMetric._.trans
d_trans_302 ::
  T_QuasiSemiMetric_234 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_302 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                  (coe d_isQuasiSemiMetric_276 (coe v0))))))
-- Function.Metric.Bundles.QuasiSemiMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_304 ::
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_304 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe d_isQuasiSemiMetric_276 (coe v0))))
-- Function.Metric.Bundles.QuasiSemiMetric._.≈⇒0
d_'8776''8658'0_306 ::
  T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8776''8658'0_306 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''8658'0_106
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe d_isQuasiSemiMetric_276 (coe v0)))
-- Function.Metric.Bundles.QuasiSemiMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_308 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'45''8776'_308 v5
du_'8764''45'resp'45''8776'_308 ::
  T_QuasiSemiMetric_234 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_308 v0
  = let v1 = d_isQuasiSemiMetric_276 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Function.Metric.Bundles.QuasiSemiMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_310 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'691''45''8776'_310 v5
du_'8764''45'resp'691''45''8776'_310 ::
  T_QuasiSemiMetric_234 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_310 v0
  = let v1 = d_isQuasiSemiMetric_276 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Function.Metric.Bundles.QuasiSemiMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_312 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'737''45''8776'_312 v5
du_'8764''45'resp'737''45''8776'_312 ::
  T_QuasiSemiMetric_234 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_312 v0
  = let v1 = d_isQuasiSemiMetric_276 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Function.Metric.Bundles.QuasiSemiMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_316 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_316 v5
du_isPartialEquivalence_316 ::
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_316 v0
  = let v1 = d_isQuasiSemiMetric_276 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe v3))
-- Function.Metric.Bundles.QuasiSemiMetric._.EqC.refl
d_refl_318 :: T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny
d_refl_318 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe d_isQuasiSemiMetric_276 (coe v0)))))
-- Function.Metric.Bundles.QuasiSemiMetric._.EqC.reflexive
d_reflexive_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_320 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_320 v5
du_reflexive_320 ::
  T_QuasiSemiMetric_234 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_320 v0
  = let v1 = d_isQuasiSemiMetric_276 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
           (coe v3))
        v4
-- Function.Metric.Bundles.QuasiSemiMetric._.EqC.sym
d_sym_322 ::
  T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_322 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe d_isQuasiSemiMetric_276 (coe v0)))))
-- Function.Metric.Bundles.QuasiSemiMetric._.EqC.trans
d_trans_324 ::
  T_QuasiSemiMetric_234 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_324 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe d_isQuasiSemiMetric_276 (coe v0)))))
-- Function.Metric.Bundles.QuasiSemiMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_328 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_328 v5
du_isPartialEquivalence_328 ::
  T_QuasiSemiMetric_234 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_328 v0
  = let v1 = d_isQuasiSemiMetric_276 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v5))
-- Function.Metric.Bundles.QuasiSemiMetric._.Eq.refl
d_refl_330 :: T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny
d_refl_330 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                     (coe d_isQuasiSemiMetric_276 (coe v0)))))))
-- Function.Metric.Bundles.QuasiSemiMetric._.Eq.reflexive
d_reflexive_332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_332 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_332 v5
du_reflexive_332 ::
  T_QuasiSemiMetric_234 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_332 v0
  = let v1 = d_isQuasiSemiMetric_276 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v3) in
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
-- Function.Metric.Bundles.QuasiSemiMetric._.Eq.sym
d_sym_334 ::
  T_QuasiSemiMetric_234 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_334 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                     (coe d_isQuasiSemiMetric_276 (coe v0)))))))
-- Function.Metric.Bundles.QuasiSemiMetric._.Eq.trans
d_trans_336 ::
  T_QuasiSemiMetric_234 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_336 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                     (coe d_isQuasiSemiMetric_276 (coe v0)))))))
-- Function.Metric.Bundles.QuasiSemiMetric.preMetric
d_preMetric_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 -> T_PreMetric_122
d_preMetric_338 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_preMetric_338 v5
du_preMetric_338 :: T_QuasiSemiMetric_234 -> T_PreMetric_122
du_preMetric_338 v0
  = coe
      C_PreMetric'46'constructor_4141 (d_0'35'_272 (coe v0))
      (d_d_274 (coe v0))
      (MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe d_isQuasiSemiMetric_276 (coe v0)))
-- Function.Metric.Bundles.QuasiSemiMetric._.protoMetric
d_protoMetric_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_234 -> T_ProtoMetric_16
d_protoMetric_342 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_protoMetric_342 v5
du_protoMetric_342 :: T_QuasiSemiMetric_234 -> T_ProtoMetric_16
du_protoMetric_342 v0
  = coe du_protoMetric_222 (coe du_preMetric_338 (coe v0))
-- Function.Metric.Bundles.SemiMetric
d_SemiMetric_354 a0 a1 a2 a3 a4 = ()
data T_SemiMetric_354
  = C_SemiMetric'46'constructor_11985 AgdaAny
                                      (AgdaAny -> AgdaAny -> AgdaAny)
                                      MAlonzo.Code.Function.Metric.Structures.T_IsSemiMetric_232
-- Function.Metric.Bundles.SemiMetric.Carrier
d_Carrier_382 :: T_SemiMetric_354 -> ()
d_Carrier_382 = erased
-- Function.Metric.Bundles.SemiMetric.Image
d_Image_384 :: T_SemiMetric_354 -> ()
d_Image_384 = erased
-- Function.Metric.Bundles.SemiMetric._≈_
d__'8776'__386 :: T_SemiMetric_354 -> AgdaAny -> AgdaAny -> ()
d__'8776'__386 = erased
-- Function.Metric.Bundles.SemiMetric._≈ᵢ_
d__'8776''7522'__388 ::
  T_SemiMetric_354 -> AgdaAny -> AgdaAny -> ()
d__'8776''7522'__388 = erased
-- Function.Metric.Bundles.SemiMetric._≤_
d__'8804'__390 :: T_SemiMetric_354 -> AgdaAny -> AgdaAny -> ()
d__'8804'__390 = erased
-- Function.Metric.Bundles.SemiMetric.0#
d_0'35'_392 :: T_SemiMetric_354 -> AgdaAny
d_0'35'_392 v0
  = case coe v0 of
      C_SemiMetric'46'constructor_11985 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.SemiMetric.d
d_d_394 :: T_SemiMetric_354 -> AgdaAny -> AgdaAny -> AgdaAny
d_d_394 v0
  = case coe v0 of
      C_SemiMetric'46'constructor_11985 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.SemiMetric.isSemiMetric
d_isSemiMetric_396 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsSemiMetric_232
d_isSemiMetric_396 v0
  = case coe v0 of
      C_SemiMetric'46'constructor_11985 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.SemiMetric._.0⇒≈
d_0'8658''8776'_400 ::
  T_SemiMetric_354 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_0'8658''8776'_400 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_0'8658''8776'_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe d_isSemiMetric_396 (coe v0)))
-- Function.Metric.Bundles.SemiMetric._.antisym
d_antisym_402 ::
  T_SemiMetric_354 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_402 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe d_isSemiMetric_396 (coe v0))))))
-- Function.Metric.Bundles.SemiMetric._.cong
d_cong_404 ::
  T_SemiMetric_354 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_404 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_cong_46
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe d_isSemiMetric_396 (coe v0)))))
-- Function.Metric.Bundles.SemiMetric._.isEquivalence
d_isEquivalence_406 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_406 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                     (coe d_isSemiMetric_396 (coe v0)))))))
-- Function.Metric.Bundles.SemiMetric._.isPartialOrder
d_isPartialOrder_408 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_408 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe d_isSemiMetric_396 (coe v0)))))
-- Function.Metric.Bundles.SemiMetric._.isPreMetric
d_isPreMetric_410 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_isPreMetric_410 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe d_isSemiMetric_396 (coe v0)))
-- Function.Metric.Bundles.SemiMetric._.isPreorder
d_isPreorder_412 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_412 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe d_isSemiMetric_396 (coe v0))))))
-- Function.Metric.Bundles.SemiMetric._.isProtoMetric
d_isProtoMetric_414 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_414 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
            (coe d_isSemiMetric_396 (coe v0))))
-- Function.Metric.Bundles.SemiMetric._.isQuasiSemiMetric
d_isQuasiSemiMetric_416 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
d_isQuasiSemiMetric_416 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
      (coe d_isSemiMetric_396 (coe v0))
-- Function.Metric.Bundles.SemiMetric._.nonNegative
d_nonNegative_418 ::
  T_SemiMetric_354 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_418 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_nonNegative_48
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe d_isSemiMetric_396 (coe v0)))))
-- Function.Metric.Bundles.SemiMetric._.refl
d_refl_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 -> AgdaAny -> AgdaAny
d_refl_420 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_refl_420 v5
du_refl_420 :: T_SemiMetric_354 -> AgdaAny -> AgdaAny
du_refl_420 v0
  = let v1 = d_isSemiMetric_396 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Function.Metric.Bundles.SemiMetric._.reflexive
d_reflexive_422 ::
  T_SemiMetric_354 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_422 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                     (coe d_isSemiMetric_396 (coe v0)))))))
-- Function.Metric.Bundles.SemiMetric._.sym
d_sym_424 :: T_SemiMetric_354 -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_424 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_sym_242
      (coe d_isSemiMetric_396 (coe v0))
-- Function.Metric.Bundles.SemiMetric._.trans
d_trans_426 ::
  T_SemiMetric_354 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_426 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                     (coe d_isSemiMetric_396 (coe v0)))))))
-- Function.Metric.Bundles.SemiMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_428 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_428 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe d_isSemiMetric_396 (coe v0)))))
-- Function.Metric.Bundles.SemiMetric._.≈⇒0
d_'8776''8658'0_430 ::
  T_SemiMetric_354 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8776''8658'0_430 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''8658'0_106
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
            (coe d_isSemiMetric_396 (coe v0))))
-- Function.Metric.Bundles.SemiMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_432 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'45''8776'_432 v5
du_'8764''45'resp'45''8776'_432 ::
  T_SemiMetric_354 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_432 v0
  = let v1 = d_isSemiMetric_396 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Function.Metric.Bundles.SemiMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_434 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'691''45''8776'_434 v5
du_'8764''45'resp'691''45''8776'_434 ::
  T_SemiMetric_354 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_434 v0
  = let v1 = d_isSemiMetric_396 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Function.Metric.Bundles.SemiMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_436 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'737''45''8776'_436 v5
du_'8764''45'resp'737''45''8776'_436 ::
  T_SemiMetric_354 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_436 v0
  = let v1 = d_isSemiMetric_396 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Function.Metric.Bundles.SemiMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_440 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_440 v5
du_isPartialEquivalence_440 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_440 v0
  = let v1 = d_isSemiMetric_396 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe v4))
-- Function.Metric.Bundles.SemiMetric._.EqC.refl
d_refl_442 :: T_SemiMetric_354 -> AgdaAny -> AgdaAny
d_refl_442 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe d_isSemiMetric_396 (coe v0))))))
-- Function.Metric.Bundles.SemiMetric._.EqC.reflexive
d_reflexive_444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_444 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_444 v5
du_reflexive_444 ::
  T_SemiMetric_354 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_444 v0
  = let v1 = d_isSemiMetric_396 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
           (coe v4))
        v5
-- Function.Metric.Bundles.SemiMetric._.EqC.sym
d_sym_446 ::
  T_SemiMetric_354 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_446 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe d_isSemiMetric_396 (coe v0))))))
-- Function.Metric.Bundles.SemiMetric._.EqC.trans
d_trans_448 ::
  T_SemiMetric_354 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_448 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe d_isSemiMetric_396 (coe v0))))))
-- Function.Metric.Bundles.SemiMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_452 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_452 v5
du_isPartialEquivalence_452 ::
  T_SemiMetric_354 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_452 v0
  = let v1 = d_isSemiMetric_396 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v4) in
    let v6
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v6))
-- Function.Metric.Bundles.SemiMetric._.Eq.refl
d_refl_454 :: T_SemiMetric_354 -> AgdaAny -> AgdaAny
d_refl_454 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                     (coe
                        MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                        (coe d_isSemiMetric_396 (coe v0))))))))
-- Function.Metric.Bundles.SemiMetric._.Eq.reflexive
d_reflexive_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_456 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_456 v5
du_reflexive_456 ::
  T_SemiMetric_354 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_456 v0
  = let v1 = d_isSemiMetric_396 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v4) in
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
-- Function.Metric.Bundles.SemiMetric._.Eq.sym
d_sym_458 ::
  T_SemiMetric_354 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_458 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                     (coe
                        MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                        (coe d_isSemiMetric_396 (coe v0))))))))
-- Function.Metric.Bundles.SemiMetric._.Eq.trans
d_trans_460 ::
  T_SemiMetric_354 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_460 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                     (coe
                        MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                        (coe d_isSemiMetric_396 (coe v0))))))))
-- Function.Metric.Bundles.SemiMetric.quasiSemiMetric
d_quasiSemiMetric_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 -> T_QuasiSemiMetric_234
d_quasiSemiMetric_462 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_quasiSemiMetric_462 v5
du_quasiSemiMetric_462 :: T_SemiMetric_354 -> T_QuasiSemiMetric_234
du_quasiSemiMetric_462 v0
  = coe
      C_QuasiSemiMetric'46'constructor_7903 (d_0'35'_392 (coe v0))
      (d_d_394 (coe v0))
      (MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe d_isSemiMetric_396 (coe v0)))
-- Function.Metric.Bundles.SemiMetric._.preMetric
d_preMetric_466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 -> T_PreMetric_122
d_preMetric_466 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_preMetric_466 v5
du_preMetric_466 :: T_SemiMetric_354 -> T_PreMetric_122
du_preMetric_466 v0
  = coe du_preMetric_338 (coe du_quasiSemiMetric_462 (coe v0))
-- Function.Metric.Bundles.SemiMetric._.protoMetric
d_protoMetric_468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_354 -> T_ProtoMetric_16
d_protoMetric_468 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_protoMetric_468 v5
du_protoMetric_468 :: T_SemiMetric_354 -> T_ProtoMetric_16
du_protoMetric_468 v0
  = let v1 = coe du_quasiSemiMetric_462 (coe v0) in
    coe du_protoMetric_222 (coe du_preMetric_338 (coe v1))
-- Function.Metric.Bundles.GeneralMetric
d_GeneralMetric_480 a0 a1 a2 a3 a4 = ()
data T_GeneralMetric_480
  = C_GeneralMetric'46'constructor_16355 AgdaAny
                                         (AgdaAny -> AgdaAny -> AgdaAny)
                                         (AgdaAny -> AgdaAny -> AgdaAny)
                                         MAlonzo.Code.Function.Metric.Structures.T_IsGeneralMetric_308
-- Function.Metric.Bundles.GeneralMetric.Carrier
d_Carrier_510 :: T_GeneralMetric_480 -> ()
d_Carrier_510 = erased
-- Function.Metric.Bundles.GeneralMetric.Image
d_Image_512 :: T_GeneralMetric_480 -> ()
d_Image_512 = erased
-- Function.Metric.Bundles.GeneralMetric._≈_
d__'8776'__514 :: T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> ()
d__'8776'__514 = erased
-- Function.Metric.Bundles.GeneralMetric._≈ᵢ_
d__'8776''7522'__516 ::
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> ()
d__'8776''7522'__516 = erased
-- Function.Metric.Bundles.GeneralMetric._≤_
d__'8804'__518 :: T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> ()
d__'8804'__518 = erased
-- Function.Metric.Bundles.GeneralMetric.0#
d_0'35'_520 :: T_GeneralMetric_480 -> AgdaAny
d_0'35'_520 v0
  = case coe v0 of
      C_GeneralMetric'46'constructor_16355 v6 v7 v8 v9 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.GeneralMetric._∙_
d__'8729'__522 ::
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__522 v0
  = case coe v0 of
      C_GeneralMetric'46'constructor_16355 v6 v7 v8 v9 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.GeneralMetric.d
d_d_524 :: T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny
d_d_524 v0
  = case coe v0 of
      C_GeneralMetric'46'constructor_16355 v6 v7 v8 v9 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.GeneralMetric.isGeneralMetric
d_isGeneralMetric_526 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsGeneralMetric_308
d_isGeneralMetric_526 v0
  = case coe v0 of
      C_GeneralMetric'46'constructor_16355 v6 v7 v8 v9 -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Bundles.GeneralMetric._.0⇒≈
d_0'8658''8776'_530 ::
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_0'8658''8776'_530 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_0'8658''8776'_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
            (coe d_isGeneralMetric_526 (coe v0))))
-- Function.Metric.Bundles.GeneralMetric._.antisym
d_antisym_532 ::
  T_GeneralMetric_480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_532 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                     (coe d_isGeneralMetric_526 (coe v0)))))))
-- Function.Metric.Bundles.GeneralMetric._.cong
d_cong_534 ::
  T_GeneralMetric_480 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_534 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_cong_46
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                  (coe d_isGeneralMetric_526 (coe v0))))))
-- Function.Metric.Bundles.GeneralMetric._.isEquivalence
d_isEquivalence_536 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_536 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                     (coe
                        MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                        (coe d_isGeneralMetric_526 (coe v0))))))))
-- Function.Metric.Bundles.GeneralMetric._.isPartialOrder
d_isPartialOrder_538 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_538 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                  (coe d_isGeneralMetric_526 (coe v0))))))
-- Function.Metric.Bundles.GeneralMetric._.isPreMetric
d_isPreMetric_540 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_isPreMetric_540 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
            (coe d_isGeneralMetric_526 (coe v0))))
-- Function.Metric.Bundles.GeneralMetric._.isPreorder
d_isPreorder_542 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_542 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                     (coe d_isGeneralMetric_526 (coe v0)))))))
-- Function.Metric.Bundles.GeneralMetric._.isProtoMetric
d_isProtoMetric_544 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_544 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
               (coe d_isGeneralMetric_526 (coe v0)))))
-- Function.Metric.Bundles.GeneralMetric._.isQuasiSemiMetric
d_isQuasiSemiMetric_546 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
d_isQuasiSemiMetric_546 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
         (coe d_isGeneralMetric_526 (coe v0)))
-- Function.Metric.Bundles.GeneralMetric._.isSemiMetric
d_isSemiMetric_548 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsSemiMetric_232
d_isSemiMetric_548 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
      (coe d_isGeneralMetric_526 (coe v0))
-- Function.Metric.Bundles.GeneralMetric._.nonNegative
d_nonNegative_550 ::
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny
d_nonNegative_550 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_nonNegative_48
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                  (coe d_isGeneralMetric_526 (coe v0))))))
-- Function.Metric.Bundles.GeneralMetric._.refl
d_refl_552 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny
d_refl_552 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_refl_552 v5
du_refl_552 :: T_GeneralMetric_480 -> AgdaAny -> AgdaAny
du_refl_552 v0
  = let v1 = d_isGeneralMetric_526 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v4) in
    let v6
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v6))
-- Function.Metric.Bundles.GeneralMetric._.reflexive
d_reflexive_554 ::
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_554 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                     (coe
                        MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                        (coe d_isGeneralMetric_526 (coe v0))))))))
-- Function.Metric.Bundles.GeneralMetric._.sym
d_sym_556 :: T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_556 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_sym_242
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
         (coe d_isGeneralMetric_526 (coe v0)))
-- Function.Metric.Bundles.GeneralMetric._.trans
d_trans_558 ::
  T_GeneralMetric_480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_558 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                     (coe
                        MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                        (coe d_isGeneralMetric_526 (coe v0))))))))
-- Function.Metric.Bundles.GeneralMetric._.triangle
d_triangle_560 ::
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_triangle_560 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_triangle_320
      (coe d_isGeneralMetric_526 (coe v0))
-- Function.Metric.Bundles.GeneralMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_562 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_562 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                  (coe d_isGeneralMetric_526 (coe v0))))))
-- Function.Metric.Bundles.GeneralMetric._.≈⇒0
d_'8776''8658'0_564 ::
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8776''8658'0_564 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''8658'0_106
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
               (coe d_isGeneralMetric_526 (coe v0)))))
-- Function.Metric.Bundles.GeneralMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_566 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'45''8776'_566 v5
du_'8764''45'resp'45''8776'_566 ::
  T_GeneralMetric_480 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_566 v0
  = let v1 = d_isGeneralMetric_526 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v4) in
    let v6
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v6))
-- Function.Metric.Bundles.GeneralMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_568 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'691''45''8776'_568 v5
du_'8764''45'resp'691''45''8776'_568 ::
  T_GeneralMetric_480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_568 v0
  = let v1 = d_isGeneralMetric_526 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v4) in
    let v6
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v6))
-- Function.Metric.Bundles.GeneralMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_570 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8764''45'resp'737''45''8776'_570 v5
du_'8764''45'resp'737''45''8776'_570 ::
  T_GeneralMetric_480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_570 v0
  = let v1 = d_isGeneralMetric_526 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v4) in
    let v6
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v6))
-- Function.Metric.Bundles.GeneralMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_574 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_574 v5
du_isPartialEquivalence_574 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_574 v0
  = let v1 = d_isGeneralMetric_526 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe v5))
-- Function.Metric.Bundles.GeneralMetric._.EqC.refl
d_refl_576 :: T_GeneralMetric_480 -> AgdaAny -> AgdaAny
d_refl_576 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                     (coe d_isGeneralMetric_526 (coe v0)))))))
-- Function.Metric.Bundles.GeneralMetric._.EqC.reflexive
d_reflexive_578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_578 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_578 v5
du_reflexive_578 ::
  T_GeneralMetric_480 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_578 v0
  = let v1 = d_isGeneralMetric_526 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
           (coe v5))
        v6
-- Function.Metric.Bundles.GeneralMetric._.EqC.sym
d_sym_580 ::
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_580 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                     (coe d_isGeneralMetric_526 (coe v0)))))))
-- Function.Metric.Bundles.GeneralMetric._.EqC.trans
d_trans_582 ::
  T_GeneralMetric_480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_582 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                     (coe d_isGeneralMetric_526 (coe v0)))))))
-- Function.Metric.Bundles.GeneralMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_586 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_586 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_586 v5
du_isPartialEquivalence_586 ::
  T_GeneralMetric_480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_586 v0
  = let v1 = d_isGeneralMetric_526 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v4) in
    let v6
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v5) in
    let v7
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v6) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v7))
-- Function.Metric.Bundles.GeneralMetric._.Eq.refl
d_refl_588 :: T_GeneralMetric_480 -> AgdaAny -> AgdaAny
d_refl_588 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                     (coe
                        MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                        (coe
                           MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                           (coe d_isGeneralMetric_526 (coe v0)))))))))
-- Function.Metric.Bundles.GeneralMetric._.Eq.reflexive
d_reflexive_590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_590 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_590 v5
du_reflexive_590 ::
  T_GeneralMetric_480 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_590 v0
  = let v1 = d_isGeneralMetric_526 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
              (coe v1) in
    let v3
          = MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
              (coe v2) in
    let v4
          = MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
              (coe v3) in
    let v5
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v4) in
    let v6
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v5) in
    let v7
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v6) in
    \ v8 v9 v10 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
           (coe v7))
        v8
-- Function.Metric.Bundles.GeneralMetric._.Eq.sym
d_sym_592 ::
  T_GeneralMetric_480 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_592 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                     (coe
                        MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                        (coe
                           MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                           (coe d_isGeneralMetric_526 (coe v0)))))))))
-- Function.Metric.Bundles.GeneralMetric._.Eq.trans
d_trans_594 ::
  T_GeneralMetric_480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_594 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
               (coe
                  MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
                  (coe
                     MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
                     (coe
                        MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
                        (coe
                           MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
                           (coe d_isGeneralMetric_526 (coe v0)))))))))
-- Function.Metric.Bundles.GeneralMetric.semiMetric
d_semiMetric_596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 -> T_SemiMetric_354
d_semiMetric_596 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_semiMetric_596 v5
du_semiMetric_596 :: T_GeneralMetric_480 -> T_SemiMetric_354
du_semiMetric_596 v0
  = coe
      C_SemiMetric'46'constructor_11985 (d_0'35'_520 (coe v0))
      (d_d_524 (coe v0))
      (MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
         (coe d_isGeneralMetric_526 (coe v0)))
-- Function.Metric.Bundles.GeneralMetric._.preMetric
d_preMetric_600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 -> T_PreMetric_122
d_preMetric_600 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_preMetric_600 v5
du_preMetric_600 :: T_GeneralMetric_480 -> T_PreMetric_122
du_preMetric_600 v0
  = let v1 = coe du_semiMetric_596 (coe v0) in
    coe du_preMetric_338 (coe du_quasiSemiMetric_462 (coe v1))
-- Function.Metric.Bundles.GeneralMetric._.protoMetric
d_protoMetric_602 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 -> T_ProtoMetric_16
d_protoMetric_602 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_protoMetric_602 v5
du_protoMetric_602 :: T_GeneralMetric_480 -> T_ProtoMetric_16
du_protoMetric_602 v0
  = let v1 = coe du_semiMetric_596 (coe v0) in
    let v2 = coe du_quasiSemiMetric_462 (coe v1) in
    coe du_protoMetric_222 (coe du_preMetric_338 (coe v2))
-- Function.Metric.Bundles.GeneralMetric._.quasiSemiMetric
d_quasiSemiMetric_604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_GeneralMetric_480 -> T_QuasiSemiMetric_234
d_quasiSemiMetric_604 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_quasiSemiMetric_604 v5
du_quasiSemiMetric_604 ::
  T_GeneralMetric_480 -> T_QuasiSemiMetric_234
du_quasiSemiMetric_604 v0
  = coe du_quasiSemiMetric_462 (coe du_semiMetric_596 (coe v0))
