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

module MAlonzo.Code.Function.Metric.Nat.Bundles where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Function.Metric.Structures
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Metric.Nat.Bundles.ProtoMetric
d_ProtoMetric_12 a0 a1 = ()
data T_ProtoMetric_12
  = C_ProtoMetric'46'constructor_187 (AgdaAny -> AgdaAny -> Integer)
                                     MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
-- Function.Metric.Nat.Bundles.ProtoMetric.Carrier
d_Carrier_26 :: T_ProtoMetric_12 -> ()
d_Carrier_26 = erased
-- Function.Metric.Nat.Bundles.ProtoMetric._≈_
d__'8776'__28 :: T_ProtoMetric_12 -> AgdaAny -> AgdaAny -> ()
d__'8776'__28 = erased
-- Function.Metric.Nat.Bundles.ProtoMetric.d
d_d_30 :: T_ProtoMetric_12 -> AgdaAny -> AgdaAny -> Integer
d_d_30 v0
  = case coe v0 of
      C_ProtoMetric'46'constructor_187 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.ProtoMetric.isProtoMetric
d_isProtoMetric_32 ::
  T_ProtoMetric_12 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_32 v0
  = case coe v0 of
      C_ProtoMetric'46'constructor_187 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.ProtoMetric._.antisym
d_antisym_36 ::
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antisym_36 = erased
-- Function.Metric.Nat.Bundles.ProtoMetric._.cong
d_cong_38 ::
  T_ProtoMetric_12 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cong_38 = erased
-- Function.Metric.Nat.Bundles.ProtoMetric._.isEquivalence
d_isEquivalence_40 ::
  T_ProtoMetric_12 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_40 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe d_isProtoMetric_32 (coe v0))))
-- Function.Metric.Nat.Bundles.ProtoMetric._.isPartialOrder
d_isPartialOrder_42 ::
  T_ProtoMetric_12 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_42 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
      (coe d_isProtoMetric_32 (coe v0))
-- Function.Metric.Nat.Bundles.ProtoMetric._.isPreorder
d_isPreorder_44 ::
  T_ProtoMetric_12 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_44 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe d_isProtoMetric_32 (coe v0)))
-- Function.Metric.Nat.Bundles.ProtoMetric._.nonNegative
d_nonNegative_46 ::
  T_ProtoMetric_12 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_nonNegative_46 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_nonNegative_48
      (coe d_isProtoMetric_32 (coe v0))
-- Function.Metric.Nat.Bundles.ProtoMetric._.refl
d_refl_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_12 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_refl_48 ~v0 ~v1 v2 = du_refl_48 v2
du_refl_48 ::
  T_ProtoMetric_12 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_refl_48 v0
  = let v1 = d_isProtoMetric_32 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Nat.Bundles.ProtoMetric._.reflexive
d_reflexive_50 ::
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_reflexive_50 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe d_isProtoMetric_32 (coe v0))))
-- Function.Metric.Nat.Bundles.ProtoMetric._.trans
d_trans_52 ::
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_trans_52 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe d_isProtoMetric_32 (coe v0))))
-- Function.Metric.Nat.Bundles.ProtoMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_54 ::
  T_ProtoMetric_12 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_54 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
      (coe d_isProtoMetric_32 (coe v0))
-- Function.Metric.Nat.Bundles.ProtoMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_12 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_56 ~v0 ~v1 v2
  = du_'8764''45'resp'45''8776'_56 v2
du_'8764''45'resp'45''8776'_56 ::
  T_ProtoMetric_12 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_56 v0
  = let v1 = d_isProtoMetric_32 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Nat.Bundles.ProtoMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'691''45''8776'_58 ~v0 ~v1 v2
  = du_'8764''45'resp'691''45''8776'_58 v2
du_'8764''45'resp'691''45''8776'_58 ::
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'691''45''8776'_58 v0
  = let v1 = d_isProtoMetric_32 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Nat.Bundles.ProtoMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'737''45''8776'_60 ~v0 ~v1 v2
  = du_'8764''45'resp'737''45''8776'_60 v2
du_'8764''45'resp'737''45''8776'_60 ::
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'737''45''8776'_60 v0
  = let v1 = d_isProtoMetric_32 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Function.Metric.Nat.Bundles.ProtoMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_12 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_64 ~v0 ~v1 v2
  = du_isPartialEquivalence_64 v2
du_isPartialEquivalence_64 ::
  T_ProtoMetric_12 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_64 v0
  = let v1 = d_isProtoMetric_32 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe v1))
-- Function.Metric.Nat.Bundles.ProtoMetric._.EqC.refl
d_refl_66 :: T_ProtoMetric_12 -> AgdaAny -> AgdaAny
d_refl_66 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe d_isProtoMetric_32 (coe v0)))
-- Function.Metric.Nat.Bundles.ProtoMetric._.EqC.reflexive
d_reflexive_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_12 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_68 ~v0 ~v1 v2 = du_reflexive_68 v2
du_reflexive_68 ::
  T_ProtoMetric_12 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_68 v0
  = let v1 = d_isProtoMetric_32 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
           (coe v1))
        v2
-- Function.Metric.Nat.Bundles.ProtoMetric._.EqC.sym
d_sym_70 ::
  T_ProtoMetric_12 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_70 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe d_isProtoMetric_32 (coe v0)))
-- Function.Metric.Nat.Bundles.ProtoMetric._.EqC.trans
d_trans_72 ::
  T_ProtoMetric_12 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_72 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe d_isProtoMetric_32 (coe v0)))
-- Function.Metric.Nat.Bundles.ProtoMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_12 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_76 ~v0 ~v1 v2
  = du_isPartialEquivalence_76 v2
du_isPartialEquivalence_76 ::
  T_ProtoMetric_12 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_76 v0
  = let v1 = d_isProtoMetric_32 (coe v0) in
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
-- Function.Metric.Nat.Bundles.ProtoMetric._.Eq.refl
d_refl_78 ::
  T_ProtoMetric_12 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_refl_78 = erased
-- Function.Metric.Nat.Bundles.ProtoMetric._.Eq.reflexive
d_reflexive_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reflexive_80 = erased
-- Function.Metric.Nat.Bundles.ProtoMetric._.Eq.sym
d_sym_82 ::
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_82 = erased
-- Function.Metric.Nat.Bundles.ProtoMetric._.Eq.trans
d_trans_84 ::
  T_ProtoMetric_12 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_trans_84 = erased
-- Function.Metric.Nat.Bundles.PreMetric
d_PreMetric_90 a0 a1 = ()
data T_PreMetric_90
  = C_PreMetric'46'constructor_1511 (AgdaAny -> AgdaAny -> Integer)
                                    MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
-- Function.Metric.Nat.Bundles.PreMetric.Carrier
d_Carrier_104 :: T_PreMetric_90 -> ()
d_Carrier_104 = erased
-- Function.Metric.Nat.Bundles.PreMetric._≈_
d__'8776'__106 :: T_PreMetric_90 -> AgdaAny -> AgdaAny -> ()
d__'8776'__106 = erased
-- Function.Metric.Nat.Bundles.PreMetric.d
d_d_108 :: T_PreMetric_90 -> AgdaAny -> AgdaAny -> Integer
d_d_108 v0
  = case coe v0 of
      C_PreMetric'46'constructor_1511 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.PreMetric.isPreMetric
d_isPreMetric_110 ::
  T_PreMetric_90 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_isPreMetric_110 v0
  = case coe v0 of
      C_PreMetric'46'constructor_1511 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.PreMetric._.antisym
d_antisym_114 ::
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antisym_114 = erased
-- Function.Metric.Nat.Bundles.PreMetric._.cong
d_cong_116 ::
  T_PreMetric_90 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cong_116 = erased
-- Function.Metric.Nat.Bundles.PreMetric._.isEquivalence
d_isEquivalence_118 ::
  T_PreMetric_90 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_118 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe d_isPreMetric_110 (coe v0)))))
-- Function.Metric.Nat.Bundles.PreMetric._.isPartialOrder
d_isPartialOrder_120 ::
  T_PreMetric_90 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_120 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe d_isPreMetric_110 (coe v0)))
-- Function.Metric.Nat.Bundles.PreMetric._.isPreorder
d_isPreorder_122 ::
  T_PreMetric_90 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_122 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe d_isPreMetric_110 (coe v0))))
-- Function.Metric.Nat.Bundles.PreMetric._.isProtoMetric
d_isProtoMetric_124 ::
  T_PreMetric_90 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_124 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
      (coe d_isPreMetric_110 (coe v0))
-- Function.Metric.Nat.Bundles.PreMetric._.nonNegative
d_nonNegative_126 ::
  T_PreMetric_90 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_nonNegative_126 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_nonNegative_48
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe d_isPreMetric_110 (coe v0)))
-- Function.Metric.Nat.Bundles.PreMetric._.refl
d_refl_128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_90 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_refl_128 ~v0 ~v1 v2 = du_refl_128 v2
du_refl_128 ::
  T_PreMetric_90 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_refl_128 v0
  = let v1 = d_isPreMetric_110 (coe v0) in
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
-- Function.Metric.Nat.Bundles.PreMetric._.reflexive
d_reflexive_130 ::
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_reflexive_130 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe d_isPreMetric_110 (coe v0)))))
-- Function.Metric.Nat.Bundles.PreMetric._.trans
d_trans_132 ::
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_trans_132 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
               (coe d_isPreMetric_110 (coe v0)))))
-- Function.Metric.Nat.Bundles.PreMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_134 ::
  T_PreMetric_90 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_134 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe d_isPreMetric_110 (coe v0)))
-- Function.Metric.Nat.Bundles.PreMetric._.≈⇒0
d_'8776''8658'0_136 ::
  T_PreMetric_90 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''8658'0_136 = erased
-- Function.Metric.Nat.Bundles.PreMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_90 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_138 ~v0 ~v1 v2
  = du_'8764''45'resp'45''8776'_138 v2
du_'8764''45'resp'45''8776'_138 ::
  T_PreMetric_90 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_138 v0
  = let v1 = d_isPreMetric_110 (coe v0) in
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
-- Function.Metric.Nat.Bundles.PreMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'691''45''8776'_140 ~v0 ~v1 v2
  = du_'8764''45'resp'691''45''8776'_140 v2
du_'8764''45'resp'691''45''8776'_140 ::
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'691''45''8776'_140 v0
  = let v1 = d_isPreMetric_110 (coe v0) in
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
-- Function.Metric.Nat.Bundles.PreMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'737''45''8776'_142 ~v0 ~v1 v2
  = du_'8764''45'resp'737''45''8776'_142 v2
du_'8764''45'resp'737''45''8776'_142 ::
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'737''45''8776'_142 v0
  = let v1 = d_isPreMetric_110 (coe v0) in
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
-- Function.Metric.Nat.Bundles.PreMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_90 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_146 ~v0 ~v1 v2
  = du_isPartialEquivalence_146 v2
du_isPartialEquivalence_146 ::
  T_PreMetric_90 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_146 v0
  = let v1 = d_isPreMetric_110 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe v2))
-- Function.Metric.Nat.Bundles.PreMetric._.EqC.refl
d_refl_148 :: T_PreMetric_90 -> AgdaAny -> AgdaAny
d_refl_148 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe d_isPreMetric_110 (coe v0))))
-- Function.Metric.Nat.Bundles.PreMetric._.EqC.reflexive
d_reflexive_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_90 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_150 ~v0 ~v1 v2 = du_reflexive_150 v2
du_reflexive_150 ::
  T_PreMetric_90 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_150 v0
  = let v1 = d_isPreMetric_110 (coe v0) in
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
-- Function.Metric.Nat.Bundles.PreMetric._.EqC.sym
d_sym_152 ::
  T_PreMetric_90 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_152 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe d_isPreMetric_110 (coe v0))))
-- Function.Metric.Nat.Bundles.PreMetric._.EqC.trans
d_trans_154 ::
  T_PreMetric_90 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_154 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe d_isPreMetric_110 (coe v0))))
-- Function.Metric.Nat.Bundles.PreMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_90 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_158 ~v0 ~v1 v2
  = du_isPartialEquivalence_158 v2
du_isPartialEquivalence_158 ::
  T_PreMetric_90 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_158 v0
  = let v1 = d_isPreMetric_110 (coe v0) in
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
-- Function.Metric.Nat.Bundles.PreMetric._.Eq.refl
d_refl_160 ::
  T_PreMetric_90 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_refl_160 = erased
-- Function.Metric.Nat.Bundles.PreMetric._.Eq.reflexive
d_reflexive_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reflexive_162 = erased
-- Function.Metric.Nat.Bundles.PreMetric._.Eq.sym
d_sym_164 ::
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_164 = erased
-- Function.Metric.Nat.Bundles.PreMetric._.Eq.trans
d_trans_166 ::
  T_PreMetric_90 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_trans_166 = erased
-- Function.Metric.Nat.Bundles.PreMetric.protoMetric
d_protoMetric_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PreMetric_90 -> T_ProtoMetric_12
d_protoMetric_168 ~v0 ~v1 v2 = du_protoMetric_168 v2
du_protoMetric_168 :: T_PreMetric_90 -> T_ProtoMetric_12
du_protoMetric_168 v0
  = coe
      C_ProtoMetric'46'constructor_187 (d_d_108 (coe v0))
      (MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe d_isPreMetric_110 (coe v0)))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric
d_QuasiSemiMetric_174 a0 a1 = ()
data T_QuasiSemiMetric_174
  = C_QuasiSemiMetric'46'constructor_3025 (AgdaAny ->
                                           AgdaAny -> Integer)
                                          MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
-- Function.Metric.Nat.Bundles.QuasiSemiMetric.Carrier
d_Carrier_188 :: T_QuasiSemiMetric_174 -> ()
d_Carrier_188 = erased
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._≈_
d__'8776'__190 :: T_QuasiSemiMetric_174 -> AgdaAny -> AgdaAny -> ()
d__'8776'__190 = erased
-- Function.Metric.Nat.Bundles.QuasiSemiMetric.d
d_d_192 :: T_QuasiSemiMetric_174 -> AgdaAny -> AgdaAny -> Integer
d_d_192 v0
  = case coe v0 of
      C_QuasiSemiMetric'46'constructor_3025 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.QuasiSemiMetric.isQuasiSemiMetric
d_isQuasiSemiMetric_194 ::
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
d_isQuasiSemiMetric_194 v0
  = case coe v0 of
      C_QuasiSemiMetric'46'constructor_3025 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.0⇒≈
d_0'8658''8776'_198 ::
  T_QuasiSemiMetric_174 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_0'8658''8776'_198 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_0'8658''8776'_172
      (coe d_isQuasiSemiMetric_194 (coe v0))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.antisym
d_antisym_200 ::
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antisym_200 = erased
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.cong
d_cong_202 ::
  T_QuasiSemiMetric_174 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cong_202 = erased
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.isEquivalence
d_isEquivalence_204 ::
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_204 v0
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
                  (coe d_isQuasiSemiMetric_194 (coe v0))))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.isPartialOrder
d_isPartialOrder_206 ::
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_206 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe d_isQuasiSemiMetric_194 (coe v0))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.isPreMetric
d_isPreMetric_208 ::
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_isPreMetric_208 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
      (coe d_isQuasiSemiMetric_194 (coe v0))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.isPreorder
d_isPreorder_210 ::
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_210 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe d_isQuasiSemiMetric_194 (coe v0)))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.isProtoMetric
d_isProtoMetric_212 ::
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_212 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe d_isQuasiSemiMetric_194 (coe v0)))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.nonNegative
d_nonNegative_214 ::
  T_QuasiSemiMetric_174 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_nonNegative_214 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_nonNegative_48
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe d_isQuasiSemiMetric_194 (coe v0))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.refl
d_refl_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_refl_216 ~v0 ~v1 v2 = du_refl_216 v2
du_refl_216 ::
  T_QuasiSemiMetric_174 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_refl_216 v0
  = let v1 = d_isQuasiSemiMetric_194 (coe v0) in
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
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.reflexive
d_reflexive_218 ::
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_reflexive_218 v0
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
                  (coe d_isQuasiSemiMetric_194 (coe v0))))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.trans
d_trans_220 ::
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_trans_220 v0
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
                  (coe d_isQuasiSemiMetric_194 (coe v0))))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_222 ::
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_222 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe d_isQuasiSemiMetric_194 (coe v0))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.≈⇒0
d_'8776''8658'0_224 ::
  T_QuasiSemiMetric_174 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''8658'0_224 = erased
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_226 ~v0 ~v1 v2
  = du_'8764''45'resp'45''8776'_226 v2
du_'8764''45'resp'45''8776'_226 ::
  T_QuasiSemiMetric_174 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_226 v0
  = let v1 = d_isQuasiSemiMetric_194 (coe v0) in
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
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'691''45''8776'_228 ~v0 ~v1 v2
  = du_'8764''45'resp'691''45''8776'_228 v2
du_'8764''45'resp'691''45''8776'_228 ::
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'691''45''8776'_228 v0
  = let v1 = d_isQuasiSemiMetric_194 (coe v0) in
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
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'737''45''8776'_230 ~v0 ~v1 v2
  = du_'8764''45'resp'737''45''8776'_230 v2
du_'8764''45'resp'737''45''8776'_230 ::
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'737''45''8776'_230 v0
  = let v1 = d_isQuasiSemiMetric_194 (coe v0) in
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
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_234 ~v0 ~v1 v2
  = du_isPartialEquivalence_234 v2
du_isPartialEquivalence_234 ::
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_234 v0
  = let v1 = d_isQuasiSemiMetric_194 (coe v0) in
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
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.EqC.refl
d_refl_236 :: T_QuasiSemiMetric_174 -> AgdaAny -> AgdaAny
d_refl_236 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe d_isQuasiSemiMetric_194 (coe v0)))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.EqC.reflexive
d_reflexive_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_238 ~v0 ~v1 v2 = du_reflexive_238 v2
du_reflexive_238 ::
  T_QuasiSemiMetric_174 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_238 v0
  = let v1 = d_isQuasiSemiMetric_194 (coe v0) in
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
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.EqC.sym
d_sym_240 ::
  T_QuasiSemiMetric_174 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_240 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe d_isQuasiSemiMetric_194 (coe v0)))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.EqC.trans
d_trans_242 ::
  T_QuasiSemiMetric_174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_242 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
               (coe d_isQuasiSemiMetric_194 (coe v0)))))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_246 ~v0 ~v1 v2
  = du_isPartialEquivalence_246 v2
du_isPartialEquivalence_246 ::
  T_QuasiSemiMetric_174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_246 v0
  = let v1 = d_isQuasiSemiMetric_194 (coe v0) in
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
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.Eq.refl
d_refl_248 ::
  T_QuasiSemiMetric_174 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_refl_248 = erased
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.Eq.reflexive
d_reflexive_250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reflexive_250 = erased
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.Eq.sym
d_sym_252 ::
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_252 = erased
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.Eq.trans
d_trans_254 ::
  T_QuasiSemiMetric_174 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_trans_254 = erased
-- Function.Metric.Nat.Bundles.QuasiSemiMetric.preMetric
d_preMetric_256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 -> T_PreMetric_90
d_preMetric_256 ~v0 ~v1 v2 = du_preMetric_256 v2
du_preMetric_256 :: T_QuasiSemiMetric_174 -> T_PreMetric_90
du_preMetric_256 v0
  = coe
      C_PreMetric'46'constructor_1511 (d_d_192 (coe v0))
      (MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe d_isQuasiSemiMetric_194 (coe v0)))
-- Function.Metric.Nat.Bundles.QuasiSemiMetric._.protoMetric
d_protoMetric_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_QuasiSemiMetric_174 -> T_ProtoMetric_12
d_protoMetric_260 ~v0 ~v1 v2 = du_protoMetric_260 v2
du_protoMetric_260 :: T_QuasiSemiMetric_174 -> T_ProtoMetric_12
du_protoMetric_260 v0
  = coe du_protoMetric_168 (coe du_preMetric_256 (coe v0))
-- Function.Metric.Nat.Bundles.SemiMetric
d_SemiMetric_266 a0 a1 = ()
data T_SemiMetric_266
  = C_SemiMetric'46'constructor_4649 (AgdaAny -> AgdaAny -> Integer)
                                     MAlonzo.Code.Function.Metric.Structures.T_IsSemiMetric_232
-- Function.Metric.Nat.Bundles.SemiMetric.Carrier
d_Carrier_280 :: T_SemiMetric_266 -> ()
d_Carrier_280 = erased
-- Function.Metric.Nat.Bundles.SemiMetric._≈_
d__'8776'__282 :: T_SemiMetric_266 -> AgdaAny -> AgdaAny -> ()
d__'8776'__282 = erased
-- Function.Metric.Nat.Bundles.SemiMetric.d
d_d_284 :: T_SemiMetric_266 -> AgdaAny -> AgdaAny -> Integer
d_d_284 v0
  = case coe v0 of
      C_SemiMetric'46'constructor_4649 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.SemiMetric.isSemiMetric
d_isSemiMetric_286 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsSemiMetric_232
d_isSemiMetric_286 v0
  = case coe v0 of
      C_SemiMetric'46'constructor_4649 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.SemiMetric._.0⇒≈
d_0'8658''8776'_290 ::
  T_SemiMetric_266 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_0'8658''8776'_290 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_0'8658''8776'_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe d_isSemiMetric_286 (coe v0)))
-- Function.Metric.Nat.Bundles.SemiMetric._.antisym
d_antisym_292 ::
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antisym_292 = erased
-- Function.Metric.Nat.Bundles.SemiMetric._.cong
d_cong_294 ::
  T_SemiMetric_266 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cong_294 = erased
-- Function.Metric.Nat.Bundles.SemiMetric._.isEquivalence
d_isEquivalence_296 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_296 v0
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
                     (coe d_isSemiMetric_286 (coe v0)))))))
-- Function.Metric.Nat.Bundles.SemiMetric._.isPartialOrder
d_isPartialOrder_298 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_298 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPartialOrder_42
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe d_isSemiMetric_286 (coe v0)))))
-- Function.Metric.Nat.Bundles.SemiMetric._.isPreMetric
d_isPreMetric_300 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_isPreMetric_300 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe d_isSemiMetric_286 (coe v0)))
-- Function.Metric.Nat.Bundles.SemiMetric._.isPreorder
d_isPreorder_302 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_302 v0
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
                  (coe d_isSemiMetric_286 (coe v0))))))
-- Function.Metric.Nat.Bundles.SemiMetric._.isProtoMetric
d_isProtoMetric_304 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_304 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
            (coe d_isSemiMetric_286 (coe v0))))
-- Function.Metric.Nat.Bundles.SemiMetric._.isQuasiSemiMetric
d_isQuasiSemiMetric_306 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
d_isQuasiSemiMetric_306 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
      (coe d_isSemiMetric_286 (coe v0))
-- Function.Metric.Nat.Bundles.SemiMetric._.nonNegative
d_nonNegative_308 ::
  T_SemiMetric_266 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_nonNegative_308 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_nonNegative_48
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe d_isSemiMetric_286 (coe v0)))))
-- Function.Metric.Nat.Bundles.SemiMetric._.refl
d_refl_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_refl_310 ~v0 ~v1 v2 = du_refl_310 v2
du_refl_310 ::
  T_SemiMetric_266 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_refl_310 v0
  = let v1 = d_isSemiMetric_286 (coe v0) in
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
-- Function.Metric.Nat.Bundles.SemiMetric._.reflexive
d_reflexive_312 ::
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_reflexive_312 v0
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
                     (coe d_isSemiMetric_286 (coe v0)))))))
-- Function.Metric.Nat.Bundles.SemiMetric._.sym
d_sym_314 ::
  T_SemiMetric_266 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_314 = erased
-- Function.Metric.Nat.Bundles.SemiMetric._.trans
d_trans_316 ::
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_trans_316 v0
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
                     (coe d_isSemiMetric_286 (coe v0)))))))
-- Function.Metric.Nat.Bundles.SemiMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_318 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_318 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_'8776''45'isEquivalence_44
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
               (coe d_isSemiMetric_286 (coe v0)))))
-- Function.Metric.Nat.Bundles.SemiMetric._.≈⇒0
d_'8776''8658'0_320 ::
  T_SemiMetric_266 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''8658'0_320 = erased
-- Function.Metric.Nat.Bundles.SemiMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_322 ~v0 ~v1 v2
  = du_'8764''45'resp'45''8776'_322 v2
du_'8764''45'resp'45''8776'_322 ::
  T_SemiMetric_266 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_322 v0
  = let v1 = d_isSemiMetric_286 (coe v0) in
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
-- Function.Metric.Nat.Bundles.SemiMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'691''45''8776'_324 ~v0 ~v1 v2
  = du_'8764''45'resp'691''45''8776'_324 v2
du_'8764''45'resp'691''45''8776'_324 ::
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'691''45''8776'_324 v0
  = let v1 = d_isSemiMetric_286 (coe v0) in
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
-- Function.Metric.Nat.Bundles.SemiMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'737''45''8776'_326 ~v0 ~v1 v2
  = du_'8764''45'resp'737''45''8776'_326 v2
du_'8764''45'resp'737''45''8776'_326 ::
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'737''45''8776'_326 v0
  = let v1 = d_isSemiMetric_286 (coe v0) in
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
-- Function.Metric.Nat.Bundles.SemiMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_330 ~v0 ~v1 v2
  = du_isPartialEquivalence_330 v2
du_isPartialEquivalence_330 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_330 v0
  = let v1 = d_isSemiMetric_286 (coe v0) in
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
-- Function.Metric.Nat.Bundles.SemiMetric._.EqC.refl
d_refl_332 :: T_SemiMetric_266 -> AgdaAny -> AgdaAny
d_refl_332 v0
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
                  (coe d_isSemiMetric_286 (coe v0))))))
-- Function.Metric.Nat.Bundles.SemiMetric._.EqC.reflexive
d_reflexive_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_334 ~v0 ~v1 v2 = du_reflexive_334 v2
du_reflexive_334 ::
  T_SemiMetric_266 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_334 v0
  = let v1 = d_isSemiMetric_286 (coe v0) in
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
-- Function.Metric.Nat.Bundles.SemiMetric._.EqC.sym
d_sym_336 ::
  T_SemiMetric_266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_336 v0
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
                  (coe d_isSemiMetric_286 (coe v0))))))
-- Function.Metric.Nat.Bundles.SemiMetric._.EqC.trans
d_trans_338 ::
  T_SemiMetric_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_338 v0
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
                  (coe d_isSemiMetric_286 (coe v0))))))
-- Function.Metric.Nat.Bundles.SemiMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_342 ~v0 ~v1 v2
  = du_isPartialEquivalence_342 v2
du_isPartialEquivalence_342 ::
  T_SemiMetric_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_342 v0
  = let v1 = d_isSemiMetric_286 (coe v0) in
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
-- Function.Metric.Nat.Bundles.SemiMetric._.Eq.refl
d_refl_344 ::
  T_SemiMetric_266 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_refl_344 = erased
-- Function.Metric.Nat.Bundles.SemiMetric._.Eq.reflexive
d_reflexive_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reflexive_346 = erased
-- Function.Metric.Nat.Bundles.SemiMetric._.Eq.sym
d_sym_348 ::
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_348 = erased
-- Function.Metric.Nat.Bundles.SemiMetric._.Eq.trans
d_trans_350 ::
  T_SemiMetric_266 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_trans_350 = erased
-- Function.Metric.Nat.Bundles.SemiMetric.quasiSemiMetric
d_quasiSemiMetric_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 -> T_QuasiSemiMetric_174
d_quasiSemiMetric_352 ~v0 ~v1 v2 = du_quasiSemiMetric_352 v2
du_quasiSemiMetric_352 :: T_SemiMetric_266 -> T_QuasiSemiMetric_174
du_quasiSemiMetric_352 v0
  = coe
      C_QuasiSemiMetric'46'constructor_3025 (d_d_284 (coe v0))
      (MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe d_isSemiMetric_286 (coe v0)))
-- Function.Metric.Nat.Bundles.SemiMetric._.preMetric
d_preMetric_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 -> T_PreMetric_90
d_preMetric_356 ~v0 ~v1 v2 = du_preMetric_356 v2
du_preMetric_356 :: T_SemiMetric_266 -> T_PreMetric_90
du_preMetric_356 v0
  = coe du_preMetric_256 (coe du_quasiSemiMetric_352 (coe v0))
-- Function.Metric.Nat.Bundles.SemiMetric._.protoMetric
d_protoMetric_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiMetric_266 -> T_ProtoMetric_12
d_protoMetric_358 ~v0 ~v1 v2 = du_protoMetric_358 v2
du_protoMetric_358 :: T_SemiMetric_266 -> T_ProtoMetric_12
du_protoMetric_358 v0
  = let v1 = coe du_quasiSemiMetric_352 (coe v0) in
    coe du_protoMetric_168 (coe du_preMetric_256 (coe v1))
-- Function.Metric.Nat.Bundles.Metric
d_Metric_364 a0 a1 = ()
data T_Metric_364
  = C_Metric'46'constructor_6343 (AgdaAny -> AgdaAny -> Integer)
                                 MAlonzo.Code.Function.Metric.Structures.T_IsGeneralMetric_308
-- Function.Metric.Nat.Bundles.Metric.Carrier
d_Carrier_378 :: T_Metric_364 -> ()
d_Carrier_378 = erased
-- Function.Metric.Nat.Bundles.Metric._≈_
d__'8776'__380 :: T_Metric_364 -> AgdaAny -> AgdaAny -> ()
d__'8776'__380 = erased
-- Function.Metric.Nat.Bundles.Metric.d
d_d_382 :: T_Metric_364 -> AgdaAny -> AgdaAny -> Integer
d_d_382 v0
  = case coe v0 of
      C_Metric'46'constructor_6343 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.Metric.isMetric
d_isMetric_384 ::
  T_Metric_364 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsGeneralMetric_308
d_isMetric_384 v0
  = case coe v0 of
      C_Metric'46'constructor_6343 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.Metric._.0⇒≈
d_0'8658''8776'_388 ::
  T_Metric_364 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_0'8658''8776'_388 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_0'8658''8776'_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
            (coe d_isMetric_384 (coe v0))))
-- Function.Metric.Nat.Bundles.Metric._.antisym
d_antisym_390 ::
  T_Metric_364 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antisym_390 = erased
-- Function.Metric.Nat.Bundles.Metric._.cong
d_cong_392 ::
  T_Metric_364 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cong_392 = erased
-- Function.Metric.Nat.Bundles.Metric._.isEquivalence
d_isEquivalence_394 ::
  T_Metric_364 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_394 v0
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
                        (coe d_isMetric_384 (coe v0))))))))
-- Function.Metric.Nat.Bundles.Metric._.isPartialOrder
d_isPartialOrder_396 ::
  T_Metric_364 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_396 v0
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
                  (coe d_isMetric_384 (coe v0))))))
-- Function.Metric.Nat.Bundles.Metric._.isPreMetric
d_isPreMetric_398 ::
  T_Metric_364 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_isPreMetric_398 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
            (coe d_isMetric_384 (coe v0))))
-- Function.Metric.Nat.Bundles.Metric._.isPreorder
d_isPreorder_400 ::
  T_Metric_364 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_400 v0
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
                     (coe d_isMetric_384 (coe v0)))))))
-- Function.Metric.Nat.Bundles.Metric._.isProtoMetric
d_isProtoMetric_402 ::
  T_Metric_364 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_402 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
               (coe d_isMetric_384 (coe v0)))))
-- Function.Metric.Nat.Bundles.Metric._.isQuasiSemiMetric
d_isQuasiSemiMetric_404 ::
  T_Metric_364 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
d_isQuasiSemiMetric_404 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
         (coe d_isMetric_384 (coe v0)))
-- Function.Metric.Nat.Bundles.Metric._.isSemiMetric
d_isSemiMetric_406 ::
  T_Metric_364 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsSemiMetric_232
d_isSemiMetric_406 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
      (coe d_isMetric_384 (coe v0))
-- Function.Metric.Nat.Bundles.Metric._.nonNegative
d_nonNegative_408 ::
  T_Metric_364 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_nonNegative_408 v0
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
                  (coe d_isMetric_384 (coe v0))))))
-- Function.Metric.Nat.Bundles.Metric._.refl
d_refl_410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_refl_410 ~v0 ~v1 v2 = du_refl_410 v2
du_refl_410 ::
  T_Metric_364 -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_refl_410 v0
  = let v1 = d_isMetric_384 (coe v0) in
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
-- Function.Metric.Nat.Bundles.Metric._.reflexive
d_reflexive_412 ::
  T_Metric_364 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_reflexive_412 v0
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
                        (coe d_isMetric_384 (coe v0))))))))
-- Function.Metric.Nat.Bundles.Metric._.sym
d_sym_414 ::
  T_Metric_364 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_414 = erased
-- Function.Metric.Nat.Bundles.Metric._.trans
d_trans_416 ::
  T_Metric_364 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_trans_416 v0
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
                        (coe d_isMetric_384 (coe v0))))))))
-- Function.Metric.Nat.Bundles.Metric._.triangle
d_triangle_418 ::
  T_Metric_364 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_triangle_418 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_triangle_320
      (coe d_isMetric_384 (coe v0))
-- Function.Metric.Nat.Bundles.Metric._.≈-isEquivalence
d_'8776''45'isEquivalence_420 ::
  T_Metric_364 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_420 v0
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
                  (coe d_isMetric_384 (coe v0))))))
-- Function.Metric.Nat.Bundles.Metric._.≈⇒0
d_'8776''8658'0_422 ::
  T_Metric_364 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''8658'0_422 = erased
-- Function.Metric.Nat.Bundles.Metric._.∼-resp-≈
d_'8764''45'resp'45''8776'_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_424 ~v0 ~v1 v2
  = du_'8764''45'resp'45''8776'_424 v2
du_'8764''45'resp'45''8776'_424 ::
  T_Metric_364 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_424 v0
  = let v1 = d_isMetric_384 (coe v0) in
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
-- Function.Metric.Nat.Bundles.Metric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'691''45''8776'_426 ~v0 ~v1 v2
  = du_'8764''45'resp'691''45''8776'_426 v2
du_'8764''45'resp'691''45''8776'_426 ::
  T_Metric_364 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'691''45''8776'_426 v0
  = let v1 = d_isMetric_384 (coe v0) in
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
-- Function.Metric.Nat.Bundles.Metric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'737''45''8776'_428 ~v0 ~v1 v2
  = du_'8764''45'resp'737''45''8776'_428 v2
du_'8764''45'resp'737''45''8776'_428 ::
  T_Metric_364 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'737''45''8776'_428 v0
  = let v1 = d_isMetric_384 (coe v0) in
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
-- Function.Metric.Nat.Bundles.Metric._.EqC.isPartialEquivalence
d_isPartialEquivalence_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_432 ~v0 ~v1 v2
  = du_isPartialEquivalence_432 v2
du_isPartialEquivalence_432 ::
  T_Metric_364 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_432 v0
  = let v1 = d_isMetric_384 (coe v0) in
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
-- Function.Metric.Nat.Bundles.Metric._.EqC.refl
d_refl_434 :: T_Metric_364 -> AgdaAny -> AgdaAny
d_refl_434 v0
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
                     (coe d_isMetric_384 (coe v0)))))))
-- Function.Metric.Nat.Bundles.Metric._.EqC.reflexive
d_reflexive_436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_436 ~v0 ~v1 v2 = du_reflexive_436 v2
du_reflexive_436 ::
  T_Metric_364 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_436 v0
  = let v1 = d_isMetric_384 (coe v0) in
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
-- Function.Metric.Nat.Bundles.Metric._.EqC.sym
d_sym_438 ::
  T_Metric_364 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_438 v0
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
                     (coe d_isMetric_384 (coe v0)))))))
-- Function.Metric.Nat.Bundles.Metric._.EqC.trans
d_trans_440 ::
  T_Metric_364 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_440 v0
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
                     (coe d_isMetric_384 (coe v0)))))))
-- Function.Metric.Nat.Bundles.Metric._.Eq.isPartialEquivalence
d_isPartialEquivalence_444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_444 ~v0 ~v1 v2
  = du_isPartialEquivalence_444 v2
du_isPartialEquivalence_444 ::
  T_Metric_364 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_444 v0
  = let v1 = d_isMetric_384 (coe v0) in
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
-- Function.Metric.Nat.Bundles.Metric._.Eq.refl
d_refl_446 ::
  T_Metric_364 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_refl_446 = erased
-- Function.Metric.Nat.Bundles.Metric._.Eq.reflexive
d_reflexive_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reflexive_448 = erased
-- Function.Metric.Nat.Bundles.Metric._.Eq.sym
d_sym_450 ::
  T_Metric_364 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_450 = erased
-- Function.Metric.Nat.Bundles.Metric._.Eq.trans
d_trans_452 ::
  T_Metric_364 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_trans_452 = erased
-- Function.Metric.Nat.Bundles.Metric.semiMetric
d_semiMetric_454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 -> T_SemiMetric_266
d_semiMetric_454 ~v0 ~v1 v2 = du_semiMetric_454 v2
du_semiMetric_454 :: T_Metric_364 -> T_SemiMetric_266
du_semiMetric_454 v0
  = coe
      C_SemiMetric'46'constructor_4649 (d_d_382 (coe v0))
      (MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
         (coe d_isMetric_384 (coe v0)))
-- Function.Metric.Nat.Bundles.Metric._.preMetric
d_preMetric_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 -> T_PreMetric_90
d_preMetric_458 ~v0 ~v1 v2 = du_preMetric_458 v2
du_preMetric_458 :: T_Metric_364 -> T_PreMetric_90
du_preMetric_458 v0
  = let v1 = coe du_semiMetric_454 (coe v0) in
    coe du_preMetric_256 (coe du_quasiSemiMetric_352 (coe v1))
-- Function.Metric.Nat.Bundles.Metric._.protoMetric
d_protoMetric_460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 -> T_ProtoMetric_12
d_protoMetric_460 ~v0 ~v1 v2 = du_protoMetric_460 v2
du_protoMetric_460 :: T_Metric_364 -> T_ProtoMetric_12
du_protoMetric_460 v0
  = let v1 = coe du_semiMetric_454 (coe v0) in
    let v2 = coe du_quasiSemiMetric_352 (coe v1) in
    coe du_protoMetric_168 (coe du_preMetric_256 (coe v2))
-- Function.Metric.Nat.Bundles.Metric._.quasiSemiMetric
d_quasiSemiMetric_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Metric_364 -> T_QuasiSemiMetric_174
d_quasiSemiMetric_462 ~v0 ~v1 v2 = du_quasiSemiMetric_462 v2
du_quasiSemiMetric_462 :: T_Metric_364 -> T_QuasiSemiMetric_174
du_quasiSemiMetric_462 v0
  = coe du_quasiSemiMetric_352 (coe du_semiMetric_454 (coe v0))
-- Function.Metric.Nat.Bundles.UltraMetric
d_UltraMetric_468 a0 a1 = ()
data T_UltraMetric_468
  = C_UltraMetric'46'constructor_7937 (AgdaAny -> AgdaAny -> Integer)
                                      MAlonzo.Code.Function.Metric.Structures.T_IsGeneralMetric_308
-- Function.Metric.Nat.Bundles.UltraMetric.Carrier
d_Carrier_482 :: T_UltraMetric_468 -> ()
d_Carrier_482 = erased
-- Function.Metric.Nat.Bundles.UltraMetric._≈_
d__'8776'__484 :: T_UltraMetric_468 -> AgdaAny -> AgdaAny -> ()
d__'8776'__484 = erased
-- Function.Metric.Nat.Bundles.UltraMetric.d
d_d_486 :: T_UltraMetric_468 -> AgdaAny -> AgdaAny -> Integer
d_d_486 v0
  = case coe v0 of
      C_UltraMetric'46'constructor_7937 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.UltraMetric.isUltraMetric
d_isUltraMetric_488 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsGeneralMetric_308
d_isUltraMetric_488 v0
  = case coe v0 of
      C_UltraMetric'46'constructor_7937 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Metric.Nat.Bundles.UltraMetric._.0⇒≈
d_0'8658''8776'_492 ::
  T_UltraMetric_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_0'8658''8776'_492 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_0'8658''8776'_172
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
            (coe d_isUltraMetric_488 (coe v0))))
-- Function.Metric.Nat.Bundles.UltraMetric._.antisym
d_antisym_494 ::
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_antisym_494 = erased
-- Function.Metric.Nat.Bundles.UltraMetric._.cong
d_cong_496 ::
  T_UltraMetric_468 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cong_496 = erased
-- Function.Metric.Nat.Bundles.UltraMetric._.isEquivalence
d_isEquivalence_498 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_498 v0
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
                        (coe d_isUltraMetric_488 (coe v0))))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.isPartialOrder
d_isPartialOrder_500 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_500 v0
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
                  (coe d_isUltraMetric_488 (coe v0))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.isPreMetric
d_isPreMetric_502 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsPreMetric_96
d_isPreMetric_502 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
            (coe d_isUltraMetric_488 (coe v0))))
-- Function.Metric.Nat.Bundles.UltraMetric._.isPreorder
d_isPreorder_504 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_504 v0
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
                     (coe d_isUltraMetric_488 (coe v0)))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.isProtoMetric
d_isProtoMetric_506 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsProtoMetric_30
d_isProtoMetric_506 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isProtoMetric_104
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isPreMetric_170
         (coe
            MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
            (coe
               MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
               (coe d_isUltraMetric_488 (coe v0)))))
-- Function.Metric.Nat.Bundles.UltraMetric._.isQuasiSemiMetric
d_isQuasiSemiMetric_508 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsQuasiSemiMetric_162
d_isQuasiSemiMetric_508 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isQuasiSemiMetric_240
      (coe
         MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
         (coe d_isUltraMetric_488 (coe v0)))
-- Function.Metric.Nat.Bundles.UltraMetric._.isSemiMetric
d_isSemiMetric_510 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Function.Metric.Structures.T_IsSemiMetric_232
d_isSemiMetric_510 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
      (coe d_isUltraMetric_488 (coe v0))
-- Function.Metric.Nat.Bundles.UltraMetric._.nonNegative
d_nonNegative_512 ::
  T_UltraMetric_468 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_nonNegative_512 v0
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
                  (coe d_isUltraMetric_488 (coe v0))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.refl
d_refl_514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_refl_514 ~v0 ~v1 v2 = du_refl_514 v2
du_refl_514 ::
  T_UltraMetric_468 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_refl_514 v0
  = let v1 = d_isUltraMetric_488 (coe v0) in
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
-- Function.Metric.Nat.Bundles.UltraMetric._.reflexive
d_reflexive_516 ::
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_reflexive_516 v0
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
                        (coe d_isUltraMetric_488 (coe v0))))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.sym
d_sym_518 ::
  T_UltraMetric_468 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_518 = erased
-- Function.Metric.Nat.Bundles.UltraMetric._.trans
d_trans_520 ::
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_trans_520 v0
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
                        (coe d_isUltraMetric_488 (coe v0))))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.triangle
d_triangle_522 ::
  T_UltraMetric_468 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_triangle_522 v0
  = coe
      MAlonzo.Code.Function.Metric.Structures.d_triangle_320
      (coe d_isUltraMetric_488 (coe v0))
-- Function.Metric.Nat.Bundles.UltraMetric._.≈-isEquivalence
d_'8776''45'isEquivalence_524 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_524 v0
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
                  (coe d_isUltraMetric_488 (coe v0))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.≈⇒0
d_'8776''8658'0_526 ::
  T_UltraMetric_468 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''8658'0_526 = erased
-- Function.Metric.Nat.Bundles.UltraMetric._.∼-resp-≈
d_'8764''45'resp'45''8776'_528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_528 ~v0 ~v1 v2
  = du_'8764''45'resp'45''8776'_528 v2
du_'8764''45'resp'45''8776'_528 ::
  T_UltraMetric_468 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_528 v0
  = let v1 = d_isUltraMetric_488 (coe v0) in
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
-- Function.Metric.Nat.Bundles.UltraMetric._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'691''45''8776'_530 ~v0 ~v1 v2
  = du_'8764''45'resp'691''45''8776'_530 v2
du_'8764''45'resp'691''45''8776'_530 ::
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'691''45''8776'_530 v0
  = let v1 = d_isUltraMetric_488 (coe v0) in
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
-- Function.Metric.Nat.Bundles.UltraMetric._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'8764''45'resp'737''45''8776'_532 ~v0 ~v1 v2
  = du_'8764''45'resp'737''45''8776'_532 v2
du_'8764''45'resp'737''45''8776'_532 ::
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'8764''45'resp'737''45''8776'_532 v0
  = let v1 = d_isUltraMetric_488 (coe v0) in
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
-- Function.Metric.Nat.Bundles.UltraMetric._.EqC.isPartialEquivalence
d_isPartialEquivalence_536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_536 ~v0 ~v1 v2
  = du_isPartialEquivalence_536 v2
du_isPartialEquivalence_536 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_536 v0
  = let v1 = d_isUltraMetric_488 (coe v0) in
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
-- Function.Metric.Nat.Bundles.UltraMetric._.EqC.refl
d_refl_538 :: T_UltraMetric_468 -> AgdaAny -> AgdaAny
d_refl_538 v0
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
                     (coe d_isUltraMetric_488 (coe v0)))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.EqC.reflexive
d_reflexive_540 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_540 ~v0 ~v1 v2 = du_reflexive_540 v2
du_reflexive_540 ::
  T_UltraMetric_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_540 v0
  = let v1 = d_isUltraMetric_488 (coe v0) in
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
-- Function.Metric.Nat.Bundles.UltraMetric._.EqC.sym
d_sym_542 ::
  T_UltraMetric_468 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_542 v0
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
                     (coe d_isUltraMetric_488 (coe v0)))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.EqC.trans
d_trans_544 ::
  T_UltraMetric_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_544 v0
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
                     (coe d_isUltraMetric_488 (coe v0)))))))
-- Function.Metric.Nat.Bundles.UltraMetric._.Eq.isPartialEquivalence
d_isPartialEquivalence_548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_548 ~v0 ~v1 v2
  = du_isPartialEquivalence_548 v2
du_isPartialEquivalence_548 ::
  T_UltraMetric_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_548 v0
  = let v1 = d_isUltraMetric_488 (coe v0) in
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
-- Function.Metric.Nat.Bundles.UltraMetric._.Eq.refl
d_refl_550 ::
  T_UltraMetric_468 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_refl_550 = erased
-- Function.Metric.Nat.Bundles.UltraMetric._.Eq.reflexive
d_reflexive_552 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reflexive_552 = erased
-- Function.Metric.Nat.Bundles.UltraMetric._.Eq.sym
d_sym_554 ::
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_554 = erased
-- Function.Metric.Nat.Bundles.UltraMetric._.Eq.trans
d_trans_556 ::
  T_UltraMetric_468 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_trans_556 = erased
-- Function.Metric.Nat.Bundles.UltraMetric.semiMetric
d_semiMetric_558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 -> T_SemiMetric_266
d_semiMetric_558 ~v0 ~v1 v2 = du_semiMetric_558 v2
du_semiMetric_558 :: T_UltraMetric_468 -> T_SemiMetric_266
du_semiMetric_558 v0
  = coe
      C_SemiMetric'46'constructor_4649 (d_d_486 (coe v0))
      (MAlonzo.Code.Function.Metric.Structures.d_isSemiMetric_318
         (coe d_isUltraMetric_488 (coe v0)))
-- Function.Metric.Nat.Bundles.UltraMetric._.preMetric
d_preMetric_562 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 -> T_PreMetric_90
d_preMetric_562 ~v0 ~v1 v2 = du_preMetric_562 v2
du_preMetric_562 :: T_UltraMetric_468 -> T_PreMetric_90
du_preMetric_562 v0
  = let v1 = coe du_semiMetric_558 (coe v0) in
    coe du_preMetric_256 (coe du_quasiSemiMetric_352 (coe v1))
-- Function.Metric.Nat.Bundles.UltraMetric._.protoMetric
d_protoMetric_564 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 -> T_ProtoMetric_12
d_protoMetric_564 ~v0 ~v1 v2 = du_protoMetric_564 v2
du_protoMetric_564 :: T_UltraMetric_468 -> T_ProtoMetric_12
du_protoMetric_564 v0
  = let v1 = coe du_semiMetric_558 (coe v0) in
    let v2 = coe du_quasiSemiMetric_352 (coe v1) in
    coe du_protoMetric_168 (coe du_preMetric_256 (coe v2))
-- Function.Metric.Nat.Bundles.UltraMetric._.quasiSemiMetric
d_quasiSemiMetric_566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UltraMetric_468 -> T_QuasiSemiMetric_174
d_quasiSemiMetric_566 ~v0 ~v1 v2 = du_quasiSemiMetric_566 v2
du_quasiSemiMetric_566 ::
  T_UltraMetric_468 -> T_QuasiSemiMetric_174
du_quasiSemiMetric_566 v0
  = coe du_quasiSemiMetric_352 (coe du_semiMetric_558 (coe v0))
