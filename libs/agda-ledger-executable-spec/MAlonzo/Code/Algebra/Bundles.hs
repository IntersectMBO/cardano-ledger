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

module MAlonzo.Code.Algebra.Bundles where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Bundles.Magma
d_Magma_8 a0 a1 = ()
data T_Magma_8
  = C_Magma'46'constructor_187 (AgdaAny -> AgdaAny -> AgdaAny)
                               MAlonzo.Code.Algebra.Structures.T_IsMagma_140
-- Algebra.Bundles.Magma.Carrier
d_Carrier_22 :: T_Magma_8 -> ()
d_Carrier_22 = erased
-- Algebra.Bundles.Magma._≈_
d__'8776'__24 :: T_Magma_8 -> AgdaAny -> AgdaAny -> ()
d__'8776'__24 = erased
-- Algebra.Bundles.Magma._∙_
d__'8729'__26 :: T_Magma_8 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__26 v0
  = case coe v0 of
      C_Magma'46'constructor_187 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Magma.isMagma
d_isMagma_28 ::
  T_Magma_8 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_28 v0
  = case coe v0 of
      C_Magma'46'constructor_187 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Magma._.isEquivalence
d_isEquivalence_32 ::
  T_Magma_8 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_32 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe d_isMagma_28 (coe v0))
-- Algebra.Bundles.Magma._.isPartialEquivalence
d_isPartialEquivalence_34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Magma_8 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_34 ~v0 ~v1 v2
  = du_isPartialEquivalence_34 v2
du_isPartialEquivalence_34 ::
  T_Magma_8 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_34 v0
  = let v1 = d_isMagma_28 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1))
-- Algebra.Bundles.Magma._.refl
d_refl_36 :: T_Magma_8 -> AgdaAny -> AgdaAny
d_refl_36 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe d_isMagma_28 (coe v0)))
-- Algebra.Bundles.Magma._.reflexive
d_reflexive_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Magma_8 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_38 ~v0 ~v1 v2 = du_reflexive_38 v2
du_reflexive_38 ::
  T_Magma_8 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_38 v0
  = let v1 = d_isMagma_28 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v1))
        v2
-- Algebra.Bundles.Magma._.setoid
d_setoid_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Magma_8 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_40 ~v0 ~v1 v2 = du_setoid_40 v2
du_setoid_40 ::
  T_Magma_8 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_40 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe d_isMagma_28 (coe v0))
-- Algebra.Bundles.Magma._.sym
d_sym_42 :: T_Magma_8 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_42 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe d_isMagma_28 (coe v0)))
-- Algebra.Bundles.Magma._.trans
d_trans_44 ::
  T_Magma_8 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_44 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe d_isMagma_28 (coe v0)))
-- Algebra.Bundles.Magma._.∙-cong
d_'8729''45'cong_46 ::
  T_Magma_8 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_46 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe d_isMagma_28 (coe v0))
-- Algebra.Bundles.Magma._.∙-congʳ
d_'8729''45'cong'691'_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Magma_8 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_48 ~v0 ~v1 v2 = du_'8729''45'cong'691'_48 v2
du_'8729''45'cong'691'_48 ::
  T_Magma_8 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_48 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe d_isMagma_28 (coe v0))
-- Algebra.Bundles.Magma._.∙-congˡ
d_'8729''45'cong'737'_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Magma_8 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_50 ~v0 ~v1 v2 = du_'8729''45'cong'737'_50 v2
du_'8729''45'cong'737'_50 ::
  T_Magma_8 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_50 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe d_isMagma_28 (coe v0))
-- Algebra.Bundles.Magma.rawMagma
d_rawMagma_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Magma_8 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_52 ~v0 ~v1 v2 = du_rawMagma_52 v2
du_rawMagma_52 ::
  T_Magma_8 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_52 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMagma'46'constructor_77
      (d__'8729'__26 (coe v0))
-- Algebra.Bundles.Magma._._≉_
d__'8777'__56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Magma_8 -> AgdaAny -> AgdaAny -> ()
d__'8777'__56 = erased
-- Algebra.Bundles.SelectiveMagma
d_SelectiveMagma_62 a0 a1 = ()
data T_SelectiveMagma_62
  = C_SelectiveMagma'46'constructor_1177 (AgdaAny ->
                                          AgdaAny -> AgdaAny)
                                         MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
-- Algebra.Bundles.SelectiveMagma.Carrier
d_Carrier_76 :: T_SelectiveMagma_62 -> ()
d_Carrier_76 = erased
-- Algebra.Bundles.SelectiveMagma._≈_
d__'8776'__78 :: T_SelectiveMagma_62 -> AgdaAny -> AgdaAny -> ()
d__'8776'__78 = erased
-- Algebra.Bundles.SelectiveMagma._∙_
d__'8729'__80 ::
  T_SelectiveMagma_62 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__80 v0
  = case coe v0 of
      C_SelectiveMagma'46'constructor_1177 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SelectiveMagma.isSelectiveMagma
d_isSelectiveMagma_82 ::
  T_SelectiveMagma_62 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_isSelectiveMagma_82 v0
  = case coe v0 of
      C_SelectiveMagma'46'constructor_1177 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SelectiveMagma._.isEquivalence
d_isEquivalence_86 ::
  T_SelectiveMagma_62 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_86 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_408
         (coe d_isSelectiveMagma_82 (coe v0)))
-- Algebra.Bundles.SelectiveMagma._.isMagma
d_isMagma_88 ::
  T_SelectiveMagma_62 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_88 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_408
      (coe d_isSelectiveMagma_82 (coe v0))
-- Algebra.Bundles.SelectiveMagma._.isPartialEquivalence
d_isPartialEquivalence_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SelectiveMagma_62 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_90 ~v0 ~v1 v2
  = du_isPartialEquivalence_90 v2
du_isPartialEquivalence_90 ::
  T_SelectiveMagma_62 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_90 v0
  = let v1 = d_isSelectiveMagma_82 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.SelectiveMagma._.refl
d_refl_92 :: T_SelectiveMagma_62 -> AgdaAny -> AgdaAny
d_refl_92 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_408
            (coe d_isSelectiveMagma_82 (coe v0))))
-- Algebra.Bundles.SelectiveMagma._.reflexive
d_reflexive_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SelectiveMagma_62 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_94 ~v0 ~v1 v2 = du_reflexive_94 v2
du_reflexive_94 ::
  T_SelectiveMagma_62 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_94 v0
  = let v1 = d_isSelectiveMagma_82 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.SelectiveMagma._.sel
d_sel_96 ::
  T_SelectiveMagma_62 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_96 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_sel_410
      (coe d_isSelectiveMagma_82 (coe v0))
-- Algebra.Bundles.SelectiveMagma._.setoid
d_setoid_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SelectiveMagma_62 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_98 ~v0 ~v1 v2 = du_setoid_98 v2
du_setoid_98 ::
  T_SelectiveMagma_62 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_98 v0
  = let v1 = d_isSelectiveMagma_82 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1))
-- Algebra.Bundles.SelectiveMagma._.sym
d_sym_100 ::
  T_SelectiveMagma_62 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_100 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_408
            (coe d_isSelectiveMagma_82 (coe v0))))
-- Algebra.Bundles.SelectiveMagma._.trans
d_trans_102 ::
  T_SelectiveMagma_62 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_102 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_408
            (coe d_isSelectiveMagma_82 (coe v0))))
-- Algebra.Bundles.SelectiveMagma._.∙-cong
d_'8729''45'cong_104 ::
  T_SelectiveMagma_62 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_104 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_408
         (coe d_isSelectiveMagma_82 (coe v0)))
-- Algebra.Bundles.SelectiveMagma._.∙-congʳ
d_'8729''45'cong'691'_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SelectiveMagma_62 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_106 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_106 v2
du_'8729''45'cong'691'_106 ::
  T_SelectiveMagma_62 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_106 v0
  = let v1 = d_isSelectiveMagma_82 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1))
-- Algebra.Bundles.SelectiveMagma._.∙-congˡ
d_'8729''45'cong'737'_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SelectiveMagma_62 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_108 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_108 v2
du_'8729''45'cong'737'_108 ::
  T_SelectiveMagma_62 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_108 v0
  = let v1 = d_isSelectiveMagma_82 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1))
-- Algebra.Bundles.SelectiveMagma.magma
d_magma_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SelectiveMagma_62 -> T_Magma_8
d_magma_110 ~v0 ~v1 v2 = du_magma_110 v2
du_magma_110 :: T_SelectiveMagma_62 -> T_Magma_8
du_magma_110 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__80 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_408
         (coe d_isSelectiveMagma_82 (coe v0)))
-- Algebra.Bundles.SelectiveMagma._.rawMagma
d_rawMagma_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SelectiveMagma_62 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_114 ~v0 ~v1 v2 = du_rawMagma_114 v2
du_rawMagma_114 ::
  T_SelectiveMagma_62 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_114 v0 = coe du_rawMagma_52 (coe du_magma_110 (coe v0))
-- Algebra.Bundles.CommutativeMagma
d_CommutativeMagma_120 a0 a1 = ()
data T_CommutativeMagma_120
  = C_CommutativeMagma'46'constructor_2217 (AgdaAny ->
                                            AgdaAny -> AgdaAny)
                                           MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
-- Algebra.Bundles.CommutativeMagma.Carrier
d_Carrier_134 :: T_CommutativeMagma_120 -> ()
d_Carrier_134 = erased
-- Algebra.Bundles.CommutativeMagma._≈_
d__'8776'__136 ::
  T_CommutativeMagma_120 -> AgdaAny -> AgdaAny -> ()
d__'8776'__136 = erased
-- Algebra.Bundles.CommutativeMagma._∙_
d__'8729'__138 ::
  T_CommutativeMagma_120 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__138 v0
  = case coe v0 of
      C_CommutativeMagma'46'constructor_2217 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeMagma.isCommutativeMagma
d_isCommutativeMagma_140 ::
  T_CommutativeMagma_120 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_140 v0
  = case coe v0 of
      C_CommutativeMagma'46'constructor_2217 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeMagma._.comm
d_comm_144 ::
  T_CommutativeMagma_120 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_144 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_186
      (coe d_isCommutativeMagma_140 (coe v0))
-- Algebra.Bundles.CommutativeMagma._.isEquivalence
d_isEquivalence_146 ::
  T_CommutativeMagma_120 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_146 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_184
         (coe d_isCommutativeMagma_140 (coe v0)))
-- Algebra.Bundles.CommutativeMagma._.isMagma
d_isMagma_148 ::
  T_CommutativeMagma_120 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_148 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_184
      (coe d_isCommutativeMagma_140 (coe v0))
-- Algebra.Bundles.CommutativeMagma._.isPartialEquivalence
d_isPartialEquivalence_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMagma_120 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_150 ~v0 ~v1 v2
  = du_isPartialEquivalence_150 v2
du_isPartialEquivalence_150 ::
  T_CommutativeMagma_120 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_150 v0
  = let v1 = d_isCommutativeMagma_140 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.CommutativeMagma._.refl
d_refl_152 :: T_CommutativeMagma_120 -> AgdaAny -> AgdaAny
d_refl_152 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_184
            (coe d_isCommutativeMagma_140 (coe v0))))
-- Algebra.Bundles.CommutativeMagma._.reflexive
d_reflexive_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMagma_120 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_154 ~v0 ~v1 v2 = du_reflexive_154 v2
du_reflexive_154 ::
  T_CommutativeMagma_120 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_154 v0
  = let v1 = d_isCommutativeMagma_140 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.CommutativeMagma._.setoid
d_setoid_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMagma_120 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_156 ~v0 ~v1 v2 = du_setoid_156 v2
du_setoid_156 ::
  T_CommutativeMagma_120 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_156 v0
  = let v1 = d_isCommutativeMagma_140 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v1))
-- Algebra.Bundles.CommutativeMagma._.sym
d_sym_158 ::
  T_CommutativeMagma_120 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_158 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_184
            (coe d_isCommutativeMagma_140 (coe v0))))
-- Algebra.Bundles.CommutativeMagma._.trans
d_trans_160 ::
  T_CommutativeMagma_120 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_160 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_184
            (coe d_isCommutativeMagma_140 (coe v0))))
-- Algebra.Bundles.CommutativeMagma._.∙-cong
d_'8729''45'cong_162 ::
  T_CommutativeMagma_120 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_162 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_184
         (coe d_isCommutativeMagma_140 (coe v0)))
-- Algebra.Bundles.CommutativeMagma._.∙-congʳ
d_'8729''45'cong'691'_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMagma_120 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_164 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_164 v2
du_'8729''45'cong'691'_164 ::
  T_CommutativeMagma_120 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_164 v0
  = let v1 = d_isCommutativeMagma_140 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v1))
-- Algebra.Bundles.CommutativeMagma._.∙-congˡ
d_'8729''45'cong'737'_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMagma_120 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_166 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_166 v2
du_'8729''45'cong'737'_166 ::
  T_CommutativeMagma_120 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_166 v0
  = let v1 = d_isCommutativeMagma_140 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_184 (coe v1))
-- Algebra.Bundles.CommutativeMagma.magma
d_magma_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMagma_120 -> T_Magma_8
d_magma_168 ~v0 ~v1 v2 = du_magma_168 v2
du_magma_168 :: T_CommutativeMagma_120 -> T_Magma_8
du_magma_168 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__138 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_184
         (coe d_isCommutativeMagma_140 (coe v0)))
-- Algebra.Bundles.CommutativeMagma._.rawMagma
d_rawMagma_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMagma_120 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_172 ~v0 ~v1 v2 = du_rawMagma_172 v2
du_rawMagma_172 ::
  T_CommutativeMagma_120 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_172 v0 = coe du_rawMagma_52 (coe du_magma_168 (coe v0))
-- Algebra.Bundles.IdempotentMagma
d_IdempotentMagma_178 a0 a1 = ()
data T_IdempotentMagma_178
  = C_IdempotentMagma'46'constructor_3257 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212
-- Algebra.Bundles.IdempotentMagma.Carrier
d_Carrier_192 :: T_IdempotentMagma_178 -> ()
d_Carrier_192 = erased
-- Algebra.Bundles.IdempotentMagma._≈_
d__'8776'__194 :: T_IdempotentMagma_178 -> AgdaAny -> AgdaAny -> ()
d__'8776'__194 = erased
-- Algebra.Bundles.IdempotentMagma._∙_
d__'8729'__196 ::
  T_IdempotentMagma_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__196 v0
  = case coe v0 of
      C_IdempotentMagma'46'constructor_3257 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentMagma.isIdempotentMagma
d_isIdempotentMagma_198 ::
  T_IdempotentMagma_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentMagma_212
d_isIdempotentMagma_198 v0
  = case coe v0 of
      C_IdempotentMagma'46'constructor_3257 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentMagma._.idem
d_idem_202 :: T_IdempotentMagma_178 -> AgdaAny -> AgdaAny
d_idem_202 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_222
      (coe d_isIdempotentMagma_198 (coe v0))
-- Algebra.Bundles.IdempotentMagma._.isEquivalence
d_isEquivalence_204 ::
  T_IdempotentMagma_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_204 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_220
         (coe d_isIdempotentMagma_198 (coe v0)))
-- Algebra.Bundles.IdempotentMagma._.isMagma
d_isMagma_206 ::
  T_IdempotentMagma_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_206 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_220
      (coe d_isIdempotentMagma_198 (coe v0))
-- Algebra.Bundles.IdempotentMagma._.isPartialEquivalence
d_isPartialEquivalence_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentMagma_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_208 ~v0 ~v1 v2
  = du_isPartialEquivalence_208 v2
du_isPartialEquivalence_208 ::
  T_IdempotentMagma_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_208 v0
  = let v1 = d_isIdempotentMagma_198 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.IdempotentMagma._.refl
d_refl_210 :: T_IdempotentMagma_178 -> AgdaAny -> AgdaAny
d_refl_210 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_220
            (coe d_isIdempotentMagma_198 (coe v0))))
-- Algebra.Bundles.IdempotentMagma._.reflexive
d_reflexive_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentMagma_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_212 ~v0 ~v1 v2 = du_reflexive_212 v2
du_reflexive_212 ::
  T_IdempotentMagma_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_212 v0
  = let v1 = d_isIdempotentMagma_198 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.IdempotentMagma._.setoid
d_setoid_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentMagma_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_214 ~v0 ~v1 v2 = du_setoid_214 v2
du_setoid_214 ::
  T_IdempotentMagma_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_214 v0
  = let v1 = d_isIdempotentMagma_198 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v1))
-- Algebra.Bundles.IdempotentMagma._.sym
d_sym_216 ::
  T_IdempotentMagma_178 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_216 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_220
            (coe d_isIdempotentMagma_198 (coe v0))))
-- Algebra.Bundles.IdempotentMagma._.trans
d_trans_218 ::
  T_IdempotentMagma_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_218 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_220
            (coe d_isIdempotentMagma_198 (coe v0))))
-- Algebra.Bundles.IdempotentMagma._.∙-cong
d_'8729''45'cong_220 ::
  T_IdempotentMagma_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_220 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_220
         (coe d_isIdempotentMagma_198 (coe v0)))
-- Algebra.Bundles.IdempotentMagma._.∙-congʳ
d_'8729''45'cong'691'_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentMagma_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_222 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_222 v2
du_'8729''45'cong'691'_222 ::
  T_IdempotentMagma_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_222 v0
  = let v1 = d_isIdempotentMagma_198 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v1))
-- Algebra.Bundles.IdempotentMagma._.∙-congˡ
d_'8729''45'cong'737'_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentMagma_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_224 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_224 v2
du_'8729''45'cong'737'_224 ::
  T_IdempotentMagma_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_224 v0
  = let v1 = d_isIdempotentMagma_198 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_220 (coe v1))
-- Algebra.Bundles.IdempotentMagma.magma
d_magma_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentMagma_178 -> T_Magma_8
d_magma_226 ~v0 ~v1 v2 = du_magma_226 v2
du_magma_226 :: T_IdempotentMagma_178 -> T_Magma_8
du_magma_226 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__196 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_220
         (coe d_isIdempotentMagma_198 (coe v0)))
-- Algebra.Bundles.IdempotentMagma._.rawMagma
d_rawMagma_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentMagma_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_230 ~v0 ~v1 v2 = du_rawMagma_230 v2
du_rawMagma_230 ::
  T_IdempotentMagma_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_230 v0 = coe du_rawMagma_52 (coe du_magma_226 (coe v0))
-- Algebra.Bundles.AlternativeMagma
d_AlternativeMagma_236 a0 a1 = ()
data T_AlternativeMagma_236
  = C_AlternativeMagma'46'constructor_4293 (AgdaAny ->
                                            AgdaAny -> AgdaAny)
                                           MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248
-- Algebra.Bundles.AlternativeMagma.Carrier
d_Carrier_250 :: T_AlternativeMagma_236 -> ()
d_Carrier_250 = erased
-- Algebra.Bundles.AlternativeMagma._≈_
d__'8776'__252 ::
  T_AlternativeMagma_236 -> AgdaAny -> AgdaAny -> ()
d__'8776'__252 = erased
-- Algebra.Bundles.AlternativeMagma._∙_
d__'8729'__254 ::
  T_AlternativeMagma_236 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__254 v0
  = case coe v0 of
      C_AlternativeMagma'46'constructor_4293 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.AlternativeMagma.isAlternativeMagma
d_isAlternativeMagma_256 ::
  T_AlternativeMagma_236 ->
  MAlonzo.Code.Algebra.Structures.T_IsAlternativeMagma_248
d_isAlternativeMagma_256 v0
  = case coe v0 of
      C_AlternativeMagma'46'constructor_4293 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.AlternativeMagma._.alter
d_alter_260 ::
  T_AlternativeMagma_236 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_alter_260 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_alter_258
      (coe d_isAlternativeMagma_256 (coe v0))
-- Algebra.Bundles.AlternativeMagma._.alternativeʳ
d_alternative'691'_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlternativeMagma_236 -> AgdaAny -> AgdaAny -> AgdaAny
d_alternative'691'_262 ~v0 ~v1 v2 = du_alternative'691'_262 v2
du_alternative'691'_262 ::
  T_AlternativeMagma_236 -> AgdaAny -> AgdaAny -> AgdaAny
du_alternative'691'_262 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_alternative'691'_284
      (coe d_isAlternativeMagma_256 (coe v0))
-- Algebra.Bundles.AlternativeMagma._.alternativeˡ
d_alternative'737'_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlternativeMagma_236 -> AgdaAny -> AgdaAny -> AgdaAny
d_alternative'737'_264 ~v0 ~v1 v2 = du_alternative'737'_264 v2
du_alternative'737'_264 ::
  T_AlternativeMagma_236 -> AgdaAny -> AgdaAny -> AgdaAny
du_alternative'737'_264 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_alternative'737'_282
      (coe d_isAlternativeMagma_256 (coe v0))
-- Algebra.Bundles.AlternativeMagma._.isEquivalence
d_isEquivalence_266 ::
  T_AlternativeMagma_236 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_266 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_256
         (coe d_isAlternativeMagma_256 (coe v0)))
-- Algebra.Bundles.AlternativeMagma._.isMagma
d_isMagma_268 ::
  T_AlternativeMagma_236 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_268 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_256
      (coe d_isAlternativeMagma_256 (coe v0))
-- Algebra.Bundles.AlternativeMagma._.isPartialEquivalence
d_isPartialEquivalence_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlternativeMagma_236 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_270 ~v0 ~v1 v2
  = du_isPartialEquivalence_270 v2
du_isPartialEquivalence_270 ::
  T_AlternativeMagma_236 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_270 v0
  = let v1 = d_isAlternativeMagma_256 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.AlternativeMagma._.refl
d_refl_272 :: T_AlternativeMagma_236 -> AgdaAny -> AgdaAny
d_refl_272 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_256
            (coe d_isAlternativeMagma_256 (coe v0))))
-- Algebra.Bundles.AlternativeMagma._.reflexive
d_reflexive_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlternativeMagma_236 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_274 ~v0 ~v1 v2 = du_reflexive_274 v2
du_reflexive_274 ::
  T_AlternativeMagma_236 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_274 v0
  = let v1 = d_isAlternativeMagma_256 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.AlternativeMagma._.setoid
d_setoid_276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlternativeMagma_236 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_276 ~v0 ~v1 v2 = du_setoid_276 v2
du_setoid_276 ::
  T_AlternativeMagma_236 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_276 v0
  = let v1 = d_isAlternativeMagma_256 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v1))
-- Algebra.Bundles.AlternativeMagma._.sym
d_sym_278 ::
  T_AlternativeMagma_236 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_278 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_256
            (coe d_isAlternativeMagma_256 (coe v0))))
-- Algebra.Bundles.AlternativeMagma._.trans
d_trans_280 ::
  T_AlternativeMagma_236 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_280 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_256
            (coe d_isAlternativeMagma_256 (coe v0))))
-- Algebra.Bundles.AlternativeMagma._.∙-cong
d_'8729''45'cong_282 ::
  T_AlternativeMagma_236 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_282 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_256
         (coe d_isAlternativeMagma_256 (coe v0)))
-- Algebra.Bundles.AlternativeMagma._.∙-congʳ
d_'8729''45'cong'691'_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlternativeMagma_236 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_284 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_284 v2
du_'8729''45'cong'691'_284 ::
  T_AlternativeMagma_236 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_284 v0
  = let v1 = d_isAlternativeMagma_256 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v1))
-- Algebra.Bundles.AlternativeMagma._.∙-congˡ
d_'8729''45'cong'737'_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlternativeMagma_236 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_286 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_286 v2
du_'8729''45'cong'737'_286 ::
  T_AlternativeMagma_236 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_286 v0
  = let v1 = d_isAlternativeMagma_256 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_256 (coe v1))
-- Algebra.Bundles.AlternativeMagma.magma
d_magma_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlternativeMagma_236 -> T_Magma_8
d_magma_288 ~v0 ~v1 v2 = du_magma_288 v2
du_magma_288 :: T_AlternativeMagma_236 -> T_Magma_8
du_magma_288 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__254 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_256
         (coe d_isAlternativeMagma_256 (coe v0)))
-- Algebra.Bundles.AlternativeMagma._.rawMagma
d_rawMagma_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlternativeMagma_236 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_292 ~v0 ~v1 v2 = du_rawMagma_292 v2
du_rawMagma_292 ::
  T_AlternativeMagma_236 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_292 v0 = coe du_rawMagma_52 (coe du_magma_288 (coe v0))
-- Algebra.Bundles.FlexibleMagma
d_FlexibleMagma_298 a0 a1 = ()
data T_FlexibleMagma_298
  = C_FlexibleMagma'46'constructor_5377 (AgdaAny ->
                                         AgdaAny -> AgdaAny)
                                        MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288
-- Algebra.Bundles.FlexibleMagma.Carrier
d_Carrier_312 :: T_FlexibleMagma_298 -> ()
d_Carrier_312 = erased
-- Algebra.Bundles.FlexibleMagma._≈_
d__'8776'__314 :: T_FlexibleMagma_298 -> AgdaAny -> AgdaAny -> ()
d__'8776'__314 = erased
-- Algebra.Bundles.FlexibleMagma._∙_
d__'8729'__316 ::
  T_FlexibleMagma_298 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__316 v0
  = case coe v0 of
      C_FlexibleMagma'46'constructor_5377 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.FlexibleMagma.isFlexibleMagma
d_isFlexibleMagma_318 ::
  T_FlexibleMagma_298 ->
  MAlonzo.Code.Algebra.Structures.T_IsFlexibleMagma_288
d_isFlexibleMagma_318 v0
  = case coe v0 of
      C_FlexibleMagma'46'constructor_5377 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.FlexibleMagma._.flex
d_flex_322 :: T_FlexibleMagma_298 -> AgdaAny -> AgdaAny -> AgdaAny
d_flex_322 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_flex_298
      (coe d_isFlexibleMagma_318 (coe v0))
-- Algebra.Bundles.FlexibleMagma._.isEquivalence
d_isEquivalence_324 ::
  T_FlexibleMagma_298 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_324 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_296
         (coe d_isFlexibleMagma_318 (coe v0)))
-- Algebra.Bundles.FlexibleMagma._.isMagma
d_isMagma_326 ::
  T_FlexibleMagma_298 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_326 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_296
      (coe d_isFlexibleMagma_318 (coe v0))
-- Algebra.Bundles.FlexibleMagma._.isPartialEquivalence
d_isPartialEquivalence_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_FlexibleMagma_298 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_328 ~v0 ~v1 v2
  = du_isPartialEquivalence_328 v2
du_isPartialEquivalence_328 ::
  T_FlexibleMagma_298 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_328 v0
  = let v1 = d_isFlexibleMagma_318 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.FlexibleMagma._.refl
d_refl_330 :: T_FlexibleMagma_298 -> AgdaAny -> AgdaAny
d_refl_330 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_296
            (coe d_isFlexibleMagma_318 (coe v0))))
-- Algebra.Bundles.FlexibleMagma._.reflexive
d_reflexive_332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_FlexibleMagma_298 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_332 ~v0 ~v1 v2 = du_reflexive_332 v2
du_reflexive_332 ::
  T_FlexibleMagma_298 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_332 v0
  = let v1 = d_isFlexibleMagma_318 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.FlexibleMagma._.setoid
d_setoid_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_FlexibleMagma_298 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_334 ~v0 ~v1 v2 = du_setoid_334 v2
du_setoid_334 ::
  T_FlexibleMagma_298 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_334 v0
  = let v1 = d_isFlexibleMagma_318 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v1))
-- Algebra.Bundles.FlexibleMagma._.sym
d_sym_336 ::
  T_FlexibleMagma_298 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_336 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_296
            (coe d_isFlexibleMagma_318 (coe v0))))
-- Algebra.Bundles.FlexibleMagma._.trans
d_trans_338 ::
  T_FlexibleMagma_298 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_338 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_296
            (coe d_isFlexibleMagma_318 (coe v0))))
-- Algebra.Bundles.FlexibleMagma._.∙-cong
d_'8729''45'cong_340 ::
  T_FlexibleMagma_298 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_340 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_296
         (coe d_isFlexibleMagma_318 (coe v0)))
-- Algebra.Bundles.FlexibleMagma._.∙-congʳ
d_'8729''45'cong'691'_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_FlexibleMagma_298 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_342 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_342 v2
du_'8729''45'cong'691'_342 ::
  T_FlexibleMagma_298 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_342 v0
  = let v1 = d_isFlexibleMagma_318 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v1))
-- Algebra.Bundles.FlexibleMagma._.∙-congˡ
d_'8729''45'cong'737'_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_FlexibleMagma_298 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_344 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_344 v2
du_'8729''45'cong'737'_344 ::
  T_FlexibleMagma_298 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_344 v0
  = let v1 = d_isFlexibleMagma_318 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_296 (coe v1))
-- Algebra.Bundles.FlexibleMagma.magma
d_magma_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_FlexibleMagma_298 -> T_Magma_8
d_magma_346 ~v0 ~v1 v2 = du_magma_346 v2
du_magma_346 :: T_FlexibleMagma_298 -> T_Magma_8
du_magma_346 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__316 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_296
         (coe d_isFlexibleMagma_318 (coe v0)))
-- Algebra.Bundles.FlexibleMagma._.rawMagma
d_rawMagma_350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_FlexibleMagma_298 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_350 ~v0 ~v1 v2 = du_rawMagma_350 v2
du_rawMagma_350 ::
  T_FlexibleMagma_298 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_350 v0 = coe du_rawMagma_52 (coe du_magma_346 (coe v0))
-- Algebra.Bundles.MedialMagma
d_MedialMagma_356 a0 a1 = ()
data T_MedialMagma_356
  = C_MedialMagma'46'constructor_6417 (AgdaAny -> AgdaAny -> AgdaAny)
                                      MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324
-- Algebra.Bundles.MedialMagma.Carrier
d_Carrier_370 :: T_MedialMagma_356 -> ()
d_Carrier_370 = erased
-- Algebra.Bundles.MedialMagma._≈_
d__'8776'__372 :: T_MedialMagma_356 -> AgdaAny -> AgdaAny -> ()
d__'8776'__372 = erased
-- Algebra.Bundles.MedialMagma._∙_
d__'8729'__374 ::
  T_MedialMagma_356 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__374 v0
  = case coe v0 of
      C_MedialMagma'46'constructor_6417 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MedialMagma.isMedialMagma
d_isMedialMagma_376 ::
  T_MedialMagma_356 ->
  MAlonzo.Code.Algebra.Structures.T_IsMedialMagma_324
d_isMedialMagma_376 v0
  = case coe v0 of
      C_MedialMagma'46'constructor_6417 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MedialMagma._.isEquivalence
d_isEquivalence_380 ::
  T_MedialMagma_356 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_380 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_332
         (coe d_isMedialMagma_376 (coe v0)))
-- Algebra.Bundles.MedialMagma._.isMagma
d_isMagma_382 ::
  T_MedialMagma_356 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_382 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_332
      (coe d_isMedialMagma_376 (coe v0))
-- Algebra.Bundles.MedialMagma._.isPartialEquivalence
d_isPartialEquivalence_384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MedialMagma_356 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_384 ~v0 ~v1 v2
  = du_isPartialEquivalence_384 v2
du_isPartialEquivalence_384 ::
  T_MedialMagma_356 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_384 v0
  = let v1 = d_isMedialMagma_376 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.MedialMagma._.medial
d_medial_386 ::
  T_MedialMagma_356 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_medial_386 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_medial_334
      (coe d_isMedialMagma_376 (coe v0))
-- Algebra.Bundles.MedialMagma._.refl
d_refl_388 :: T_MedialMagma_356 -> AgdaAny -> AgdaAny
d_refl_388 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_332
            (coe d_isMedialMagma_376 (coe v0))))
-- Algebra.Bundles.MedialMagma._.reflexive
d_reflexive_390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MedialMagma_356 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_390 ~v0 ~v1 v2 = du_reflexive_390 v2
du_reflexive_390 ::
  T_MedialMagma_356 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_390 v0
  = let v1 = d_isMedialMagma_376 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.MedialMagma._.setoid
d_setoid_392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MedialMagma_356 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_392 ~v0 ~v1 v2 = du_setoid_392 v2
du_setoid_392 ::
  T_MedialMagma_356 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_392 v0
  = let v1 = d_isMedialMagma_376 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v1))
-- Algebra.Bundles.MedialMagma._.sym
d_sym_394 ::
  T_MedialMagma_356 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_394 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_332
            (coe d_isMedialMagma_376 (coe v0))))
-- Algebra.Bundles.MedialMagma._.trans
d_trans_396 ::
  T_MedialMagma_356 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_396 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_332
            (coe d_isMedialMagma_376 (coe v0))))
-- Algebra.Bundles.MedialMagma._.∙-cong
d_'8729''45'cong_398 ::
  T_MedialMagma_356 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_398 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_332
         (coe d_isMedialMagma_376 (coe v0)))
-- Algebra.Bundles.MedialMagma._.∙-congʳ
d_'8729''45'cong'691'_400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MedialMagma_356 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_400 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_400 v2
du_'8729''45'cong'691'_400 ::
  T_MedialMagma_356 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_400 v0
  = let v1 = d_isMedialMagma_376 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v1))
-- Algebra.Bundles.MedialMagma._.∙-congˡ
d_'8729''45'cong'737'_402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MedialMagma_356 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_402 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_402 v2
du_'8729''45'cong'737'_402 ::
  T_MedialMagma_356 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_402 v0
  = let v1 = d_isMedialMagma_376 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_332 (coe v1))
-- Algebra.Bundles.MedialMagma.magma
d_magma_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MedialMagma_356 -> T_Magma_8
d_magma_404 ~v0 ~v1 v2 = du_magma_404 v2
du_magma_404 :: T_MedialMagma_356 -> T_Magma_8
du_magma_404 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__374 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_332
         (coe d_isMedialMagma_376 (coe v0)))
-- Algebra.Bundles.MedialMagma._.rawMagma
d_rawMagma_408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MedialMagma_356 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_408 ~v0 ~v1 v2 = du_rawMagma_408 v2
du_rawMagma_408 ::
  T_MedialMagma_356 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_408 v0 = coe du_rawMagma_52 (coe du_magma_404 (coe v0))
-- Algebra.Bundles.SemimedialMagma
d_SemimedialMagma_414 a0 a1 = ()
data T_SemimedialMagma_414
  = C_SemimedialMagma'46'constructor_7465 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360
-- Algebra.Bundles.SemimedialMagma.Carrier
d_Carrier_428 :: T_SemimedialMagma_414 -> ()
d_Carrier_428 = erased
-- Algebra.Bundles.SemimedialMagma._≈_
d__'8776'__430 :: T_SemimedialMagma_414 -> AgdaAny -> AgdaAny -> ()
d__'8776'__430 = erased
-- Algebra.Bundles.SemimedialMagma._∙_
d__'8729'__432 ::
  T_SemimedialMagma_414 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__432 v0
  = case coe v0 of
      C_SemimedialMagma'46'constructor_7465 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemimedialMagma.isSemimedialMagma
d_isSemimedialMagma_434 ::
  T_SemimedialMagma_414 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemimedialMagma_360
d_isSemimedialMagma_434 v0
  = case coe v0 of
      C_SemimedialMagma'46'constructor_7465 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemimedialMagma._.isEquivalence
d_isEquivalence_438 ::
  T_SemimedialMagma_414 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_438 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_368
         (coe d_isSemimedialMagma_434 (coe v0)))
-- Algebra.Bundles.SemimedialMagma._.isMagma
d_isMagma_440 ::
  T_SemimedialMagma_414 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_440 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_368
      (coe d_isSemimedialMagma_434 (coe v0))
-- Algebra.Bundles.SemimedialMagma._.isPartialEquivalence
d_isPartialEquivalence_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemimedialMagma_414 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_442 ~v0 ~v1 v2
  = du_isPartialEquivalence_442 v2
du_isPartialEquivalence_442 ::
  T_SemimedialMagma_414 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_442 v0
  = let v1 = d_isSemimedialMagma_434 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.SemimedialMagma._.refl
d_refl_444 :: T_SemimedialMagma_414 -> AgdaAny -> AgdaAny
d_refl_444 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_368
            (coe d_isSemimedialMagma_434 (coe v0))))
-- Algebra.Bundles.SemimedialMagma._.reflexive
d_reflexive_446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemimedialMagma_414 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_446 ~v0 ~v1 v2 = du_reflexive_446 v2
du_reflexive_446 ::
  T_SemimedialMagma_414 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_446 v0
  = let v1 = d_isSemimedialMagma_434 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.SemimedialMagma._.semiMedial
d_semiMedial_448 ::
  T_SemimedialMagma_414 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_semiMedial_448 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_semiMedial_370
      (coe d_isSemimedialMagma_434 (coe v0))
-- Algebra.Bundles.SemimedialMagma._.semimedialʳ
d_semimedial'691'_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemimedialMagma_414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_semimedial'691'_450 ~v0 ~v1 v2 = du_semimedial'691'_450 v2
du_semimedial'691'_450 ::
  T_SemimedialMagma_414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_semimedial'691'_450 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_semimedial'691'_396
      (coe d_isSemimedialMagma_434 (coe v0))
-- Algebra.Bundles.SemimedialMagma._.semimedialˡ
d_semimedial'737'_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemimedialMagma_414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_semimedial'737'_452 ~v0 ~v1 v2 = du_semimedial'737'_452 v2
du_semimedial'737'_452 ::
  T_SemimedialMagma_414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_semimedial'737'_452 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_semimedial'737'_394
      (coe d_isSemimedialMagma_434 (coe v0))
-- Algebra.Bundles.SemimedialMagma._.setoid
d_setoid_454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemimedialMagma_414 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_454 ~v0 ~v1 v2 = du_setoid_454 v2
du_setoid_454 ::
  T_SemimedialMagma_414 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_454 v0
  = let v1 = d_isSemimedialMagma_434 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v1))
-- Algebra.Bundles.SemimedialMagma._.sym
d_sym_456 ::
  T_SemimedialMagma_414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_456 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_368
            (coe d_isSemimedialMagma_434 (coe v0))))
-- Algebra.Bundles.SemimedialMagma._.trans
d_trans_458 ::
  T_SemimedialMagma_414 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_458 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_368
            (coe d_isSemimedialMagma_434 (coe v0))))
-- Algebra.Bundles.SemimedialMagma._.∙-cong
d_'8729''45'cong_460 ::
  T_SemimedialMagma_414 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_460 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_368
         (coe d_isSemimedialMagma_434 (coe v0)))
-- Algebra.Bundles.SemimedialMagma._.∙-congʳ
d_'8729''45'cong'691'_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemimedialMagma_414 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_462 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_462 v2
du_'8729''45'cong'691'_462 ::
  T_SemimedialMagma_414 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_462 v0
  = let v1 = d_isSemimedialMagma_434 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v1))
-- Algebra.Bundles.SemimedialMagma._.∙-congˡ
d_'8729''45'cong'737'_464 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemimedialMagma_414 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_464 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_464 v2
du_'8729''45'cong'737'_464 ::
  T_SemimedialMagma_414 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_464 v0
  = let v1 = d_isSemimedialMagma_434 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_368 (coe v1))
-- Algebra.Bundles.SemimedialMagma.magma
d_magma_466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemimedialMagma_414 -> T_Magma_8
d_magma_466 ~v0 ~v1 v2 = du_magma_466 v2
du_magma_466 :: T_SemimedialMagma_414 -> T_Magma_8
du_magma_466 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__432 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_368
         (coe d_isSemimedialMagma_434 (coe v0)))
-- Algebra.Bundles.SemimedialMagma._.rawMagma
d_rawMagma_470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemimedialMagma_414 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_470 ~v0 ~v1 v2 = du_rawMagma_470 v2
du_rawMagma_470 ::
  T_SemimedialMagma_414 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_470 v0 = coe du_rawMagma_52 (coe du_magma_466 (coe v0))
-- Algebra.Bundles.Semigroup
d_Semigroup_476 a0 a1 = ()
data T_Semigroup_476
  = C_Semigroup'46'constructor_8557 (AgdaAny -> AgdaAny -> AgdaAny)
                                    MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
-- Algebra.Bundles.Semigroup.Carrier
d_Carrier_490 :: T_Semigroup_476 -> ()
d_Carrier_490 = erased
-- Algebra.Bundles.Semigroup._≈_
d__'8776'__492 :: T_Semigroup_476 -> AgdaAny -> AgdaAny -> ()
d__'8776'__492 = erased
-- Algebra.Bundles.Semigroup._∙_
d__'8729'__494 :: T_Semigroup_476 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__494 v0
  = case coe v0 of
      C_Semigroup'46'constructor_8557 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Semigroup.isSemigroup
d_isSemigroup_496 ::
  T_Semigroup_476 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_496 v0
  = case coe v0 of
      C_Semigroup'46'constructor_8557 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Semigroup._.assoc
d_assoc_500 ::
  T_Semigroup_476 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_500 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe d_isSemigroup_496 (coe v0))
-- Algebra.Bundles.Semigroup._.isEquivalence
d_isEquivalence_502 ::
  T_Semigroup_476 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_502 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe d_isSemigroup_496 (coe v0)))
-- Algebra.Bundles.Semigroup._.isMagma
d_isMagma_504 ::
  T_Semigroup_476 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_504 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe d_isSemigroup_496 (coe v0))
-- Algebra.Bundles.Semigroup._.isPartialEquivalence
d_isPartialEquivalence_506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semigroup_476 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_506 ~v0 ~v1 v2
  = du_isPartialEquivalence_506 v2
du_isPartialEquivalence_506 ::
  T_Semigroup_476 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_506 v0
  = let v1 = d_isSemigroup_496 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.Semigroup._.refl
d_refl_508 :: T_Semigroup_476 -> AgdaAny -> AgdaAny
d_refl_508 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe d_isSemigroup_496 (coe v0))))
-- Algebra.Bundles.Semigroup._.reflexive
d_reflexive_510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semigroup_476 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_510 ~v0 ~v1 v2 = du_reflexive_510 v2
du_reflexive_510 ::
  T_Semigroup_476 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_510 v0
  = let v1 = d_isSemigroup_496 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.Semigroup._.setoid
d_setoid_512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semigroup_476 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_512 ~v0 ~v1 v2 = du_setoid_512 v2
du_setoid_512 ::
  T_Semigroup_476 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_512 v0
  = let v1 = d_isSemigroup_496 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
-- Algebra.Bundles.Semigroup._.sym
d_sym_514 ::
  T_Semigroup_476 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_514 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe d_isSemigroup_496 (coe v0))))
-- Algebra.Bundles.Semigroup._.trans
d_trans_516 ::
  T_Semigroup_476 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_516 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe d_isSemigroup_496 (coe v0))))
-- Algebra.Bundles.Semigroup._.∙-cong
d_'8729''45'cong_518 ::
  T_Semigroup_476 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_518 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe d_isSemigroup_496 (coe v0)))
-- Algebra.Bundles.Semigroup._.∙-congʳ
d_'8729''45'cong'691'_520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semigroup_476 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_520 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_520 v2
du_'8729''45'cong'691'_520 ::
  T_Semigroup_476 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_520 v0
  = let v1 = d_isSemigroup_496 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
-- Algebra.Bundles.Semigroup._.∙-congˡ
d_'8729''45'cong'737'_522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semigroup_476 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_522 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_522 v2
du_'8729''45'cong'737'_522 ::
  T_Semigroup_476 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_522 v0
  = let v1 = d_isSemigroup_496 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
-- Algebra.Bundles.Semigroup.magma
d_magma_524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semigroup_476 -> T_Magma_8
d_magma_524 ~v0 ~v1 v2 = du_magma_524 v2
du_magma_524 :: T_Semigroup_476 -> T_Magma_8
du_magma_524 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__494 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe d_isSemigroup_496 (coe v0)))
-- Algebra.Bundles.Semigroup._._≉_
d__'8777'__528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semigroup_476 -> AgdaAny -> AgdaAny -> ()
d__'8777'__528 = erased
-- Algebra.Bundles.Semigroup._.rawMagma
d_rawMagma_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semigroup_476 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_530 ~v0 ~v1 v2 = du_rawMagma_530 v2
du_rawMagma_530 ::
  T_Semigroup_476 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_530 v0 = coe du_rawMagma_52 (coe du_magma_524 (coe v0))
-- Algebra.Bundles.Band
d_Band_536 a0 a1 = ()
data T_Band_536
  = C_Band'46'constructor_9627 (AgdaAny -> AgdaAny -> AgdaAny)
                               MAlonzo.Code.Algebra.Structures.T_IsBand_472
-- Algebra.Bundles.Band.Carrier
d_Carrier_550 :: T_Band_536 -> ()
d_Carrier_550 = erased
-- Algebra.Bundles.Band._≈_
d__'8776'__552 :: T_Band_536 -> AgdaAny -> AgdaAny -> ()
d__'8776'__552 = erased
-- Algebra.Bundles.Band._∙_
d__'8729'__554 :: T_Band_536 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__554 v0
  = case coe v0 of
      C_Band'46'constructor_9627 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Band.isBand
d_isBand_556 ::
  T_Band_536 -> MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_556 v0
  = case coe v0 of
      C_Band'46'constructor_9627 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Band._.assoc
d_assoc_560 ::
  T_Band_536 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_560 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe d_isBand_556 (coe v0)))
-- Algebra.Bundles.Band._.idem
d_idem_562 :: T_Band_536 -> AgdaAny -> AgdaAny
d_idem_562 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_482
      (coe d_isBand_556 (coe v0))
-- Algebra.Bundles.Band._.isEquivalence
d_isEquivalence_564 ::
  T_Band_536 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_564 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe d_isBand_556 (coe v0))))
-- Algebra.Bundles.Band._.isMagma
d_isMagma_566 ::
  T_Band_536 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_566 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe d_isBand_556 (coe v0)))
-- Algebra.Bundles.Band._.isPartialEquivalence
d_isPartialEquivalence_568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Band_536 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_568 ~v0 ~v1 v2
  = du_isPartialEquivalence_568 v2
du_isPartialEquivalence_568 ::
  T_Band_536 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_568 v0
  = let v1 = d_isBand_556 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Bundles.Band._.isSemigroup
d_isSemigroup_570 ::
  T_Band_536 -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_570 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe d_isBand_556 (coe v0))
-- Algebra.Bundles.Band._.refl
d_refl_572 :: T_Band_536 -> AgdaAny -> AgdaAny
d_refl_572 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_556 (coe v0)))))
-- Algebra.Bundles.Band._.reflexive
d_reflexive_574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Band_536 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_574 ~v0 ~v1 v2 = du_reflexive_574 v2
du_reflexive_574 ::
  T_Band_536 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_574 v0
  = let v1 = d_isBand_556 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Bundles.Band._.setoid
d_setoid_576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Band_536 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_576 ~v0 ~v1 v2 = du_setoid_576 v2
du_setoid_576 ::
  T_Band_536 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_576 v0
  = let v1 = d_isBand_556 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Bundles.Band._.sym
d_sym_578 :: T_Band_536 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_578 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_556 (coe v0)))))
-- Algebra.Bundles.Band._.trans
d_trans_580 ::
  T_Band_536 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_580 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_556 (coe v0)))))
-- Algebra.Bundles.Band._.∙-cong
d_'8729''45'cong_582 ::
  T_Band_536 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_582 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe d_isBand_556 (coe v0))))
-- Algebra.Bundles.Band._.∙-congʳ
d_'8729''45'cong'691'_584 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Band_536 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_584 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_584 v2
du_'8729''45'cong'691'_584 ::
  T_Band_536 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_584 v0
  = let v1 = d_isBand_556 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Bundles.Band._.∙-congˡ
d_'8729''45'cong'737'_586 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Band_536 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_586 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_586 v2
du_'8729''45'cong'737'_586 ::
  T_Band_536 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_586 v0
  = let v1 = d_isBand_556 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Bundles.Band.semigroup
d_semigroup_588 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Band_536 -> T_Semigroup_476
d_semigroup_588 ~v0 ~v1 v2 = du_semigroup_588 v2
du_semigroup_588 :: T_Band_536 -> T_Semigroup_476
du_semigroup_588 v0
  = coe
      C_Semigroup'46'constructor_8557 (d__'8729'__554 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe d_isBand_556 (coe v0)))
-- Algebra.Bundles.Band._._≉_
d__'8777'__592 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Band_536 -> AgdaAny -> AgdaAny -> ()
d__'8777'__592 = erased
-- Algebra.Bundles.Band._.magma
d_magma_594 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Band_536 -> T_Magma_8
d_magma_594 ~v0 ~v1 v2 = du_magma_594 v2
du_magma_594 :: T_Band_536 -> T_Magma_8
du_magma_594 v0 = coe du_magma_524 (coe du_semigroup_588 (coe v0))
-- Algebra.Bundles.Band._.rawMagma
d_rawMagma_596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Band_536 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_596 ~v0 ~v1 v2 = du_rawMagma_596 v2
du_rawMagma_596 ::
  T_Band_536 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_596 v0
  = let v1 = coe du_semigroup_588 (coe v0) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v1))
-- Algebra.Bundles.CommutativeSemigroup
d_CommutativeSemigroup_602 a0 a1 = ()
data T_CommutativeSemigroup_602
  = C_CommutativeSemigroup'46'constructor_10763 (AgdaAny ->
                                                 AgdaAny -> AgdaAny)
                                                MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
-- Algebra.Bundles.CommutativeSemigroup.Carrier
d_Carrier_616 :: T_CommutativeSemigroup_602 -> ()
d_Carrier_616 = erased
-- Algebra.Bundles.CommutativeSemigroup._≈_
d__'8776'__618 ::
  T_CommutativeSemigroup_602 -> AgdaAny -> AgdaAny -> ()
d__'8776'__618 = erased
-- Algebra.Bundles.CommutativeSemigroup._∙_
d__'8729'__620 ::
  T_CommutativeSemigroup_602 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__620 v0
  = case coe v0 of
      C_CommutativeSemigroup'46'constructor_10763 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemigroup.isCommutativeSemigroup
d_isCommutativeSemigroup_622 ::
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_622 v0
  = case coe v0 of
      C_CommutativeSemigroup'46'constructor_10763 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemigroup._.assoc
d_assoc_626 ::
  T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_626 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
         (coe d_isCommutativeSemigroup_622 (coe v0)))
-- Algebra.Bundles.CommutativeSemigroup._.comm
d_comm_628 ::
  T_CommutativeSemigroup_602 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_628 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_522
      (coe d_isCommutativeSemigroup_622 (coe v0))
-- Algebra.Bundles.CommutativeSemigroup._.isCommutativeMagma
d_isCommutativeMagma_630 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_630 ~v0 ~v1 v2 = du_isCommutativeMagma_630 v2
du_isCommutativeMagma_630 ::
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_630 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe d_isCommutativeSemigroup_622 (coe v0))
-- Algebra.Bundles.CommutativeSemigroup._.isEquivalence
d_isEquivalence_632 ::
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_632 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
            (coe d_isCommutativeSemigroup_622 (coe v0))))
-- Algebra.Bundles.CommutativeSemigroup._.isMagma
d_isMagma_634 ::
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_634 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
         (coe d_isCommutativeSemigroup_622 (coe v0)))
-- Algebra.Bundles.CommutativeSemigroup._.isPartialEquivalence
d_isPartialEquivalence_636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_636 ~v0 ~v1 v2
  = du_isPartialEquivalence_636 v2
du_isPartialEquivalence_636 ::
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_636 v0
  = let v1 = d_isCommutativeSemigroup_622 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Bundles.CommutativeSemigroup._.isSemigroup
d_isSemigroup_638 ::
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_638 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
      (coe d_isCommutativeSemigroup_622 (coe v0))
-- Algebra.Bundles.CommutativeSemigroup._.refl
d_refl_640 :: T_CommutativeSemigroup_602 -> AgdaAny -> AgdaAny
d_refl_640 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe d_isCommutativeSemigroup_622 (coe v0)))))
-- Algebra.Bundles.CommutativeSemigroup._.reflexive
d_reflexive_642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_642 ~v0 ~v1 v2 = du_reflexive_642 v2
du_reflexive_642 ::
  T_CommutativeSemigroup_602 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_642 v0
  = let v1 = d_isCommutativeSemigroup_622 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Bundles.CommutativeSemigroup._.setoid
d_setoid_644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_644 ~v0 ~v1 v2 = du_setoid_644 v2
du_setoid_644 ::
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_644 v0
  = let v1 = d_isCommutativeSemigroup_622 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Bundles.CommutativeSemigroup._.sym
d_sym_646 ::
  T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_646 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe d_isCommutativeSemigroup_622 (coe v0)))))
-- Algebra.Bundles.CommutativeSemigroup._.trans
d_trans_648 ::
  T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_648 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
               (coe d_isCommutativeSemigroup_622 (coe v0)))))
-- Algebra.Bundles.CommutativeSemigroup._.∙-cong
d_'8729''45'cong_650 ::
  T_CommutativeSemigroup_602 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_650 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
            (coe d_isCommutativeSemigroup_622 (coe v0))))
-- Algebra.Bundles.CommutativeSemigroup._.∙-congʳ
d_'8729''45'cong'691'_652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_652 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_652 v2
du_'8729''45'cong'691'_652 ::
  T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_652 v0
  = let v1 = d_isCommutativeSemigroup_622 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Bundles.CommutativeSemigroup._.∙-congˡ
d_'8729''45'cong'737'_654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_654 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_654 v2
du_'8729''45'cong'737'_654 ::
  T_CommutativeSemigroup_602 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_654 v0
  = let v1 = d_isCommutativeSemigroup_622 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_520 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Bundles.CommutativeSemigroup.semigroup
d_semigroup_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 -> T_Semigroup_476
d_semigroup_656 ~v0 ~v1 v2 = du_semigroup_656 v2
du_semigroup_656 :: T_CommutativeSemigroup_602 -> T_Semigroup_476
du_semigroup_656 v0
  = coe
      C_Semigroup'46'constructor_8557 (d__'8729'__620 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isSemigroup_520
         (coe d_isCommutativeSemigroup_622 (coe v0)))
-- Algebra.Bundles.CommutativeSemigroup._._≉_
d__'8777'__660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 -> AgdaAny -> AgdaAny -> ()
d__'8777'__660 = erased
-- Algebra.Bundles.CommutativeSemigroup._.magma
d_magma_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 -> T_Magma_8
d_magma_662 ~v0 ~v1 v2 = du_magma_662 v2
du_magma_662 :: T_CommutativeSemigroup_602 -> T_Magma_8
du_magma_662 v0 = coe du_magma_524 (coe du_semigroup_656 (coe v0))
-- Algebra.Bundles.CommutativeSemigroup._.rawMagma
d_rawMagma_664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_664 ~v0 ~v1 v2 = du_rawMagma_664 v2
du_rawMagma_664 ::
  T_CommutativeSemigroup_602 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_664 v0
  = let v1 = coe du_semigroup_656 (coe v0) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v1))
-- Algebra.Bundles.CommutativeSemigroup.commutativeMagma
d_commutativeMagma_666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemigroup_602 -> T_CommutativeMagma_120
d_commutativeMagma_666 ~v0 ~v1 v2 = du_commutativeMagma_666 v2
du_commutativeMagma_666 ::
  T_CommutativeSemigroup_602 -> T_CommutativeMagma_120
du_commutativeMagma_666 v0
  = coe
      C_CommutativeMagma'46'constructor_2217 (d__'8729'__620 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
         (coe d_isCommutativeSemigroup_622 (coe v0)))
-- Algebra.Bundles.UnitalMagma
d_UnitalMagma_672 a0 a1 = ()
data T_UnitalMagma_672
  = C_UnitalMagma'46'constructor_12107 (AgdaAny ->
                                        AgdaAny -> AgdaAny)
                                       AgdaAny MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
-- Algebra.Bundles.UnitalMagma.Carrier
d_Carrier_688 :: T_UnitalMagma_672 -> ()
d_Carrier_688 = erased
-- Algebra.Bundles.UnitalMagma._≈_
d__'8776'__690 :: T_UnitalMagma_672 -> AgdaAny -> AgdaAny -> ()
d__'8776'__690 = erased
-- Algebra.Bundles.UnitalMagma._∙_
d__'8729'__692 ::
  T_UnitalMagma_672 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__692 v0
  = case coe v0 of
      C_UnitalMagma'46'constructor_12107 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.UnitalMagma.ε
d_ε_694 :: T_UnitalMagma_672 -> AgdaAny
d_ε_694 v0
  = case coe v0 of
      C_UnitalMagma'46'constructor_12107 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.UnitalMagma.isUnitalMagma
d_isUnitalMagma_696 ::
  T_UnitalMagma_672 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_696 v0
  = case coe v0 of
      C_UnitalMagma'46'constructor_12107 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.UnitalMagma._.identity
d_identity_700 ::
  T_UnitalMagma_672 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_700 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_568
      (coe d_isUnitalMagma_696 (coe v0))
-- Algebra.Bundles.UnitalMagma._.identityʳ
d_identity'691'_702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 -> AgdaAny -> AgdaAny
d_identity'691'_702 ~v0 ~v1 v2 = du_identity'691'_702 v2
du_identity'691'_702 :: T_UnitalMagma_672 -> AgdaAny -> AgdaAny
du_identity'691'_702 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_594
      (coe d_isUnitalMagma_696 (coe v0))
-- Algebra.Bundles.UnitalMagma._.identityˡ
d_identity'737'_704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 -> AgdaAny -> AgdaAny
d_identity'737'_704 ~v0 ~v1 v2 = du_identity'737'_704 v2
du_identity'737'_704 :: T_UnitalMagma_672 -> AgdaAny -> AgdaAny
du_identity'737'_704 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_592
      (coe d_isUnitalMagma_696 (coe v0))
-- Algebra.Bundles.UnitalMagma._.isEquivalence
d_isEquivalence_706 ::
  T_UnitalMagma_672 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_706 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_566
         (coe d_isUnitalMagma_696 (coe v0)))
-- Algebra.Bundles.UnitalMagma._.isMagma
d_isMagma_708 ::
  T_UnitalMagma_672 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_708 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_566
      (coe d_isUnitalMagma_696 (coe v0))
-- Algebra.Bundles.UnitalMagma._.isPartialEquivalence
d_isPartialEquivalence_710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_710 ~v0 ~v1 v2
  = du_isPartialEquivalence_710 v2
du_isPartialEquivalence_710 ::
  T_UnitalMagma_672 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_710 v0
  = let v1 = d_isUnitalMagma_696 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.UnitalMagma._.refl
d_refl_712 :: T_UnitalMagma_672 -> AgdaAny -> AgdaAny
d_refl_712 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_566
            (coe d_isUnitalMagma_696 (coe v0))))
-- Algebra.Bundles.UnitalMagma._.reflexive
d_reflexive_714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_714 ~v0 ~v1 v2 = du_reflexive_714 v2
du_reflexive_714 ::
  T_UnitalMagma_672 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_714 v0
  = let v1 = d_isUnitalMagma_696 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.UnitalMagma._.setoid
d_setoid_716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_716 ~v0 ~v1 v2 = du_setoid_716 v2
du_setoid_716 ::
  T_UnitalMagma_672 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_716 v0
  = let v1 = d_isUnitalMagma_696 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v1))
-- Algebra.Bundles.UnitalMagma._.sym
d_sym_718 ::
  T_UnitalMagma_672 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_718 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_566
            (coe d_isUnitalMagma_696 (coe v0))))
-- Algebra.Bundles.UnitalMagma._.trans
d_trans_720 ::
  T_UnitalMagma_672 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_720 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_566
            (coe d_isUnitalMagma_696 (coe v0))))
-- Algebra.Bundles.UnitalMagma._.∙-cong
d_'8729''45'cong_722 ::
  T_UnitalMagma_672 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_722 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_566
         (coe d_isUnitalMagma_696 (coe v0)))
-- Algebra.Bundles.UnitalMagma._.∙-congʳ
d_'8729''45'cong'691'_724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_724 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_724 v2
du_'8729''45'cong'691'_724 ::
  T_UnitalMagma_672 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_724 v0
  = let v1 = d_isUnitalMagma_696 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v1))
-- Algebra.Bundles.UnitalMagma._.∙-congˡ
d_'8729''45'cong'737'_726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_726 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_726 v2
du_'8729''45'cong'737'_726 ::
  T_UnitalMagma_672 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_726 v0
  = let v1 = d_isUnitalMagma_696 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_566 (coe v1))
-- Algebra.Bundles.UnitalMagma.magma
d_magma_728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 -> T_Magma_8
d_magma_728 ~v0 ~v1 v2 = du_magma_728 v2
du_magma_728 :: T_UnitalMagma_672 -> T_Magma_8
du_magma_728 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__692 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_566
         (coe d_isUnitalMagma_696 (coe v0)))
-- Algebra.Bundles.UnitalMagma._._≉_
d__'8777'__732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 -> AgdaAny -> AgdaAny -> ()
d__'8777'__732 = erased
-- Algebra.Bundles.UnitalMagma._.rawMagma
d_rawMagma_734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_UnitalMagma_672 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_734 ~v0 ~v1 v2 = du_rawMagma_734 v2
du_rawMagma_734 ::
  T_UnitalMagma_672 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_734 v0 = coe du_rawMagma_52 (coe du_magma_728 (coe v0))
-- Algebra.Bundles.Monoid
d_Monoid_740 a0 a1 = ()
data T_Monoid_740
  = C_Monoid'46'constructor_13309 (AgdaAny -> AgdaAny -> AgdaAny)
                                  AgdaAny MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
-- Algebra.Bundles.Monoid.Carrier
d_Carrier_756 :: T_Monoid_740 -> ()
d_Carrier_756 = erased
-- Algebra.Bundles.Monoid._≈_
d__'8776'__758 :: T_Monoid_740 -> AgdaAny -> AgdaAny -> ()
d__'8776'__758 = erased
-- Algebra.Bundles.Monoid._∙_
d__'8729'__760 :: T_Monoid_740 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__760 v0
  = case coe v0 of
      C_Monoid'46'constructor_13309 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Monoid.ε
d_ε_762 :: T_Monoid_740 -> AgdaAny
d_ε_762 v0
  = case coe v0 of
      C_Monoid'46'constructor_13309 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Monoid.isMonoid
d_isMonoid_764 ::
  T_Monoid_740 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_764 v0
  = case coe v0 of
      C_Monoid'46'constructor_13309 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Monoid._.assoc
d_assoc_768 ::
  T_Monoid_740 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_768 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe d_isMonoid_764 (coe v0)))
-- Algebra.Bundles.Monoid._.identity
d_identity_770 ::
  T_Monoid_740 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_770 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe d_isMonoid_764 (coe v0))
-- Algebra.Bundles.Monoid._.identityʳ
d_identity'691'_772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> AgdaAny -> AgdaAny
d_identity'691'_772 ~v0 ~v1 v2 = du_identity'691'_772 v2
du_identity'691'_772 :: T_Monoid_740 -> AgdaAny -> AgdaAny
du_identity'691'_772 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe d_isMonoid_764 (coe v0))
-- Algebra.Bundles.Monoid._.identityˡ
d_identity'737'_774 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> AgdaAny -> AgdaAny
d_identity'737'_774 ~v0 ~v1 v2 = du_identity'737'_774 v2
du_identity'737'_774 :: T_Monoid_740 -> AgdaAny -> AgdaAny
du_identity'737'_774 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe d_isMonoid_764 (coe v0))
-- Algebra.Bundles.Monoid._.isEquivalence
d_isEquivalence_776 ::
  T_Monoid_740 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_776 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe d_isMonoid_764 (coe v0))))
-- Algebra.Bundles.Monoid._.isMagma
d_isMagma_778 ::
  T_Monoid_740 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_778 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe d_isMonoid_764 (coe v0)))
-- Algebra.Bundles.Monoid._.isPartialEquivalence
d_isPartialEquivalence_780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_780 ~v0 ~v1 v2
  = du_isPartialEquivalence_780 v2
du_isPartialEquivalence_780 ::
  T_Monoid_740 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_780 v0
  = let v1 = d_isMonoid_764 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Bundles.Monoid._.isSemigroup
d_isSemigroup_782 ::
  T_Monoid_740 -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_782 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe d_isMonoid_764 (coe v0))
-- Algebra.Bundles.Monoid._.isUnitalMagma
d_isUnitalMagma_784 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_784 ~v0 ~v1 v2 = du_isUnitalMagma_784 v2
du_isUnitalMagma_784 ::
  T_Monoid_740 -> MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_784 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe d_isMonoid_764 (coe v0))
-- Algebra.Bundles.Monoid._.refl
d_refl_786 :: T_Monoid_740 -> AgdaAny -> AgdaAny
d_refl_786 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_isMonoid_764 (coe v0)))))
-- Algebra.Bundles.Monoid._.reflexive
d_reflexive_788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_788 ~v0 ~v1 v2 = du_reflexive_788 v2
du_reflexive_788 ::
  T_Monoid_740 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_788 v0
  = let v1 = d_isMonoid_764 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Bundles.Monoid._.setoid
d_setoid_790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_790 ~v0 ~v1 v2 = du_setoid_790 v2
du_setoid_790 ::
  T_Monoid_740 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_790 v0
  = let v1 = d_isMonoid_764 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Bundles.Monoid._.sym
d_sym_792 ::
  T_Monoid_740 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_792 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_isMonoid_764 (coe v0)))))
-- Algebra.Bundles.Monoid._.trans
d_trans_794 ::
  T_Monoid_740 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_794 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_isMonoid_764 (coe v0)))))
-- Algebra.Bundles.Monoid._.∙-cong
d_'8729''45'cong_796 ::
  T_Monoid_740 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_796 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe d_isMonoid_764 (coe v0))))
-- Algebra.Bundles.Monoid._.∙-congʳ
d_'8729''45'cong'691'_798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_798 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_798 v2
du_'8729''45'cong'691'_798 ::
  T_Monoid_740 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_798 v0
  = let v1 = d_isMonoid_764 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Bundles.Monoid._.∙-congˡ
d_'8729''45'cong'737'_800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_800 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_800 v2
du_'8729''45'cong'737'_800 ::
  T_Monoid_740 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_800 v0
  = let v1 = d_isMonoid_764 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Bundles.Monoid.semigroup
d_semigroup_802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> T_Semigroup_476
d_semigroup_802 ~v0 ~v1 v2 = du_semigroup_802 v2
du_semigroup_802 :: T_Monoid_740 -> T_Semigroup_476
du_semigroup_802 v0
  = coe
      C_Semigroup'46'constructor_8557 (d__'8729'__760 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe d_isMonoid_764 (coe v0)))
-- Algebra.Bundles.Monoid._._≉_
d__'8777'__806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> AgdaAny -> AgdaAny -> ()
d__'8777'__806 = erased
-- Algebra.Bundles.Monoid._.magma
d_magma_808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Monoid_740 -> T_Magma_8
d_magma_808 ~v0 ~v1 v2 = du_magma_808 v2
du_magma_808 :: T_Monoid_740 -> T_Magma_8
du_magma_808 v0 = coe du_magma_524 (coe du_semigroup_802 (coe v0))
-- Algebra.Bundles.Monoid._.rawMagma
d_rawMagma_810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_810 ~v0 ~v1 v2 = du_rawMagma_810 v2
du_rawMagma_810 ::
  T_Monoid_740 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_810 v0
  = let v1 = coe du_semigroup_802 (coe v0) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v1))
-- Algebra.Bundles.Monoid.rawMonoid
d_rawMonoid_812 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_812 ~v0 ~v1 v2 = du_rawMonoid_812 v2
du_rawMonoid_812 ::
  T_Monoid_740 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_812 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMonoid'46'constructor_473
      (d__'8729'__760 (coe v0)) (d_ε_762 (coe v0))
-- Algebra.Bundles.Monoid.unitalMagma
d_unitalMagma_814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Monoid_740 -> T_UnitalMagma_672
d_unitalMagma_814 ~v0 ~v1 v2 = du_unitalMagma_814 v2
du_unitalMagma_814 :: T_Monoid_740 -> T_UnitalMagma_672
du_unitalMagma_814 v0
  = coe
      C_UnitalMagma'46'constructor_12107 (d__'8729'__760 (coe v0))
      (d_ε_762 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
         (coe d_isMonoid_764 (coe v0)))
-- Algebra.Bundles.CommutativeMonoid
d_CommutativeMonoid_820 a0 a1 = ()
data T_CommutativeMonoid_820
  = C_CommutativeMonoid'46'constructor_15055 (AgdaAny ->
                                              AgdaAny -> AgdaAny)
                                             AgdaAny
                                             MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
-- Algebra.Bundles.CommutativeMonoid.Carrier
d_Carrier_836 :: T_CommutativeMonoid_820 -> ()
d_Carrier_836 = erased
-- Algebra.Bundles.CommutativeMonoid._≈_
d__'8776'__838 ::
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny -> ()
d__'8776'__838 = erased
-- Algebra.Bundles.CommutativeMonoid._∙_
d__'8729'__840 ::
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__840 v0
  = case coe v0 of
      C_CommutativeMonoid'46'constructor_15055 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeMonoid.ε
d_ε_842 :: T_CommutativeMonoid_820 -> AgdaAny
d_ε_842 v0
  = case coe v0 of
      C_CommutativeMonoid'46'constructor_15055 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeMonoid.isCommutativeMonoid
d_isCommutativeMonoid_844 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_844 v0
  = case coe v0 of
      C_CommutativeMonoid'46'constructor_15055 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeMonoid._.assoc
d_assoc_848 ::
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_848 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe d_isCommutativeMonoid_844 (coe v0))))
-- Algebra.Bundles.CommutativeMonoid._.comm
d_comm_850 ::
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_850 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe d_isCommutativeMonoid_844 (coe v0))
-- Algebra.Bundles.CommutativeMonoid._.identity
d_identity_852 ::
  T_CommutativeMonoid_820 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_852 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe d_isCommutativeMonoid_844 (coe v0)))
-- Algebra.Bundles.CommutativeMonoid._.identityʳ
d_identity'691'_854 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny
d_identity'691'_854 ~v0 ~v1 v2 = du_identity'691'_854 v2
du_identity'691'_854 ::
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny
du_identity'691'_854 v0
  = let v1 = d_isCommutativeMonoid_844 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Bundles.CommutativeMonoid._.identityˡ
d_identity'737'_856 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny
d_identity'737'_856 ~v0 ~v1 v2 = du_identity'737'_856 v2
du_identity'737'_856 ::
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny
du_identity'737'_856 v0
  = let v1 = d_isCommutativeMonoid_844 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Bundles.CommutativeMonoid._.isCommutativeMagma
d_isCommutativeMagma_858 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_858 ~v0 ~v1 v2 = du_isCommutativeMagma_858 v2
du_isCommutativeMagma_858 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_858 v0
  = let v1 = d_isCommutativeMonoid_844 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v1))
-- Algebra.Bundles.CommutativeMonoid._.isCommutativeSemigroup
d_isCommutativeSemigroup_860 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_860 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_860 v2
du_isCommutativeSemigroup_860 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_860 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe d_isCommutativeMonoid_844 (coe v0))
-- Algebra.Bundles.CommutativeMonoid._.isEquivalence
d_isEquivalence_862 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_862 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe d_isCommutativeMonoid_844 (coe v0)))))
-- Algebra.Bundles.CommutativeMonoid._.isMagma
d_isMagma_864 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_864 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe d_isCommutativeMonoid_844 (coe v0))))
-- Algebra.Bundles.CommutativeMonoid._.isMonoid
d_isMonoid_866 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_866 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe d_isCommutativeMonoid_844 (coe v0))
-- Algebra.Bundles.CommutativeMonoid._.isPartialEquivalence
d_isPartialEquivalence_868 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_868 ~v0 ~v1 v2
  = du_isPartialEquivalence_868 v2
du_isPartialEquivalence_868 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_868 v0
  = let v1 = d_isCommutativeMonoid_844 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Bundles.CommutativeMonoid._.isSemigroup
d_isSemigroup_870 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_870 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe d_isCommutativeMonoid_844 (coe v0)))
-- Algebra.Bundles.CommutativeMonoid._.isUnitalMagma
d_isUnitalMagma_872 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_872 ~v0 ~v1 v2 = du_isUnitalMagma_872 v2
du_isUnitalMagma_872 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_872 v0
  = let v1 = d_isCommutativeMonoid_844 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Bundles.CommutativeMonoid._.refl
d_refl_874 :: T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny
d_refl_874 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe d_isCommutativeMonoid_844 (coe v0))))))
-- Algebra.Bundles.CommutativeMonoid._.reflexive
d_reflexive_876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_876 ~v0 ~v1 v2 = du_reflexive_876 v2
du_reflexive_876 ::
  T_CommutativeMonoid_820 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_876 v0
  = let v1 = d_isCommutativeMonoid_844 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Bundles.CommutativeMonoid._.setoid
d_setoid_878 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_878 ~v0 ~v1 v2 = du_setoid_878 v2
du_setoid_878 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_878 v0
  = let v1 = d_isCommutativeMonoid_844 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.CommutativeMonoid._.sym
d_sym_880 ::
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_880 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe d_isCommutativeMonoid_844 (coe v0))))))
-- Algebra.Bundles.CommutativeMonoid._.trans
d_trans_882 ::
  T_CommutativeMonoid_820 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_882 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe d_isCommutativeMonoid_844 (coe v0))))))
-- Algebra.Bundles.CommutativeMonoid._.∙-cong
d_'8729''45'cong_884 ::
  T_CommutativeMonoid_820 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_884 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe d_isCommutativeMonoid_844 (coe v0)))))
-- Algebra.Bundles.CommutativeMonoid._.∙-congʳ
d_'8729''45'cong'691'_886 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_886 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_886 v2
du_'8729''45'cong'691'_886 ::
  T_CommutativeMonoid_820 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_886 v0
  = let v1 = d_isCommutativeMonoid_844 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.CommutativeMonoid._.∙-congˡ
d_'8729''45'cong'737'_888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_888 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_888 v2
du_'8729''45'cong'737'_888 ::
  T_CommutativeMonoid_820 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_888 v0
  = let v1 = d_isCommutativeMonoid_844 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.CommutativeMonoid.monoid
d_monoid_890 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 -> T_Monoid_740
d_monoid_890 ~v0 ~v1 v2 = du_monoid_890 v2
du_monoid_890 :: T_CommutativeMonoid_820 -> T_Monoid_740
du_monoid_890 v0
  = coe
      C_Monoid'46'constructor_13309 (d__'8729'__840 (coe v0))
      (d_ε_842 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe d_isCommutativeMonoid_844 (coe v0)))
-- Algebra.Bundles.CommutativeMonoid._._≉_
d__'8777'__894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 -> AgdaAny -> AgdaAny -> ()
d__'8777'__894 = erased
-- Algebra.Bundles.CommutativeMonoid._.magma
d_magma_896 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 -> T_Magma_8
d_magma_896 ~v0 ~v1 v2 = du_magma_896 v2
du_magma_896 :: T_CommutativeMonoid_820 -> T_Magma_8
du_magma_896 v0
  = let v1 = coe du_monoid_890 (coe v0) in
    coe du_magma_524 (coe du_semigroup_802 (coe v1))
-- Algebra.Bundles.CommutativeMonoid._.rawMagma
d_rawMagma_898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_898 ~v0 ~v1 v2 = du_rawMagma_898 v2
du_rawMagma_898 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_898 v0
  = let v1 = coe du_monoid_890 (coe v0) in
    let v2 = coe du_semigroup_802 (coe v1) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v2))
-- Algebra.Bundles.CommutativeMonoid._.rawMonoid
d_rawMonoid_900 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_900 ~v0 ~v1 v2 = du_rawMonoid_900 v2
du_rawMonoid_900 ::
  T_CommutativeMonoid_820 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_900 v0
  = coe du_rawMonoid_812 (coe du_monoid_890 (coe v0))
-- Algebra.Bundles.CommutativeMonoid._.semigroup
d_semigroup_902 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 -> T_Semigroup_476
d_semigroup_902 ~v0 ~v1 v2 = du_semigroup_902 v2
du_semigroup_902 :: T_CommutativeMonoid_820 -> T_Semigroup_476
du_semigroup_902 v0
  = coe du_semigroup_802 (coe du_monoid_890 (coe v0))
-- Algebra.Bundles.CommutativeMonoid._.unitalMagma
d_unitalMagma_904 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 -> T_UnitalMagma_672
d_unitalMagma_904 ~v0 ~v1 v2 = du_unitalMagma_904 v2
du_unitalMagma_904 :: T_CommutativeMonoid_820 -> T_UnitalMagma_672
du_unitalMagma_904 v0
  = coe du_unitalMagma_814 (coe du_monoid_890 (coe v0))
-- Algebra.Bundles.CommutativeMonoid.commutativeSemigroup
d_commutativeSemigroup_906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_906 ~v0 ~v1 v2
  = du_commutativeSemigroup_906 v2
du_commutativeSemigroup_906 ::
  T_CommutativeMonoid_820 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_906 v0
  = coe
      C_CommutativeSemigroup'46'constructor_10763
      (d__'8729'__840 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe d_isCommutativeMonoid_844 (coe v0)))
-- Algebra.Bundles.CommutativeMonoid._.commutativeMagma
d_commutativeMagma_910 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeMonoid_820 -> T_CommutativeMagma_120
d_commutativeMagma_910 ~v0 ~v1 v2 = du_commutativeMagma_910 v2
du_commutativeMagma_910 ::
  T_CommutativeMonoid_820 -> T_CommutativeMagma_120
du_commutativeMagma_910 v0
  = coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v0))
-- Algebra.Bundles.IdempotentCommutativeMonoid
d_IdempotentCommutativeMonoid_916 a0 a1 = ()
data T_IdempotentCommutativeMonoid_916
  = C_IdempotentCommutativeMonoid'46'constructor_16849 (AgdaAny ->
                                                        AgdaAny -> AgdaAny)
                                                       AgdaAny
                                                       MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
-- Algebra.Bundles.IdempotentCommutativeMonoid.Carrier
d_Carrier_932 :: T_IdempotentCommutativeMonoid_916 -> ()
d_Carrier_932 = erased
-- Algebra.Bundles.IdempotentCommutativeMonoid._≈_
d__'8776'__934 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny -> ()
d__'8776'__934 = erased
-- Algebra.Bundles.IdempotentCommutativeMonoid._∙_
d__'8729'__936 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__936 v0
  = case coe v0 of
      C_IdempotentCommutativeMonoid'46'constructor_16849 v3 v4 v5
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentCommutativeMonoid.ε
d_ε_938 :: T_IdempotentCommutativeMonoid_916 -> AgdaAny
d_ε_938 v0
  = case coe v0 of
      C_IdempotentCommutativeMonoid'46'constructor_16849 v3 v4 v5
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentCommutativeMonoid.isIdempotentCommutativeMonoid
d_isIdempotentCommutativeMonoid_940 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
d_isIdempotentCommutativeMonoid_940 v0
  = case coe v0 of
      C_IdempotentCommutativeMonoid'46'constructor_16849 v3 v4 v5
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentCommutativeMonoid._.assoc
d_assoc_944 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_944 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.comm
d_comm_946 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_946 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
         (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.idem
d_idem_948 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
d_idem_948 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_722
      (coe d_isIdempotentCommutativeMonoid_940 (coe v0))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.identity
d_identity_950 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_950 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe d_isIdempotentCommutativeMonoid_940 (coe v0))))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.identityʳ
d_identity'691'_952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
d_identity'691'_952 ~v0 ~v1 v2 = du_identity'691'_952 v2
du_identity'691'_952 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
du_identity'691'_952 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.identityˡ
d_identity'737'_954 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
d_identity'737'_954 ~v0 ~v1 v2 = du_identity'737'_954 v2
du_identity'737'_954 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
du_identity'737'_954 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isBand
d_isBand_956 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_956 ~v0 ~v1 v2 = du_isBand_956 v2
du_isBand_956 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_956 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isBand_768
      (coe d_isIdempotentCommutativeMonoid_940 (coe v0))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isCommutativeMagma
d_isCommutativeMagma_958 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_958 ~v0 ~v1 v2 = du_isCommutativeMagma_958 v2
du_isCommutativeMagma_958 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_958 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v2))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isCommutativeMonoid
d_isCommutativeMonoid_960 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_960 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
      (coe d_isIdempotentCommutativeMonoid_940 (coe v0))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isCommutativeSemigroup
d_isCommutativeSemigroup_962 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_962 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_962 v2
du_isCommutativeSemigroup_962 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_962 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v1))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isEquivalence
d_isEquivalence_964 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_964 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe d_isIdempotentCommutativeMonoid_940 (coe v0))))))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isMagma
d_isMagma_966 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_966 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isMonoid
d_isMonoid_968 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_968 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
         (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isPartialEquivalence
d_isPartialEquivalence_970 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_970 ~v0 ~v1 v2
  = du_isPartialEquivalence_970 v2
du_isPartialEquivalence_970 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_970 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isSemigroup
d_isSemigroup_972 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_972 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe d_isIdempotentCommutativeMonoid_940 (coe v0))))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.isUnitalMagma
d_isUnitalMagma_974 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_974 ~v0 ~v1 v2 = du_isUnitalMagma_974 v2
du_isUnitalMagma_974 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_974 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.refl
d_refl_976 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
d_refl_976 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.reflexive
d_reflexive_978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_978 ~v0 ~v1 v2 = du_reflexive_978 v2
du_reflexive_978 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_978 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Bundles.IdempotentCommutativeMonoid._.setoid
d_setoid_980 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_980 ~v0 ~v1 v2 = du_setoid_980 v2
du_setoid_980 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_980 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.sym
d_sym_982 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_982 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.trans
d_trans_984 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_984 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.∙-cong
d_'8729''45'cong_986 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_986 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe d_isIdempotentCommutativeMonoid_940 (coe v0))))))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.∙-congʳ
d_'8729''45'cong'691'_988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_988 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_988 v2
du_'8729''45'cong'691'_988 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_988 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.∙-congˡ
d_'8729''45'cong'737'_990 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_990 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_990 v2
du_'8729''45'cong'737'_990 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_990 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.IdempotentCommutativeMonoid.commutativeMonoid
d_commutativeMonoid_992 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeMonoid_820
d_commutativeMonoid_992 ~v0 ~v1 v2 = du_commutativeMonoid_992 v2
du_commutativeMonoid_992 ::
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeMonoid_820
du_commutativeMonoid_992 v0
  = coe
      C_CommutativeMonoid'46'constructor_15055 (d__'8729'__936 (coe v0))
      (d_ε_938 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
         (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))
-- Algebra.Bundles.IdempotentCommutativeMonoid._._≉_
d__'8777'__996 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny -> ()
d__'8777'__996 = erased
-- Algebra.Bundles.IdempotentCommutativeMonoid._.commutativeMagma
d_commutativeMagma_998 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeMagma_120
d_commutativeMagma_998 ~v0 ~v1 v2 = du_commutativeMagma_998 v2
du_commutativeMagma_998 ::
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeMagma_120
du_commutativeMagma_998 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v1))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.commutativeSemigroup
d_commutativeSemigroup_1000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_1000 ~v0 ~v1 v2
  = du_commutativeSemigroup_1000 v2
du_commutativeSemigroup_1000 ::
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_1000 v0
  = coe
      du_commutativeSemigroup_906 (coe du_commutativeMonoid_992 (coe v0))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.magma
d_magma_1002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_Magma_8
d_magma_1002 ~v0 ~v1 v2 = du_magma_1002 v2
du_magma_1002 :: T_IdempotentCommutativeMonoid_916 -> T_Magma_8
du_magma_1002 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    let v2 = coe du_monoid_890 (coe v1) in
    coe du_magma_524 (coe du_semigroup_802 (coe v2))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.monoid
d_monoid_1004 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_Monoid_740
d_monoid_1004 ~v0 ~v1 v2 = du_monoid_1004 v2
du_monoid_1004 :: T_IdempotentCommutativeMonoid_916 -> T_Monoid_740
du_monoid_1004 v0
  = coe du_monoid_890 (coe du_commutativeMonoid_992 (coe v0))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.rawMagma
d_rawMagma_1006 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1006 ~v0 ~v1 v2 = du_rawMagma_1006 v2
du_rawMagma_1006 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1006 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    let v2 = coe du_monoid_890 (coe v1) in
    let v3 = coe du_semigroup_802 (coe v2) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v3))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.rawMonoid
d_rawMonoid_1008 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_1008 ~v0 ~v1 v2 = du_rawMonoid_1008 v2
du_rawMonoid_1008 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_1008 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v1))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.semigroup
d_semigroup_1010 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_Semigroup_476
d_semigroup_1010 ~v0 ~v1 v2 = du_semigroup_1010 v2
du_semigroup_1010 ::
  T_IdempotentCommutativeMonoid_916 -> T_Semigroup_476
du_semigroup_1010 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v1))
-- Algebra.Bundles.IdempotentCommutativeMonoid._.unitalMagma
d_unitalMagma_1012 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_UnitalMagma_672
d_unitalMagma_1012 ~v0 ~v1 v2 = du_unitalMagma_1012 v2
du_unitalMagma_1012 ::
  T_IdempotentCommutativeMonoid_916 -> T_UnitalMagma_672
du_unitalMagma_1012 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v1))
-- Algebra.Bundles.BoundedLattice
d_BoundedLattice_1014 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> ()
d_BoundedLattice_1014 = erased
-- Algebra.Bundles.BoundedLattice._∙_
d__'8729'__1024 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__1024 v0 = coe d__'8729'__936 (coe v0)
-- Algebra.Bundles.BoundedLattice._≈_
d__'8776'__1026 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1026 = erased
-- Algebra.Bundles.BoundedLattice._≉_
d__'8777'__1028 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1028 = erased
-- Algebra.Bundles.BoundedLattice.Carrier
d_Carrier_1030 :: T_IdempotentCommutativeMonoid_916 -> ()
d_Carrier_1030 = erased
-- Algebra.Bundles.BoundedLattice.assoc
d_assoc_1032 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1032 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))
-- Algebra.Bundles.BoundedLattice.comm
d_comm_1034 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1034 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
         (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))
-- Algebra.Bundles.BoundedLattice.commutativeMagma
d_commutativeMagma_1036 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeMagma_120
d_commutativeMagma_1036 ~v0 ~v1 v2 = du_commutativeMagma_1036 v2
du_commutativeMagma_1036 ::
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeMagma_120
du_commutativeMagma_1036 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v1))
-- Algebra.Bundles.BoundedLattice.commutativeMonoid
d_commutativeMonoid_1038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeMonoid_820
d_commutativeMonoid_1038 ~v0 ~v1 v2 = du_commutativeMonoid_1038 v2
du_commutativeMonoid_1038 ::
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeMonoid_820
du_commutativeMonoid_1038 v0
  = coe du_commutativeMonoid_992 (coe v0)
-- Algebra.Bundles.BoundedLattice.commutativeSemigroup
d_commutativeSemigroup_1040 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_1040 ~v0 ~v1 v2
  = du_commutativeSemigroup_1040 v2
du_commutativeSemigroup_1040 ::
  T_IdempotentCommutativeMonoid_916 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_1040 v0
  = coe
      du_commutativeSemigroup_906 (coe du_commutativeMonoid_992 (coe v0))
-- Algebra.Bundles.BoundedLattice.idem
d_idem_1042 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
d_idem_1042 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_722
      (coe d_isIdempotentCommutativeMonoid_940 (coe v0))
-- Algebra.Bundles.BoundedLattice.identity
d_identity_1044 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1044 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe d_isIdempotentCommutativeMonoid_940 (coe v0))))
-- Algebra.Bundles.BoundedLattice.identityʳ
d_identity'691'_1046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
d_identity'691'_1046 ~v0 ~v1 v2 = du_identity'691'_1046 v2
du_identity'691'_1046 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
du_identity'691'_1046 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Bundles.BoundedLattice.identityˡ
d_identity'737'_1048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
d_identity'737'_1048 ~v0 ~v1 v2 = du_identity'737'_1048 v2
du_identity'737'_1048 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
du_identity'737'_1048 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Bundles.BoundedLattice.isBand
d_isBand_1050 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_1050 ~v0 ~v1 v2 = du_isBand_1050 v2
du_isBand_1050 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_1050 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isBand_768
      (coe d_isIdempotentCommutativeMonoid_940 (coe v0))
-- Algebra.Bundles.BoundedLattice.isCommutativeMagma
d_isCommutativeMagma_1052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_1052 ~v0 ~v1 v2
  = du_isCommutativeMagma_1052 v2
du_isCommutativeMagma_1052 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_1052 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v2))
-- Algebra.Bundles.BoundedLattice.isCommutativeMonoid
d_isCommutativeMonoid_1054 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_1054 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
      (coe d_isIdempotentCommutativeMonoid_940 (coe v0))
-- Algebra.Bundles.BoundedLattice.isCommutativeSemigroup
d_isCommutativeSemigroup_1056 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1056 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_1056 v2
du_isCommutativeSemigroup_1056 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1056 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v1))
-- Algebra.Bundles.BoundedLattice.isEquivalence
d_isEquivalence_1058 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe d_isIdempotentCommutativeMonoid_940 (coe v0))))))
-- Algebra.Bundles.BoundedLattice.isIdempotentCommutativeMonoid
d_isIdempotentCommutativeMonoid_1060 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
d_isIdempotentCommutativeMonoid_1060 v0
  = coe d_isIdempotentCommutativeMonoid_940 (coe v0)
-- Algebra.Bundles.BoundedLattice.isMagma
d_isMagma_1062 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1062 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))
-- Algebra.Bundles.BoundedLattice.isMonoid
d_isMonoid_1064 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1064 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
         (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))
-- Algebra.Bundles.BoundedLattice.isPartialEquivalence
d_isPartialEquivalence_1066 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1066 ~v0 ~v1 v2
  = du_isPartialEquivalence_1066 v2
du_isPartialEquivalence_1066 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1066 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Bundles.BoundedLattice.isSemigroup
d_isSemigroup_1068 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1068 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe d_isIdempotentCommutativeMonoid_940 (coe v0))))
-- Algebra.Bundles.BoundedLattice.isUnitalMagma
d_isUnitalMagma_1070 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_1070 ~v0 ~v1 v2 = du_isUnitalMagma_1070 v2
du_isUnitalMagma_1070 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_1070 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Bundles.BoundedLattice.magma
d_magma_1072 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_Magma_8
d_magma_1072 ~v0 ~v1 v2 = du_magma_1072 v2
du_magma_1072 :: T_IdempotentCommutativeMonoid_916 -> T_Magma_8
du_magma_1072 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    let v2 = coe du_monoid_890 (coe v1) in
    coe du_magma_524 (coe du_semigroup_802 (coe v2))
-- Algebra.Bundles.BoundedLattice.monoid
d_monoid_1074 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_Monoid_740
d_monoid_1074 ~v0 ~v1 v2 = du_monoid_1074 v2
du_monoid_1074 :: T_IdempotentCommutativeMonoid_916 -> T_Monoid_740
du_monoid_1074 v0
  = coe du_monoid_890 (coe du_commutativeMonoid_992 (coe v0))
-- Algebra.Bundles.BoundedLattice.rawMagma
d_rawMagma_1076 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1076 ~v0 ~v1 v2 = du_rawMagma_1076 v2
du_rawMagma_1076 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1076 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    let v2 = coe du_monoid_890 (coe v1) in
    let v3 = coe du_semigroup_802 (coe v2) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v3))
-- Algebra.Bundles.BoundedLattice.rawMonoid
d_rawMonoid_1078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_1078 ~v0 ~v1 v2 = du_rawMonoid_1078 v2
du_rawMonoid_1078 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_1078 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v1))
-- Algebra.Bundles.BoundedLattice.refl
d_refl_1080 ::
  T_IdempotentCommutativeMonoid_916 -> AgdaAny -> AgdaAny
d_refl_1080 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))))
-- Algebra.Bundles.BoundedLattice.reflexive
d_reflexive_1082 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1082 ~v0 ~v1 v2 = du_reflexive_1082 v2
du_reflexive_1082 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1082 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Bundles.BoundedLattice.semigroup
d_semigroup_1084 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_Semigroup_476
d_semigroup_1084 ~v0 ~v1 v2 = du_semigroup_1084 v2
du_semigroup_1084 ::
  T_IdempotentCommutativeMonoid_916 -> T_Semigroup_476
du_semigroup_1084 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v1))
-- Algebra.Bundles.BoundedLattice.setoid
d_setoid_1086 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1086 ~v0 ~v1 v2 = du_setoid_1086 v2
du_setoid_1086 ::
  T_IdempotentCommutativeMonoid_916 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1086 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.BoundedLattice.sym
d_sym_1088 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1088 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))))
-- Algebra.Bundles.BoundedLattice.trans
d_trans_1090 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1090 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe d_isIdempotentCommutativeMonoid_940 (coe v0)))))))
-- Algebra.Bundles.BoundedLattice.unitalMagma
d_unitalMagma_1092 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 -> T_UnitalMagma_672
d_unitalMagma_1092 ~v0 ~v1 v2 = du_unitalMagma_1092 v2
du_unitalMagma_1092 ::
  T_IdempotentCommutativeMonoid_916 -> T_UnitalMagma_672
du_unitalMagma_1092 v0
  = let v1 = coe du_commutativeMonoid_992 (coe v0) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v1))
-- Algebra.Bundles.BoundedLattice.ε
d_ε_1094 :: T_IdempotentCommutativeMonoid_916 -> AgdaAny
d_ε_1094 v0 = coe d_ε_938 (coe v0)
-- Algebra.Bundles.BoundedLattice.∙-cong
d_'8729''45'cong_1096 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1096 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe d_isIdempotentCommutativeMonoid_940 (coe v0))))))
-- Algebra.Bundles.BoundedLattice.∙-congʳ
d_'8729''45'cong'691'_1098 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1098 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1098 v2
du_'8729''45'cong'691'_1098 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1098 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.BoundedLattice.∙-congˡ
d_'8729''45'cong'737'_1100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1100 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1100 v2
du_'8729''45'cong'737'_1100 ::
  T_IdempotentCommutativeMonoid_916 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1100 v0
  = let v1 = d_isIdempotentCommutativeMonoid_940 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.InvertibleMagma
d_InvertibleMagma_1106 a0 a1 = ()
data T_InvertibleMagma_1106
  = C_InvertibleMagma'46'constructor_18869 (AgdaAny ->
                                            AgdaAny -> AgdaAny)
                                           AgdaAny (AgdaAny -> AgdaAny)
                                           MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
-- Algebra.Bundles.InvertibleMagma.Carrier
d_Carrier_1124 :: T_InvertibleMagma_1106 -> ()
d_Carrier_1124 = erased
-- Algebra.Bundles.InvertibleMagma._≈_
d__'8776'__1126 ::
  T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1126 = erased
-- Algebra.Bundles.InvertibleMagma._∙_
d__'8729'__1128 ::
  T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__1128 v0
  = case coe v0 of
      C_InvertibleMagma'46'constructor_18869 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.InvertibleMagma.ε
d_ε_1130 :: T_InvertibleMagma_1106 -> AgdaAny
d_ε_1130 v0
  = case coe v0 of
      C_InvertibleMagma'46'constructor_18869 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.InvertibleMagma._⁻¹
d__'8315''185'_1132 :: T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny
d__'8315''185'_1132 v0
  = case coe v0 of
      C_InvertibleMagma'46'constructor_18869 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.InvertibleMagma.isInvertibleMagma
d_isInvertibleMagma_1134 ::
  T_InvertibleMagma_1106 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_1134 v0
  = case coe v0 of
      C_InvertibleMagma'46'constructor_18869 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.InvertibleMagma._.inverse
d_inverse_1138 ::
  T_InvertibleMagma_1106 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1138 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_792
      (coe d_isInvertibleMagma_1134 (coe v0))
-- Algebra.Bundles.InvertibleMagma._.inverseʳ
d_inverse'691'_1140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny
d_inverse'691'_1140 ~v0 ~v1 v2 = du_inverse'691'_1140 v2
du_inverse'691'_1140 ::
  T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny
du_inverse'691'_1140 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_820
      (coe d_isInvertibleMagma_1134 (coe v0))
-- Algebra.Bundles.InvertibleMagma._.inverseˡ
d_inverse'737'_1142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny
d_inverse'737'_1142 ~v0 ~v1 v2 = du_inverse'737'_1142 v2
du_inverse'737'_1142 ::
  T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny
du_inverse'737'_1142 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_818
      (coe d_isInvertibleMagma_1134 (coe v0))
-- Algebra.Bundles.InvertibleMagma._.isEquivalence
d_isEquivalence_1144 ::
  T_InvertibleMagma_1106 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1144 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_790
         (coe d_isInvertibleMagma_1134 (coe v0)))
-- Algebra.Bundles.InvertibleMagma._.isMagma
d_isMagma_1146 ::
  T_InvertibleMagma_1106 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1146 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_790
      (coe d_isInvertibleMagma_1134 (coe v0))
-- Algebra.Bundles.InvertibleMagma._.isPartialEquivalence
d_isPartialEquivalence_1148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1148 ~v0 ~v1 v2
  = du_isPartialEquivalence_1148 v2
du_isPartialEquivalence_1148 ::
  T_InvertibleMagma_1106 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1148 v0
  = let v1 = d_isInvertibleMagma_1134 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.InvertibleMagma._.refl
d_refl_1150 :: T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny
d_refl_1150 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_790
            (coe d_isInvertibleMagma_1134 (coe v0))))
-- Algebra.Bundles.InvertibleMagma._.reflexive
d_reflexive_1152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1152 ~v0 ~v1 v2 = du_reflexive_1152 v2
du_reflexive_1152 ::
  T_InvertibleMagma_1106 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1152 v0
  = let v1 = d_isInvertibleMagma_1134 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.InvertibleMagma._.setoid
d_setoid_1154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1154 ~v0 ~v1 v2 = du_setoid_1154 v2
du_setoid_1154 ::
  T_InvertibleMagma_1106 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1154 v0
  = let v1 = d_isInvertibleMagma_1134 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v1))
-- Algebra.Bundles.InvertibleMagma._.sym
d_sym_1156 ::
  T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1156 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_790
            (coe d_isInvertibleMagma_1134 (coe v0))))
-- Algebra.Bundles.InvertibleMagma._.trans
d_trans_1158 ::
  T_InvertibleMagma_1106 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1158 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_790
            (coe d_isInvertibleMagma_1134 (coe v0))))
-- Algebra.Bundles.InvertibleMagma._.⁻¹-cong
d_'8315''185''45'cong_1160 ::
  T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_1160 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_794
      (coe d_isInvertibleMagma_1134 (coe v0))
-- Algebra.Bundles.InvertibleMagma._.∙-cong
d_'8729''45'cong_1162 ::
  T_InvertibleMagma_1106 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1162 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_790
         (coe d_isInvertibleMagma_1134 (coe v0)))
-- Algebra.Bundles.InvertibleMagma._.∙-congʳ
d_'8729''45'cong'691'_1164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1164 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1164 v2
du_'8729''45'cong'691'_1164 ::
  T_InvertibleMagma_1106 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1164 v0
  = let v1 = d_isInvertibleMagma_1134 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v1))
-- Algebra.Bundles.InvertibleMagma._.∙-congˡ
d_'8729''45'cong'737'_1166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1166 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1166 v2
du_'8729''45'cong'737'_1166 ::
  T_InvertibleMagma_1106 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1166 v0
  = let v1 = d_isInvertibleMagma_1134 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v1))
-- Algebra.Bundles.InvertibleMagma.magma
d_magma_1168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 -> T_Magma_8
d_magma_1168 ~v0 ~v1 v2 = du_magma_1168 v2
du_magma_1168 :: T_InvertibleMagma_1106 -> T_Magma_8
du_magma_1168 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__1128 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_790
         (coe d_isInvertibleMagma_1134 (coe v0)))
-- Algebra.Bundles.InvertibleMagma._._≉_
d__'8777'__1172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1172 = erased
-- Algebra.Bundles.InvertibleMagma._.rawMagma
d_rawMagma_1174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleMagma_1106 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1174 ~v0 ~v1 v2 = du_rawMagma_1174 v2
du_rawMagma_1174 ::
  T_InvertibleMagma_1106 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1174 v0
  = coe du_rawMagma_52 (coe du_magma_1168 (coe v0))
-- Algebra.Bundles.InvertibleUnitalMagma
d_InvertibleUnitalMagma_1180 a0 a1 = ()
data T_InvertibleUnitalMagma_1180
  = C_InvertibleUnitalMagma'46'constructor_20321 (AgdaAny ->
                                                  AgdaAny -> AgdaAny)
                                                 AgdaAny (AgdaAny -> AgdaAny)
                                                 MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
-- Algebra.Bundles.InvertibleUnitalMagma.Carrier
d_Carrier_1198 :: T_InvertibleUnitalMagma_1180 -> ()
d_Carrier_1198 = erased
-- Algebra.Bundles.InvertibleUnitalMagma._≈_
d__'8776'__1200 ::
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1200 = erased
-- Algebra.Bundles.InvertibleUnitalMagma._∙_
d__'8729'__1202 ::
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__1202 v0
  = case coe v0 of
      C_InvertibleUnitalMagma'46'constructor_20321 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.InvertibleUnitalMagma.ε
d_ε_1204 :: T_InvertibleUnitalMagma_1180 -> AgdaAny
d_ε_1204 v0
  = case coe v0 of
      C_InvertibleUnitalMagma'46'constructor_20321 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.InvertibleUnitalMagma._⁻¹
d__'8315''185'_1206 ::
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
d__'8315''185'_1206 v0
  = case coe v0 of
      C_InvertibleUnitalMagma'46'constructor_20321 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.InvertibleUnitalMagma.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_1208 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_1208 v0
  = case coe v0 of
      C_InvertibleUnitalMagma'46'constructor_20321 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.InvertibleUnitalMagma._.identity
d_identity_1212 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1212 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_842
      (coe d_isInvertibleUnitalMagma_1208 (coe v0))
-- Algebra.Bundles.InvertibleUnitalMagma._.identityʳ
d_identity'691'_1214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
d_identity'691'_1214 ~v0 ~v1 v2 = du_identity'691'_1214 v2
du_identity'691'_1214 ::
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
du_identity'691'_1214 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_878
      (coe d_isInvertibleUnitalMagma_1208 (coe v0))
-- Algebra.Bundles.InvertibleUnitalMagma._.identityˡ
d_identity'737'_1216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
d_identity'737'_1216 ~v0 ~v1 v2 = du_identity'737'_1216 v2
du_identity'737'_1216 ::
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
du_identity'737'_1216 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_876
      (coe d_isInvertibleUnitalMagma_1208 (coe v0))
-- Algebra.Bundles.InvertibleUnitalMagma._.inverse
d_inverse_1218 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1218 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_792
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
         (coe d_isInvertibleUnitalMagma_1208 (coe v0)))
-- Algebra.Bundles.InvertibleUnitalMagma._.inverseʳ
d_inverse'691'_1220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
d_inverse'691'_1220 ~v0 ~v1 v2 = du_inverse'691'_1220 v2
du_inverse'691'_1220 ::
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
du_inverse'691'_1220 v0
  = let v1 = d_isInvertibleUnitalMagma_1208 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_820
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v1))
-- Algebra.Bundles.InvertibleUnitalMagma._.inverseˡ
d_inverse'737'_1222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
d_inverse'737'_1222 ~v0 ~v1 v2 = du_inverse'737'_1222 v2
du_inverse'737'_1222 ::
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
du_inverse'737'_1222 v0
  = let v1 = d_isInvertibleUnitalMagma_1208 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_818
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840 (coe v1))
-- Algebra.Bundles.InvertibleUnitalMagma._.isEquivalence
d_isEquivalence_1224 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1224 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_790
         (coe
            MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
            (coe d_isInvertibleUnitalMagma_1208 (coe v0))))
-- Algebra.Bundles.InvertibleUnitalMagma._.isInvertibleMagma
d_isInvertibleMagma_1226 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_1226 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
      (coe d_isInvertibleUnitalMagma_1208 (coe v0))
-- Algebra.Bundles.InvertibleUnitalMagma._.isMagma
d_isMagma_1228 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1228 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_790
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
         (coe d_isInvertibleUnitalMagma_1208 (coe v0)))
-- Algebra.Bundles.InvertibleUnitalMagma._.isPartialEquivalence
d_isPartialEquivalence_1230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1230 ~v0 ~v1 v2
  = du_isPartialEquivalence_1230 v2
du_isPartialEquivalence_1230 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1230 v0
  = let v1 = d_isInvertibleUnitalMagma_1208 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Bundles.InvertibleUnitalMagma._.isUnitalMagma
d_isUnitalMagma_1232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_1232 ~v0 ~v1 v2 = du_isUnitalMagma_1232 v2
du_isUnitalMagma_1232 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_1232 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_880
      (coe d_isInvertibleUnitalMagma_1208 (coe v0))
-- Algebra.Bundles.InvertibleUnitalMagma._.refl
d_refl_1234 :: T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny
d_refl_1234 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_790
            (coe
               MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
               (coe d_isInvertibleUnitalMagma_1208 (coe v0)))))
-- Algebra.Bundles.InvertibleUnitalMagma._.reflexive
d_reflexive_1236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1236 ~v0 ~v1 v2 = du_reflexive_1236 v2
du_reflexive_1236 ::
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1236 v0
  = let v1 = d_isInvertibleUnitalMagma_1208 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Bundles.InvertibleUnitalMagma._.setoid
d_setoid_1238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1238 ~v0 ~v1 v2 = du_setoid_1238 v2
du_setoid_1238 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1238 v0
  = let v1 = d_isInvertibleUnitalMagma_1208 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v2))
-- Algebra.Bundles.InvertibleUnitalMagma._.sym
d_sym_1240 ::
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1240 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_790
            (coe
               MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
               (coe d_isInvertibleUnitalMagma_1208 (coe v0)))))
-- Algebra.Bundles.InvertibleUnitalMagma._.trans
d_trans_1242 ::
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1242 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_790
            (coe
               MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
               (coe d_isInvertibleUnitalMagma_1208 (coe v0)))))
-- Algebra.Bundles.InvertibleUnitalMagma._.⁻¹-cong
d_'8315''185''45'cong_1244 ::
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_1244 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_794
      (coe
         MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
         (coe d_isInvertibleUnitalMagma_1208 (coe v0)))
-- Algebra.Bundles.InvertibleUnitalMagma._.∙-cong
d_'8729''45'cong_1246 ::
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1246 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_790
         (coe
            MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
            (coe d_isInvertibleUnitalMagma_1208 (coe v0))))
-- Algebra.Bundles.InvertibleUnitalMagma._.∙-congʳ
d_'8729''45'cong'691'_1248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1248 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1248 v2
du_'8729''45'cong'691'_1248 ::
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1248 v0
  = let v1 = d_isInvertibleUnitalMagma_1208 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v2))
-- Algebra.Bundles.InvertibleUnitalMagma._.∙-congˡ
d_'8729''45'cong'737'_1250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1250 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1250 v2
du_'8729''45'cong'737'_1250 ::
  T_InvertibleUnitalMagma_1180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1250 v0
  = let v1 = d_isInvertibleUnitalMagma_1208 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_790 (coe v2))
-- Algebra.Bundles.InvertibleUnitalMagma.invertibleMagma
d_invertibleMagma_1252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 -> T_InvertibleMagma_1106
d_invertibleMagma_1252 ~v0 ~v1 v2 = du_invertibleMagma_1252 v2
du_invertibleMagma_1252 ::
  T_InvertibleUnitalMagma_1180 -> T_InvertibleMagma_1106
du_invertibleMagma_1252 v0
  = coe
      C_InvertibleMagma'46'constructor_18869 (d__'8729'__1202 (coe v0))
      (d_ε_1204 (coe v0)) (d__'8315''185'_1206 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isInvertibleMagma_840
         (coe d_isInvertibleUnitalMagma_1208 (coe v0)))
-- Algebra.Bundles.InvertibleUnitalMagma._._≉_
d__'8777'__1256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1256 = erased
-- Algebra.Bundles.InvertibleUnitalMagma._.magma
d_magma_1258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 -> T_Magma_8
d_magma_1258 ~v0 ~v1 v2 = du_magma_1258 v2
du_magma_1258 :: T_InvertibleUnitalMagma_1180 -> T_Magma_8
du_magma_1258 v0
  = coe du_magma_1168 (coe du_invertibleMagma_1252 (coe v0))
-- Algebra.Bundles.InvertibleUnitalMagma._.rawMagma
d_rawMagma_1260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1260 ~v0 ~v1 v2 = du_rawMagma_1260 v2
du_rawMagma_1260 ::
  T_InvertibleUnitalMagma_1180 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1260 v0
  = let v1 = coe du_invertibleMagma_1252 (coe v0) in
    coe du_rawMagma_52 (coe du_magma_1168 (coe v1))
-- Algebra.Bundles.Group
d_Group_1266 a0 a1 = ()
data T_Group_1266
  = C_Group'46'constructor_21965 (AgdaAny -> AgdaAny -> AgdaAny)
                                 AgdaAny (AgdaAny -> AgdaAny)
                                 MAlonzo.Code.Algebra.Structures.T_IsGroup_888
-- Algebra.Bundles.Group.Carrier
d_Carrier_1284 :: T_Group_1266 -> ()
d_Carrier_1284 = erased
-- Algebra.Bundles.Group._≈_
d__'8776'__1286 :: T_Group_1266 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1286 = erased
-- Algebra.Bundles.Group._∙_
d__'8729'__1288 :: T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__1288 v0
  = case coe v0 of
      C_Group'46'constructor_21965 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Group.ε
d_ε_1290 :: T_Group_1266 -> AgdaAny
d_ε_1290 v0
  = case coe v0 of
      C_Group'46'constructor_21965 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Group._⁻¹
d__'8315''185'_1292 :: T_Group_1266 -> AgdaAny -> AgdaAny
d__'8315''185'_1292 v0
  = case coe v0 of
      C_Group'46'constructor_21965 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Group.isGroup
d_isGroup_1294 ::
  T_Group_1266 -> MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1294 v0
  = case coe v0 of
      C_Group'46'constructor_21965 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Group._._-_
d__'45'__1298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__1298 ~v0 ~v1 v2 = du__'45'__1298 v2
du__'45'__1298 :: T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__1298 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du__'45'__944
      (coe d__'8729'__1288 (coe v0)) (coe d__'8315''185'_1292 (coe v0))
-- Algebra.Bundles.Group._.assoc
d_assoc_1300 ::
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1300 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe d_isGroup_1294 (coe v0))))
-- Algebra.Bundles.Group._.identity
d_identity_1302 ::
  T_Group_1266 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1302 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe d_isGroup_1294 (coe v0)))
-- Algebra.Bundles.Group._.identityʳ
d_identity'691'_1304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny
d_identity'691'_1304 ~v0 ~v1 v2 = du_identity'691'_1304 v2
du_identity'691'_1304 :: T_Group_1266 -> AgdaAny -> AgdaAny
du_identity'691'_1304 v0
  = let v1 = d_isGroup_1294 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1))
-- Algebra.Bundles.Group._.identityˡ
d_identity'737'_1306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny
d_identity'737'_1306 ~v0 ~v1 v2 = du_identity'737'_1306 v2
du_identity'737'_1306 :: T_Group_1266 -> AgdaAny -> AgdaAny
du_identity'737'_1306 v0
  = let v1 = d_isGroup_1294 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1))
-- Algebra.Bundles.Group._.inverse
d_inverse_1308 ::
  T_Group_1266 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1308 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe d_isGroup_1294 (coe v0))
-- Algebra.Bundles.Group._.inverseʳ
d_inverse'691'_1310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny
d_inverse'691'_1310 ~v0 ~v1 v2 = du_inverse'691'_1310 v2
du_inverse'691'_1310 :: T_Group_1266 -> AgdaAny -> AgdaAny
du_inverse'691'_1310 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
      (coe d_isGroup_1294 (coe v0))
-- Algebra.Bundles.Group._.inverseˡ
d_inverse'737'_1312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny
d_inverse'737'_1312 ~v0 ~v1 v2 = du_inverse'737'_1312 v2
du_inverse'737'_1312 :: T_Group_1266 -> AgdaAny -> AgdaAny
du_inverse'737'_1312 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
      (coe d_isGroup_1294 (coe v0))
-- Algebra.Bundles.Group._.isEquivalence
d_isEquivalence_1314 ::
  T_Group_1266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1314 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe d_isGroup_1294 (coe v0)))))
-- Algebra.Bundles.Group._.isInvertibleMagma
d_isInvertibleMagma_1316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_1316 ~v0 ~v1 v2 = du_isInvertibleMagma_1316 v2
du_isInvertibleMagma_1316 ::
  T_Group_1266 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
du_isInvertibleMagma_1316 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleMagma_966
      (coe d_isGroup_1294 (coe v0))
-- Algebra.Bundles.Group._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_1318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_1318 ~v0 ~v1 v2
  = du_isInvertibleUnitalMagma_1318 v2
du_isInvertibleUnitalMagma_1318 ::
  T_Group_1266 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_1318 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleUnitalMagma_968
      (coe d_isGroup_1294 (coe v0))
-- Algebra.Bundles.Group._.isMagma
d_isMagma_1320 ::
  T_Group_1266 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1320 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe d_isGroup_1294 (coe v0))))
-- Algebra.Bundles.Group._.isMonoid
d_isMonoid_1322 ::
  T_Group_1266 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1322 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe d_isGroup_1294 (coe v0))
-- Algebra.Bundles.Group._.isPartialEquivalence
d_isPartialEquivalence_1324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1324 ~v0 ~v1 v2
  = du_isPartialEquivalence_1324 v2
du_isPartialEquivalence_1324 ::
  T_Group_1266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1324 v0
  = let v1 = d_isGroup_1294 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Bundles.Group._.isSemigroup
d_isSemigroup_1326 ::
  T_Group_1266 -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1326 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe d_isGroup_1294 (coe v0)))
-- Algebra.Bundles.Group._.isUnitalMagma
d_isUnitalMagma_1328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_1328 ~v0 ~v1 v2 = du_isUnitalMagma_1328 v2
du_isUnitalMagma_1328 ::
  T_Group_1266 -> MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_1328 v0
  = let v1 = d_isGroup_1294 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1))
-- Algebra.Bundles.Group._.refl
d_refl_1330 :: T_Group_1266 -> AgdaAny -> AgdaAny
d_refl_1330 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe d_isGroup_1294 (coe v0))))))
-- Algebra.Bundles.Group._.reflexive
d_reflexive_1332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1332 ~v0 ~v1 v2 = du_reflexive_1332 v2
du_reflexive_1332 ::
  T_Group_1266 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1332 v0
  = let v1 = d_isGroup_1294 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Bundles.Group._.setoid
d_setoid_1334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1334 ~v0 ~v1 v2 = du_setoid_1334 v2
du_setoid_1334 ::
  T_Group_1266 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1334 v0
  = let v1 = d_isGroup_1294 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Group._.sym
d_sym_1336 ::
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1336 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe d_isGroup_1294 (coe v0))))))
-- Algebra.Bundles.Group._.trans
d_trans_1338 ::
  T_Group_1266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1338 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe d_isGroup_1294 (coe v0))))))
-- Algebra.Bundles.Group._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_1340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_1340 ~v0 ~v1 v2
  = du_unique'691''45''8315''185'_1340 v2
du_unique'691''45''8315''185'_1340 ::
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_1340 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_unique'691''45''8315''185'_964
      (coe d__'8729'__1288 (coe v0)) (coe d_ε_1290 (coe v0))
      (coe d__'8315''185'_1292 (coe v0)) (coe d_isGroup_1294 (coe v0))
-- Algebra.Bundles.Group._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_1342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_1342 ~v0 ~v1 v2
  = du_unique'737''45''8315''185'_1342 v2
du_unique'737''45''8315''185'_1342 ::
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_1342 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_unique'737''45''8315''185'_958
      (coe d__'8729'__1288 (coe v0)) (coe d_ε_1290 (coe v0))
      (coe d__'8315''185'_1292 (coe v0)) (coe d_isGroup_1294 (coe v0))
-- Algebra.Bundles.Group._.⁻¹-cong
d_'8315''185''45'cong_1344 ::
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_1344 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
      (coe d_isGroup_1294 (coe v0))
-- Algebra.Bundles.Group._.∙-cong
d_'8729''45'cong_1346 ::
  T_Group_1266 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1346 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe d_isGroup_1294 (coe v0)))))
-- Algebra.Bundles.Group._.∙-congʳ
d_'8729''45'cong'691'_1348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1348 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1348 v2
du_'8729''45'cong'691'_1348 ::
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1348 v0
  = let v1 = d_isGroup_1294 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Group._.∙-congˡ
d_'8729''45'cong'737'_1350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1350 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1350 v2
du_'8729''45'cong'737'_1350 ::
  T_Group_1266 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1350 v0
  = let v1 = d_isGroup_1294 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Group.rawGroup
d_rawGroup_1352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
d_rawGroup_1352 ~v0 ~v1 v2 = du_rawGroup_1352 v2
du_rawGroup_1352 ::
  T_Group_1266 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
du_rawGroup_1352 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawGroup'46'constructor_921
      (d__'8729'__1288 (coe v0)) (d_ε_1290 (coe v0))
      (d__'8315''185'_1292 (coe v0))
-- Algebra.Bundles.Group.monoid
d_monoid_1354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> T_Monoid_740
d_monoid_1354 ~v0 ~v1 v2 = du_monoid_1354 v2
du_monoid_1354 :: T_Group_1266 -> T_Monoid_740
du_monoid_1354 v0
  = coe
      C_Monoid'46'constructor_13309 (d__'8729'__1288 (coe v0))
      (d_ε_1290 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe d_isGroup_1294 (coe v0)))
-- Algebra.Bundles.Group._._≉_
d__'8777'__1358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1358 = erased
-- Algebra.Bundles.Group._.magma
d_magma_1360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Group_1266 -> T_Magma_8
d_magma_1360 ~v0 ~v1 v2 = du_magma_1360 v2
du_magma_1360 :: T_Group_1266 -> T_Magma_8
du_magma_1360 v0
  = let v1 = coe du_monoid_1354 (coe v0) in
    coe du_magma_524 (coe du_semigroup_802 (coe v1))
-- Algebra.Bundles.Group._.rawMagma
d_rawMagma_1362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1362 ~v0 ~v1 v2 = du_rawMagma_1362 v2
du_rawMagma_1362 ::
  T_Group_1266 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1362 v0
  = let v1 = coe du_monoid_1354 (coe v0) in
    let v2 = coe du_semigroup_802 (coe v1) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v2))
-- Algebra.Bundles.Group._.rawMonoid
d_rawMonoid_1364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_1364 ~v0 ~v1 v2 = du_rawMonoid_1364 v2
du_rawMonoid_1364 ::
  T_Group_1266 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_1364 v0
  = coe du_rawMonoid_812 (coe du_monoid_1354 (coe v0))
-- Algebra.Bundles.Group._.semigroup
d_semigroup_1366 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> T_Semigroup_476
d_semigroup_1366 ~v0 ~v1 v2 = du_semigroup_1366 v2
du_semigroup_1366 :: T_Group_1266 -> T_Semigroup_476
du_semigroup_1366 v0
  = coe du_semigroup_802 (coe du_monoid_1354 (coe v0))
-- Algebra.Bundles.Group._.unitalMagma
d_unitalMagma_1368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> T_UnitalMagma_672
d_unitalMagma_1368 ~v0 ~v1 v2 = du_unitalMagma_1368 v2
du_unitalMagma_1368 :: T_Group_1266 -> T_UnitalMagma_672
du_unitalMagma_1368 v0
  = coe du_unitalMagma_814 (coe du_monoid_1354 (coe v0))
-- Algebra.Bundles.Group.invertibleMagma
d_invertibleMagma_1370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> T_InvertibleMagma_1106
d_invertibleMagma_1370 ~v0 ~v1 v2 = du_invertibleMagma_1370 v2
du_invertibleMagma_1370 :: T_Group_1266 -> T_InvertibleMagma_1106
du_invertibleMagma_1370 v0
  = coe
      C_InvertibleMagma'46'constructor_18869 (d__'8729'__1288 (coe v0))
      (d_ε_1290 (coe v0)) (d__'8315''185'_1292 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isInvertibleMagma_966
         (coe d_isGroup_1294 (coe v0)))
-- Algebra.Bundles.Group.invertibleUnitalMagma
d_invertibleUnitalMagma_1372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Group_1266 -> T_InvertibleUnitalMagma_1180
d_invertibleUnitalMagma_1372 ~v0 ~v1 v2
  = du_invertibleUnitalMagma_1372 v2
du_invertibleUnitalMagma_1372 ::
  T_Group_1266 -> T_InvertibleUnitalMagma_1180
du_invertibleUnitalMagma_1372 v0
  = coe
      C_InvertibleUnitalMagma'46'constructor_20321
      (d__'8729'__1288 (coe v0)) (d_ε_1290 (coe v0))
      (d__'8315''185'_1292 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isInvertibleUnitalMagma_968
         (coe d_isGroup_1294 (coe v0)))
-- Algebra.Bundles.AbelianGroup
d_AbelianGroup_1378 a0 a1 = ()
data T_AbelianGroup_1378
  = C_AbelianGroup'46'constructor_24425 (AgdaAny ->
                                         AgdaAny -> AgdaAny)
                                        AgdaAny (AgdaAny -> AgdaAny)
                                        MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
-- Algebra.Bundles.AbelianGroup.Carrier
d_Carrier_1396 :: T_AbelianGroup_1378 -> ()
d_Carrier_1396 = erased
-- Algebra.Bundles.AbelianGroup._≈_
d__'8776'__1398 :: T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1398 = erased
-- Algebra.Bundles.AbelianGroup._∙_
d__'8729'__1400 ::
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__1400 v0
  = case coe v0 of
      C_AbelianGroup'46'constructor_24425 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.AbelianGroup.ε
d_ε_1402 :: T_AbelianGroup_1378 -> AgdaAny
d_ε_1402 v0
  = case coe v0 of
      C_AbelianGroup'46'constructor_24425 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.AbelianGroup._⁻¹
d__'8315''185'_1404 :: T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
d__'8315''185'_1404 v0
  = case coe v0 of
      C_AbelianGroup'46'constructor_24425 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.AbelianGroup.isAbelianGroup
d_isAbelianGroup_1406 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_isAbelianGroup_1406 v0
  = case coe v0 of
      C_AbelianGroup'46'constructor_24425 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.AbelianGroup._._-_
d__'45'__1410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__1410 ~v0 ~v1 v2 = du__'45'__1410 v2
du__'45'__1410 ::
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__1410 v0
  = let v1 = d__'8729'__1400 (coe v0) in
    let v2 = d__'8315''185'_1404 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v1) (coe v2)
-- Algebra.Bundles.AbelianGroup._.assoc
d_assoc_1412 ::
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1412 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe d_isAbelianGroup_1406 (coe v0)))))
-- Algebra.Bundles.AbelianGroup._.comm
d_comm_1414 :: T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1414 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_990
      (coe d_isAbelianGroup_1406 (coe v0))
-- Algebra.Bundles.AbelianGroup._.identity
d_identity_1416 ::
  T_AbelianGroup_1378 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1416 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe d_isAbelianGroup_1406 (coe v0))))
-- Algebra.Bundles.AbelianGroup._.identityʳ
d_identity'691'_1418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
d_identity'691'_1418 ~v0 ~v1 v2 = du_identity'691'_1418 v2
du_identity'691'_1418 :: T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
du_identity'691'_1418 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2))
-- Algebra.Bundles.AbelianGroup._.identityˡ
d_identity'737'_1420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
d_identity'737'_1420 ~v0 ~v1 v2 = du_identity'737'_1420 v2
du_identity'737'_1420 :: T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
du_identity'737'_1420 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2))
-- Algebra.Bundles.AbelianGroup._.inverse
d_inverse_1422 ::
  T_AbelianGroup_1378 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1422 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe d_isAbelianGroup_1406 (coe v0)))
-- Algebra.Bundles.AbelianGroup._.inverseʳ
d_inverse'691'_1424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
d_inverse'691'_1424 ~v0 ~v1 v2 = du_inverse'691'_1424 v2
du_inverse'691'_1424 :: T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
du_inverse'691'_1424 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1))
-- Algebra.Bundles.AbelianGroup._.inverseˡ
d_inverse'737'_1426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
d_inverse'737'_1426 ~v0 ~v1 v2 = du_inverse'737'_1426 v2
du_inverse'737'_1426 :: T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
du_inverse'737'_1426 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1))
-- Algebra.Bundles.AbelianGroup._.isCommutativeMagma
d_isCommutativeMagma_1428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_1428 ~v0 ~v1 v2
  = du_isCommutativeMagma_1428 v2
du_isCommutativeMagma_1428 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_1428 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v2))
-- Algebra.Bundles.AbelianGroup._.isCommutativeMonoid
d_isCommutativeMonoid_1430 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_1430 ~v0 ~v1 v2
  = du_isCommutativeMonoid_1430 v2
du_isCommutativeMonoid_1430 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_1430 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
      (coe d_isAbelianGroup_1406 (coe v0))
-- Algebra.Bundles.AbelianGroup._.isCommutativeSemigroup
d_isCommutativeSemigroup_1432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1432 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_1432 v2
du_isCommutativeSemigroup_1432 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1432 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
         (coe v1))
-- Algebra.Bundles.AbelianGroup._.isEquivalence
d_isEquivalence_1434 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1434 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe d_isAbelianGroup_1406 (coe v0))))))
-- Algebra.Bundles.AbelianGroup._.isGroup
d_isGroup_1436 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_1436 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe d_isAbelianGroup_1406 (coe v0))
-- Algebra.Bundles.AbelianGroup._.isInvertibleMagma
d_isInvertibleMagma_1438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_1438 ~v0 ~v1 v2 = du_isInvertibleMagma_1438 v2
du_isInvertibleMagma_1438 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
du_isInvertibleMagma_1438 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleMagma_966
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1))
-- Algebra.Bundles.AbelianGroup._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_1440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_1440 ~v0 ~v1 v2
  = du_isInvertibleUnitalMagma_1440 v2
du_isInvertibleUnitalMagma_1440 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_1440 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleUnitalMagma_968
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1))
-- Algebra.Bundles.AbelianGroup._.isMagma
d_isMagma_1442 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1442 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe d_isAbelianGroup_1406 (coe v0)))))
-- Algebra.Bundles.AbelianGroup._.isMonoid
d_isMonoid_1444 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1444 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe d_isAbelianGroup_1406 (coe v0)))
-- Algebra.Bundles.AbelianGroup._.isPartialEquivalence
d_isPartialEquivalence_1446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1446 ~v0 ~v1 v2
  = du_isPartialEquivalence_1446 v2
du_isPartialEquivalence_1446 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1446 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Bundles.AbelianGroup._.isSemigroup
d_isSemigroup_1448 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1448 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe d_isAbelianGroup_1406 (coe v0))))
-- Algebra.Bundles.AbelianGroup._.isUnitalMagma
d_isUnitalMagma_1450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_1450 ~v0 ~v1 v2 = du_isUnitalMagma_1450 v2
du_isUnitalMagma_1450 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_1450 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2))
-- Algebra.Bundles.AbelianGroup._.refl
d_refl_1452 :: T_AbelianGroup_1378 -> AgdaAny -> AgdaAny
d_refl_1452 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe d_isAbelianGroup_1406 (coe v0)))))))
-- Algebra.Bundles.AbelianGroup._.reflexive
d_reflexive_1454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1454 ~v0 ~v1 v2 = du_reflexive_1454 v2
du_reflexive_1454 ::
  T_AbelianGroup_1378 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1454 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Bundles.AbelianGroup._.setoid
d_setoid_1456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1456 ~v0 ~v1 v2 = du_setoid_1456 v2
du_setoid_1456 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1456 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.AbelianGroup._.sym
d_sym_1458 ::
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1458 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe d_isAbelianGroup_1406 (coe v0)))))))
-- Algebra.Bundles.AbelianGroup._.trans
d_trans_1460 ::
  T_AbelianGroup_1378 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1460 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe d_isAbelianGroup_1406 (coe v0)))))))
-- Algebra.Bundles.AbelianGroup._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_1462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_1462 ~v0 ~v1 v2
  = du_unique'691''45''8315''185'_1462 v2
du_unique'691''45''8315''185'_1462 ::
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_1462 v0
  = let v1 = d__'8729'__1400 (coe v0) in
    let v2 = d_ε_1402 (coe v0) in
    let v3 = d__'8315''185'_1404 (coe v0) in
    let v4 = d_isAbelianGroup_1406 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'691''45''8315''185'_964
      (coe v1) (coe v2) (coe v3)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4))
-- Algebra.Bundles.AbelianGroup._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_1464 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_1464 ~v0 ~v1 v2
  = du_unique'737''45''8315''185'_1464 v2
du_unique'737''45''8315''185'_1464 ::
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_1464 v0
  = let v1 = d__'8729'__1400 (coe v0) in
    let v2 = d_ε_1402 (coe v0) in
    let v3 = d__'8315''185'_1404 (coe v0) in
    let v4 = d_isAbelianGroup_1406 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'737''45''8315''185'_958
      (coe v1) (coe v2) (coe v3)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4))
-- Algebra.Bundles.AbelianGroup._.⁻¹-cong
d_'8315''185''45'cong_1466 ::
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_1466 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe d_isAbelianGroup_1406 (coe v0)))
-- Algebra.Bundles.AbelianGroup._.∙-cong
d_'8729''45'cong_1468 ::
  T_AbelianGroup_1378 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1468 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe d_isAbelianGroup_1406 (coe v0))))))
-- Algebra.Bundles.AbelianGroup._.∙-congʳ
d_'8729''45'cong'691'_1470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1470 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1470 v2
du_'8729''45'cong'691'_1470 ::
  T_AbelianGroup_1378 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1470 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.AbelianGroup._.∙-congˡ
d_'8729''45'cong'737'_1472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1472 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1472 v2
du_'8729''45'cong'737'_1472 ::
  T_AbelianGroup_1378 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1472 v0
  = let v1 = d_isAbelianGroup_1406 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.AbelianGroup.group
d_group_1474 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> T_Group_1266
d_group_1474 ~v0 ~v1 v2 = du_group_1474 v2
du_group_1474 :: T_AbelianGroup_1378 -> T_Group_1266
du_group_1474 v0
  = coe
      C_Group'46'constructor_21965 (d__'8729'__1400 (coe v0))
      (d_ε_1402 (coe v0)) (d__'8315''185'_1404 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe d_isAbelianGroup_1406 (coe v0)))
-- Algebra.Bundles.AbelianGroup._._≉_
d__'8777'__1478 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1478 = erased
-- Algebra.Bundles.AbelianGroup._.invertibleMagma
d_invertibleMagma_1480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> T_InvertibleMagma_1106
d_invertibleMagma_1480 ~v0 ~v1 v2 = du_invertibleMagma_1480 v2
du_invertibleMagma_1480 ::
  T_AbelianGroup_1378 -> T_InvertibleMagma_1106
du_invertibleMagma_1480 v0
  = coe du_invertibleMagma_1370 (coe du_group_1474 (coe v0))
-- Algebra.Bundles.AbelianGroup._.invertibleUnitalMagma
d_invertibleUnitalMagma_1482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> T_InvertibleUnitalMagma_1180
d_invertibleUnitalMagma_1482 ~v0 ~v1 v2
  = du_invertibleUnitalMagma_1482 v2
du_invertibleUnitalMagma_1482 ::
  T_AbelianGroup_1378 -> T_InvertibleUnitalMagma_1180
du_invertibleUnitalMagma_1482 v0
  = coe du_invertibleUnitalMagma_1372 (coe du_group_1474 (coe v0))
-- Algebra.Bundles.AbelianGroup._.magma
d_magma_1484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> T_Magma_8
d_magma_1484 ~v0 ~v1 v2 = du_magma_1484 v2
du_magma_1484 :: T_AbelianGroup_1378 -> T_Magma_8
du_magma_1484 v0
  = let v1 = coe du_group_1474 (coe v0) in
    let v2 = coe du_monoid_1354 (coe v1) in
    coe du_magma_524 (coe du_semigroup_802 (coe v2))
-- Algebra.Bundles.AbelianGroup._.monoid
d_monoid_1486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> T_Monoid_740
d_monoid_1486 ~v0 ~v1 v2 = du_monoid_1486 v2
du_monoid_1486 :: T_AbelianGroup_1378 -> T_Monoid_740
du_monoid_1486 v0 = coe du_monoid_1354 (coe du_group_1474 (coe v0))
-- Algebra.Bundles.AbelianGroup._.rawGroup
d_rawGroup_1488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
d_rawGroup_1488 ~v0 ~v1 v2 = du_rawGroup_1488 v2
du_rawGroup_1488 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
du_rawGroup_1488 v0
  = coe du_rawGroup_1352 (coe du_group_1474 (coe v0))
-- Algebra.Bundles.AbelianGroup._.rawMagma
d_rawMagma_1490 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1490 ~v0 ~v1 v2 = du_rawMagma_1490 v2
du_rawMagma_1490 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1490 v0
  = let v1 = coe du_group_1474 (coe v0) in
    let v2 = coe du_monoid_1354 (coe v1) in
    let v3 = coe du_semigroup_802 (coe v2) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v3))
-- Algebra.Bundles.AbelianGroup._.rawMonoid
d_rawMonoid_1492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_1492 ~v0 ~v1 v2 = du_rawMonoid_1492 v2
du_rawMonoid_1492 ::
  T_AbelianGroup_1378 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_1492 v0
  = let v1 = coe du_group_1474 (coe v0) in
    coe du_rawMonoid_812 (coe du_monoid_1354 (coe v1))
-- Algebra.Bundles.AbelianGroup._.semigroup
d_semigroup_1494 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> T_Semigroup_476
d_semigroup_1494 ~v0 ~v1 v2 = du_semigroup_1494 v2
du_semigroup_1494 :: T_AbelianGroup_1378 -> T_Semigroup_476
du_semigroup_1494 v0
  = let v1 = coe du_group_1474 (coe v0) in
    coe du_semigroup_802 (coe du_monoid_1354 (coe v1))
-- Algebra.Bundles.AbelianGroup.commutativeMonoid
d_commutativeMonoid_1496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> T_CommutativeMonoid_820
d_commutativeMonoid_1496 ~v0 ~v1 v2 = du_commutativeMonoid_1496 v2
du_commutativeMonoid_1496 ::
  T_AbelianGroup_1378 -> T_CommutativeMonoid_820
du_commutativeMonoid_1496 v0
  = coe
      C_CommutativeMonoid'46'constructor_15055 (d__'8729'__1400 (coe v0))
      (d_ε_1402 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
         (coe d_isAbelianGroup_1406 (coe v0)))
-- Algebra.Bundles.AbelianGroup._.commutativeMagma
d_commutativeMagma_1500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> T_CommutativeMagma_120
d_commutativeMagma_1500 ~v0 ~v1 v2 = du_commutativeMagma_1500 v2
du_commutativeMagma_1500 ::
  T_AbelianGroup_1378 -> T_CommutativeMagma_120
du_commutativeMagma_1500 v0
  = let v1 = coe du_commutativeMonoid_1496 (coe v0) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v1))
-- Algebra.Bundles.AbelianGroup._.commutativeSemigroup
d_commutativeSemigroup_1502 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AbelianGroup_1378 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_1502 ~v0 ~v1 v2
  = du_commutativeSemigroup_1502 v2
du_commutativeSemigroup_1502 ::
  T_AbelianGroup_1378 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_1502 v0
  = coe
      du_commutativeSemigroup_906
      (coe du_commutativeMonoid_1496 (coe v0))
-- Algebra.Bundles.NearSemiring
d_NearSemiring_1508 a0 a1 = ()
data T_NearSemiring_1508
  = C_NearSemiring'46'constructor_26799 (AgdaAny ->
                                         AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
                                        MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
-- Algebra.Bundles.NearSemiring.Carrier
d_Carrier_1526 :: T_NearSemiring_1508 -> ()
d_Carrier_1526 = erased
-- Algebra.Bundles.NearSemiring._≈_
d__'8776'__1528 :: T_NearSemiring_1508 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1528 = erased
-- Algebra.Bundles.NearSemiring._+_
d__'43'__1530 ::
  T_NearSemiring_1508 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__1530 v0
  = case coe v0 of
      C_NearSemiring'46'constructor_26799 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NearSemiring._*_
d__'42'__1532 ::
  T_NearSemiring_1508 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__1532 v0
  = case coe v0 of
      C_NearSemiring'46'constructor_26799 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NearSemiring.0#
d_0'35'_1534 :: T_NearSemiring_1508 -> AgdaAny
d_0'35'_1534 v0
  = case coe v0 of
      C_NearSemiring'46'constructor_26799 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NearSemiring.isNearSemiring
d_isNearSemiring_1536 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_1536 v0
  = case coe v0 of
      C_NearSemiring'46'constructor_26799 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NearSemiring._.*-assoc
d_'42''45'assoc_1540 ::
  T_NearSemiring_1508 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1540 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1084
      (coe d_isNearSemiring_1536 (coe v0))
-- Algebra.Bundles.NearSemiring._.*-cong
d_'42''45'cong_1542 ::
  T_NearSemiring_1508 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1542 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1082
      (coe d_isNearSemiring_1536 (coe v0))
-- Algebra.Bundles.NearSemiring._.∙-congʳ
d_'8729''45'cong'691'_1544 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1544 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1544 v2
du_'8729''45'cong'691'_1544 ::
  T_NearSemiring_1508 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1544 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1126 (coe v1))
-- Algebra.Bundles.NearSemiring._.∙-congˡ
d_'8729''45'cong'737'_1546 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1546 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1546 v2
du_'8729''45'cong'737'_1546 ::
  T_NearSemiring_1508 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1546 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1126 (coe v1))
-- Algebra.Bundles.NearSemiring._.*-isMagma
d_'42''45'isMagma_1548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_1548 ~v0 ~v1 v2 = du_'42''45'isMagma_1548 v2
du_'42''45'isMagma_1548 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_1548 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1126
      (coe d_isNearSemiring_1536 (coe v0))
-- Algebra.Bundles.NearSemiring._.*-isSemigroup
d_'42''45'isSemigroup_1550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_1550 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_1550 v2
du_'42''45'isSemigroup_1550 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_1550 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1128
      (coe d_isNearSemiring_1536 (coe v0))
-- Algebra.Bundles.NearSemiring._.assoc
d_assoc_1552 ::
  T_NearSemiring_1508 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1552 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
            (coe d_isNearSemiring_1536 (coe v0))))
-- Algebra.Bundles.NearSemiring._.∙-cong
d_'8729''45'cong_1554 ::
  T_NearSemiring_1508 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1554 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
               (coe d_isNearSemiring_1536 (coe v0)))))
-- Algebra.Bundles.NearSemiring._.∙-congʳ
d_'8729''45'cong'691'_1556 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1556 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1556 v2
du_'8729''45'cong'691'_1556 ::
  T_NearSemiring_1508 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1556 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.NearSemiring._.∙-congˡ
d_'8729''45'cong'737'_1558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1558 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1558 v2
du_'8729''45'cong'737'_1558 ::
  T_NearSemiring_1508 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1558 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.NearSemiring._.identity
d_identity_1560 ::
  T_NearSemiring_1508 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1560 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
         (coe d_isNearSemiring_1536 (coe v0)))
-- Algebra.Bundles.NearSemiring._.identityʳ
d_identity'691'_1562 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 -> AgdaAny -> AgdaAny
d_identity'691'_1562 ~v0 ~v1 v2 = du_identity'691'_1562 v2
du_identity'691'_1562 :: T_NearSemiring_1508 -> AgdaAny -> AgdaAny
du_identity'691'_1562 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v1))
-- Algebra.Bundles.NearSemiring._.identityˡ
d_identity'737'_1564 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 -> AgdaAny -> AgdaAny
d_identity'737'_1564 ~v0 ~v1 v2 = du_identity'737'_1564 v2
du_identity'737'_1564 :: T_NearSemiring_1508 -> AgdaAny -> AgdaAny
du_identity'737'_1564 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v1))
-- Algebra.Bundles.NearSemiring._.isMagma
d_isMagma_1566 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1566 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
            (coe d_isNearSemiring_1536 (coe v0))))
-- Algebra.Bundles.NearSemiring._.+-isMonoid
d_'43''45'isMonoid_1568 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_1568 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
      (coe d_isNearSemiring_1536 (coe v0))
-- Algebra.Bundles.NearSemiring._.isSemigroup
d_isSemigroup_1570 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1570 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
         (coe d_isNearSemiring_1536 (coe v0)))
-- Algebra.Bundles.NearSemiring._.isUnitalMagma
d_isUnitalMagma_1572 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_1572 ~v0 ~v1 v2 = du_isUnitalMagma_1572 v2
du_isUnitalMagma_1572 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_1572 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080 (coe v1))
-- Algebra.Bundles.NearSemiring._.distribʳ
d_distrib'691'_1574 ::
  T_NearSemiring_1508 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib'691'_1086
      (coe d_isNearSemiring_1536 (coe v0))
-- Algebra.Bundles.NearSemiring._.isEquivalence
d_isEquivalence_1576 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1576 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
               (coe d_isNearSemiring_1536 (coe v0)))))
-- Algebra.Bundles.NearSemiring._.isPartialEquivalence
d_isPartialEquivalence_1578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1578 ~v0 ~v1 v2
  = du_isPartialEquivalence_1578 v2
du_isPartialEquivalence_1578 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1578 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Bundles.NearSemiring._.refl
d_refl_1580 :: T_NearSemiring_1508 -> AgdaAny -> AgdaAny
d_refl_1580 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
                  (coe d_isNearSemiring_1536 (coe v0))))))
-- Algebra.Bundles.NearSemiring._.reflexive
d_reflexive_1582 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1582 ~v0 ~v1 v2 = du_reflexive_1582 v2
du_reflexive_1582 ::
  T_NearSemiring_1508 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1582 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Bundles.NearSemiring._.setoid
d_setoid_1584 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1584 ~v0 ~v1 v2 = du_setoid_1584 v2
du_setoid_1584 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1584 v0
  = let v1 = d_isNearSemiring_1536 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.NearSemiring._.sym
d_sym_1586 ::
  T_NearSemiring_1508 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1586 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
                  (coe d_isNearSemiring_1536 (coe v0))))))
-- Algebra.Bundles.NearSemiring._.trans
d_trans_1588 ::
  T_NearSemiring_1508 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1588 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
                  (coe d_isNearSemiring_1536 (coe v0))))))
-- Algebra.Bundles.NearSemiring._.zeroˡ
d_zero'737'_1590 :: T_NearSemiring_1508 -> AgdaAny -> AgdaAny
d_zero'737'_1590 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero'737'_1088
      (coe d_isNearSemiring_1536 (coe v0))
-- Algebra.Bundles.NearSemiring.rawNearSemiring
d_rawNearSemiring_1592 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
d_rawNearSemiring_1592 ~v0 ~v1 v2 = du_rawNearSemiring_1592 v2
du_rawNearSemiring_1592 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
du_rawNearSemiring_1592 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawNearSemiring'46'constructor_1421
      (d__'43'__1530 (coe v0)) (d__'42'__1532 (coe v0))
      (d_0'35'_1534 (coe v0))
-- Algebra.Bundles.NearSemiring.+-monoid
d_'43''45'monoid_1594 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 -> T_Monoid_740
d_'43''45'monoid_1594 ~v0 ~v1 v2 = du_'43''45'monoid_1594 v2
du_'43''45'monoid_1594 :: T_NearSemiring_1508 -> T_Monoid_740
du_'43''45'monoid_1594 v0
  = coe
      C_Monoid'46'constructor_13309 (d__'43'__1530 (coe v0))
      (d_0'35'_1534 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1080
         (coe d_isNearSemiring_1536 (coe v0)))
-- Algebra.Bundles.NearSemiring._._≉_
d__'8777'__1598 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1598 = erased
-- Algebra.Bundles.NearSemiring._.magma
d_magma_1600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 -> T_Magma_8
d_magma_1600 ~v0 ~v1 v2 = du_magma_1600 v2
du_magma_1600 :: T_NearSemiring_1508 -> T_Magma_8
du_magma_1600 v0
  = let v1 = coe du_'43''45'monoid_1594 (coe v0) in
    coe du_magma_524 (coe du_semigroup_802 (coe v1))
-- Algebra.Bundles.NearSemiring._.rawMagma
d_rawMagma_1602 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1602 ~v0 ~v1 v2 = du_rawMagma_1602 v2
du_rawMagma_1602 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1602 v0
  = let v1 = coe du_'43''45'monoid_1594 (coe v0) in
    let v2 = coe du_semigroup_802 (coe v1) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v2))
-- Algebra.Bundles.NearSemiring._.rawMonoid
d_rawMonoid_1604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_1604 ~v0 ~v1 v2 = du_rawMonoid_1604 v2
du_rawMonoid_1604 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_1604 v0
  = coe du_rawMonoid_812 (coe du_'43''45'monoid_1594 (coe v0))
-- Algebra.Bundles.NearSemiring._.semigroup
d_semigroup_1606 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 -> T_Semigroup_476
d_semigroup_1606 ~v0 ~v1 v2 = du_semigroup_1606 v2
du_semigroup_1606 :: T_NearSemiring_1508 -> T_Semigroup_476
du_semigroup_1606 v0
  = coe du_semigroup_802 (coe du_'43''45'monoid_1594 (coe v0))
-- Algebra.Bundles.NearSemiring._.unitalMagma
d_unitalMagma_1608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 -> T_UnitalMagma_672
d_unitalMagma_1608 ~v0 ~v1 v2 = du_unitalMagma_1608 v2
du_unitalMagma_1608 :: T_NearSemiring_1508 -> T_UnitalMagma_672
du_unitalMagma_1608 v0
  = coe du_unitalMagma_814 (coe du_'43''45'monoid_1594 (coe v0))
-- Algebra.Bundles.NearSemiring.*-semigroup
d_'42''45'semigroup_1610 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 -> T_Semigroup_476
d_'42''45'semigroup_1610 ~v0 ~v1 v2 = du_'42''45'semigroup_1610 v2
du_'42''45'semigroup_1610 :: T_NearSemiring_1508 -> T_Semigroup_476
du_'42''45'semigroup_1610 v0
  = coe
      C_Semigroup'46'constructor_8557 (d__'42'__1532 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1128
         (coe d_isNearSemiring_1536 (coe v0)))
-- Algebra.Bundles.NearSemiring._.magma
d_magma_1614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 -> T_Magma_8
d_magma_1614 ~v0 ~v1 v2 = du_magma_1614 v2
du_magma_1614 :: T_NearSemiring_1508 -> T_Magma_8
du_magma_1614 v0
  = coe du_magma_524 (coe du_'42''45'semigroup_1610 (coe v0))
-- Algebra.Bundles.NearSemiring._.rawMagma
d_rawMagma_1616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1616 ~v0 ~v1 v2 = du_rawMagma_1616 v2
du_rawMagma_1616 ::
  T_NearSemiring_1508 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1616 v0
  = let v1 = coe du_'42''45'semigroup_1610 (coe v0) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v1))
-- Algebra.Bundles.SemiringWithoutOne
d_SemiringWithoutOne_1622 a0 a1 = ()
data T_SemiringWithoutOne_1622
  = C_SemiringWithoutOne'46'constructor_29099 (AgdaAny ->
                                               AgdaAny -> AgdaAny)
                                              (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
                                              MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
-- Algebra.Bundles.SemiringWithoutOne.Carrier
d_Carrier_1640 :: T_SemiringWithoutOne_1622 -> ()
d_Carrier_1640 = erased
-- Algebra.Bundles.SemiringWithoutOne._≈_
d__'8776'__1642 ::
  T_SemiringWithoutOne_1622 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1642 = erased
-- Algebra.Bundles.SemiringWithoutOne._+_
d__'43'__1644 ::
  T_SemiringWithoutOne_1622 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__1644 v0
  = case coe v0 of
      C_SemiringWithoutOne'46'constructor_29099 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemiringWithoutOne._*_
d__'42'__1646 ::
  T_SemiringWithoutOne_1622 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__1646 v0
  = case coe v0 of
      C_SemiringWithoutOne'46'constructor_29099 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemiringWithoutOne.0#
d_0'35'_1648 :: T_SemiringWithoutOne_1622 -> AgdaAny
d_0'35'_1648 v0
  = case coe v0 of
      C_SemiringWithoutOne'46'constructor_29099 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemiringWithoutOne.isSemiringWithoutOne
d_isSemiringWithoutOne_1650 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1650 v0
  = case coe v0 of
      C_SemiringWithoutOne'46'constructor_29099 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemiringWithoutOne._.*-assoc
d_'42''45'assoc_1654 ::
  T_SemiringWithoutOne_1622 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1654 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1164
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.*-cong
d_'42''45'cong_1656 ::
  T_SemiringWithoutOne_1622 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1656 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1162
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.∙-congʳ
d_'8729''45'cong'691'_1658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1658 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1658 v2
du_'8729''45'cong'691'_1658 ::
  T_SemiringWithoutOne_1622 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1658 v0
  = let v1 = d_isSemiringWithoutOne_1650 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1182 (coe v1))
-- Algebra.Bundles.SemiringWithoutOne._.∙-congˡ
d_'8729''45'cong'737'_1660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1660 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1660 v2
du_'8729''45'cong'737'_1660 ::
  T_SemiringWithoutOne_1622 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1660 v0
  = let v1 = d_isSemiringWithoutOne_1650 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1182 (coe v1))
-- Algebra.Bundles.SemiringWithoutOne._.*-isMagma
d_'42''45'isMagma_1662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_1662 ~v0 ~v1 v2 = du_'42''45'isMagma_1662 v2
du_'42''45'isMagma_1662 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_1662 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1182
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.*-isSemigroup
d_'42''45'isSemigroup_1664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_1664 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_1664 v2
du_'42''45'isSemigroup_1664 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_1664 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1184
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.comm
d_comm_1666 ::
  T_SemiringWithoutOne_1622 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1666 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe d_isSemiringWithoutOne_1650 (coe v0)))
-- Algebra.Bundles.SemiringWithoutOne._.isCommutativeMagma
d_isCommutativeMagma_1668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_1668 ~v0 ~v1 v2
  = du_isCommutativeMagma_1668 v2
du_isCommutativeMagma_1668 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_1668 v0
  = let v1 = d_isSemiringWithoutOne_1650 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v2))
-- Algebra.Bundles.SemiringWithoutOne._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1670 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1670 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.isCommutativeSemigroup
d_isCommutativeSemigroup_1672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1672 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_1672 v2
du_isCommutativeSemigroup_1672 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1672 v0
  = let v1 = d_isSemiringWithoutOne_1650 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe v1))
-- Algebra.Bundles.SemiringWithoutOne._.isMonoid
d_isMonoid_1674 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1674 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe d_isSemiringWithoutOne_1650 (coe v0)))
-- Algebra.Bundles.SemiringWithoutOne._.distrib
d_distrib_1676 ::
  T_SemiringWithoutOne_1622 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1676 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1166
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.isEquivalence
d_isEquivalence_1678 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1678 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
                  (coe d_isSemiringWithoutOne_1650 (coe v0))))))
-- Algebra.Bundles.SemiringWithoutOne._.isNearSemiring
d_isNearSemiring_1680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_1680 ~v0 ~v1 v2 = du_isNearSemiring_1680 v2
du_isNearSemiring_1680 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_1680 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.zero
d_zero_1682 ::
  T_SemiringWithoutOne_1622 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1682 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1168
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.zeroʳ
d_zero'691'_1684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> AgdaAny -> AgdaAny
d_zero'691'_1684 ~v0 ~v1 v2 = du_zero'691'_1684 v2
du_zero'691'_1684 ::
  T_SemiringWithoutOne_1622 -> AgdaAny -> AgdaAny
du_zero'691'_1684 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.zeroˡ
d_zero'737'_1686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> AgdaAny -> AgdaAny
d_zero'737'_1686 ~v0 ~v1 v2 = du_zero'737'_1686 v2
du_zero'737'_1686 ::
  T_SemiringWithoutOne_1622 -> AgdaAny -> AgdaAny
du_zero'737'_1686 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe d_isSemiringWithoutOne_1650 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne.nearSemiring
d_nearSemiring_1688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_NearSemiring_1508
d_nearSemiring_1688 ~v0 ~v1 v2 = du_nearSemiring_1688 v2
du_nearSemiring_1688 ::
  T_SemiringWithoutOne_1622 -> T_NearSemiring_1508
du_nearSemiring_1688 v0
  = coe
      C_NearSemiring'46'constructor_26799 (d__'43'__1644 (coe v0))
      (d__'42'__1646 (coe v0)) (d_0'35'_1648 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
         (coe d_isSemiringWithoutOne_1650 (coe v0)))
-- Algebra.Bundles.SemiringWithoutOne._._≉_
d__'8777'__1692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1692 = erased
-- Algebra.Bundles.SemiringWithoutOne._.magma
d_magma_1694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_Magma_8
d_magma_1694 ~v0 ~v1 v2 = du_magma_1694 v2
du_magma_1694 :: T_SemiringWithoutOne_1622 -> T_Magma_8
du_magma_1694 v0
  = let v1 = coe du_nearSemiring_1688 (coe v0) in
    coe du_magma_524 (coe du_'42''45'semigroup_1610 (coe v1))
-- Algebra.Bundles.SemiringWithoutOne._.rawMagma
d_rawMagma_1696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1696 ~v0 ~v1 v2 = du_rawMagma_1696 v2
du_rawMagma_1696 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1696 v0
  = let v1 = coe du_nearSemiring_1688 (coe v0) in
    let v2 = coe du_'42''45'semigroup_1610 (coe v1) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v2))
-- Algebra.Bundles.SemiringWithoutOne._.*-semigroup
d_'42''45'semigroup_1698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_Semigroup_476
d_'42''45'semigroup_1698 ~v0 ~v1 v2 = du_'42''45'semigroup_1698 v2
du_'42''45'semigroup_1698 ::
  T_SemiringWithoutOne_1622 -> T_Semigroup_476
du_'42''45'semigroup_1698 v0
  = coe du_'42''45'semigroup_1610 (coe du_nearSemiring_1688 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.magma
d_magma_1700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_Magma_8
d_magma_1700 ~v0 ~v1 v2 = du_magma_1700 v2
du_magma_1700 :: T_SemiringWithoutOne_1622 -> T_Magma_8
du_magma_1700 v0
  = let v1 = coe du_nearSemiring_1688 (coe v0) in
    let v2 = coe du_'43''45'monoid_1594 (coe v1) in
    coe du_magma_524 (coe du_semigroup_802 (coe v2))
-- Algebra.Bundles.SemiringWithoutOne._.+-monoid
d_'43''45'monoid_1702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_Monoid_740
d_'43''45'monoid_1702 ~v0 ~v1 v2 = du_'43''45'monoid_1702 v2
du_'43''45'monoid_1702 :: T_SemiringWithoutOne_1622 -> T_Monoid_740
du_'43''45'monoid_1702 v0
  = coe du_'43''45'monoid_1594 (coe du_nearSemiring_1688 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne._.rawMagma
d_rawMagma_1704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1704 ~v0 ~v1 v2 = du_rawMagma_1704 v2
du_rawMagma_1704 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1704 v0
  = let v1 = coe du_nearSemiring_1688 (coe v0) in
    let v2 = coe du_'43''45'monoid_1594 (coe v1) in
    let v3 = coe du_semigroup_802 (coe v2) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v3))
-- Algebra.Bundles.SemiringWithoutOne._.rawMonoid
d_rawMonoid_1706 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_1706 ~v0 ~v1 v2 = du_rawMonoid_1706 v2
du_rawMonoid_1706 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_1706 v0
  = let v1 = coe du_nearSemiring_1688 (coe v0) in
    coe du_rawMonoid_812 (coe du_'43''45'monoid_1594 (coe v1))
-- Algebra.Bundles.SemiringWithoutOne._.semigroup
d_semigroup_1708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_Semigroup_476
d_semigroup_1708 ~v0 ~v1 v2 = du_semigroup_1708 v2
du_semigroup_1708 :: T_SemiringWithoutOne_1622 -> T_Semigroup_476
du_semigroup_1708 v0
  = let v1 = coe du_nearSemiring_1688 (coe v0) in
    coe du_semigroup_802 (coe du_'43''45'monoid_1594 (coe v1))
-- Algebra.Bundles.SemiringWithoutOne._.unitalMagma
d_unitalMagma_1710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_UnitalMagma_672
d_unitalMagma_1710 ~v0 ~v1 v2 = du_unitalMagma_1710 v2
du_unitalMagma_1710 ::
  T_SemiringWithoutOne_1622 -> T_UnitalMagma_672
du_unitalMagma_1710 v0
  = let v1 = coe du_nearSemiring_1688 (coe v0) in
    coe du_unitalMagma_814 (coe du_'43''45'monoid_1594 (coe v1))
-- Algebra.Bundles.SemiringWithoutOne._.rawNearSemiring
d_rawNearSemiring_1712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
d_rawNearSemiring_1712 ~v0 ~v1 v2 = du_rawNearSemiring_1712 v2
du_rawNearSemiring_1712 ::
  T_SemiringWithoutOne_1622 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
du_rawNearSemiring_1712 v0
  = coe du_rawNearSemiring_1592 (coe du_nearSemiring_1688 (coe v0))
-- Algebra.Bundles.SemiringWithoutOne.+-commutativeMonoid
d_'43''45'commutativeMonoid_1714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_1714 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_1714 v2
du_'43''45'commutativeMonoid_1714 ::
  T_SemiringWithoutOne_1622 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_1714 v0
  = coe
      C_CommutativeMonoid'46'constructor_15055 (d__'43'__1644 (coe v0))
      (d_0'35'_1648 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe d_isSemiringWithoutOne_1650 (coe v0)))
-- Algebra.Bundles.SemiringWithoutOne._.commutativeMagma
d_commutativeMagma_1718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_CommutativeMagma_120
d_commutativeMagma_1718 ~v0 ~v1 v2 = du_commutativeMagma_1718 v2
du_commutativeMagma_1718 ::
  T_SemiringWithoutOne_1622 -> T_CommutativeMagma_120
du_commutativeMagma_1718 v0
  = let v1 = coe du_'43''45'commutativeMonoid_1714 (coe v0) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v1))
-- Algebra.Bundles.SemiringWithoutOne._.commutativeSemigroup
d_commutativeSemigroup_1720 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutOne_1622 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_1720 ~v0 ~v1 v2
  = du_commutativeSemigroup_1720 v2
du_commutativeSemigroup_1720 ::
  T_SemiringWithoutOne_1622 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_1720 v0
  = coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1714 (coe v0))
-- Algebra.Bundles.CommutativeSemiringWithoutOne
d_CommutativeSemiringWithoutOne_1726 a0 a1 = ()
data T_CommutativeSemiringWithoutOne_1726
  = C_CommutativeSemiringWithoutOne'46'constructor_31105 (AgdaAny ->
                                                          AgdaAny -> AgdaAny)
                                                         (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
                                                         MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
-- Algebra.Bundles.CommutativeSemiringWithoutOne.Carrier
d_Carrier_1744 :: T_CommutativeSemiringWithoutOne_1726 -> ()
d_Carrier_1744 = erased
-- Algebra.Bundles.CommutativeSemiringWithoutOne._≈_
d__'8776'__1746 ::
  T_CommutativeSemiringWithoutOne_1726 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1746 = erased
-- Algebra.Bundles.CommutativeSemiringWithoutOne._+_
d__'43'__1748 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43'__1748 v0
  = case coe v0 of
      C_CommutativeSemiringWithoutOne'46'constructor_31105 v3 v4 v5 v6
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemiringWithoutOne._*_
d__'42'__1750 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__1750 v0
  = case coe v0 of
      C_CommutativeSemiringWithoutOne'46'constructor_31105 v3 v4 v5 v6
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemiringWithoutOne.0#
d_0'35'_1752 :: T_CommutativeSemiringWithoutOne_1726 -> AgdaAny
d_0'35'_1752 v0
  = case coe v0 of
      C_CommutativeSemiringWithoutOne'46'constructor_31105 v3 v4 v5 v6
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemiringWithoutOne.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_1754 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_1754 v0
  = case coe v0 of
      C_CommutativeSemiringWithoutOne'46'constructor_31105 v3 v4 v5 v6
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.*-assoc
d_'42''45'assoc_1758 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1758 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1164
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0)))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.*-comm
d_'42''45'comm_1760 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_1760 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1218
      (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.*-cong
d_'42''45'cong_1762 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1762 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1162
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0)))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.∙-congʳ
d_'8729''45'cong'691'_1764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1764 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1764 v2
du_'8729''45'cong'691'_1764 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1764 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1182 (coe v2))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.∙-congˡ
d_'8729''45'cong'737'_1766 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1766 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1766 v2
du_'8729''45'cong'737'_1766 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1766 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1182 (coe v2))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.isCommutativeMagma
d_isCommutativeMagma_1768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_1768 ~v0 ~v1 v2
  = du_isCommutativeMagma_1768 v2
du_isCommutativeMagma_1768 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_1768 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v1))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_1770 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_1770 ~v0 ~v1 v2
  = du_'42''45'isCommutativeSemigroup_1770 v2
du_'42''45'isCommutativeSemigroup_1770 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_1770 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.*-isMagma
d_'42''45'isMagma_1772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_1772 ~v0 ~v1 v2 = du_'42''45'isMagma_1772 v2
du_'42''45'isMagma_1772 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_1772 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1182
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v1))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.*-isSemigroup
d_'42''45'isSemigroup_1774 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_1774 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_1774 v2
du_'42''45'isSemigroup_1774 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_1774 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1184
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v1))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.comm
d_comm_1776 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_1776 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
            (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0))))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.isCommutativeMagma
d_isCommutativeMagma_1778 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_1778 ~v0 ~v1 v2
  = du_isCommutativeMagma_1778 v2
du_isCommutativeMagma_1778 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_1778 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v3))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1780 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1780 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0)))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.isCommutativeSemigroup
d_isCommutativeSemigroup_1782 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1782 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_1782 v2
du_isCommutativeSemigroup_1782 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1782 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe v2))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.isMonoid
d_isMonoid_1784 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1784 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
            (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0))))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.distrib
d_distrib_1786 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1786 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1166
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0)))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.isEquivalence
d_isEquivalence_1788 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1788 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1160
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
                     (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0)))))))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.isNearSemiring
d_isNearSemiring_1790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_1790 ~v0 ~v1 v2 = du_isNearSemiring_1790 v2
du_isNearSemiring_1790 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_1790 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v1))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.isSemiringWithoutOne
d_isSemiringWithoutOne_1792 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1792 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
      (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.zero
d_zero_1794 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1794 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1168
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0)))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.zeroʳ
d_zero'691'_1796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> AgdaAny -> AgdaAny
d_zero'691'_1796 ~v0 ~v1 v2 = du_zero'691'_1796 v2
du_zero'691'_1796 ::
  T_CommutativeSemiringWithoutOne_1726 -> AgdaAny -> AgdaAny
du_zero'691'_1796 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v1))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.zeroˡ
d_zero'737'_1798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> AgdaAny -> AgdaAny
d_zero'737'_1798 ~v0 ~v1 v2 = du_zero'737'_1798 v2
du_zero'737'_1798 ::
  T_CommutativeSemiringWithoutOne_1726 -> AgdaAny -> AgdaAny
du_zero'737'_1798 v0
  = let v1 = d_isCommutativeSemiringWithoutOne_1754 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe v1))
-- Algebra.Bundles.CommutativeSemiringWithoutOne.semiringWithoutOne
d_semiringWithoutOne_1800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_SemiringWithoutOne_1622
d_semiringWithoutOne_1800 ~v0 ~v1 v2
  = du_semiringWithoutOne_1800 v2
du_semiringWithoutOne_1800 ::
  T_CommutativeSemiringWithoutOne_1726 -> T_SemiringWithoutOne_1622
du_semiringWithoutOne_1800 v0
  = coe
      C_SemiringWithoutOne'46'constructor_29099 (d__'43'__1748 (coe v0))
      (d__'42'__1750 (coe v0)) (d_0'35'_1752 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutOne_1216
         (coe d_isCommutativeSemiringWithoutOne_1754 (coe v0)))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._._≉_
d__'8777'__1804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1804 = erased
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.magma
d_magma_1806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_Magma_8
d_magma_1806 ~v0 ~v1 v2 = du_magma_1806 v2
du_magma_1806 :: T_CommutativeSemiringWithoutOne_1726 -> T_Magma_8
du_magma_1806 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    let v2 = coe du_nearSemiring_1688 (coe v1) in
    coe du_magma_524 (coe du_'42''45'semigroup_1610 (coe v2))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.rawMagma
d_rawMagma_1808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1808 ~v0 ~v1 v2 = du_rawMagma_1808 v2
du_rawMagma_1808 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1808 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    let v2 = coe du_nearSemiring_1688 (coe v1) in
    let v3 = coe du_'42''45'semigroup_1610 (coe v2) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v3))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.*-semigroup
d_'42''45'semigroup_1810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_Semigroup_476
d_'42''45'semigroup_1810 ~v0 ~v1 v2 = du_'42''45'semigroup_1810 v2
du_'42''45'semigroup_1810 ::
  T_CommutativeSemiringWithoutOne_1726 -> T_Semigroup_476
du_'42''45'semigroup_1810 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    coe du_'42''45'semigroup_1610 (coe du_nearSemiring_1688 (coe v1))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.+-commutativeMonoid
d_'43''45'commutativeMonoid_1812 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_1812 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_1812 v2
du_'43''45'commutativeMonoid_1812 ::
  T_CommutativeSemiringWithoutOne_1726 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_1812 v0
  = coe
      du_'43''45'commutativeMonoid_1714
      (coe du_semiringWithoutOne_1800 (coe v0))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.commutativeSemigroup
d_commutativeSemigroup_1814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_1814 ~v0 ~v1 v2
  = du_commutativeSemigroup_1814 v2
du_commutativeSemigroup_1814 ::
  T_CommutativeSemiringWithoutOne_1726 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_1814 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1714 (coe v1))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.magma
d_magma_1816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_Magma_8
d_magma_1816 ~v0 ~v1 v2 = du_magma_1816 v2
du_magma_1816 :: T_CommutativeSemiringWithoutOne_1726 -> T_Magma_8
du_magma_1816 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    let v2 = coe du_nearSemiring_1688 (coe v1) in
    let v3 = coe du_'43''45'monoid_1594 (coe v2) in
    coe du_magma_524 (coe du_semigroup_802 (coe v3))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.+-monoid
d_'43''45'monoid_1818 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_Monoid_740
d_'43''45'monoid_1818 ~v0 ~v1 v2 = du_'43''45'monoid_1818 v2
du_'43''45'monoid_1818 ::
  T_CommutativeSemiringWithoutOne_1726 -> T_Monoid_740
du_'43''45'monoid_1818 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    coe du_'43''45'monoid_1594 (coe du_nearSemiring_1688 (coe v1))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.rawMagma
d_rawMagma_1820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1820 ~v0 ~v1 v2 = du_rawMagma_1820 v2
du_rawMagma_1820 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1820 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    let v2 = coe du_nearSemiring_1688 (coe v1) in
    let v3 = coe du_'43''45'monoid_1594 (coe v2) in
    let v4 = coe du_semigroup_802 (coe v3) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v4))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.rawMonoid
d_rawMonoid_1822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_1822 ~v0 ~v1 v2 = du_rawMonoid_1822 v2
du_rawMonoid_1822 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_1822 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    let v2 = coe du_nearSemiring_1688 (coe v1) in
    coe du_rawMonoid_812 (coe du_'43''45'monoid_1594 (coe v2))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.semigroup
d_semigroup_1824 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_Semigroup_476
d_semigroup_1824 ~v0 ~v1 v2 = du_semigroup_1824 v2
du_semigroup_1824 ::
  T_CommutativeSemiringWithoutOne_1726 -> T_Semigroup_476
du_semigroup_1824 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    let v2 = coe du_nearSemiring_1688 (coe v1) in
    coe du_semigroup_802 (coe du_'43''45'monoid_1594 (coe v2))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.unitalMagma
d_unitalMagma_1826 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_UnitalMagma_672
d_unitalMagma_1826 ~v0 ~v1 v2 = du_unitalMagma_1826 v2
du_unitalMagma_1826 ::
  T_CommutativeSemiringWithoutOne_1726 -> T_UnitalMagma_672
du_unitalMagma_1826 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    let v2 = coe du_nearSemiring_1688 (coe v1) in
    coe du_unitalMagma_814 (coe du_'43''45'monoid_1594 (coe v2))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.nearSemiring
d_nearSemiring_1828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 -> T_NearSemiring_1508
d_nearSemiring_1828 ~v0 ~v1 v2 = du_nearSemiring_1828 v2
du_nearSemiring_1828 ::
  T_CommutativeSemiringWithoutOne_1726 -> T_NearSemiring_1508
du_nearSemiring_1828 v0
  = coe
      du_nearSemiring_1688 (coe du_semiringWithoutOne_1800 (coe v0))
-- Algebra.Bundles.CommutativeSemiringWithoutOne._.rawNearSemiring
d_rawNearSemiring_1830 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
d_rawNearSemiring_1830 ~v0 ~v1 v2 = du_rawNearSemiring_1830 v2
du_rawNearSemiring_1830 ::
  T_CommutativeSemiringWithoutOne_1726 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
du_rawNearSemiring_1830 v0
  = let v1 = coe du_semiringWithoutOne_1800 (coe v0) in
    coe du_rawNearSemiring_1592 (coe du_nearSemiring_1688 (coe v1))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero
d_SemiringWithoutAnnihilatingZero_1836 a0 a1 = ()
data T_SemiringWithoutAnnihilatingZero_1836
  = C_SemiringWithoutAnnihilatingZero'46'constructor_32973 (AgdaAny ->
                                                            AgdaAny -> AgdaAny)
                                                           (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
                                                           AgdaAny
                                                           MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero.Carrier
d_Carrier_1856 :: T_SemiringWithoutAnnihilatingZero_1836 -> ()
d_Carrier_1856 = erased
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._≈_
d__'8776'__1858 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1858 = erased
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._+_
d__'43'__1860 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43'__1860 v0
  = case coe v0 of
      C_SemiringWithoutAnnihilatingZero'46'constructor_32973 v3 v4 v5 v6 v7
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._*_
d__'42'__1862 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__1862 v0
  = case coe v0 of
      C_SemiringWithoutAnnihilatingZero'46'constructor_32973 v3 v4 v5 v6 v7
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero.0#
d_0'35'_1864 :: T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny
d_0'35'_1864 v0
  = case coe v0 of
      C_SemiringWithoutAnnihilatingZero'46'constructor_32973 v3 v4 v5 v6 v7
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero.1#
d_1'35'_1866 :: T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny
d_1'35'_1866 v0
  = case coe v0 of
      C_SemiringWithoutAnnihilatingZero'46'constructor_32973 v3 v4 v5 v6 v7
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1868 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1868 v0
  = case coe v0 of
      C_SemiringWithoutAnnihilatingZero'46'constructor_32973 v3 v4 v5 v6 v7
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.*-assoc
d_'42''45'assoc_1872 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1872 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.*-cong
d_'42''45'cong_1874 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1874 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.∙-congʳ
d_'8729''45'cong'691'_1876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1876 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1876 v2
du_'8729''45'cong'691'_1876 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1876 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.∙-congˡ
d_'8729''45'cong'737'_1878 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1878 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1878 v2
du_'8729''45'cong'737'_1878 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1878 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.*-identity
d_'42''45'identity_1880 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1880 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.identityʳ
d_identity'691'_1882 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny
d_identity'691'_1882 ~v0 ~v1 v2 = du_identity'691'_1882 v2
du_identity'691'_1882 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny
du_identity'691'_1882 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v1))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.identityˡ
d_identity'737'_1884 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny
d_identity'737'_1884 ~v0 ~v1 v2 = du_identity'737'_1884 v2
du_identity'737'_1884 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny
du_identity'737'_1884 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v1))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.*-isMagma
d_'42''45'isMagma_1886 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_1886 ~v0 ~v1 v2 = du_'42''45'isMagma_1886 v2
du_'42''45'isMagma_1886 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_1886 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.*-isMonoid
d_'42''45'isMonoid_1888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_1888 ~v0 ~v1 v2 = du_'42''45'isMonoid_1888 v2
du_'42''45'isMonoid_1888 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_1888 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.*-isSemigroup
d_'42''45'isSemigroup_1890 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_1890 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_1890 v2
du_'42''45'isSemigroup_1890 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_1890 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.assoc
d_assoc_1892 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1892 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0)))))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.comm
d_comm_1894 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_1894 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0)))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.∙-cong
d_'8729''45'cong_1896 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1896 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))))))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.∙-congʳ
d_'8729''45'cong'691'_1898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1898 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_1898 v2
du_'8729''45'cong'691'_1898 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1898 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.∙-congˡ
d_'8729''45'cong'737'_1900 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1900 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_1900 v2
du_'8729''45'cong'737'_1900 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1900 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.identity
d_identity_1902 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1902 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.identityʳ
d_identity'691'_1904 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny
d_identity'691'_1904 ~v0 ~v1 v2 = du_identity'691'_1904 v2
du_identity'691'_1904 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny
du_identity'691'_1904 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.identityˡ
d_identity'737'_1906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny
d_identity'737'_1906 ~v0 ~v1 v2 = du_identity'737'_1906 v2
du_identity'737'_1906 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny
du_identity'737'_1906 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.isCommutativeMagma
d_isCommutativeMagma_1908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_1908 ~v0 ~v1 v2
  = du_isCommutativeMagma_1908 v2
du_isCommutativeMagma_1908 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_1908 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v2))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1910 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1910 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.isCommutativeSemigroup
d_isCommutativeSemigroup_1912 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1912 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_1912 v2
du_isCommutativeSemigroup_1912 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1912 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v1))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.isMagma
d_isMagma_1914 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1914 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0)))))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.isMonoid
d_isMonoid_1916 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1916 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0)))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.isSemigroup
d_isSemigroup_1918 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1918 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.isUnitalMagma
d_isUnitalMagma_1920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_1920 ~v0 ~v1 v2 = du_isUnitalMagma_1920 v2
du_isUnitalMagma_1920 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_1920 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.distrib
d_distrib_1922 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1922 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.distribʳ
d_distrib'691'_1924 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1924 ~v0 ~v1 v2 = du_distrib'691'_1924 v2
du_distrib'691'_1924 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_1924 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.distribˡ
d_distrib'737'_1926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_1926 ~v0 ~v1 v2 = du_distrib'737'_1926 v2
du_distrib'737'_1926 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_1926 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.isEquivalence
d_isEquivalence_1928 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1928 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0))))))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.isPartialEquivalence
d_isPartialEquivalence_1930 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1930 ~v0 ~v1 v2
  = du_isPartialEquivalence_1930 v2
du_isPartialEquivalence_1930 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1930 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.refl
d_refl_1932 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny
d_refl_1932 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0)))))))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.reflexive
d_reflexive_1934 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1934 ~v0 ~v1 v2 = du_reflexive_1934 v2
du_reflexive_1934 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1934 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.setoid
d_setoid_1936 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1936 ~v0 ~v1 v2 = du_setoid_1936 v2
du_setoid_1936 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1936 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1868 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.sym
d_sym_1938 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1938 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0)))))))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.trans
d_trans_1940 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1940 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0)))))))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero.rawSemiring
d_rawSemiring_1942 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_1942 ~v0 ~v1 v2 = du_rawSemiring_1942 v2
du_rawSemiring_1942 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_1942 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawSemiring'46'constructor_2023
      (d__'43'__1860 (coe v0)) (d__'42'__1862 (coe v0))
      (d_0'35'_1864 (coe v0)) (d_1'35'_1866 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.rawNearSemiring
d_rawNearSemiring_1946 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
d_rawNearSemiring_1946 ~v0 ~v1 v2 = du_rawNearSemiring_1946 v2
du_rawNearSemiring_1946 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
du_rawNearSemiring_1946 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
      (coe du_rawSemiring_1942 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero.+-commutativeMonoid
d_'43''45'commutativeMonoid_1948 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_1948 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_1948 v2
du_'43''45'commutativeMonoid_1948 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_1948 v0
  = coe
      C_CommutativeMonoid'46'constructor_15055 (d__'43'__1860 (coe v0))
      (d_0'35'_1864 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0)))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._._≉_
d__'8777'__1952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1952 = erased
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.commutativeMagma
d_commutativeMagma_1954 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> T_CommutativeMagma_120
d_commutativeMagma_1954 ~v0 ~v1 v2 = du_commutativeMagma_1954 v2
du_commutativeMagma_1954 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> T_CommutativeMagma_120
du_commutativeMagma_1954 v0
  = let v1 = coe du_'43''45'commutativeMonoid_1948 (coe v0) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v1))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.commutativeSemigroup
d_commutativeSemigroup_1956 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  T_CommutativeSemigroup_602
d_commutativeSemigroup_1956 ~v0 ~v1 v2
  = du_commutativeSemigroup_1956 v2
du_commutativeSemigroup_1956 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  T_CommutativeSemigroup_602
du_commutativeSemigroup_1956 v0
  = coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1948 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.magma
d_magma_1958 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Magma_8
d_magma_1958 ~v0 ~v1 v2 = du_magma_1958 v2
du_magma_1958 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Magma_8
du_magma_1958 v0
  = let v1 = coe du_'43''45'commutativeMonoid_1948 (coe v0) in
    let v2 = coe du_monoid_890 (coe v1) in
    coe du_magma_524 (coe du_semigroup_802 (coe v2))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.monoid
d_monoid_1960 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Monoid_740
d_monoid_1960 ~v0 ~v1 v2 = du_monoid_1960 v2
du_monoid_1960 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Monoid_740
du_monoid_1960 v0
  = coe
      du_monoid_890 (coe du_'43''45'commutativeMonoid_1948 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.rawMagma
d_rawMagma_1962 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1962 ~v0 ~v1 v2 = du_rawMagma_1962 v2
du_rawMagma_1962 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1962 v0
  = let v1 = coe du_'43''45'commutativeMonoid_1948 (coe v0) in
    let v2 = coe du_monoid_890 (coe v1) in
    let v3 = coe du_semigroup_802 (coe v2) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v3))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.rawMonoid
d_rawMonoid_1964 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_1964 ~v0 ~v1 v2 = du_rawMonoid_1964 v2
du_rawMonoid_1964 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_1964 v0
  = let v1 = coe du_'43''45'commutativeMonoid_1948 (coe v0) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v1))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.semigroup
d_semigroup_1966 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Semigroup_476
d_semigroup_1966 ~v0 ~v1 v2 = du_semigroup_1966 v2
du_semigroup_1966 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Semigroup_476
du_semigroup_1966 v0
  = let v1 = coe du_'43''45'commutativeMonoid_1948 (coe v0) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v1))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.unitalMagma
d_unitalMagma_1968 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> T_UnitalMagma_672
d_unitalMagma_1968 ~v0 ~v1 v2 = du_unitalMagma_1968 v2
du_unitalMagma_1968 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> T_UnitalMagma_672
du_unitalMagma_1968 v0
  = let v1 = coe du_'43''45'commutativeMonoid_1948 (coe v0) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v1))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero.*-monoid
d_'42''45'monoid_1970 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Monoid_740
d_'42''45'monoid_1970 ~v0 ~v1 v2 = du_'42''45'monoid_1970 v2
du_'42''45'monoid_1970 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Monoid_740
du_'42''45'monoid_1970 v0
  = coe
      C_Monoid'46'constructor_13309 (d__'42'__1862 (coe v0))
      (d_1'35'_1866 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
         (coe d_isSemiringWithoutAnnihilatingZero_1868 (coe v0)))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.magma
d_magma_1974 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Magma_8
d_magma_1974 ~v0 ~v1 v2 = du_magma_1974 v2
du_magma_1974 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Magma_8
du_magma_1974 v0
  = let v1 = coe du_'42''45'monoid_1970 (coe v0) in
    coe du_magma_524 (coe du_semigroup_802 (coe v1))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.rawMagma
d_rawMagma_1976 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_1976 ~v0 ~v1 v2 = du_rawMagma_1976 v2
du_rawMagma_1976 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_1976 v0
  = let v1 = coe du_'42''45'monoid_1970 (coe v0) in
    let v2 = coe du_semigroup_802 (coe v1) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v2))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.rawMonoid
d_rawMonoid_1978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_1978 ~v0 ~v1 v2 = du_rawMonoid_1978 v2
du_rawMonoid_1978 ::
  T_SemiringWithoutAnnihilatingZero_1836 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_1978 v0
  = coe du_rawMonoid_812 (coe du_'42''45'monoid_1970 (coe v0))
-- Algebra.Bundles.SemiringWithoutAnnihilatingZero._.semigroup
d_semigroup_1980 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Semigroup_476
d_semigroup_1980 ~v0 ~v1 v2 = du_semigroup_1980 v2
du_semigroup_1980 ::
  T_SemiringWithoutAnnihilatingZero_1836 -> T_Semigroup_476
du_semigroup_1980 v0
  = coe du_semigroup_802 (coe du_'42''45'monoid_1970 (coe v0))
-- Algebra.Bundles.Semiring
d_Semiring_1986 a0 a1 = ()
data T_Semiring_1986
  = C_Semiring'46'constructor_35691 (AgdaAny -> AgdaAny -> AgdaAny)
                                    (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
                                    MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
-- Algebra.Bundles.Semiring.Carrier
d_Carrier_2006 :: T_Semiring_1986 -> ()
d_Carrier_2006 = erased
-- Algebra.Bundles.Semiring._≈_
d__'8776'__2008 :: T_Semiring_1986 -> AgdaAny -> AgdaAny -> ()
d__'8776'__2008 = erased
-- Algebra.Bundles.Semiring._+_
d__'43'__2010 :: T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__2010 v0
  = case coe v0 of
      C_Semiring'46'constructor_35691 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Semiring._*_
d__'42'__2012 :: T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__2012 v0
  = case coe v0 of
      C_Semiring'46'constructor_35691 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Semiring.0#
d_0'35'_2014 :: T_Semiring_1986 -> AgdaAny
d_0'35'_2014 v0
  = case coe v0 of
      C_Semiring'46'constructor_35691 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Semiring.1#
d_1'35'_2016 :: T_Semiring_1986 -> AgdaAny
d_1'35'_2016 v0
  = case coe v0 of
      C_Semiring'46'constructor_35691 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Semiring.isSemiring
d_isSemiring_2018 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_2018 v0
  = case coe v0 of
      C_Semiring'46'constructor_35691 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Semiring._.*-assoc
d_'42''45'assoc_2022 ::
  T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2022 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_2018 (coe v0)))
-- Algebra.Bundles.Semiring._.*-cong
d_'42''45'cong_2024 ::
  T_Semiring_1986 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2024 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_2018 (coe v0)))
-- Algebra.Bundles.Semiring._.∙-congʳ
d_'8729''45'cong'691'_2026 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2026 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2026 v2
du_'8729''45'cong'691'_2026 ::
  T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2026 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.Semiring._.∙-congˡ
d_'8729''45'cong'737'_2028 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2028 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2028 v2
du_'8729''45'cong'737'_2028 ::
  T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2028 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.Semiring._.*-identity
d_'42''45'identity_2030 ::
  T_Semiring_1986 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2030 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_2018 (coe v0)))
-- Algebra.Bundles.Semiring._.identityʳ
d_identity'691'_2032 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> AgdaAny -> AgdaAny
d_identity'691'_2032 ~v0 ~v1 v2 = du_identity'691'_2032 v2
du_identity'691'_2032 :: T_Semiring_1986 -> AgdaAny -> AgdaAny
du_identity'691'_2032 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v2))
-- Algebra.Bundles.Semiring._.identityˡ
d_identity'737'_2034 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> AgdaAny -> AgdaAny
d_identity'737'_2034 ~v0 ~v1 v2 = du_identity'737'_2034 v2
du_identity'737'_2034 :: T_Semiring_1986 -> AgdaAny -> AgdaAny
du_identity'737'_2034 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v2))
-- Algebra.Bundles.Semiring._.*-isMagma
d_'42''45'isMagma_2036 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_2036 ~v0 ~v1 v2 = du_'42''45'isMagma_2036 v2
du_'42''45'isMagma_2036 ::
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_2036 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v1))
-- Algebra.Bundles.Semiring._.*-isMonoid
d_'42''45'isMonoid_2038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_2038 ~v0 ~v1 v2 = du_'42''45'isMonoid_2038 v2
du_'42''45'isMonoid_2038 ::
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_2038 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v1))
-- Algebra.Bundles.Semiring._.*-isSemigroup
d_'42''45'isSemigroup_2040 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2040 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_2040 v2
du_'42''45'isSemigroup_2040 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_2040 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v1))
-- Algebra.Bundles.Semiring._.assoc
d_assoc_2042 ::
  T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2042 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe d_isSemiring_2018 (coe v0))))))
-- Algebra.Bundles.Semiring._.comm
d_comm_2044 :: T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2044 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe d_isSemiring_2018 (coe v0))))
-- Algebra.Bundles.Semiring._.∙-cong
d_'8729''45'cong_2046 ::
  T_Semiring_1986 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2046 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe d_isSemiring_2018 (coe v0)))))))
-- Algebra.Bundles.Semiring._.∙-congʳ
d_'8729''45'cong'691'_2048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2048 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2048 v2
du_'8729''45'cong'691'_2048 ::
  T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2048 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.Semiring._.∙-congˡ
d_'8729''45'cong'737'_2050 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2050 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2050 v2
du_'8729''45'cong'737'_2050 ::
  T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2050 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.Semiring._.identity
d_identity_2052 ::
  T_Semiring_1986 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2052 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe d_isSemiring_2018 (coe v0)))))
-- Algebra.Bundles.Semiring._.identityʳ
d_identity'691'_2054 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> AgdaAny -> AgdaAny
d_identity'691'_2054 ~v0 ~v1 v2 = du_identity'691'_2054 v2
du_identity'691'_2054 :: T_Semiring_1986 -> AgdaAny -> AgdaAny
du_identity'691'_2054 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v3))
-- Algebra.Bundles.Semiring._.identityˡ
d_identity'737'_2056 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> AgdaAny -> AgdaAny
d_identity'737'_2056 ~v0 ~v1 v2 = du_identity'737'_2056 v2
du_identity'737'_2056 :: T_Semiring_1986 -> AgdaAny -> AgdaAny
du_identity'737'_2056 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v3))
-- Algebra.Bundles.Semiring._.isCommutativeMagma
d_isCommutativeMagma_2058 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_2058 ~v0 ~v1 v2
  = du_isCommutativeMagma_2058 v2
du_isCommutativeMagma_2058 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_2058 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v3))
-- Algebra.Bundles.Semiring._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2060 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2060 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_2018 (coe v0)))
-- Algebra.Bundles.Semiring._.isCommutativeSemigroup
d_isCommutativeSemigroup_2062 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2062 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_2062 v2
du_isCommutativeSemigroup_2062 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2062 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v2))
-- Algebra.Bundles.Semiring._.isMagma
d_isMagma_2064 ::
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2064 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe d_isSemiring_2018 (coe v0))))))
-- Algebra.Bundles.Semiring._.isMonoid
d_isMonoid_2066 ::
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2066 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe d_isSemiring_2018 (coe v0))))
-- Algebra.Bundles.Semiring._.isSemigroup
d_isSemigroup_2068 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2068 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe d_isSemiring_2018 (coe v0)))))
-- Algebra.Bundles.Semiring._.isUnitalMagma
d_isUnitalMagma_2070 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_2070 ~v0 ~v1 v2 = du_isUnitalMagma_2070 v2
du_isUnitalMagma_2070 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_2070 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v3))
-- Algebra.Bundles.Semiring._.distrib
d_distrib_2072 ::
  T_Semiring_1986 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2072 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_2018 (coe v0)))
-- Algebra.Bundles.Semiring._.distribʳ
d_distrib'691'_2074 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2074 ~v0 ~v1 v2 = du_distrib'691'_2074 v2
du_distrib'691'_2074 ::
  T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_2074 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v1))
-- Algebra.Bundles.Semiring._.distribˡ
d_distrib'737'_2076 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_2076 ~v0 ~v1 v2 = du_distrib'737'_2076 v2
du_distrib'737'_2076 ::
  T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_2076 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v1))
-- Algebra.Bundles.Semiring._.isEquivalence
d_isEquivalence_2078 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2078 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe d_isSemiring_2018 (coe v0)))))))
-- Algebra.Bundles.Semiring._.isNearSemiring
d_isNearSemiring_2080 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_2080 ~v0 ~v1 v2 = du_isNearSemiring_2080 v2
du_isNearSemiring_2080 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_2080 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v1))
-- Algebra.Bundles.Semiring._.isPartialEquivalence
d_isPartialEquivalence_2082 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2082 ~v0 ~v1 v2
  = du_isPartialEquivalence_2082 v2
du_isPartialEquivalence_2082 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2082 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v6))
-- Algebra.Bundles.Semiring._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2084 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2084 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe d_isSemiring_2018 (coe v0))
-- Algebra.Bundles.Semiring._.isSemiringWithoutOne
d_isSemiringWithoutOne_2086 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_2086 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_2086 v2
du_isSemiringWithoutOne_2086 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_2086 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe d_isSemiring_2018 (coe v0))
-- Algebra.Bundles.Semiring._.refl
d_refl_2088 :: T_Semiring_1986 -> AgdaAny -> AgdaAny
d_refl_2088 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe d_isSemiring_2018 (coe v0))))))))
-- Algebra.Bundles.Semiring._.reflexive
d_reflexive_2090 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2090 ~v0 ~v1 v2 = du_reflexive_2090 v2
du_reflexive_2090 ::
  T_Semiring_1986 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2090 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v6))
        v7
-- Algebra.Bundles.Semiring._.setoid
d_setoid_2092 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2092 ~v0 ~v1 v2 = du_setoid_2092 v2
du_setoid_2092 ::
  T_Semiring_1986 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2092 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.Semiring._.sym
d_sym_2094 ::
  T_Semiring_1986 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2094 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe d_isSemiring_2018 (coe v0))))))))
-- Algebra.Bundles.Semiring._.trans
d_trans_2096 ::
  T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2096 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe d_isSemiring_2018 (coe v0))))))))
-- Algebra.Bundles.Semiring._.zero
d_zero_2098 ::
  T_Semiring_1986 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2098 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe d_isSemiring_2018 (coe v0))
-- Algebra.Bundles.Semiring._.zeroʳ
d_zero'691'_2100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> AgdaAny -> AgdaAny
d_zero'691'_2100 ~v0 ~v1 v2 = du_zero'691'_2100 v2
du_zero'691'_2100 :: T_Semiring_1986 -> AgdaAny -> AgdaAny
du_zero'691'_2100 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v1))
-- Algebra.Bundles.Semiring._.zeroˡ
d_zero'737'_2102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> AgdaAny -> AgdaAny
d_zero'737'_2102 ~v0 ~v1 v2 = du_zero'737'_2102 v2
du_zero'737'_2102 :: T_Semiring_1986 -> AgdaAny -> AgdaAny
du_zero'737'_2102 v0
  = let v1 = d_isSemiring_2018 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v1))
-- Algebra.Bundles.Semiring.semiringWithoutAnnihilatingZero
d_semiringWithoutAnnihilatingZero_2104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_SemiringWithoutAnnihilatingZero_1836
d_semiringWithoutAnnihilatingZero_2104 ~v0 ~v1 v2
  = du_semiringWithoutAnnihilatingZero_2104 v2
du_semiringWithoutAnnihilatingZero_2104 ::
  T_Semiring_1986 -> T_SemiringWithoutAnnihilatingZero_1836
du_semiringWithoutAnnihilatingZero_2104 v0
  = coe
      C_SemiringWithoutAnnihilatingZero'46'constructor_32973
      (d__'43'__2010 (coe v0)) (d__'42'__2012 (coe v0))
      (d_0'35'_2014 (coe v0)) (d_1'35'_2016 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_2018 (coe v0)))
-- Algebra.Bundles.Semiring._._≉_
d__'8777'__2108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> AgdaAny -> AgdaAny -> ()
d__'8777'__2108 = erased
-- Algebra.Bundles.Semiring._.magma
d_magma_2110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_Magma_8
d_magma_2110 ~v0 ~v1 v2 = du_magma_2110 v2
du_magma_2110 :: T_Semiring_1986 -> T_Magma_8
du_magma_2110 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    let v2 = coe du_'42''45'monoid_1970 (coe v1) in
    coe du_magma_524 (coe du_semigroup_802 (coe v2))
-- Algebra.Bundles.Semiring._.*-monoid
d_'42''45'monoid_2112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_Monoid_740
d_'42''45'monoid_2112 ~v0 ~v1 v2 = du_'42''45'monoid_2112 v2
du_'42''45'monoid_2112 :: T_Semiring_1986 -> T_Monoid_740
du_'42''45'monoid_2112 v0
  = coe
      du_'42''45'monoid_1970
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v0))
-- Algebra.Bundles.Semiring._.rawMagma
d_rawMagma_2114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2114 ~v0 ~v1 v2 = du_rawMagma_2114 v2
du_rawMagma_2114 ::
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2114 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    let v2 = coe du_'42''45'monoid_1970 (coe v1) in
    let v3 = coe du_semigroup_802 (coe v2) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v3))
-- Algebra.Bundles.Semiring._.rawMonoid
d_rawMonoid_2116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2116 ~v0 ~v1 v2 = du_rawMonoid_2116 v2
du_rawMonoid_2116 ::
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2116 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    coe du_rawMonoid_812 (coe du_'42''45'monoid_1970 (coe v1))
-- Algebra.Bundles.Semiring._.semigroup
d_semigroup_2118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_Semigroup_476
d_semigroup_2118 ~v0 ~v1 v2 = du_semigroup_2118 v2
du_semigroup_2118 :: T_Semiring_1986 -> T_Semigroup_476
du_semigroup_2118 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    coe du_semigroup_802 (coe du_'42''45'monoid_1970 (coe v1))
-- Algebra.Bundles.Semiring._.commutativeMagma
d_commutativeMagma_2120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_CommutativeMagma_120
d_commutativeMagma_2120 ~v0 ~v1 v2 = du_commutativeMagma_2120 v2
du_commutativeMagma_2120 ::
  T_Semiring_1986 -> T_CommutativeMagma_120
du_commutativeMagma_2120 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    let v2 = coe du_'43''45'commutativeMonoid_1948 (coe v1) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v2))
-- Algebra.Bundles.Semiring._.+-commutativeMonoid
d_'43''45'commutativeMonoid_2122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_2122 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_2122 v2
du_'43''45'commutativeMonoid_2122 ::
  T_Semiring_1986 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_2122 v0
  = coe
      du_'43''45'commutativeMonoid_1948
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v0))
-- Algebra.Bundles.Semiring._.commutativeSemigroup
d_commutativeSemigroup_2124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_2124 ~v0 ~v1 v2
  = du_commutativeSemigroup_2124 v2
du_commutativeSemigroup_2124 ::
  T_Semiring_1986 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_2124 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1948 (coe v1))
-- Algebra.Bundles.Semiring._.magma
d_magma_2126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_Magma_8
d_magma_2126 ~v0 ~v1 v2 = du_magma_2126 v2
du_magma_2126 :: T_Semiring_1986 -> T_Magma_8
du_magma_2126 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    let v2 = coe du_'43''45'commutativeMonoid_1948 (coe v1) in
    let v3 = coe du_monoid_890 (coe v2) in
    coe du_magma_524 (coe du_semigroup_802 (coe v3))
-- Algebra.Bundles.Semiring._.monoid
d_monoid_2128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_Monoid_740
d_monoid_2128 ~v0 ~v1 v2 = du_monoid_2128 v2
du_monoid_2128 :: T_Semiring_1986 -> T_Monoid_740
du_monoid_2128 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    coe du_monoid_890 (coe du_'43''45'commutativeMonoid_1948 (coe v1))
-- Algebra.Bundles.Semiring._.rawMagma
d_rawMagma_2130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2130 ~v0 ~v1 v2 = du_rawMagma_2130 v2
du_rawMagma_2130 ::
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2130 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    let v2 = coe du_'43''45'commutativeMonoid_1948 (coe v1) in
    let v3 = coe du_monoid_890 (coe v2) in
    let v4 = coe du_semigroup_802 (coe v3) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v4))
-- Algebra.Bundles.Semiring._.rawMonoid
d_rawMonoid_2132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2132 ~v0 ~v1 v2 = du_rawMonoid_2132 v2
du_rawMonoid_2132 ::
  T_Semiring_1986 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2132 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    let v2 = coe du_'43''45'commutativeMonoid_1948 (coe v1) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v2))
-- Algebra.Bundles.Semiring._.semigroup
d_semigroup_2134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_Semigroup_476
d_semigroup_2134 ~v0 ~v1 v2 = du_semigroup_2134 v2
du_semigroup_2134 :: T_Semiring_1986 -> T_Semigroup_476
du_semigroup_2134 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    let v2 = coe du_'43''45'commutativeMonoid_1948 (coe v1) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v2))
-- Algebra.Bundles.Semiring._.unitalMagma
d_unitalMagma_2136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_UnitalMagma_672
d_unitalMagma_2136 ~v0 ~v1 v2 = du_unitalMagma_2136 v2
du_unitalMagma_2136 :: T_Semiring_1986 -> T_UnitalMagma_672
du_unitalMagma_2136 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    let v2 = coe du_'43''45'commutativeMonoid_1948 (coe v1) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v2))
-- Algebra.Bundles.Semiring._.rawNearSemiring
d_rawNearSemiring_2138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
d_rawNearSemiring_2138 ~v0 ~v1 v2 = du_rawNearSemiring_2138 v2
du_rawNearSemiring_2138 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
du_rawNearSemiring_2138 v0
  = let v1 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
      (coe du_rawSemiring_1942 (coe v1))
-- Algebra.Bundles.Semiring._.rawSemiring
d_rawSemiring_2140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_2140 ~v0 ~v1 v2 = du_rawSemiring_2140 v2
du_rawSemiring_2140 ::
  T_Semiring_1986 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_2140 v0
  = coe
      du_rawSemiring_1942
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v0))
-- Algebra.Bundles.Semiring.semiringWithoutOne
d_semiringWithoutOne_2142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_SemiringWithoutOne_1622
d_semiringWithoutOne_2142 ~v0 ~v1 v2
  = du_semiringWithoutOne_2142 v2
du_semiringWithoutOne_2142 ::
  T_Semiring_1986 -> T_SemiringWithoutOne_1622
du_semiringWithoutOne_2142 v0
  = coe
      C_SemiringWithoutOne'46'constructor_29099 (d__'43'__2010 (coe v0))
      (d__'42'__2012 (coe v0)) (d_0'35'_2014 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe d_isSemiring_2018 (coe v0)))
-- Algebra.Bundles.Semiring._.nearSemiring
d_nearSemiring_2146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semiring_1986 -> T_NearSemiring_1508
d_nearSemiring_2146 ~v0 ~v1 v2 = du_nearSemiring_2146 v2
du_nearSemiring_2146 :: T_Semiring_1986 -> T_NearSemiring_1508
du_nearSemiring_2146 v0
  = coe
      du_nearSemiring_1688 (coe du_semiringWithoutOne_2142 (coe v0))
-- Algebra.Bundles.CommutativeSemiring
d_CommutativeSemiring_2152 a0 a1 = ()
data T_CommutativeSemiring_2152
  = C_CommutativeSemiring'46'constructor_38603 (AgdaAny ->
                                                AgdaAny -> AgdaAny)
                                               (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
                                               MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
-- Algebra.Bundles.CommutativeSemiring.Carrier
d_Carrier_2172 :: T_CommutativeSemiring_2152 -> ()
d_Carrier_2172 = erased
-- Algebra.Bundles.CommutativeSemiring._≈_
d__'8776'__2174 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny -> ()
d__'8776'__2174 = erased
-- Algebra.Bundles.CommutativeSemiring._+_
d__'43'__2176 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__2176 v0
  = case coe v0 of
      C_CommutativeSemiring'46'constructor_38603 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemiring._*_
d__'42'__2178 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__2178 v0
  = case coe v0 of
      C_CommutativeSemiring'46'constructor_38603 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemiring.0#
d_0'35'_2180 :: T_CommutativeSemiring_2152 -> AgdaAny
d_0'35'_2180 v0
  = case coe v0 of
      C_CommutativeSemiring'46'constructor_38603 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemiring.1#
d_1'35'_2182 :: T_CommutativeSemiring_2152 -> AgdaAny
d_1'35'_2182 v0
  = case coe v0 of
      C_CommutativeSemiring'46'constructor_38603 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemiring.isCommutativeSemiring
d_isCommutativeSemiring_2184 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_2184 v0
  = case coe v0 of
      C_CommutativeSemiring'46'constructor_38603 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeSemiring._.*-assoc
d_'42''45'assoc_2188 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2188 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_2184 (coe v0))))
-- Algebra.Bundles.CommutativeSemiring._.*-comm
d_'42''45'comm_2190 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_2190 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe d_isCommutativeSemiring_2184 (coe v0))
-- Algebra.Bundles.CommutativeSemiring._.*-cong
d_'42''45'cong_2192 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2192 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_2184 (coe v0))))
-- Algebra.Bundles.CommutativeSemiring._.∙-congʳ
d_'8729''45'cong'691'_2194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2194 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2194 v2
du_'8729''45'cong'691'_2194 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2194 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.CommutativeSemiring._.∙-congˡ
d_'8729''45'cong'737'_2196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2196 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2196 v2
du_'8729''45'cong'737'_2196 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2196 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.CommutativeSemiring._.*-identity
d_'42''45'identity_2198 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2198 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_2184 (coe v0))))
-- Algebra.Bundles.CommutativeSemiring._.identityʳ
d_identity'691'_2200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
d_identity'691'_2200 ~v0 ~v1 v2 = du_identity'691'_2200 v2
du_identity'691'_2200 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
du_identity'691'_2200 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v3))
-- Algebra.Bundles.CommutativeSemiring._.identityˡ
d_identity'737'_2202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
d_identity'737'_2202 ~v0 ~v1 v2 = du_identity'737'_2202 v2
du_identity'737'_2202 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
du_identity'737'_2202 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v3))
-- Algebra.Bundles.CommutativeSemiring._.isCommutativeMagma
d_isCommutativeMagma_2204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_2204 ~v0 ~v1 v2
  = du_isCommutativeMagma_2204 v2
du_isCommutativeMagma_2204 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_2204 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_2206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_2206 ~v0 ~v1 v2
  = du_'42''45'isCommutativeMonoid_2206 v2
du_'42''45'isCommutativeMonoid_2206 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_2206 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_2184 (coe v0))
-- Algebra.Bundles.CommutativeSemiring._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_2208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_2208 ~v0 ~v1 v2
  = du_'42''45'isCommutativeSemigroup_2208 v2
du_'42''45'isCommutativeSemigroup_2208 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_2208 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v1))
-- Algebra.Bundles.CommutativeSemiring._.*-isMagma
d_'42''45'isMagma_2210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_2210 ~v0 ~v1 v2 = du_'42''45'isMagma_2210 v2
du_'42''45'isMagma_2210 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_2210 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.*-isMonoid
d_'42''45'isMonoid_2212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_2212 ~v0 ~v1 v2 = du_'42''45'isMonoid_2212 v2
du_'42''45'isMonoid_2212 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_2212 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.*-isSemigroup
d_'42''45'isSemigroup_2214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2214 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_2214 v2
du_'42''45'isSemigroup_2214 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_2214 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.assoc
d_assoc_2216 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2216 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                     (coe d_isCommutativeSemiring_2184 (coe v0)))))))
-- Algebra.Bundles.CommutativeSemiring._.comm
d_comm_2218 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2218 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe d_isCommutativeSemiring_2184 (coe v0)))))
-- Algebra.Bundles.CommutativeSemiring._.∙-cong
d_'8729''45'cong_2220 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2220 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                        (coe d_isCommutativeSemiring_2184 (coe v0))))))))
-- Algebra.Bundles.CommutativeSemiring._.∙-congʳ
d_'8729''45'cong'691'_2222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2222 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2222 v2
du_'8729''45'cong'691'_2222 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2222 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.CommutativeSemiring._.∙-congˡ
d_'8729''45'cong'737'_2224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2224 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2224 v2
du_'8729''45'cong'737'_2224 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2224 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.CommutativeSemiring._.identity
d_identity_2226 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2226 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                  (coe d_isCommutativeSemiring_2184 (coe v0))))))
-- Algebra.Bundles.CommutativeSemiring._.identityʳ
d_identity'691'_2228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
d_identity'691'_2228 ~v0 ~v1 v2 = du_identity'691'_2228 v2
du_identity'691'_2228 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
du_identity'691'_2228 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4))
-- Algebra.Bundles.CommutativeSemiring._.identityˡ
d_identity'737'_2230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
d_identity'737'_2230 ~v0 ~v1 v2 = du_identity'737'_2230 v2
du_identity'737'_2230 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
du_identity'737'_2230 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4))
-- Algebra.Bundles.CommutativeSemiring._.isCommutativeMagma
d_isCommutativeMagma_2232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_2232 ~v0 ~v1 v2
  = du_isCommutativeMagma_2232 v2
du_isCommutativeMagma_2232 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_2232 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v4))
-- Algebra.Bundles.CommutativeSemiring._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2234 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2234 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_2184 (coe v0))))
-- Algebra.Bundles.CommutativeSemiring._.isCommutativeSemigroup
d_isCommutativeSemigroup_2236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2236 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_2236 v2
du_isCommutativeSemigroup_2236 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2236 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v3))
-- Algebra.Bundles.CommutativeSemiring._.isMagma
d_isMagma_2238 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2238 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                     (coe d_isCommutativeSemiring_2184 (coe v0)))))))
-- Algebra.Bundles.CommutativeSemiring._.isMonoid
d_isMonoid_2240 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe d_isCommutativeSemiring_2184 (coe v0)))))
-- Algebra.Bundles.CommutativeSemiring._.isSemigroup
d_isSemigroup_2242 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2242 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                  (coe d_isCommutativeSemiring_2184 (coe v0))))))
-- Algebra.Bundles.CommutativeSemiring._.isUnitalMagma
d_isUnitalMagma_2244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_2244 ~v0 ~v1 v2 = du_isUnitalMagma_2244 v2
du_isUnitalMagma_2244 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_2244 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4))
-- Algebra.Bundles.CommutativeSemiring._.distrib
d_distrib_2246 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2246 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_2184 (coe v0))))
-- Algebra.Bundles.CommutativeSemiring._.distribʳ
d_distrib'691'_2248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2248 ~v0 ~v1 v2 = du_distrib'691'_2248 v2
du_distrib'691'_2248 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_2248 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.distribˡ
d_distrib'737'_2250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_2250 ~v0 ~v1 v2 = du_distrib'737'_2250 v2
du_distrib'737'_2250 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_2250 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_2252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_2252 ~v0 ~v1 v2
  = du_isCommutativeSemiringWithoutOne_2252 v2
du_isCommutativeSemiringWithoutOne_2252 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_2252 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_2184 (coe v0))
-- Algebra.Bundles.CommutativeSemiring._.isEquivalence
d_isEquivalence_2254 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                        (coe d_isCommutativeSemiring_2184 (coe v0))))))))
-- Algebra.Bundles.CommutativeSemiring._.isNearSemiring
d_isNearSemiring_2256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_2256 ~v0 ~v1 v2 = du_isNearSemiring_2256 v2
du_isNearSemiring_2256 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_2256 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.isPartialEquivalence
d_isPartialEquivalence_2258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2258 ~v0 ~v1 v2
  = du_isPartialEquivalence_2258 v2
du_isPartialEquivalence_2258 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2258 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v7))
-- Algebra.Bundles.CommutativeSemiring._.isSemiring
d_isSemiring_2260 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_2260 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe d_isCommutativeSemiring_2184 (coe v0))
-- Algebra.Bundles.CommutativeSemiring._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2262 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2262 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe d_isCommutativeSemiring_2184 (coe v0)))
-- Algebra.Bundles.CommutativeSemiring._.isSemiringWithoutOne
d_isSemiringWithoutOne_2264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_2264 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_2264 v2
du_isSemiringWithoutOne_2264 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_2264 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1))
-- Algebra.Bundles.CommutativeSemiring._.refl
d_refl_2266 :: T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
d_refl_2266 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                           (coe d_isCommutativeSemiring_2184 (coe v0)))))))))
-- Algebra.Bundles.CommutativeSemiring._.reflexive
d_reflexive_2268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2268 ~v0 ~v1 v2 = du_reflexive_2268 v2
du_reflexive_2268 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2268 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6) in
    \ v8 v9 v10 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v7))
        v8
-- Algebra.Bundles.CommutativeSemiring._.setoid
d_setoid_2270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2270 ~v0 ~v1 v2 = du_setoid_2270 v2
du_setoid_2270 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2270 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.CommutativeSemiring._.sym
d_sym_2272 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2272 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                           (coe d_isCommutativeSemiring_2184 (coe v0)))))))))
-- Algebra.Bundles.CommutativeSemiring._.trans
d_trans_2274 ::
  T_CommutativeSemiring_2152 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2274 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                           (coe d_isCommutativeSemiring_2184 (coe v0)))))))))
-- Algebra.Bundles.CommutativeSemiring._.zero
d_zero_2276 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2276 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe d_isCommutativeSemiring_2184 (coe v0)))
-- Algebra.Bundles.CommutativeSemiring._.zeroʳ
d_zero'691'_2278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
d_zero'691'_2278 ~v0 ~v1 v2 = du_zero'691'_2278 v2
du_zero'691'_2278 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
du_zero'691'_2278 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.zeroˡ
d_zero'737'_2280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
d_zero'737'_2280 ~v0 ~v1 v2 = du_zero'737'_2280 v2
du_zero'737'_2280 ::
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny
du_zero'737'_2280 v0
  = let v1 = d_isCommutativeSemiring_2184 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Algebra.Bundles.CommutativeSemiring.semiring
d_semiring_2282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_Semiring_1986
d_semiring_2282 ~v0 ~v1 v2 = du_semiring_2282 v2
du_semiring_2282 :: T_CommutativeSemiring_2152 -> T_Semiring_1986
du_semiring_2282 v0
  = coe
      C_Semiring'46'constructor_35691 (d__'43'__2176 (coe v0))
      (d__'42'__2178 (coe v0)) (d_0'35'_2180 (coe v0))
      (d_1'35'_2182 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe d_isCommutativeSemiring_2184 (coe v0)))
-- Algebra.Bundles.CommutativeSemiring._._≉_
d__'8777'__2286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> AgdaAny -> AgdaAny -> ()
d__'8777'__2286 = erased
-- Algebra.Bundles.CommutativeSemiring._.magma
d_magma_2288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_Magma_8
d_magma_2288 ~v0 ~v1 v2 = du_magma_2288 v2
du_magma_2288 :: T_CommutativeSemiring_2152 -> T_Magma_8
du_magma_2288 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'42''45'monoid_1970 (coe v2) in
    coe du_magma_524 (coe du_semigroup_802 (coe v3))
-- Algebra.Bundles.CommutativeSemiring._.*-monoid
d_'42''45'monoid_2290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_Monoid_740
d_'42''45'monoid_2290 ~v0 ~v1 v2 = du_'42''45'monoid_2290 v2
du_'42''45'monoid_2290 ::
  T_CommutativeSemiring_2152 -> T_Monoid_740
du_'42''45'monoid_2290 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    coe
      du_'42''45'monoid_1970
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v1))
-- Algebra.Bundles.CommutativeSemiring._.rawMagma
d_rawMagma_2292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2292 ~v0 ~v1 v2 = du_rawMagma_2292 v2
du_rawMagma_2292 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2292 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'42''45'monoid_1970 (coe v2) in
    let v4 = coe du_semigroup_802 (coe v3) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v4))
-- Algebra.Bundles.CommutativeSemiring._.rawMonoid
d_rawMonoid_2294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2294 ~v0 ~v1 v2 = du_rawMonoid_2294 v2
du_rawMonoid_2294 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2294 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe du_rawMonoid_812 (coe du_'42''45'monoid_1970 (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.semigroup
d_semigroup_2296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_Semigroup_476
d_semigroup_2296 ~v0 ~v1 v2 = du_semigroup_2296 v2
du_semigroup_2296 :: T_CommutativeSemiring_2152 -> T_Semigroup_476
du_semigroup_2296 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe du_semigroup_802 (coe du_'42''45'monoid_1970 (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.commutativeMagma
d_commutativeMagma_2298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_CommutativeMagma_120
d_commutativeMagma_2298 ~v0 ~v1 v2 = du_commutativeMagma_2298 v2
du_commutativeMagma_2298 ::
  T_CommutativeSemiring_2152 -> T_CommutativeMagma_120
du_commutativeMagma_2298 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v3))
-- Algebra.Bundles.CommutativeSemiring._.+-commutativeMonoid
d_'43''45'commutativeMonoid_2300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_2300 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_2300 v2
du_'43''45'commutativeMonoid_2300 ::
  T_CommutativeSemiring_2152 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_2300 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    coe
      du_'43''45'commutativeMonoid_1948
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v1))
-- Algebra.Bundles.CommutativeSemiring._.commutativeSemigroup
d_commutativeSemigroup_2302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_2302 ~v0 ~v1 v2
  = du_commutativeSemigroup_2302 v2
du_commutativeSemigroup_2302 ::
  T_CommutativeSemiring_2152 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_2302 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1948 (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.magma
d_magma_2304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_Magma_8
d_magma_2304 ~v0 ~v1 v2 = du_magma_2304 v2
du_magma_2304 :: T_CommutativeSemiring_2152 -> T_Magma_8
du_magma_2304 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    let v4 = coe du_monoid_890 (coe v3) in
    coe du_magma_524 (coe du_semigroup_802 (coe v4))
-- Algebra.Bundles.CommutativeSemiring._.monoid
d_monoid_2306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_Monoid_740
d_monoid_2306 ~v0 ~v1 v2 = du_monoid_2306 v2
du_monoid_2306 :: T_CommutativeSemiring_2152 -> T_Monoid_740
du_monoid_2306 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe du_monoid_890 (coe du_'43''45'commutativeMonoid_1948 (coe v2))
-- Algebra.Bundles.CommutativeSemiring._.rawMagma
d_rawMagma_2308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2308 ~v0 ~v1 v2 = du_rawMagma_2308 v2
du_rawMagma_2308 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2308 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    let v4 = coe du_monoid_890 (coe v3) in
    let v5 = coe du_semigroup_802 (coe v4) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v5))
-- Algebra.Bundles.CommutativeSemiring._.rawMonoid
d_rawMonoid_2310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2310 ~v0 ~v1 v2 = du_rawMonoid_2310 v2
du_rawMonoid_2310 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2310 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v3))
-- Algebra.Bundles.CommutativeSemiring._.semigroup
d_semigroup_2312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_Semigroup_476
d_semigroup_2312 ~v0 ~v1 v2 = du_semigroup_2312 v2
du_semigroup_2312 :: T_CommutativeSemiring_2152 -> T_Semigroup_476
du_semigroup_2312 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v3))
-- Algebra.Bundles.CommutativeSemiring._.unitalMagma
d_unitalMagma_2314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_UnitalMagma_672
d_unitalMagma_2314 ~v0 ~v1 v2 = du_unitalMagma_2314 v2
du_unitalMagma_2314 ::
  T_CommutativeSemiring_2152 -> T_UnitalMagma_672
du_unitalMagma_2314 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v3))
-- Algebra.Bundles.CommutativeSemiring._.nearSemiring
d_nearSemiring_2316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_NearSemiring_1508
d_nearSemiring_2316 ~v0 ~v1 v2 = du_nearSemiring_2316 v2
du_nearSemiring_2316 ::
  T_CommutativeSemiring_2152 -> T_NearSemiring_1508
du_nearSemiring_2316 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    coe du_nearSemiring_1688 (coe du_semiringWithoutOne_2142 (coe v1))
-- Algebra.Bundles.CommutativeSemiring._.rawSemiring
d_rawSemiring_2318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_2318 ~v0 ~v1 v2 = du_rawSemiring_2318 v2
du_rawSemiring_2318 ::
  T_CommutativeSemiring_2152 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_2318 v0
  = let v1 = coe du_semiring_2282 (coe v0) in
    coe
      du_rawSemiring_1942
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v1))
-- Algebra.Bundles.CommutativeSemiring._.semiringWithoutAnnihilatingZero
d_semiringWithoutAnnihilatingZero_2320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 ->
  T_SemiringWithoutAnnihilatingZero_1836
d_semiringWithoutAnnihilatingZero_2320 ~v0 ~v1 v2
  = du_semiringWithoutAnnihilatingZero_2320 v2
du_semiringWithoutAnnihilatingZero_2320 ::
  T_CommutativeSemiring_2152 ->
  T_SemiringWithoutAnnihilatingZero_1836
du_semiringWithoutAnnihilatingZero_2320 v0
  = coe
      du_semiringWithoutAnnihilatingZero_2104
      (coe du_semiring_2282 (coe v0))
-- Algebra.Bundles.CommutativeSemiring._.semiringWithoutOne
d_semiringWithoutOne_2322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_SemiringWithoutOne_1622
d_semiringWithoutOne_2322 ~v0 ~v1 v2
  = du_semiringWithoutOne_2322 v2
du_semiringWithoutOne_2322 ::
  T_CommutativeSemiring_2152 -> T_SemiringWithoutOne_1622
du_semiringWithoutOne_2322 v0
  = coe du_semiringWithoutOne_2142 (coe du_semiring_2282 (coe v0))
-- Algebra.Bundles.CommutativeSemiring.*-commutativeMonoid
d_'42''45'commutativeMonoid_2324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_2324 ~v0 ~v1 v2
  = du_'42''45'commutativeMonoid_2324 v2
du_'42''45'commutativeMonoid_2324 ::
  T_CommutativeSemiring_2152 -> T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_2324 v0
  = coe
      C_CommutativeMonoid'46'constructor_15055 (d__'42'__2178 (coe v0))
      (d_1'35'_2182 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
         (coe d_isCommutativeSemiring_2184 (coe v0)))
-- Algebra.Bundles.CommutativeSemiring._.commutativeMagma
d_commutativeMagma_2328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_CommutativeMagma_120
d_commutativeMagma_2328 ~v0 ~v1 v2 = du_commutativeMagma_2328 v2
du_commutativeMagma_2328 ::
  T_CommutativeSemiring_2152 -> T_CommutativeMagma_120
du_commutativeMagma_2328 v0
  = let v1 = coe du_'42''45'commutativeMonoid_2324 (coe v0) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v1))
-- Algebra.Bundles.CommutativeSemiring._.commutativeSemigroup
d_commutativeSemigroup_2330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_2330 ~v0 ~v1 v2
  = du_commutativeSemigroup_2330 v2
du_commutativeSemigroup_2330 ::
  T_CommutativeSemiring_2152 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_2330 v0
  = coe
      du_commutativeSemigroup_906
      (coe du_'42''45'commutativeMonoid_2324 (coe v0))
-- Algebra.Bundles.CommutativeSemiring.commutativeSemiringWithoutOne
d_commutativeSemiringWithoutOne_2332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeSemiring_2152 -> T_CommutativeSemiringWithoutOne_1726
d_commutativeSemiringWithoutOne_2332 ~v0 ~v1 v2
  = du_commutativeSemiringWithoutOne_2332 v2
du_commutativeSemiringWithoutOne_2332 ::
  T_CommutativeSemiring_2152 -> T_CommutativeSemiringWithoutOne_1726
du_commutativeSemiringWithoutOne_2332 v0
  = coe
      C_CommutativeSemiringWithoutOne'46'constructor_31105
      (d__'43'__2176 (coe v0)) (d__'42'__2178 (coe v0))
      (d_0'35'_2180 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe d_isCommutativeSemiring_2184 (coe v0)))
-- Algebra.Bundles.CancellativeCommutativeSemiring
d_CancellativeCommutativeSemiring_2338 a0 a1 = ()
data T_CancellativeCommutativeSemiring_2338
  = C_CancellativeCommutativeSemiring'46'constructor_41937 (AgdaAny ->
                                                            AgdaAny -> AgdaAny)
                                                           (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
                                                           AgdaAny
                                                           MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600
-- Algebra.Bundles.CancellativeCommutativeSemiring.Carrier
d_Carrier_2358 :: T_CancellativeCommutativeSemiring_2338 -> ()
d_Carrier_2358 = erased
-- Algebra.Bundles.CancellativeCommutativeSemiring._≈_
d__'8776'__2360 ::
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny -> ()
d__'8776'__2360 = erased
-- Algebra.Bundles.CancellativeCommutativeSemiring._+_
d__'43'__2362 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43'__2362 v0
  = case coe v0 of
      C_CancellativeCommutativeSemiring'46'constructor_41937 v3 v4 v5 v6 v7
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CancellativeCommutativeSemiring._*_
d__'42'__2364 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__2364 v0
  = case coe v0 of
      C_CancellativeCommutativeSemiring'46'constructor_41937 v3 v4 v5 v6 v7
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CancellativeCommutativeSemiring.0#
d_0'35'_2366 :: T_CancellativeCommutativeSemiring_2338 -> AgdaAny
d_0'35'_2366 v0
  = case coe v0 of
      C_CancellativeCommutativeSemiring'46'constructor_41937 v3 v4 v5 v6 v7
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CancellativeCommutativeSemiring.1#
d_1'35'_2368 :: T_CancellativeCommutativeSemiring_2338 -> AgdaAny
d_1'35'_2368 v0
  = case coe v0 of
      C_CancellativeCommutativeSemiring'46'constructor_41937 v3 v4 v5 v6 v7
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CancellativeCommutativeSemiring.isCancellativeCommutativeSemiring
d_isCancellativeCommutativeSemiring_2370 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCancellativeCommutativeSemiring_1600
d_isCancellativeCommutativeSemiring_2370 v0
  = case coe v0 of
      C_CancellativeCommutativeSemiring'46'constructor_41937 v3 v4 v5 v6 v7
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-assoc
d_'42''45'assoc_2374 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2374 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-cancelˡ-nonZero
d_'42''45'cancel'737''45'nonZero_2376 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> AgdaAny
d_'42''45'cancel'737''45'nonZero_2376 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cancel'737''45'nonZero_1616
      (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-comm
d_'42''45'comm_2378 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_2378 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-cong
d_'42''45'cong_2380 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2380 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.∙-congʳ
d_'8729''45'cong'691'_2382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2382 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2382 v2
du_'8729''45'cong'691'_2382 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2382 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.∙-congˡ
d_'8729''45'cong'737'_2384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2384 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2384 v2
du_'8729''45'cong'737'_2384 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2384 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-identity
d_'42''45'identity_2386 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2386 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.identityʳ
d_identity'691'_2388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
d_identity'691'_2388 ~v0 ~v1 v2 = du_identity'691'_2388 v2
du_identity'691'_2388 ::
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
du_identity'691'_2388 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.identityˡ
d_identity'737'_2390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
d_identity'737'_2390 ~v0 ~v1 v2 = du_identity'737'_2390 v2
du_identity'737'_2390 ::
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
du_identity'737'_2390 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isCommutativeMagma
d_isCommutativeMagma_2392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_2392 ~v0 ~v1 v2
  = du_isCommutativeMagma_2392 v2
du_isCommutativeMagma_2392 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_2392 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_2394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_2394 ~v0 ~v1 v2
  = du_'42''45'isCommutativeMonoid_2394 v2
du_'42''45'isCommutativeMonoid_2394 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_2394 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe v1))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_2396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_2396 ~v0 ~v1 v2
  = du_'42''45'isCommutativeSemigroup_2396 v2
du_'42''45'isCommutativeSemigroup_2396 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_2396 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-isMagma
d_'42''45'isMagma_2398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_2398 ~v0 ~v1 v2 = du_'42''45'isMagma_2398 v2
du_'42''45'isMagma_2398 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_2398 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-isMonoid
d_'42''45'isMonoid_2400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_2400 ~v0 ~v1 v2 = du_'42''45'isMonoid_2400 v2
du_'42''45'isMonoid_2400 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_2400 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-isSemigroup
d_'42''45'isSemigroup_2402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2402 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_2402 v2
du_'42''45'isSemigroup_2402 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_2402 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.assoc
d_assoc_2404 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2404 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                        (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.comm
d_comm_2406 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_2406 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                  (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.∙-cong
d_'8729''45'cong_2408 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2408 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                           (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.∙-congʳ
d_'8729''45'cong'691'_2410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2410 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2410 v2
du_'8729''45'cong'691'_2410 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2410 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.∙-congˡ
d_'8729''45'cong'737'_2412 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2412 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2412 v2
du_'8729''45'cong'737'_2412 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2412 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.identity
d_identity_2414 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2414 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                     (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.identityʳ
d_identity'691'_2416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
d_identity'691'_2416 ~v0 ~v1 v2 = du_identity'691'_2416 v2
du_identity'691'_2416 ::
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
du_identity'691'_2416 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.identityˡ
d_identity'737'_2418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
d_identity'737'_2418 ~v0 ~v1 v2 = du_identity'737'_2418 v2
du_identity'737'_2418 ::
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
du_identity'737'_2418 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isCommutativeMagma
d_isCommutativeMagma_2420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_2420 ~v0 ~v1 v2
  = du_isCommutativeMagma_2420 v2
du_isCommutativeMagma_2420 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_2420 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v5))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2422 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2422 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isCommutativeSemigroup
d_isCommutativeSemigroup_2424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2424 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_2424 v2
du_isCommutativeSemigroup_2424 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2424 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v4))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isMagma
d_isMagma_2426 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2426 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                        (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isMonoid
d_isMonoid_2428 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2428 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                  (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isSemigroup
d_isSemigroup_2430 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2430 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                     (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isUnitalMagma
d_isUnitalMagma_2432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_2432 ~v0 ~v1 v2 = du_isUnitalMagma_2432 v2
du_isUnitalMagma_2432 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_2432 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.distrib
d_distrib_2434 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2434 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
               (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.distribʳ
d_distrib'691'_2436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2436 ~v0 ~v1 v2 = du_distrib'691'_2436 v2
du_distrib'691'_2436 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_2436 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.distribˡ
d_distrib'737'_2438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_2438 ~v0 ~v1 v2 = du_distrib'737'_2438 v2
du_distrib'737'_2438 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_2438 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isCommutativeSemiring
d_isCommutativeSemiring_2440 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_2440 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
      (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_2442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_2442 ~v0 ~v1 v2
  = du_isCommutativeSemiringWithoutOne_2442 v2
du_isCommutativeSemiringWithoutOne_2442 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_2442 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe v1))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isEquivalence
d_isEquivalence_2444 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2444 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                           (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isNearSemiring
d_isNearSemiring_2446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_2446 ~v0 ~v1 v2 = du_isNearSemiring_2446 v2
du_isNearSemiring_2446 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_2446 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isPartialEquivalence
d_isPartialEquivalence_2448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2448 ~v0 ~v1 v2
  = du_isPartialEquivalence_2448 v2
du_isPartialEquivalence_2448 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2448 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v8))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isSemiring
d_isSemiring_2450 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_2450 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2452 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2452 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.isSemiringWithoutOne
d_isSemiringWithoutOne_2454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_2454 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_2454 v2
du_isSemiringWithoutOne_2454 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_2454 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.refl
d_refl_2456 ::
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
d_refl_2456 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                              (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))))))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.reflexive
d_reflexive_2458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2458 ~v0 ~v1 v2 = du_reflexive_2458 v2
du_reflexive_2458 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2458 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7) in
    \ v9 v10 v11 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v8))
        v9
-- Algebra.Bundles.CancellativeCommutativeSemiring._.setoid
d_setoid_2460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2460 ~v0 ~v1 v2 = du_setoid_2460 v2
du_setoid_2460 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2460 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.sym
d_sym_2462 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2462 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                              (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))))))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.trans
d_trans_2464 ::
  T_CancellativeCommutativeSemiring_2338 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2464 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
                              (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))))))))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.zero
d_zero_2466 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2466 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
            (coe d_isCancellativeCommutativeSemiring_2370 (coe v0))))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.zeroʳ
d_zero'691'_2468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
d_zero'691'_2468 ~v0 ~v1 v2 = du_zero'691'_2468 v2
du_zero'691'_2468 ::
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
du_zero'691'_2468 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.zeroˡ
d_zero'737'_2470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
d_zero'737'_2470 ~v0 ~v1 v2 = du_zero'737'_2470 v2
du_zero'737'_2470 ::
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny
du_zero'737'_2470 v0
  = let v1 = d_isCancellativeCommutativeSemiring_2370 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring.commutativeSemiring
d_commutativeSemiring_2472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  T_CommutativeSemiring_2152
d_commutativeSemiring_2472 ~v0 ~v1 v2
  = du_commutativeSemiring_2472 v2
du_commutativeSemiring_2472 ::
  T_CancellativeCommutativeSemiring_2338 ->
  T_CommutativeSemiring_2152
du_commutativeSemiring_2472 v0
  = coe
      C_CommutativeSemiring'46'constructor_38603 (d__'43'__2362 (coe v0))
      (d__'42'__2364 (coe v0)) (d_0'35'_2366 (coe v0))
      (d_1'35'_2368 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isCommutativeSemiring_1614
         (coe d_isCancellativeCommutativeSemiring_2370 (coe v0)))
-- Algebra.Bundles.CancellativeCommutativeSemiring._._≉_
d__'8777'__2476 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> AgdaAny -> AgdaAny -> ()
d__'8777'__2476 = erased
-- Algebra.Bundles.CancellativeCommutativeSemiring._.commutativeMagma
d_commutativeMagma_2478 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_CommutativeMagma_120
d_commutativeMagma_2478 ~v0 ~v1 v2 = du_commutativeMagma_2478 v2
du_commutativeMagma_2478 ::
  T_CancellativeCommutativeSemiring_2338 -> T_CommutativeMagma_120
du_commutativeMagma_2478 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_'42''45'commutativeMonoid_2324 (coe v1) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v2))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-commutativeMonoid
d_'42''45'commutativeMonoid_2480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_2480 ~v0 ~v1 v2
  = du_'42''45'commutativeMonoid_2480 v2
du_'42''45'commutativeMonoid_2480 ::
  T_CancellativeCommutativeSemiring_2338 -> T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_2480 v0
  = coe
      du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_2472 (coe v0))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.commutativeSemigroup
d_commutativeSemigroup_2482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  T_CommutativeSemigroup_602
d_commutativeSemigroup_2482 ~v0 ~v1 v2
  = du_commutativeSemigroup_2482 v2
du_commutativeSemigroup_2482 ::
  T_CancellativeCommutativeSemiring_2338 ->
  T_CommutativeSemigroup_602
du_commutativeSemigroup_2482 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    coe
      du_commutativeSemigroup_906
      (coe du_'42''45'commutativeMonoid_2324 (coe v1))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.magma
d_magma_2484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_Magma_8
d_magma_2484 ~v0 ~v1 v2 = du_magma_2484 v2
du_magma_2484 ::
  T_CancellativeCommutativeSemiring_2338 -> T_Magma_8
du_magma_2484 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'42''45'monoid_1970 (coe v3) in
    coe du_magma_524 (coe du_semigroup_802 (coe v4))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.*-monoid
d_'42''45'monoid_2486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_Monoid_740
d_'42''45'monoid_2486 ~v0 ~v1 v2 = du_'42''45'monoid_2486 v2
du_'42''45'monoid_2486 ::
  T_CancellativeCommutativeSemiring_2338 -> T_Monoid_740
du_'42''45'monoid_2486 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    coe
      du_'42''45'monoid_1970
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v2))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.rawMagma
d_rawMagma_2488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2488 ~v0 ~v1 v2 = du_rawMagma_2488 v2
du_rawMagma_2488 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2488 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'42''45'monoid_1970 (coe v3) in
    let v5 = coe du_semigroup_802 (coe v4) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v5))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.rawMonoid
d_rawMonoid_2490 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2490 ~v0 ~v1 v2 = du_rawMonoid_2490 v2
du_rawMonoid_2490 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2490 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe du_rawMonoid_812 (coe du_'42''45'monoid_1970 (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.semigroup
d_semigroup_2492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_Semigroup_476
d_semigroup_2492 ~v0 ~v1 v2 = du_semigroup_2492 v2
du_semigroup_2492 ::
  T_CancellativeCommutativeSemiring_2338 -> T_Semigroup_476
du_semigroup_2492 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe du_semigroup_802 (coe du_'42''45'monoid_1970 (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.commutativeMagma
d_commutativeMagma_2494 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_CommutativeMagma_120
d_commutativeMagma_2494 ~v0 ~v1 v2 = du_commutativeMagma_2494 v2
du_commutativeMagma_2494 ::
  T_CancellativeCommutativeSemiring_2338 -> T_CommutativeMagma_120
du_commutativeMagma_2494 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v4))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.+-commutativeMonoid
d_'43''45'commutativeMonoid_2496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_2496 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_2496 v2
du_'43''45'commutativeMonoid_2496 ::
  T_CancellativeCommutativeSemiring_2338 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_2496 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    coe
      du_'43''45'commutativeMonoid_1948
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v2))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.commutativeSemigroup
d_commutativeSemigroup_2498 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  T_CommutativeSemigroup_602
d_commutativeSemigroup_2498 ~v0 ~v1 v2
  = du_commutativeSemigroup_2498 v2
du_commutativeSemigroup_2498 ::
  T_CancellativeCommutativeSemiring_2338 ->
  T_CommutativeSemigroup_602
du_commutativeSemigroup_2498 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1948 (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.magma
d_magma_2500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_Magma_8
d_magma_2500 ~v0 ~v1 v2 = du_magma_2500 v2
du_magma_2500 ::
  T_CancellativeCommutativeSemiring_2338 -> T_Magma_8
du_magma_2500 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    let v5 = coe du_monoid_890 (coe v4) in
    coe du_magma_524 (coe du_semigroup_802 (coe v5))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.monoid
d_monoid_2502 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_Monoid_740
d_monoid_2502 ~v0 ~v1 v2 = du_monoid_2502 v2
du_monoid_2502 ::
  T_CancellativeCommutativeSemiring_2338 -> T_Monoid_740
du_monoid_2502 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe du_monoid_890 (coe du_'43''45'commutativeMonoid_1948 (coe v3))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.rawMagma
d_rawMagma_2504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2504 ~v0 ~v1 v2 = du_rawMagma_2504 v2
du_rawMagma_2504 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2504 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    let v5 = coe du_monoid_890 (coe v4) in
    let v6 = coe du_semigroup_802 (coe v5) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v6))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.rawMonoid
d_rawMonoid_2506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2506 ~v0 ~v1 v2 = du_rawMonoid_2506 v2
du_rawMonoid_2506 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2506 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v4))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.semigroup
d_semigroup_2508 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_Semigroup_476
d_semigroup_2508 ~v0 ~v1 v2 = du_semigroup_2508 v2
du_semigroup_2508 ::
  T_CancellativeCommutativeSemiring_2338 -> T_Semigroup_476
du_semigroup_2508 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v4))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.unitalMagma
d_unitalMagma_2510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_UnitalMagma_672
d_unitalMagma_2510 ~v0 ~v1 v2 = du_unitalMagma_2510 v2
du_unitalMagma_2510 ::
  T_CancellativeCommutativeSemiring_2338 -> T_UnitalMagma_672
du_unitalMagma_2510 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v4))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.nearSemiring
d_nearSemiring_2512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_NearSemiring_1508
d_nearSemiring_2512 ~v0 ~v1 v2 = du_nearSemiring_2512 v2
du_nearSemiring_2512 ::
  T_CancellativeCommutativeSemiring_2338 -> T_NearSemiring_1508
du_nearSemiring_2512 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    coe du_nearSemiring_1688 (coe du_semiringWithoutOne_2142 (coe v2))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.rawSemiring
d_rawSemiring_2514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_2514 ~v0 ~v1 v2 = du_rawSemiring_2514 v2
du_rawSemiring_2514 ::
  T_CancellativeCommutativeSemiring_2338 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_2514 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    coe
      du_rawSemiring_1942
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v2))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.semiring
d_semiring_2516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_Semiring_1986
d_semiring_2516 ~v0 ~v1 v2 = du_semiring_2516 v2
du_semiring_2516 ::
  T_CancellativeCommutativeSemiring_2338 -> T_Semiring_1986
du_semiring_2516 v0
  = coe du_semiring_2282 (coe du_commutativeSemiring_2472 (coe v0))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.semiringWithoutAnnihilatingZero
d_semiringWithoutAnnihilatingZero_2518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 ->
  T_SemiringWithoutAnnihilatingZero_1836
d_semiringWithoutAnnihilatingZero_2518 ~v0 ~v1 v2
  = du_semiringWithoutAnnihilatingZero_2518 v2
du_semiringWithoutAnnihilatingZero_2518 ::
  T_CancellativeCommutativeSemiring_2338 ->
  T_SemiringWithoutAnnihilatingZero_1836
du_semiringWithoutAnnihilatingZero_2518 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    coe
      du_semiringWithoutAnnihilatingZero_2104
      (coe du_semiring_2282 (coe v1))
-- Algebra.Bundles.CancellativeCommutativeSemiring._.semiringWithoutOne
d_semiringWithoutOne_2520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CancellativeCommutativeSemiring_2338 -> T_SemiringWithoutOne_1622
d_semiringWithoutOne_2520 ~v0 ~v1 v2
  = du_semiringWithoutOne_2520 v2
du_semiringWithoutOne_2520 ::
  T_CancellativeCommutativeSemiring_2338 -> T_SemiringWithoutOne_1622
du_semiringWithoutOne_2520 v0
  = let v1 = coe du_commutativeSemiring_2472 (coe v0) in
    coe du_semiringWithoutOne_2142 (coe du_semiring_2282 (coe v1))
-- Algebra.Bundles.IdempotentSemiring
d_IdempotentSemiring_2526 a0 a1 = ()
data T_IdempotentSemiring_2526
  = C_IdempotentSemiring'46'constructor_44741 (AgdaAny ->
                                               AgdaAny -> AgdaAny)
                                              (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
                                              MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722
-- Algebra.Bundles.IdempotentSemiring.Carrier
d_Carrier_2546 :: T_IdempotentSemiring_2526 -> ()
d_Carrier_2546 = erased
-- Algebra.Bundles.IdempotentSemiring._≈_
d__'8776'__2548 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny -> ()
d__'8776'__2548 = erased
-- Algebra.Bundles.IdempotentSemiring._+_
d__'43'__2550 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__2550 v0
  = case coe v0 of
      C_IdempotentSemiring'46'constructor_44741 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentSemiring._*_
d__'42'__2552 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__2552 v0
  = case coe v0 of
      C_IdempotentSemiring'46'constructor_44741 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentSemiring.0#
d_0'35'_2554 :: T_IdempotentSemiring_2526 -> AgdaAny
d_0'35'_2554 v0
  = case coe v0 of
      C_IdempotentSemiring'46'constructor_44741 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentSemiring.1#
d_1'35'_2556 :: T_IdempotentSemiring_2526 -> AgdaAny
d_1'35'_2556 v0
  = case coe v0 of
      C_IdempotentSemiring'46'constructor_44741 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentSemiring.isIdempotentSemiring
d_isIdempotentSemiring_2558 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722
d_isIdempotentSemiring_2558 v0
  = case coe v0 of
      C_IdempotentSemiring'46'constructor_44741 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.IdempotentSemiring._.*-assoc
d_'42''45'assoc_2562 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2562 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe d_isIdempotentSemiring_2558 (coe v0))))
-- Algebra.Bundles.IdempotentSemiring._.*-cong
d_'42''45'cong_2564 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2564 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe d_isIdempotentSemiring_2558 (coe v0))))
-- Algebra.Bundles.IdempotentSemiring._.∙-congʳ
d_'8729''45'cong'691'_2566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2566 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2566 v2
du_'8729''45'cong'691'_2566 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2566 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.IdempotentSemiring._.∙-congˡ
d_'8729''45'cong'737'_2568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2568 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2568 v2
du_'8729''45'cong'737'_2568 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2568 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.IdempotentSemiring._.*-identity
d_'42''45'identity_2570 ::
  T_IdempotentSemiring_2526 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2570 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe d_isIdempotentSemiring_2558 (coe v0))))
-- Algebra.Bundles.IdempotentSemiring._.identityʳ
d_identity'691'_2572 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
d_identity'691'_2572 ~v0 ~v1 v2 = du_identity'691'_2572 v2
du_identity'691'_2572 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
du_identity'691'_2572 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v3))
-- Algebra.Bundles.IdempotentSemiring._.identityˡ
d_identity'737'_2574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
d_identity'737'_2574 ~v0 ~v1 v2 = du_identity'737'_2574 v2
du_identity'737'_2574 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
du_identity'737'_2574 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v3))
-- Algebra.Bundles.IdempotentSemiring._.*-isMagma
d_'42''45'isMagma_2576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_2576 ~v0 ~v1 v2 = du_'42''45'isMagma_2576 v2
du_'42''45'isMagma_2576 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_2576 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.*-isMonoid
d_'42''45'isMonoid_2578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_2578 ~v0 ~v1 v2 = du_'42''45'isMonoid_2578 v2
du_'42''45'isMonoid_2578 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_2578 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.*-isSemigroup
d_'42''45'isSemigroup_2580 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2580 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_2580 v2
du_'42''45'isSemigroup_2580 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_2580 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.assoc
d_assoc_2582 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2582 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                     (coe d_isIdempotentSemiring_2558 (coe v0)))))))
-- Algebra.Bundles.IdempotentSemiring._.comm
d_comm_2584 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2584 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
               (coe d_isIdempotentSemiring_2558 (coe v0)))))
-- Algebra.Bundles.IdempotentSemiring._.∙-cong
d_'8729''45'cong_2586 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2586 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                        (coe d_isIdempotentSemiring_2558 (coe v0))))))))
-- Algebra.Bundles.IdempotentSemiring._.∙-congʳ
d_'8729''45'cong'691'_2588 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2588 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2588 v2
du_'8729''45'cong'691'_2588 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2588 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.IdempotentSemiring._.∙-congˡ
d_'8729''45'cong'737'_2590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2590 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2590 v2
du_'8729''45'cong'737'_2590 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2590 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.IdempotentSemiring._.+-idem
d_'43''45'idem_2592 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
d_'43''45'idem_2592 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'idem_1738
      (coe d_isIdempotentSemiring_2558 (coe v0))
-- Algebra.Bundles.IdempotentSemiring._.identity
d_identity_2594 ::
  T_IdempotentSemiring_2526 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2594 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                  (coe d_isIdempotentSemiring_2558 (coe v0))))))
-- Algebra.Bundles.IdempotentSemiring._.identityʳ
d_identity'691'_2596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
d_identity'691'_2596 ~v0 ~v1 v2 = du_identity'691'_2596 v2
du_identity'691'_2596 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
du_identity'691'_2596 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4))
-- Algebra.Bundles.IdempotentSemiring._.identityˡ
d_identity'737'_2598 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
d_identity'737'_2598 ~v0 ~v1 v2 = du_identity'737'_2598 v2
du_identity'737'_2598 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
du_identity'737'_2598 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4))
-- Algebra.Bundles.IdempotentSemiring._.isCommutativeMagma
d_isCommutativeMagma_2600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_2600 ~v0 ~v1 v2
  = du_isCommutativeMagma_2600 v2
du_isCommutativeMagma_2600 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_2600 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v4))
-- Algebra.Bundles.IdempotentSemiring._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2602 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2602 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe d_isIdempotentSemiring_2558 (coe v0))))
-- Algebra.Bundles.IdempotentSemiring._.isCommutativeSemigroup
d_isCommutativeSemigroup_2604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2604 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_2604 v2
du_isCommutativeSemigroup_2604 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2604 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v3))
-- Algebra.Bundles.IdempotentSemiring._.isMagma
d_isMagma_2606 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2606 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                     (coe d_isIdempotentSemiring_2558 (coe v0)))))))
-- Algebra.Bundles.IdempotentSemiring._.isMonoid
d_isMonoid_2608 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2608 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
               (coe d_isIdempotentSemiring_2558 (coe v0)))))
-- Algebra.Bundles.IdempotentSemiring._.isSemigroup
d_isSemigroup_2610 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2610 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                  (coe d_isIdempotentSemiring_2558 (coe v0))))))
-- Algebra.Bundles.IdempotentSemiring._.isUnitalMagma
d_isUnitalMagma_2612 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_2612 ~v0 ~v1 v2 = du_isUnitalMagma_2612 v2
du_isUnitalMagma_2612 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_2612 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4))
-- Algebra.Bundles.IdempotentSemiring._.distrib
d_distrib_2614 ::
  T_IdempotentSemiring_2526 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2614 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe d_isIdempotentSemiring_2558 (coe v0))))
-- Algebra.Bundles.IdempotentSemiring._.distribʳ
d_distrib'691'_2616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2616 ~v0 ~v1 v2 = du_distrib'691'_2616 v2
du_distrib'691'_2616 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_2616 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.distribˡ
d_distrib'737'_2618 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_2618 ~v0 ~v1 v2 = du_distrib'737'_2618 v2
du_distrib'737'_2618 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_2618 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.isEquivalence
d_isEquivalence_2620 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2620 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                        (coe d_isIdempotentSemiring_2558 (coe v0))))))))
-- Algebra.Bundles.IdempotentSemiring._.isNearSemiring
d_isNearSemiring_2622 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_2622 ~v0 ~v1 v2 = du_isNearSemiring_2622 v2
du_isNearSemiring_2622 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_2622 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.isPartialEquivalence
d_isPartialEquivalence_2624 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2624 ~v0 ~v1 v2
  = du_isPartialEquivalence_2624 v2
du_isPartialEquivalence_2624 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2624 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v7))
-- Algebra.Bundles.IdempotentSemiring._.isSemiring
d_isSemiring_2626 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_2626 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
      (coe d_isIdempotentSemiring_2558 (coe v0))
-- Algebra.Bundles.IdempotentSemiring._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2628 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2628 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe d_isIdempotentSemiring_2558 (coe v0)))
-- Algebra.Bundles.IdempotentSemiring._.isSemiringWithoutOne
d_isSemiringWithoutOne_2630 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_2630 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_2630 v2
du_isSemiringWithoutOne_2630 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_2630 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1))
-- Algebra.Bundles.IdempotentSemiring._.refl
d_refl_2632 :: T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
d_refl_2632 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                           (coe d_isIdempotentSemiring_2558 (coe v0)))))))))
-- Algebra.Bundles.IdempotentSemiring._.reflexive
d_reflexive_2634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2634 ~v0 ~v1 v2 = du_reflexive_2634 v2
du_reflexive_2634 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2634 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6) in
    \ v8 v9 v10 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v7))
        v8
-- Algebra.Bundles.IdempotentSemiring._.setoid
d_setoid_2636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2636 ~v0 ~v1 v2 = du_setoid_2636 v2
du_setoid_2636 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2636 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.IdempotentSemiring._.sym
d_sym_2638 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2638 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                           (coe d_isIdempotentSemiring_2558 (coe v0)))))))))
-- Algebra.Bundles.IdempotentSemiring._.trans
d_trans_2640 ::
  T_IdempotentSemiring_2526 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2640 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                           (coe d_isIdempotentSemiring_2558 (coe v0)))))))))
-- Algebra.Bundles.IdempotentSemiring._.zero
d_zero_2642 ::
  T_IdempotentSemiring_2526 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe d_isIdempotentSemiring_2558 (coe v0)))
-- Algebra.Bundles.IdempotentSemiring._.zeroʳ
d_zero'691'_2644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
d_zero'691'_2644 ~v0 ~v1 v2 = du_zero'691'_2644 v2
du_zero'691'_2644 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
du_zero'691'_2644 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.zeroˡ
d_zero'737'_2646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
d_zero'737'_2646 ~v0 ~v1 v2 = du_zero'737'_2646 v2
du_zero'737'_2646 ::
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny
du_zero'737'_2646 v0
  = let v1 = d_isIdempotentSemiring_2558 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Algebra.Bundles.IdempotentSemiring.semiring
d_semiring_2648 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_Semiring_1986
d_semiring_2648 ~v0 ~v1 v2 = du_semiring_2648 v2
du_semiring_2648 :: T_IdempotentSemiring_2526 -> T_Semiring_1986
du_semiring_2648 v0
  = coe
      C_Semiring'46'constructor_35691 (d__'43'__2550 (coe v0))
      (d__'42'__2552 (coe v0)) (d_0'35'_2554 (coe v0))
      (d_1'35'_2556 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe d_isIdempotentSemiring_2558 (coe v0)))
-- Algebra.Bundles.IdempotentSemiring._._≉_
d__'8777'__2652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> AgdaAny -> AgdaAny -> ()
d__'8777'__2652 = erased
-- Algebra.Bundles.IdempotentSemiring._.magma
d_magma_2654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_Magma_8
d_magma_2654 ~v0 ~v1 v2 = du_magma_2654 v2
du_magma_2654 :: T_IdempotentSemiring_2526 -> T_Magma_8
du_magma_2654 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'42''45'monoid_1970 (coe v2) in
    coe du_magma_524 (coe du_semigroup_802 (coe v3))
-- Algebra.Bundles.IdempotentSemiring._.*-monoid
d_'42''45'monoid_2656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_Monoid_740
d_'42''45'monoid_2656 ~v0 ~v1 v2 = du_'42''45'monoid_2656 v2
du_'42''45'monoid_2656 :: T_IdempotentSemiring_2526 -> T_Monoid_740
du_'42''45'monoid_2656 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    coe
      du_'42''45'monoid_1970
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v1))
-- Algebra.Bundles.IdempotentSemiring._.rawMagma
d_rawMagma_2658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2658 ~v0 ~v1 v2 = du_rawMagma_2658 v2
du_rawMagma_2658 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2658 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'42''45'monoid_1970 (coe v2) in
    let v4 = coe du_semigroup_802 (coe v3) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v4))
-- Algebra.Bundles.IdempotentSemiring._.rawMonoid
d_rawMonoid_2660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2660 ~v0 ~v1 v2 = du_rawMonoid_2660 v2
du_rawMonoid_2660 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2660 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe du_rawMonoid_812 (coe du_'42''45'monoid_1970 (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.semigroup
d_semigroup_2662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_Semigroup_476
d_semigroup_2662 ~v0 ~v1 v2 = du_semigroup_2662 v2
du_semigroup_2662 :: T_IdempotentSemiring_2526 -> T_Semigroup_476
du_semigroup_2662 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe du_semigroup_802 (coe du_'42''45'monoid_1970 (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.commutativeMagma
d_commutativeMagma_2664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_CommutativeMagma_120
d_commutativeMagma_2664 ~v0 ~v1 v2 = du_commutativeMagma_2664 v2
du_commutativeMagma_2664 ::
  T_IdempotentSemiring_2526 -> T_CommutativeMagma_120
du_commutativeMagma_2664 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v3))
-- Algebra.Bundles.IdempotentSemiring._.+-commutativeMonoid
d_'43''45'commutativeMonoid_2666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_2666 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_2666 v2
du_'43''45'commutativeMonoid_2666 ::
  T_IdempotentSemiring_2526 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_2666 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    coe
      du_'43''45'commutativeMonoid_1948
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v1))
-- Algebra.Bundles.IdempotentSemiring._.commutativeSemigroup
d_commutativeSemigroup_2668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_2668 ~v0 ~v1 v2
  = du_commutativeSemigroup_2668 v2
du_commutativeSemigroup_2668 ::
  T_IdempotentSemiring_2526 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_2668 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1948 (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.magma
d_magma_2670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_Magma_8
d_magma_2670 ~v0 ~v1 v2 = du_magma_2670 v2
du_magma_2670 :: T_IdempotentSemiring_2526 -> T_Magma_8
du_magma_2670 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    let v4 = coe du_monoid_890 (coe v3) in
    coe du_magma_524 (coe du_semigroup_802 (coe v4))
-- Algebra.Bundles.IdempotentSemiring._.monoid
d_monoid_2672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_Monoid_740
d_monoid_2672 ~v0 ~v1 v2 = du_monoid_2672 v2
du_monoid_2672 :: T_IdempotentSemiring_2526 -> T_Monoid_740
du_monoid_2672 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe du_monoid_890 (coe du_'43''45'commutativeMonoid_1948 (coe v2))
-- Algebra.Bundles.IdempotentSemiring._.rawMagma
d_rawMagma_2674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2674 ~v0 ~v1 v2 = du_rawMagma_2674 v2
du_rawMagma_2674 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2674 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    let v4 = coe du_monoid_890 (coe v3) in
    let v5 = coe du_semigroup_802 (coe v4) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v5))
-- Algebra.Bundles.IdempotentSemiring._.rawMonoid
d_rawMonoid_2676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2676 ~v0 ~v1 v2 = du_rawMonoid_2676 v2
du_rawMonoid_2676 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2676 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v3))
-- Algebra.Bundles.IdempotentSemiring._.semigroup
d_semigroup_2678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_Semigroup_476
d_semigroup_2678 ~v0 ~v1 v2 = du_semigroup_2678 v2
du_semigroup_2678 :: T_IdempotentSemiring_2526 -> T_Semigroup_476
du_semigroup_2678 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v3))
-- Algebra.Bundles.IdempotentSemiring._.unitalMagma
d_unitalMagma_2680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_UnitalMagma_672
d_unitalMagma_2680 ~v0 ~v1 v2 = du_unitalMagma_2680 v2
du_unitalMagma_2680 ::
  T_IdempotentSemiring_2526 -> T_UnitalMagma_672
du_unitalMagma_2680 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v3))
-- Algebra.Bundles.IdempotentSemiring._.nearSemiring
d_nearSemiring_2682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_NearSemiring_1508
d_nearSemiring_2682 ~v0 ~v1 v2 = du_nearSemiring_2682 v2
du_nearSemiring_2682 ::
  T_IdempotentSemiring_2526 -> T_NearSemiring_1508
du_nearSemiring_2682 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    coe du_nearSemiring_1688 (coe du_semiringWithoutOne_2142 (coe v1))
-- Algebra.Bundles.IdempotentSemiring._.rawSemiring
d_rawSemiring_2684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_2684 ~v0 ~v1 v2 = du_rawSemiring_2684 v2
du_rawSemiring_2684 ::
  T_IdempotentSemiring_2526 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_2684 v0
  = let v1 = coe du_semiring_2648 (coe v0) in
    coe
      du_rawSemiring_1942
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v1))
-- Algebra.Bundles.IdempotentSemiring._.semiringWithoutAnnihilatingZero
d_semiringWithoutAnnihilatingZero_2686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_SemiringWithoutAnnihilatingZero_1836
d_semiringWithoutAnnihilatingZero_2686 ~v0 ~v1 v2
  = du_semiringWithoutAnnihilatingZero_2686 v2
du_semiringWithoutAnnihilatingZero_2686 ::
  T_IdempotentSemiring_2526 -> T_SemiringWithoutAnnihilatingZero_1836
du_semiringWithoutAnnihilatingZero_2686 v0
  = coe
      du_semiringWithoutAnnihilatingZero_2104
      (coe du_semiring_2648 (coe v0))
-- Algebra.Bundles.IdempotentSemiring._.semiringWithoutOne
d_semiringWithoutOne_2688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_IdempotentSemiring_2526 -> T_SemiringWithoutOne_1622
d_semiringWithoutOne_2688 ~v0 ~v1 v2
  = du_semiringWithoutOne_2688 v2
du_semiringWithoutOne_2688 ::
  T_IdempotentSemiring_2526 -> T_SemiringWithoutOne_1622
du_semiringWithoutOne_2688 v0
  = coe du_semiringWithoutOne_2142 (coe du_semiring_2648 (coe v0))
-- Algebra.Bundles.KleeneAlgebra
d_KleeneAlgebra_2694 a0 a1 = ()
data T_KleeneAlgebra_2694
  = C_KleeneAlgebra'46'constructor_47471 (AgdaAny ->
                                          AgdaAny -> AgdaAny)
                                         (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                         AgdaAny AgdaAny
                                         MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834
-- Algebra.Bundles.KleeneAlgebra.Carrier
d_Carrier_2716 :: T_KleeneAlgebra_2694 -> ()
d_Carrier_2716 = erased
-- Algebra.Bundles.KleeneAlgebra._≈_
d__'8776'__2718 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> ()
d__'8776'__2718 = erased
-- Algebra.Bundles.KleeneAlgebra._+_
d__'43'__2720 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__2720 v0
  = case coe v0 of
      C_KleeneAlgebra'46'constructor_47471 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.KleeneAlgebra._*_
d__'42'__2722 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__2722 v0
  = case coe v0 of
      C_KleeneAlgebra'46'constructor_47471 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.KleeneAlgebra._⋆
d__'8902'_2724 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d__'8902'_2724 v0
  = case coe v0 of
      C_KleeneAlgebra'46'constructor_47471 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.KleeneAlgebra.0#
d_0'35'_2726 :: T_KleeneAlgebra_2694 -> AgdaAny
d_0'35'_2726 v0
  = case coe v0 of
      C_KleeneAlgebra'46'constructor_47471 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.KleeneAlgebra.1#
d_1'35'_2728 :: T_KleeneAlgebra_2694 -> AgdaAny
d_1'35'_2728 v0
  = case coe v0 of
      C_KleeneAlgebra'46'constructor_47471 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.KleeneAlgebra.isKleeneAlgebra
d_isKleeneAlgebra_2730 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsKleeneAlgebra_1834
d_isKleeneAlgebra_2730 v0
  = case coe v0 of
      C_KleeneAlgebra'46'constructor_47471 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.KleeneAlgebra._.*-assoc
d_'42''45'assoc_2734 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2734 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe d_isKleeneAlgebra_2730 (coe v0)))))
-- Algebra.Bundles.KleeneAlgebra._.*-cong
d_'42''45'cong_2736 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2736 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe d_isKleeneAlgebra_2730 (coe v0)))))
-- Algebra.Bundles.KleeneAlgebra._.∙-congʳ
d_'8729''45'cong'691'_2738 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2738 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2738 v2
du_'8729''45'cong'691'_2738 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2738 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.KleeneAlgebra._.∙-congˡ
d_'8729''45'cong'737'_2740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2740 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2740 v2
du_'8729''45'cong'737'_2740 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2740 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.KleeneAlgebra._.*-identity
d_'42''45'identity_2742 ::
  T_KleeneAlgebra_2694 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2742 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe d_isKleeneAlgebra_2730 (coe v0)))))
-- Algebra.Bundles.KleeneAlgebra._.identityʳ
d_identity'691'_2744 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_identity'691'_2744 ~v0 ~v1 v2 = du_identity'691'_2744 v2
du_identity'691'_2744 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
du_identity'691'_2744 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Algebra.Bundles.KleeneAlgebra._.identityˡ
d_identity'737'_2746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_identity'737'_2746 ~v0 ~v1 v2 = du_identity'737'_2746 v2
du_identity'737'_2746 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
du_identity'737'_2746 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Algebra.Bundles.KleeneAlgebra._.*-isMagma
d_'42''45'isMagma_2748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_2748 ~v0 ~v1 v2 = du_'42''45'isMagma_2748 v2
du_'42''45'isMagma_2748 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_2748 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.*-isMonoid
d_'42''45'isMonoid_2750 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_2750 ~v0 ~v1 v2 = du_'42''45'isMonoid_2750 v2
du_'42''45'isMonoid_2750 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_2750 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.*-isSemigroup
d_'42''45'isSemigroup_2752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2752 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_2752 v2
du_'42''45'isSemigroup_2752 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_2752 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.assoc
d_assoc_2754 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2754 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                        (coe d_isKleeneAlgebra_2730 (coe v0))))))))
-- Algebra.Bundles.KleeneAlgebra._.comm
d_comm_2756 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2756 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                  (coe d_isKleeneAlgebra_2730 (coe v0))))))
-- Algebra.Bundles.KleeneAlgebra._.∙-cong
d_'8729''45'cong_2758 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2758 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                           (coe d_isKleeneAlgebra_2730 (coe v0)))))))))
-- Algebra.Bundles.KleeneAlgebra._.∙-congʳ
d_'8729''45'cong'691'_2760 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2760 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2760 v2
du_'8729''45'cong'691'_2760 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2760 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
-- Algebra.Bundles.KleeneAlgebra._.∙-congˡ
d_'8729''45'cong'737'_2762 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2762 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2762 v2
du_'8729''45'cong'737'_2762 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2762 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
-- Algebra.Bundles.KleeneAlgebra._.+-idem
d_'43''45'idem_2764 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_'43''45'idem_2764 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'idem_1738
      (coe
         MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
         (coe d_isKleeneAlgebra_2730 (coe v0)))
-- Algebra.Bundles.KleeneAlgebra._.identity
d_identity_2766 ::
  T_KleeneAlgebra_2694 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2766 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                     (coe d_isKleeneAlgebra_2730 (coe v0)))))))
-- Algebra.Bundles.KleeneAlgebra._.identityʳ
d_identity'691'_2768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_identity'691'_2768 ~v0 ~v1 v2 = du_identity'691'_2768 v2
du_identity'691'_2768 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
du_identity'691'_2768 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5))
-- Algebra.Bundles.KleeneAlgebra._.identityˡ
d_identity'737'_2770 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_identity'737'_2770 ~v0 ~v1 v2 = du_identity'737'_2770 v2
du_identity'737'_2770 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
du_identity'737'_2770 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5))
-- Algebra.Bundles.KleeneAlgebra._.isCommutativeMagma
d_isCommutativeMagma_2772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_2772 ~v0 ~v1 v2
  = du_isCommutativeMagma_2772 v2
du_isCommutativeMagma_2772 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_2772 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v5))
-- Algebra.Bundles.KleeneAlgebra._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2774 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2774 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe d_isKleeneAlgebra_2730 (coe v0)))))
-- Algebra.Bundles.KleeneAlgebra._.isCommutativeSemigroup
d_isCommutativeSemigroup_2776 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2776 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_2776 v2
du_isCommutativeSemigroup_2776 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2776 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v4))
-- Algebra.Bundles.KleeneAlgebra._.isMagma
d_isMagma_2778 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2778 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                        (coe d_isKleeneAlgebra_2730 (coe v0))))))))
-- Algebra.Bundles.KleeneAlgebra._.isMonoid
d_isMonoid_2780 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2780 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                  (coe d_isKleeneAlgebra_2730 (coe v0))))))
-- Algebra.Bundles.KleeneAlgebra._.isSemigroup
d_isSemigroup_2782 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2782 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                     (coe d_isKleeneAlgebra_2730 (coe v0)))))))
-- Algebra.Bundles.KleeneAlgebra._.isUnitalMagma
d_isUnitalMagma_2784 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_2784 ~v0 ~v1 v2 = du_isUnitalMagma_2784 v2
du_isUnitalMagma_2784 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_2784 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5))
-- Algebra.Bundles.KleeneAlgebra._.distrib
d_distrib_2786 ::
  T_KleeneAlgebra_2694 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2786 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
            (coe
               MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
               (coe d_isKleeneAlgebra_2730 (coe v0)))))
-- Algebra.Bundles.KleeneAlgebra._.distribʳ
d_distrib'691'_2788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2788 ~v0 ~v1 v2 = du_distrib'691'_2788 v2
du_distrib'691'_2788 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_2788 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.distribˡ
d_distrib'737'_2790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_2790 ~v0 ~v1 v2 = du_distrib'737'_2790 v2
du_distrib'737'_2790 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_2790 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.isEquivalence
d_isEquivalence_2792 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2792 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                           (coe d_isKleeneAlgebra_2730 (coe v0)))))))))
-- Algebra.Bundles.KleeneAlgebra._.isIdempotentSemiring
d_isIdempotentSemiring_2794 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentSemiring_1722
d_isIdempotentSemiring_2794 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
      (coe d_isKleeneAlgebra_2730 (coe v0))
-- Algebra.Bundles.KleeneAlgebra._.isNearSemiring
d_isNearSemiring_2796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_2796 ~v0 ~v1 v2 = du_isNearSemiring_2796 v2
du_isNearSemiring_2796 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_2796 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.isPartialEquivalence
d_isPartialEquivalence_2798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2798 ~v0 ~v1 v2
  = du_isPartialEquivalence_2798 v2
du_isPartialEquivalence_2798 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2798 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v8))
-- Algebra.Bundles.KleeneAlgebra._.isSemiring
d_isSemiring_2800 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_2800 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
      (coe
         MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
         (coe d_isKleeneAlgebra_2730 (coe v0)))
-- Algebra.Bundles.KleeneAlgebra._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2802 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2802 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe d_isKleeneAlgebra_2730 (coe v0))))
-- Algebra.Bundles.KleeneAlgebra._.isSemiringWithoutOne
d_isSemiringWithoutOne_2804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_2804 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_2804 v2
du_isSemiringWithoutOne_2804 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_2804 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2))
-- Algebra.Bundles.KleeneAlgebra._.refl
d_refl_2806 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_refl_2806 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                              (coe d_isKleeneAlgebra_2730 (coe v0))))))))))
-- Algebra.Bundles.KleeneAlgebra._.reflexive
d_reflexive_2808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2808 ~v0 ~v1 v2 = du_reflexive_2808 v2
du_reflexive_2808 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2808 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7) in
    \ v9 v10 v11 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v8))
        v9
-- Algebra.Bundles.KleeneAlgebra._.setoid
d_setoid_2810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2810 ~v0 ~v1 v2 = du_setoid_2810 v2
du_setoid_2810 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2810 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
-- Algebra.Bundles.KleeneAlgebra._.starDestructive
d_starDestructive_2812 ::
  T_KleeneAlgebra_2694 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starDestructive_2812 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_starDestructive_1856
      (coe d_isKleeneAlgebra_2730 (coe v0))
-- Algebra.Bundles.KleeneAlgebra._.starDestructiveʳ
d_starDestructive'691'_2814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_starDestructive'691'_2814 ~v0 ~v1 v2
  = du_starDestructive'691'_2814 v2
du_starDestructive'691'_2814 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_starDestructive'691'_2814 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_starDestructive'691'_1952
      (coe d_isKleeneAlgebra_2730 (coe v0))
-- Algebra.Bundles.KleeneAlgebra._.starDestructiveˡ
d_starDestructive'737'_2816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_starDestructive'737'_2816 ~v0 ~v1 v2
  = du_starDestructive'737'_2816 v2
du_starDestructive'737'_2816 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_starDestructive'737'_2816 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_starDestructive'737'_1950
      (coe d_isKleeneAlgebra_2730 (coe v0))
-- Algebra.Bundles.KleeneAlgebra._.starExpansive
d_starExpansive_2818 ::
  T_KleeneAlgebra_2694 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starExpansive_2818 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_starExpansive_1854
      (coe d_isKleeneAlgebra_2730 (coe v0))
-- Algebra.Bundles.KleeneAlgebra._.starExpansiveʳ
d_starExpansive'691'_2820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_starExpansive'691'_2820 ~v0 ~v1 v2
  = du_starExpansive'691'_2820 v2
du_starExpansive'691'_2820 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
du_starExpansive'691'_2820 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_starExpansive'691'_1948
      (coe d_isKleeneAlgebra_2730 (coe v0))
-- Algebra.Bundles.KleeneAlgebra._.starExpansiveˡ
d_starExpansive'737'_2822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_starExpansive'737'_2822 ~v0 ~v1 v2
  = du_starExpansive'737'_2822 v2
du_starExpansive'737'_2822 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
du_starExpansive'737'_2822 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_starExpansive'737'_1946
      (coe d_isKleeneAlgebra_2730 (coe v0))
-- Algebra.Bundles.KleeneAlgebra._.sym
d_sym_2824 ::
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2824 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                              (coe d_isKleeneAlgebra_2730 (coe v0))))))))))
-- Algebra.Bundles.KleeneAlgebra._.trans
d_trans_2826 ::
  T_KleeneAlgebra_2694 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2826 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
                              (coe d_isKleeneAlgebra_2730 (coe v0))))))))))
-- Algebra.Bundles.KleeneAlgebra._.zero
d_zero_2828 ::
  T_KleeneAlgebra_2694 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2828 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1736
         (coe
            MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
            (coe d_isKleeneAlgebra_2730 (coe v0))))
-- Algebra.Bundles.KleeneAlgebra._.zeroʳ
d_zero'691'_2830 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_zero'691'_2830 ~v0 ~v1 v2 = du_zero'691'_2830 v2
du_zero'691'_2830 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
du_zero'691'_2830 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.zeroˡ
d_zero'737'_2832 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
d_zero'737'_2832 ~v0 ~v1 v2 = du_zero'737'_2832 v2
du_zero'737'_2832 :: T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny
du_zero'737'_2832 v0
  = let v1 = d_isKleeneAlgebra_2730 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1736 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Bundles.KleeneAlgebra.idempotentSemiring
d_idempotentSemiring_2834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_IdempotentSemiring_2526
d_idempotentSemiring_2834 ~v0 ~v1 v2
  = du_idempotentSemiring_2834 v2
du_idempotentSemiring_2834 ::
  T_KleeneAlgebra_2694 -> T_IdempotentSemiring_2526
du_idempotentSemiring_2834 v0
  = coe
      C_IdempotentSemiring'46'constructor_44741 (d__'43'__2720 (coe v0))
      (d__'42'__2722 (coe v0)) (d_0'35'_2726 (coe v0))
      (d_1'35'_2728 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isIdempotentSemiring_1852
         (coe d_isKleeneAlgebra_2730 (coe v0)))
-- Algebra.Bundles.KleeneAlgebra._._≉_
d__'8777'__2838 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> AgdaAny -> AgdaAny -> ()
d__'8777'__2838 = erased
-- Algebra.Bundles.KleeneAlgebra._.magma
d_magma_2840 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_Magma_8
d_magma_2840 ~v0 ~v1 v2 = du_magma_2840 v2
du_magma_2840 :: T_KleeneAlgebra_2694 -> T_Magma_8
du_magma_2840 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'42''45'monoid_1970 (coe v3) in
    coe du_magma_524 (coe du_semigroup_802 (coe v4))
-- Algebra.Bundles.KleeneAlgebra._.*-monoid
d_'42''45'monoid_2842 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_Monoid_740
d_'42''45'monoid_2842 ~v0 ~v1 v2 = du_'42''45'monoid_2842 v2
du_'42''45'monoid_2842 :: T_KleeneAlgebra_2694 -> T_Monoid_740
du_'42''45'monoid_2842 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    coe
      du_'42''45'monoid_1970
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v2))
-- Algebra.Bundles.KleeneAlgebra._.rawMagma
d_rawMagma_2844 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2844 ~v0 ~v1 v2 = du_rawMagma_2844 v2
du_rawMagma_2844 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2844 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'42''45'monoid_1970 (coe v3) in
    let v5 = coe du_semigroup_802 (coe v4) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v5))
-- Algebra.Bundles.KleeneAlgebra._.rawMonoid
d_rawMonoid_2846 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2846 ~v0 ~v1 v2 = du_rawMonoid_2846 v2
du_rawMonoid_2846 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2846 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe du_rawMonoid_812 (coe du_'42''45'monoid_1970 (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.semigroup
d_semigroup_2848 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_Semigroup_476
d_semigroup_2848 ~v0 ~v1 v2 = du_semigroup_2848 v2
du_semigroup_2848 :: T_KleeneAlgebra_2694 -> T_Semigroup_476
du_semigroup_2848 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe du_semigroup_802 (coe du_'42''45'monoid_1970 (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.commutativeMagma
d_commutativeMagma_2850 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_CommutativeMagma_120
d_commutativeMagma_2850 ~v0 ~v1 v2 = du_commutativeMagma_2850 v2
du_commutativeMagma_2850 ::
  T_KleeneAlgebra_2694 -> T_CommutativeMagma_120
du_commutativeMagma_2850 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v4))
-- Algebra.Bundles.KleeneAlgebra._.+-commutativeMonoid
d_'43''45'commutativeMonoid_2852 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_2852 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_2852 v2
du_'43''45'commutativeMonoid_2852 ::
  T_KleeneAlgebra_2694 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_2852 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    coe
      du_'43''45'commutativeMonoid_1948
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v2))
-- Algebra.Bundles.KleeneAlgebra._.commutativeSemigroup
d_commutativeSemigroup_2854 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_2854 ~v0 ~v1 v2
  = du_commutativeSemigroup_2854 v2
du_commutativeSemigroup_2854 ::
  T_KleeneAlgebra_2694 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_2854 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1948 (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.magma
d_magma_2856 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_Magma_8
d_magma_2856 ~v0 ~v1 v2 = du_magma_2856 v2
du_magma_2856 :: T_KleeneAlgebra_2694 -> T_Magma_8
du_magma_2856 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    let v5 = coe du_monoid_890 (coe v4) in
    coe du_magma_524 (coe du_semigroup_802 (coe v5))
-- Algebra.Bundles.KleeneAlgebra._.monoid
d_monoid_2858 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_Monoid_740
d_monoid_2858 ~v0 ~v1 v2 = du_monoid_2858 v2
du_monoid_2858 :: T_KleeneAlgebra_2694 -> T_Monoid_740
du_monoid_2858 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe du_monoid_890 (coe du_'43''45'commutativeMonoid_1948 (coe v3))
-- Algebra.Bundles.KleeneAlgebra._.rawMagma
d_rawMagma_2860 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2860 ~v0 ~v1 v2 = du_rawMagma_2860 v2
du_rawMagma_2860 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2860 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    let v5 = coe du_monoid_890 (coe v4) in
    let v6 = coe du_semigroup_802 (coe v5) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v6))
-- Algebra.Bundles.KleeneAlgebra._.rawMonoid
d_rawMonoid_2862 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2862 ~v0 ~v1 v2 = du_rawMonoid_2862 v2
du_rawMonoid_2862 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2862 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v4))
-- Algebra.Bundles.KleeneAlgebra._.semigroup
d_semigroup_2864 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_Semigroup_476
d_semigroup_2864 ~v0 ~v1 v2 = du_semigroup_2864 v2
du_semigroup_2864 :: T_KleeneAlgebra_2694 -> T_Semigroup_476
du_semigroup_2864 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v4))
-- Algebra.Bundles.KleeneAlgebra._.unitalMagma
d_unitalMagma_2866 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_UnitalMagma_672
d_unitalMagma_2866 ~v0 ~v1 v2 = du_unitalMagma_2866 v2
du_unitalMagma_2866 :: T_KleeneAlgebra_2694 -> T_UnitalMagma_672
du_unitalMagma_2866 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v4))
-- Algebra.Bundles.KleeneAlgebra._.nearSemiring
d_nearSemiring_2868 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_NearSemiring_1508
d_nearSemiring_2868 ~v0 ~v1 v2 = du_nearSemiring_2868 v2
du_nearSemiring_2868 :: T_KleeneAlgebra_2694 -> T_NearSemiring_1508
du_nearSemiring_2868 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    coe du_nearSemiring_1688 (coe du_semiringWithoutOne_2142 (coe v2))
-- Algebra.Bundles.KleeneAlgebra._.rawSemiring
d_rawSemiring_2870 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_2870 ~v0 ~v1 v2 = du_rawSemiring_2870 v2
du_rawSemiring_2870 ::
  T_KleeneAlgebra_2694 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_2870 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    let v2 = coe du_semiring_2648 (coe v1) in
    coe
      du_rawSemiring_1942
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v2))
-- Algebra.Bundles.KleeneAlgebra._.semiring
d_semiring_2872 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_Semiring_1986
d_semiring_2872 ~v0 ~v1 v2 = du_semiring_2872 v2
du_semiring_2872 :: T_KleeneAlgebra_2694 -> T_Semiring_1986
du_semiring_2872 v0
  = coe du_semiring_2648 (coe du_idempotentSemiring_2834 (coe v0))
-- Algebra.Bundles.KleeneAlgebra._.semiringWithoutAnnihilatingZero
d_semiringWithoutAnnihilatingZero_2874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_SemiringWithoutAnnihilatingZero_1836
d_semiringWithoutAnnihilatingZero_2874 ~v0 ~v1 v2
  = du_semiringWithoutAnnihilatingZero_2874 v2
du_semiringWithoutAnnihilatingZero_2874 ::
  T_KleeneAlgebra_2694 -> T_SemiringWithoutAnnihilatingZero_1836
du_semiringWithoutAnnihilatingZero_2874 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    coe
      du_semiringWithoutAnnihilatingZero_2104
      (coe du_semiring_2648 (coe v1))
-- Algebra.Bundles.KleeneAlgebra._.semiringWithoutOne
d_semiringWithoutOne_2876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_KleeneAlgebra_2694 -> T_SemiringWithoutOne_1622
d_semiringWithoutOne_2876 ~v0 ~v1 v2
  = du_semiringWithoutOne_2876 v2
du_semiringWithoutOne_2876 ::
  T_KleeneAlgebra_2694 -> T_SemiringWithoutOne_1622
du_semiringWithoutOne_2876 v0
  = let v1 = coe du_idempotentSemiring_2834 (coe v0) in
    coe du_semiringWithoutOne_2142 (coe du_semiring_2648 (coe v1))
-- Algebra.Bundles.Quasiring
d_Quasiring_2882 a0 a1 = ()
data T_Quasiring_2882
  = C_Quasiring'46'constructor_50409 (AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
                                     MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962
-- Algebra.Bundles.Quasiring.Carrier
d_Carrier_2902 :: T_Quasiring_2882 -> ()
d_Carrier_2902 = erased
-- Algebra.Bundles.Quasiring._≈_
d__'8776'__2904 :: T_Quasiring_2882 -> AgdaAny -> AgdaAny -> ()
d__'8776'__2904 = erased
-- Algebra.Bundles.Quasiring._+_
d__'43'__2906 :: T_Quasiring_2882 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__2906 v0
  = case coe v0 of
      C_Quasiring'46'constructor_50409 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Quasiring._*_
d__'42'__2908 :: T_Quasiring_2882 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__2908 v0
  = case coe v0 of
      C_Quasiring'46'constructor_50409 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Quasiring.0#
d_0'35'_2910 :: T_Quasiring_2882 -> AgdaAny
d_0'35'_2910 v0
  = case coe v0 of
      C_Quasiring'46'constructor_50409 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Quasiring.1#
d_1'35'_2912 :: T_Quasiring_2882 -> AgdaAny
d_1'35'_2912 v0
  = case coe v0 of
      C_Quasiring'46'constructor_50409 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Quasiring.isQuasiring
d_isQuasiring_2914 ::
  T_Quasiring_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962
d_isQuasiring_2914 v0
  = case coe v0 of
      C_Quasiring'46'constructor_50409 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Quasiring._.*-assoc
d_'42''45'assoc_2918 ::
  T_Quasiring_2882 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2918 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1988
      (coe d_isQuasiring_2914 (coe v0))
-- Algebra.Bundles.Quasiring._.*-cong
d_'42''45'cong_2920 ::
  T_Quasiring_2882 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2920 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1986
      (coe d_isQuasiring_2914 (coe v0))
-- Algebra.Bundles.Quasiring._.∙-congʳ
d_'8729''45'cong'691'_2922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2922 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2922 v2
du_'8729''45'cong'691'_2922 ::
  T_Quasiring_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2922 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Quasiring._.∙-congˡ
d_'8729''45'cong'737'_2924 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2924 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2924 v2
du_'8729''45'cong'737'_2924 ::
  T_Quasiring_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2924 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Quasiring._.*-identity
d_'42''45'identity_2926 ::
  T_Quasiring_2882 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2926 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990
      (coe d_isQuasiring_2914 (coe v0))
-- Algebra.Bundles.Quasiring._.identityʳ
d_identity'691'_2928 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> AgdaAny -> AgdaAny
d_identity'691'_2928 ~v0 ~v1 v2 = du_identity'691'_2928 v2
du_identity'691'_2928 :: T_Quasiring_2882 -> AgdaAny -> AgdaAny
du_identity'691'_2928 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036 (coe v1))
-- Algebra.Bundles.Quasiring._.identityˡ
d_identity'737'_2930 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> AgdaAny -> AgdaAny
d_identity'737'_2930 ~v0 ~v1 v2 = du_identity'737'_2930 v2
du_identity'737'_2930 :: T_Quasiring_2882 -> AgdaAny -> AgdaAny
du_identity'737'_2930 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036 (coe v1))
-- Algebra.Bundles.Quasiring._.*-isMagma
d_'42''45'isMagma_2932 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_2932 ~v0 ~v1 v2 = du_'42''45'isMagma_2932 v2
du_'42''45'isMagma_2932 ::
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_2932 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2032
      (coe d_isQuasiring_2914 (coe v0))
-- Algebra.Bundles.Quasiring._.*-isMonoid
d_'42''45'isMonoid_2934 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_2934 ~v0 ~v1 v2 = du_'42''45'isMonoid_2934 v2
du_'42''45'isMonoid_2934 ::
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_2934 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036
      (coe d_isQuasiring_2914 (coe v0))
-- Algebra.Bundles.Quasiring._.*-isSemigroup
d_'42''45'isSemigroup_2936 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2936 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_2936 v2
du_'42''45'isSemigroup_2936 ::
  T_Quasiring_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_2936 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_2034
      (coe d_isQuasiring_2914 (coe v0))
-- Algebra.Bundles.Quasiring._.assoc
d_assoc_2938 ::
  T_Quasiring_2882 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2938 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
            (coe d_isQuasiring_2914 (coe v0))))
-- Algebra.Bundles.Quasiring._.∙-cong
d_'8729''45'cong_2940 ::
  T_Quasiring_2882 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2940 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
               (coe d_isQuasiring_2914 (coe v0)))))
-- Algebra.Bundles.Quasiring._.∙-congʳ
d_'8729''45'cong'691'_2942 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2942 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_2942 v2
du_'8729''45'cong'691'_2942 ::
  T_Quasiring_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2942 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Quasiring._.∙-congˡ
d_'8729''45'cong'737'_2944 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2944 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_2944 v2
du_'8729''45'cong'737'_2944 ::
  T_Quasiring_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2944 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Quasiring._.identity
d_identity_2946 ::
  T_Quasiring_2882 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2946 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe d_isQuasiring_2914 (coe v0)))
-- Algebra.Bundles.Quasiring._.identityʳ
d_identity'691'_2948 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> AgdaAny -> AgdaAny
d_identity'691'_2948 ~v0 ~v1 v2 = du_identity'691'_2948 v2
du_identity'691'_2948 :: T_Quasiring_2882 -> AgdaAny -> AgdaAny
du_identity'691'_2948 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v1))
-- Algebra.Bundles.Quasiring._.identityˡ
d_identity'737'_2950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> AgdaAny -> AgdaAny
d_identity'737'_2950 ~v0 ~v1 v2 = du_identity'737'_2950 v2
du_identity'737'_2950 :: T_Quasiring_2882 -> AgdaAny -> AgdaAny
du_identity'737'_2950 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v1))
-- Algebra.Bundles.Quasiring._.isMagma
d_isMagma_2952 ::
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2952 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
            (coe d_isQuasiring_2914 (coe v0))))
-- Algebra.Bundles.Quasiring._.+-isMonoid
d_'43''45'isMonoid_2954 ::
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_2954 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
      (coe d_isQuasiring_2914 (coe v0))
-- Algebra.Bundles.Quasiring._.isSemigroup
d_isSemigroup_2956 ::
  T_Quasiring_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2956 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe d_isQuasiring_2914 (coe v0)))
-- Algebra.Bundles.Quasiring._.isUnitalMagma
d_isUnitalMagma_2958 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_2958 ~v0 ~v1 v2 = du_isUnitalMagma_2958 v2
du_isUnitalMagma_2958 ::
  T_Quasiring_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_2958 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v1))
-- Algebra.Bundles.Quasiring._.distrib
d_distrib_2960 ::
  T_Quasiring_2882 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2960 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1992
      (coe d_isQuasiring_2914 (coe v0))
-- Algebra.Bundles.Quasiring._.isEquivalence
d_isEquivalence_2962 ::
  T_Quasiring_2882 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2962 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
               (coe d_isQuasiring_2914 (coe v0)))))
-- Algebra.Bundles.Quasiring._.isPartialEquivalence
d_isPartialEquivalence_2964 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2964 ~v0 ~v1 v2
  = du_isPartialEquivalence_2964 v2
du_isPartialEquivalence_2964 ::
  T_Quasiring_2882 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2964 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Bundles.Quasiring._.refl
d_refl_2966 :: T_Quasiring_2882 -> AgdaAny -> AgdaAny
d_refl_2966 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
                  (coe d_isQuasiring_2914 (coe v0))))))
-- Algebra.Bundles.Quasiring._.reflexive
d_reflexive_2968 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2968 ~v0 ~v1 v2 = du_reflexive_2968 v2
du_reflexive_2968 ::
  T_Quasiring_2882 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2968 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Bundles.Quasiring._.setoid
d_setoid_2970 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2970 ~v0 ~v1 v2 = du_setoid_2970 v2
du_setoid_2970 ::
  T_Quasiring_2882 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2970 v0
  = let v1 = d_isQuasiring_2914 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Quasiring._.sym
d_sym_2972 ::
  T_Quasiring_2882 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2972 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
                  (coe d_isQuasiring_2914 (coe v0))))))
-- Algebra.Bundles.Quasiring._.trans
d_trans_2974 ::
  T_Quasiring_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2974 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
                  (coe d_isQuasiring_2914 (coe v0))))))
-- Algebra.Bundles.Quasiring._.zero
d_zero_2976 ::
  T_Quasiring_2882 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2976 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1994
      (coe d_isQuasiring_2914 (coe v0))
-- Algebra.Bundles.Quasiring.+-monoid
d_'43''45'monoid_2978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> T_Monoid_740
d_'43''45'monoid_2978 ~v0 ~v1 v2 = du_'43''45'monoid_2978 v2
du_'43''45'monoid_2978 :: T_Quasiring_2882 -> T_Monoid_740
du_'43''45'monoid_2978 v0
  = coe
      C_Monoid'46'constructor_13309 (d__'43'__2906 (coe v0))
      (d_0'35'_2910 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe d_isQuasiring_2914 (coe v0)))
-- Algebra.Bundles.Quasiring._._≉_
d__'8777'__2982 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> AgdaAny -> AgdaAny -> ()
d__'8777'__2982 = erased
-- Algebra.Bundles.Quasiring._.magma
d_magma_2984 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> T_Magma_8
d_magma_2984 ~v0 ~v1 v2 = du_magma_2984 v2
du_magma_2984 :: T_Quasiring_2882 -> T_Magma_8
du_magma_2984 v0
  = let v1 = coe du_'43''45'monoid_2978 (coe v0) in
    coe du_magma_524 (coe du_semigroup_802 (coe v1))
-- Algebra.Bundles.Quasiring._.rawMagma
d_rawMagma_2986 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_2986 ~v0 ~v1 v2 = du_rawMagma_2986 v2
du_rawMagma_2986 ::
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_2986 v0
  = let v1 = coe du_'43''45'monoid_2978 (coe v0) in
    let v2 = coe du_semigroup_802 (coe v1) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v2))
-- Algebra.Bundles.Quasiring._.rawMonoid
d_rawMonoid_2988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_2988 ~v0 ~v1 v2 = du_rawMonoid_2988 v2
du_rawMonoid_2988 ::
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_2988 v0
  = coe du_rawMonoid_812 (coe du_'43''45'monoid_2978 (coe v0))
-- Algebra.Bundles.Quasiring._.semigroup
d_semigroup_2990 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> T_Semigroup_476
d_semigroup_2990 ~v0 ~v1 v2 = du_semigroup_2990 v2
du_semigroup_2990 :: T_Quasiring_2882 -> T_Semigroup_476
du_semigroup_2990 v0
  = coe du_semigroup_802 (coe du_'43''45'monoid_2978 (coe v0))
-- Algebra.Bundles.Quasiring._.unitalMagma
d_unitalMagma_2992 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> T_UnitalMagma_672
d_unitalMagma_2992 ~v0 ~v1 v2 = du_unitalMagma_2992 v2
du_unitalMagma_2992 :: T_Quasiring_2882 -> T_UnitalMagma_672
du_unitalMagma_2992 v0
  = coe du_unitalMagma_814 (coe du_'43''45'monoid_2978 (coe v0))
-- Algebra.Bundles.Quasiring.*-monoid
d_'42''45'monoid_2994 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> T_Monoid_740
d_'42''45'monoid_2994 ~v0 ~v1 v2 = du_'42''45'monoid_2994 v2
du_'42''45'monoid_2994 :: T_Quasiring_2882 -> T_Monoid_740
du_'42''45'monoid_2994 v0
  = coe
      C_Monoid'46'constructor_13309 (d__'42'__2908 (coe v0))
      (d_1'35'_2912 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036
         (coe d_isQuasiring_2914 (coe v0)))
-- Algebra.Bundles.Quasiring._.magma
d_magma_2998 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> T_Magma_8
d_magma_2998 ~v0 ~v1 v2 = du_magma_2998 v2
du_magma_2998 :: T_Quasiring_2882 -> T_Magma_8
du_magma_2998 v0
  = let v1 = coe du_'42''45'monoid_2994 (coe v0) in
    coe du_magma_524 (coe du_semigroup_802 (coe v1))
-- Algebra.Bundles.Quasiring._.rawMagma
d_rawMagma_3000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_3000 ~v0 ~v1 v2 = du_rawMagma_3000 v2
du_rawMagma_3000 ::
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_3000 v0
  = let v1 = coe du_'42''45'monoid_2994 (coe v0) in
    let v2 = coe du_semigroup_802 (coe v1) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v2))
-- Algebra.Bundles.Quasiring._.rawMonoid
d_rawMonoid_3002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_3002 ~v0 ~v1 v2 = du_rawMonoid_3002 v2
du_rawMonoid_3002 ::
  T_Quasiring_2882 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_3002 v0
  = coe du_rawMonoid_812 (coe du_'42''45'monoid_2994 (coe v0))
-- Algebra.Bundles.Quasiring._.semigroup
d_semigroup_3004 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasiring_2882 -> T_Semigroup_476
d_semigroup_3004 ~v0 ~v1 v2 = du_semigroup_3004 v2
du_semigroup_3004 :: T_Quasiring_2882 -> T_Semigroup_476
du_semigroup_3004 v0
  = coe du_semigroup_802 (coe du_'42''45'monoid_2994 (coe v0))
-- Algebra.Bundles.RingWithoutOne
d_RingWithoutOne_3010 a0 a1 = ()
data T_RingWithoutOne_3010
  = C_RingWithoutOne'46'constructor_52845 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                          AgdaAny
                                          MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056
-- Algebra.Bundles.RingWithoutOne.Carrier
d_Carrier_3030 :: T_RingWithoutOne_3010 -> ()
d_Carrier_3030 = erased
-- Algebra.Bundles.RingWithoutOne._≈_
d__'8776'__3032 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> ()
d__'8776'__3032 = erased
-- Algebra.Bundles.RingWithoutOne._+_
d__'43'__3034 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__3034 v0
  = case coe v0 of
      C_RingWithoutOne'46'constructor_52845 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RingWithoutOne._*_
d__'42'__3036 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__3036 v0
  = case coe v0 of
      C_RingWithoutOne'46'constructor_52845 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RingWithoutOne.-_
d_'45'__3038 :: T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
d_'45'__3038 v0
  = case coe v0 of
      C_RingWithoutOne'46'constructor_52845 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RingWithoutOne.0#
d_0'35'_3040 :: T_RingWithoutOne_3010 -> AgdaAny
d_0'35'_3040 v0
  = case coe v0 of
      C_RingWithoutOne'46'constructor_52845 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RingWithoutOne.isRingWithoutOne
d_isRingWithoutOne_3042 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsRingWithoutOne_2056
d_isRingWithoutOne_3042 v0
  = case coe v0 of
      C_RingWithoutOne'46'constructor_52845 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RingWithoutOne._._-_
d__'45'__3046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__3046 ~v0 ~v1 v2 = du__'45'__3046 v2
du__'45'__3046 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__3046 v0
  = let v1 = d__'43'__3034 (coe v0) in
    let v2 = d_'45'__3038 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v1) (coe v2)
-- Algebra.Bundles.RingWithoutOne._.*-assoc
d_'42''45'assoc_3048 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_3048 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_2080
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.*-cong
d_'42''45'cong_3050 ::
  T_RingWithoutOne_3010 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_3050 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_2078
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.∙-congʳ
d_'8729''45'cong'691'_3052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3052 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3052 v2
du_'8729''45'cong'691'_3052 ::
  T_RingWithoutOne_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3052 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2152 (coe v1))
-- Algebra.Bundles.RingWithoutOne._.∙-congˡ
d_'8729''45'cong'737'_3054 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3054 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3054 v2
du_'8729''45'cong'737'_3054 ::
  T_RingWithoutOne_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3054 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2152 (coe v1))
-- Algebra.Bundles.RingWithoutOne._.*-isMagma
d_'42''45'isMagma_3056 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_3056 ~v0 ~v1 v2 = du_'42''45'isMagma_3056 v2
du_'42''45'isMagma_3056 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_3056 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2152
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.*-isSemigroup
d_'42''45'isSemigroup_3058 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_3058 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_3058 v2
du_'42''45'isSemigroup_3058 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_3058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_2162
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.assoc
d_assoc_3060 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_3060 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                  (coe d_isRingWithoutOne_3042 (coe v0))))))
-- Algebra.Bundles.RingWithoutOne._.comm
d_comm_3062 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_3062 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_990
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
         (coe d_isRingWithoutOne_3042 (coe v0)))
-- Algebra.Bundles.RingWithoutOne._.∙-cong
d_'8729''45'cong_3064 ::
  T_RingWithoutOne_3010 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3064 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                     (coe d_isRingWithoutOne_3042 (coe v0)))))))
-- Algebra.Bundles.RingWithoutOne._.∙-congʳ
d_'8729''45'cong'691'_3066 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3066 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3066 v2
du_'8729''45'cong'691'_3066 ::
  T_RingWithoutOne_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3066 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.RingWithoutOne._.∙-congˡ
d_'8729''45'cong'737'_3068 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3068 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3068 v2
du_'8729''45'cong'737'_3068 ::
  T_RingWithoutOne_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3068 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.RingWithoutOne._.identity
d_identity_3070 ::
  T_RingWithoutOne_3010 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3070 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe d_isRingWithoutOne_3042 (coe v0)))))
-- Algebra.Bundles.RingWithoutOne._.identityʳ
d_identity'691'_3072 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
d_identity'691'_3072 ~v0 ~v1 v2 = du_identity'691'_3072 v2
du_identity'691'_3072 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
du_identity'691'_3072 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3))
-- Algebra.Bundles.RingWithoutOne._.identityˡ
d_identity'737'_3074 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
d_identity'737'_3074 ~v0 ~v1 v2 = du_identity'737'_3074 v2
du_identity'737'_3074 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
du_identity'737'_3074 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3))
-- Algebra.Bundles.RingWithoutOne._.+-isAbelianGroup
d_'43''45'isAbelianGroup_3076 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_3076 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.isCommutativeMagma
d_isCommutativeMagma_3078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_3078 ~v0 ~v1 v2
  = du_isCommutativeMagma_3078 v2
du_isCommutativeMagma_3078 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_3078 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v3))
-- Algebra.Bundles.RingWithoutOne._.isCommutativeMonoid
d_isCommutativeMonoid_3080 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_3080 ~v0 ~v1 v2
  = du_isCommutativeMonoid_3080 v2
du_isCommutativeMonoid_3080 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_3080 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
         (coe v1))
-- Algebra.Bundles.RingWithoutOne._.isCommutativeSemigroup
d_isCommutativeSemigroup_3082 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_3082 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_3082 v2
du_isCommutativeSemigroup_3082 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_3082 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
         (coe v2))
-- Algebra.Bundles.RingWithoutOne._.isGroup
d_isGroup_3084 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_3084 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
         (coe d_isRingWithoutOne_3042 (coe v0)))
-- Algebra.Bundles.RingWithoutOne._.isInvertibleMagma
d_isInvertibleMagma_3086 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_3086 ~v0 ~v1 v2 = du_isInvertibleMagma_3086 v2
du_isInvertibleMagma_3086 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
du_isInvertibleMagma_3086 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleMagma_966
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.RingWithoutOne._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_3088 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_3088 ~v0 ~v1 v2
  = du_isInvertibleUnitalMagma_3088 v2
du_isInvertibleUnitalMagma_3088 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_3088 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleUnitalMagma_968
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.RingWithoutOne._.isMagma
d_isMagma_3090 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_3090 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                  (coe d_isRingWithoutOne_3042 (coe v0))))))
-- Algebra.Bundles.RingWithoutOne._.isMonoid
d_isMonoid_3092 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_3092 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe d_isRingWithoutOne_3042 (coe v0))))
-- Algebra.Bundles.RingWithoutOne._.isSemigroup
d_isSemigroup_3094 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_3094 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
               (coe d_isRingWithoutOne_3042 (coe v0)))))
-- Algebra.Bundles.RingWithoutOne._.isUnitalMagma
d_isUnitalMagma_3096 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_3096 ~v0 ~v1 v2 = du_isUnitalMagma_3096 v2
du_isUnitalMagma_3096 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_3096 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3))
-- Algebra.Bundles.RingWithoutOne._.⁻¹-cong
d_'8315''185''45'cong_3098 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_3098 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe d_isRingWithoutOne_3042 (coe v0))))
-- Algebra.Bundles.RingWithoutOne._.inverse
d_inverse_3100 ::
  T_RingWithoutOne_3010 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_3100 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
            (coe d_isRingWithoutOne_3042 (coe v0))))
-- Algebra.Bundles.RingWithoutOne._.inverseʳ
d_inverse'691'_3102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
d_inverse'691'_3102 ~v0 ~v1 v2 = du_inverse'691'_3102 v2
du_inverse'691'_3102 :: T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
du_inverse'691'_3102 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.RingWithoutOne._.inverseˡ
d_inverse'737'_3104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
d_inverse'737'_3104 ~v0 ~v1 v2 = du_inverse'737'_3104 v2
du_inverse'737'_3104 :: T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
du_inverse'737'_3104 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.RingWithoutOne._.distrib
d_distrib_3106 ::
  T_RingWithoutOne_3010 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_3106 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_2082
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.distribʳ
d_distrib'691'_3108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_3108 ~v0 ~v1 v2 = du_distrib'691'_3108 v2
du_distrib'691'_3108 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_3108 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_2160
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.distribˡ
d_distrib'737'_3110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_3110 ~v0 ~v1 v2 = du_distrib'737'_3110 v2
du_distrib'737'_3110 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_3110 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_2158
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.isEquivalence
d_isEquivalence_3112 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3112 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                     (coe d_isRingWithoutOne_3042 (coe v0)))))))
-- Algebra.Bundles.RingWithoutOne._.isPartialEquivalence
d_isPartialEquivalence_3114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3114 ~v0 ~v1 v2
  = du_isPartialEquivalence_3114 v2
du_isPartialEquivalence_3114 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3114 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v6))
-- Algebra.Bundles.RingWithoutOne._.refl
d_refl_3116 :: T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
d_refl_3116 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                        (coe d_isRingWithoutOne_3042 (coe v0))))))))
-- Algebra.Bundles.RingWithoutOne._.reflexive
d_reflexive_3118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3118 ~v0 ~v1 v2 = du_reflexive_3118 v2
du_reflexive_3118 ::
  T_RingWithoutOne_3010 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3118 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v6))
        v7
-- Algebra.Bundles.RingWithoutOne._.setoid
d_setoid_3120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3120 ~v0 ~v1 v2 = du_setoid_3120 v2
du_setoid_3120 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3120 v0
  = let v1 = d_isRingWithoutOne_3042 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.RingWithoutOne._.sym
d_sym_3122 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3122 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                        (coe d_isRingWithoutOne_3042 (coe v0))))))))
-- Algebra.Bundles.RingWithoutOne._.trans
d_trans_3124 ::
  T_RingWithoutOne_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3124 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
                        (coe d_isRingWithoutOne_3042 (coe v0))))))))
-- Algebra.Bundles.RingWithoutOne._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_3126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_3126 ~v0 ~v1 v2
  = du_unique'691''45''8315''185'_3126 v2
du_unique'691''45''8315''185'_3126 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_3126 v0
  = let v1 = d__'43'__3034 (coe v0) in
    let v2 = d_'45'__3038 (coe v0) in
    let v3 = d_0'35'_3040 (coe v0) in
    let v4 = d_isRingWithoutOne_3042 (coe v0) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'691''45''8315''185'_964
      (coe v1) (coe v3) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5))
-- Algebra.Bundles.RingWithoutOne._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_3128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_3128 ~v0 ~v1 v2
  = du_unique'737''45''8315''185'_3128 v2
du_unique'737''45''8315''185'_3128 ::
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_3128 v0
  = let v1 = d__'43'__3034 (coe v0) in
    let v2 = d_'45'__3038 (coe v0) in
    let v3 = d_0'35'_3040 (coe v0) in
    let v4 = d_isRingWithoutOne_3042 (coe v0) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'737''45''8315''185'_958
      (coe v1) (coe v3) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5))
-- Algebra.Bundles.RingWithoutOne._.zero
d_zero_3130 ::
  T_RingWithoutOne_3010 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_3130 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_2084
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.zeroʳ
d_zero'691'_3132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
d_zero'691'_3132 ~v0 ~v1 v2 = du_zero'691'_3132 v2
du_zero'691'_3132 :: T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
du_zero'691'_3132 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_2156
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.zeroˡ
d_zero'737'_3134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
d_zero'737'_3134 ~v0 ~v1 v2 = du_zero'737'_3134 v2
du_zero'737'_3134 :: T_RingWithoutOne_3010 -> AgdaAny -> AgdaAny
du_zero'737'_3134 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_2154
      (coe d_isRingWithoutOne_3042 (coe v0))
-- Algebra.Bundles.RingWithoutOne.+-abelianGroup
d_'43''45'abelianGroup_3136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> T_AbelianGroup_1378
d_'43''45'abelianGroup_3136 ~v0 ~v1 v2
  = du_'43''45'abelianGroup_3136 v2
du_'43''45'abelianGroup_3136 ::
  T_RingWithoutOne_3010 -> T_AbelianGroup_1378
du_'43''45'abelianGroup_3136 v0
  = coe
      C_AbelianGroup'46'constructor_24425 (d__'43'__3034 (coe v0))
      (d_0'35'_3040 (coe v0)) (d_'45'__3038 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2076
         (coe d_isRingWithoutOne_3042 (coe v0)))
-- Algebra.Bundles.RingWithoutOne.*-semigroup
d_'42''45'semigroup_3138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> T_Semigroup_476
d_'42''45'semigroup_3138 ~v0 ~v1 v2 = du_'42''45'semigroup_3138 v2
du_'42''45'semigroup_3138 ::
  T_RingWithoutOne_3010 -> T_Semigroup_476
du_'42''45'semigroup_3138 v0
  = coe
      C_Semigroup'46'constructor_8557 (d__'42'__3036 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_2162
         (coe d_isRingWithoutOne_3042 (coe v0)))
-- Algebra.Bundles.RingWithoutOne._.group
d_group_3142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> T_Group_1266
d_group_3142 ~v0 ~v1 v2 = du_group_3142 v2
du_group_3142 :: T_RingWithoutOne_3010 -> T_Group_1266
du_group_3142 v0
  = coe du_group_1474 (coe du_'43''45'abelianGroup_3136 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.invertibleMagma
d_invertibleMagma_3144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> T_InvertibleMagma_1106
d_invertibleMagma_3144 ~v0 ~v1 v2 = du_invertibleMagma_3144 v2
du_invertibleMagma_3144 ::
  T_RingWithoutOne_3010 -> T_InvertibleMagma_1106
du_invertibleMagma_3144 v0
  = let v1 = coe du_'43''45'abelianGroup_3136 (coe v0) in
    coe du_invertibleMagma_1370 (coe du_group_1474 (coe v1))
-- Algebra.Bundles.RingWithoutOne._.invertibleUnitalMagma
d_invertibleUnitalMagma_3146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> T_InvertibleUnitalMagma_1180
d_invertibleUnitalMagma_3146 ~v0 ~v1 v2
  = du_invertibleUnitalMagma_3146 v2
du_invertibleUnitalMagma_3146 ::
  T_RingWithoutOne_3010 -> T_InvertibleUnitalMagma_1180
du_invertibleUnitalMagma_3146 v0
  = let v1 = coe du_'43''45'abelianGroup_3136 (coe v0) in
    coe du_invertibleUnitalMagma_1372 (coe du_group_1474 (coe v1))
-- Algebra.Bundles.RingWithoutOne._.magma
d_magma_3150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 -> T_Magma_8
d_magma_3150 ~v0 ~v1 v2 = du_magma_3150 v2
du_magma_3150 :: T_RingWithoutOne_3010 -> T_Magma_8
du_magma_3150 v0
  = coe du_magma_524 (coe du_'42''45'semigroup_3138 (coe v0))
-- Algebra.Bundles.RingWithoutOne._.rawMagma
d_rawMagma_3152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_3152 ~v0 ~v1 v2 = du_rawMagma_3152 v2
du_rawMagma_3152 ::
  T_RingWithoutOne_3010 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_3152 v0
  = let v1 = coe du_'42''45'semigroup_3138 (coe v0) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v1))
-- Algebra.Bundles.NonAssociativeRing
d_NonAssociativeRing_3158 a0 a1 = ()
data T_NonAssociativeRing_3158
  = C_NonAssociativeRing'46'constructor_55637 (AgdaAny ->
                                               AgdaAny -> AgdaAny)
                                              (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                              AgdaAny AgdaAny
                                              MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180
-- Algebra.Bundles.NonAssociativeRing.Carrier
d_Carrier_3180 :: T_NonAssociativeRing_3158 -> ()
d_Carrier_3180 = erased
-- Algebra.Bundles.NonAssociativeRing._≈_
d__'8776'__3182 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny -> ()
d__'8776'__3182 = erased
-- Algebra.Bundles.NonAssociativeRing._+_
d__'43'__3184 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__3184 v0
  = case coe v0 of
      C_NonAssociativeRing'46'constructor_55637 v3 v4 v5 v6 v7 v8
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NonAssociativeRing._*_
d__'42'__3186 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__3186 v0
  = case coe v0 of
      C_NonAssociativeRing'46'constructor_55637 v3 v4 v5 v6 v7 v8
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NonAssociativeRing.-_
d_'45'__3188 :: T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
d_'45'__3188 v0
  = case coe v0 of
      C_NonAssociativeRing'46'constructor_55637 v3 v4 v5 v6 v7 v8
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NonAssociativeRing.0#
d_0'35'_3190 :: T_NonAssociativeRing_3158 -> AgdaAny
d_0'35'_3190 v0
  = case coe v0 of
      C_NonAssociativeRing'46'constructor_55637 v3 v4 v5 v6 v7 v8
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NonAssociativeRing.1#
d_1'35'_3192 :: T_NonAssociativeRing_3158 -> AgdaAny
d_1'35'_3192 v0
  = case coe v0 of
      C_NonAssociativeRing'46'constructor_55637 v3 v4 v5 v6 v7 v8
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NonAssociativeRing.isNonAssociativeRing
d_isNonAssociativeRing_3194 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsNonAssociativeRing_2180
d_isNonAssociativeRing_3194 v0
  = case coe v0 of
      C_NonAssociativeRing'46'constructor_55637 v3 v4 v5 v6 v7 v8
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.NonAssociativeRing._._-_
d__'45'__3198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__3198 ~v0 ~v1 v2 = du__'45'__3198 v2
du__'45'__3198 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__3198 v0
  = let v1 = d__'43'__3184 (coe v0) in
    let v2 = d_'45'__3188 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v1) (coe v2)
-- Algebra.Bundles.NonAssociativeRing._.*-cong
d_'42''45'cong_3200 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_3200 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_2204
      (coe d_isNonAssociativeRing_3194 (coe v0))
-- Algebra.Bundles.NonAssociativeRing._.*-identityʳ
d_'42''45'identity'691'_3202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
d_'42''45'identity'691'_3202 ~v0 ~v1 v2
  = du_'42''45'identity'691'_3202 v2
du_'42''45'identity'691'_3202 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
du_'42''45'identity'691'_3202 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'identity'691'_2282
      (coe d_isNonAssociativeRing_3194 (coe v0))
-- Algebra.Bundles.NonAssociativeRing._.*-identityˡ
d_'42''45'identity'737'_3204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
d_'42''45'identity'737'_3204 ~v0 ~v1 v2
  = du_'42''45'identity'737'_3204 v2
du_'42''45'identity'737'_3204 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
du_'42''45'identity'737'_3204 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'identity'737'_2280
      (coe d_isNonAssociativeRing_3194 (coe v0))
-- Algebra.Bundles.NonAssociativeRing._.*-isMagma
d_'42''45'isMagma_3206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_3206 ~v0 ~v1 v2 = du_'42''45'isMagma_3206 v2
du_'42''45'isMagma_3206 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_3206 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2278
      (coe d_isNonAssociativeRing_3194 (coe v0))
-- Algebra.Bundles.NonAssociativeRing._.assoc
d_assoc_3208 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_3208 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                  (coe d_isNonAssociativeRing_3194 (coe v0))))))
-- Algebra.Bundles.NonAssociativeRing._.comm
d_comm_3210 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_3210 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_990
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
         (coe d_isNonAssociativeRing_3194 (coe v0)))
-- Algebra.Bundles.NonAssociativeRing._.∙-cong
d_'8729''45'cong_3212 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3212 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                     (coe d_isNonAssociativeRing_3194 (coe v0)))))))
-- Algebra.Bundles.NonAssociativeRing._.∙-congʳ
d_'8729''45'cong'691'_3214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3214 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3214 v2
du_'8729''45'cong'691'_3214 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3214 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.NonAssociativeRing._.∙-congˡ
d_'8729''45'cong'737'_3216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3216 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3216 v2
du_'8729''45'cong'737'_3216 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3216 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.NonAssociativeRing._.identity
d_identity_3218 ::
  T_NonAssociativeRing_3158 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3218 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe d_isNonAssociativeRing_3194 (coe v0)))))
-- Algebra.Bundles.NonAssociativeRing._.identityʳ
d_identity'691'_3220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
d_identity'691'_3220 ~v0 ~v1 v2 = du_identity'691'_3220 v2
du_identity'691'_3220 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
du_identity'691'_3220 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3))
-- Algebra.Bundles.NonAssociativeRing._.identityˡ
d_identity'737'_3222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
d_identity'737'_3222 ~v0 ~v1 v2 = du_identity'737'_3222 v2
du_identity'737'_3222 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
du_identity'737'_3222 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3))
-- Algebra.Bundles.NonAssociativeRing._.+-isAbelianGroup
d_'43''45'isAbelianGroup_3224 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_3224 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
      (coe d_isNonAssociativeRing_3194 (coe v0))
-- Algebra.Bundles.NonAssociativeRing._.isCommutativeMagma
d_isCommutativeMagma_3226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_3226 ~v0 ~v1 v2
  = du_isCommutativeMagma_3226 v2
du_isCommutativeMagma_3226 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_3226 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v3))
-- Algebra.Bundles.NonAssociativeRing._.isCommutativeMonoid
d_isCommutativeMonoid_3228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_3228 ~v0 ~v1 v2
  = du_isCommutativeMonoid_3228 v2
du_isCommutativeMonoid_3228 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_3228 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
         (coe v1))
-- Algebra.Bundles.NonAssociativeRing._.isCommutativeSemigroup
d_isCommutativeSemigroup_3230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_3230 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_3230 v2
du_isCommutativeSemigroup_3230 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_3230 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
         (coe v2))
-- Algebra.Bundles.NonAssociativeRing._.isGroup
d_isGroup_3232 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_3232 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
         (coe d_isNonAssociativeRing_3194 (coe v0)))
-- Algebra.Bundles.NonAssociativeRing._.isInvertibleMagma
d_isInvertibleMagma_3234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_3234 ~v0 ~v1 v2 = du_isInvertibleMagma_3234 v2
du_isInvertibleMagma_3234 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
du_isInvertibleMagma_3234 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleMagma_966
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.NonAssociativeRing._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_3236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_3236 ~v0 ~v1 v2
  = du_isInvertibleUnitalMagma_3236 v2
du_isInvertibleUnitalMagma_3236 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_3236 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleUnitalMagma_968
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.NonAssociativeRing._.isMagma
d_isMagma_3238 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_3238 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                  (coe d_isNonAssociativeRing_3194 (coe v0))))))
-- Algebra.Bundles.NonAssociativeRing._.isMonoid
d_isMonoid_3240 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_3240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe d_isNonAssociativeRing_3194 (coe v0))))
-- Algebra.Bundles.NonAssociativeRing._.isSemigroup
d_isSemigroup_3242 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_3242 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
               (coe d_isNonAssociativeRing_3194 (coe v0)))))
-- Algebra.Bundles.NonAssociativeRing._.isUnitalMagma
d_isUnitalMagma_3244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_3244 ~v0 ~v1 v2 = du_isUnitalMagma_3244 v2
du_isUnitalMagma_3244 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_3244 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3))
-- Algebra.Bundles.NonAssociativeRing._.⁻¹-cong
d_'8315''185''45'cong_3246 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_3246 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe d_isNonAssociativeRing_3194 (coe v0))))
-- Algebra.Bundles.NonAssociativeRing._.inverse
d_inverse_3248 ::
  T_NonAssociativeRing_3158 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_3248 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
            (coe d_isNonAssociativeRing_3194 (coe v0))))
-- Algebra.Bundles.NonAssociativeRing._.inverseʳ
d_inverse'691'_3250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
d_inverse'691'_3250 ~v0 ~v1 v2 = du_inverse'691'_3250 v2
du_inverse'691'_3250 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
du_inverse'691'_3250 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.NonAssociativeRing._.inverseˡ
d_inverse'737'_3252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
d_inverse'737'_3252 ~v0 ~v1 v2 = du_inverse'737'_3252 v2
du_inverse'737'_3252 ::
  T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
du_inverse'737'_3252 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.NonAssociativeRing._.distrib
d_distrib_3254 ::
  T_NonAssociativeRing_3158 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_3254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_2208
      (coe d_isNonAssociativeRing_3194 (coe v0))
-- Algebra.Bundles.NonAssociativeRing._.identity
d_identity_3256 ::
  T_NonAssociativeRing_3158 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3256 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2206
      (coe d_isNonAssociativeRing_3194 (coe v0))
-- Algebra.Bundles.NonAssociativeRing._.isEquivalence
d_isEquivalence_3258 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3258 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                     (coe d_isNonAssociativeRing_3194 (coe v0)))))))
-- Algebra.Bundles.NonAssociativeRing._.isPartialEquivalence
d_isPartialEquivalence_3260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3260 ~v0 ~v1 v2
  = du_isPartialEquivalence_3260 v2
du_isPartialEquivalence_3260 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3260 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v6))
-- Algebra.Bundles.NonAssociativeRing._.refl
d_refl_3262 :: T_NonAssociativeRing_3158 -> AgdaAny -> AgdaAny
d_refl_3262 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                        (coe d_isNonAssociativeRing_3194 (coe v0))))))))
-- Algebra.Bundles.NonAssociativeRing._.reflexive
d_reflexive_3264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3264 ~v0 ~v1 v2 = du_reflexive_3264 v2
du_reflexive_3264 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3264 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v6))
        v7
-- Algebra.Bundles.NonAssociativeRing._.setoid
d_setoid_3266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3266 ~v0 ~v1 v2 = du_setoid_3266 v2
du_setoid_3266 ::
  T_NonAssociativeRing_3158 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3266 v0
  = let v1 = d_isNonAssociativeRing_3194 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.NonAssociativeRing._.sym
d_sym_3268 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3268 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                        (coe d_isNonAssociativeRing_3194 (coe v0))))))))
-- Algebra.Bundles.NonAssociativeRing._.trans
d_trans_3270 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3270 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
                        (coe d_isNonAssociativeRing_3194 (coe v0))))))))
-- Algebra.Bundles.NonAssociativeRing._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_3272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_3272 ~v0 ~v1 v2
  = du_unique'691''45''8315''185'_3272 v2
du_unique'691''45''8315''185'_3272 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_3272 v0
  = let v1 = d__'43'__3184 (coe v0) in
    let v2 = d_'45'__3188 (coe v0) in
    let v3 = d_0'35'_3190 (coe v0) in
    let v4 = d_isNonAssociativeRing_3194 (coe v0) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'691''45''8315''185'_964
      (coe v1) (coe v3) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5))
-- Algebra.Bundles.NonAssociativeRing._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_3274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_3274 ~v0 ~v1 v2
  = du_unique'737''45''8315''185'_3274 v2
du_unique'737''45''8315''185'_3274 ::
  T_NonAssociativeRing_3158 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_3274 v0
  = let v1 = d__'43'__3184 (coe v0) in
    let v2 = d_'45'__3188 (coe v0) in
    let v3 = d_0'35'_3190 (coe v0) in
    let v4 = d_isNonAssociativeRing_3194 (coe v0) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'737''45''8315''185'_958
      (coe v1) (coe v3) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5))
-- Algebra.Bundles.NonAssociativeRing._.zero
d_zero_3276 ::
  T_NonAssociativeRing_3158 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_3276 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_2210
      (coe d_isNonAssociativeRing_3194 (coe v0))
-- Algebra.Bundles.NonAssociativeRing.+-abelianGroup
d_'43''45'abelianGroup_3278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> T_AbelianGroup_1378
d_'43''45'abelianGroup_3278 ~v0 ~v1 v2
  = du_'43''45'abelianGroup_3278 v2
du_'43''45'abelianGroup_3278 ::
  T_NonAssociativeRing_3158 -> T_AbelianGroup_1378
du_'43''45'abelianGroup_3278 v0
  = coe
      C_AbelianGroup'46'constructor_24425 (d__'43'__3184 (coe v0))
      (d_0'35'_3190 (coe v0)) (d_'45'__3188 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2202
         (coe d_isNonAssociativeRing_3194 (coe v0)))
-- Algebra.Bundles.NonAssociativeRing._.group
d_group_3282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> T_Group_1266
d_group_3282 ~v0 ~v1 v2 = du_group_3282 v2
du_group_3282 :: T_NonAssociativeRing_3158 -> T_Group_1266
du_group_3282 v0
  = coe du_group_1474 (coe du_'43''45'abelianGroup_3278 (coe v0))
-- Algebra.Bundles.NonAssociativeRing._.invertibleMagma
d_invertibleMagma_3284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> T_InvertibleMagma_1106
d_invertibleMagma_3284 ~v0 ~v1 v2 = du_invertibleMagma_3284 v2
du_invertibleMagma_3284 ::
  T_NonAssociativeRing_3158 -> T_InvertibleMagma_1106
du_invertibleMagma_3284 v0
  = let v1 = coe du_'43''45'abelianGroup_3278 (coe v0) in
    coe du_invertibleMagma_1370 (coe du_group_1474 (coe v1))
-- Algebra.Bundles.NonAssociativeRing._.invertibleUnitalMagma
d_invertibleUnitalMagma_3286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_NonAssociativeRing_3158 -> T_InvertibleUnitalMagma_1180
d_invertibleUnitalMagma_3286 ~v0 ~v1 v2
  = du_invertibleUnitalMagma_3286 v2
du_invertibleUnitalMagma_3286 ::
  T_NonAssociativeRing_3158 -> T_InvertibleUnitalMagma_1180
du_invertibleUnitalMagma_3286 v0
  = let v1 = coe du_'43''45'abelianGroup_3278 (coe v0) in
    coe du_invertibleUnitalMagma_1372 (coe du_group_1474 (coe v1))
-- Algebra.Bundles.Nearring
d_Nearring_3292 a0 a1 = ()
data T_Nearring_3292
  = C_Nearring'46'constructor_58065 (AgdaAny -> AgdaAny -> AgdaAny)
                                    (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny) AgdaAny
                                    AgdaAny MAlonzo.Code.Algebra.Structures.T_IsNearring_2294
-- Algebra.Bundles.Nearring.Carrier
d_Carrier_3314 :: T_Nearring_3292 -> ()
d_Carrier_3314 = erased
-- Algebra.Bundles.Nearring._≈_
d__'8776'__3316 :: T_Nearring_3292 -> AgdaAny -> AgdaAny -> ()
d__'8776'__3316 = erased
-- Algebra.Bundles.Nearring._+_
d__'43'__3318 :: T_Nearring_3292 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__3318 v0
  = case coe v0 of
      C_Nearring'46'constructor_58065 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Nearring._*_
d__'42'__3320 :: T_Nearring_3292 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__3320 v0
  = case coe v0 of
      C_Nearring'46'constructor_58065 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Nearring.-_
d_'45'__3322 :: T_Nearring_3292 -> AgdaAny -> AgdaAny
d_'45'__3322 v0
  = case coe v0 of
      C_Nearring'46'constructor_58065 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Nearring.0#
d_0'35'_3324 :: T_Nearring_3292 -> AgdaAny
d_0'35'_3324 v0
  = case coe v0 of
      C_Nearring'46'constructor_58065 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Nearring.1#
d_1'35'_3326 :: T_Nearring_3292 -> AgdaAny
d_1'35'_3326 v0
  = case coe v0 of
      C_Nearring'46'constructor_58065 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Nearring.isNearring
d_isNearring_3328 ::
  T_Nearring_3292 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearring_2294
d_isNearring_3328 v0
  = case coe v0 of
      C_Nearring'46'constructor_58065 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Nearring._.*-assoc
d_'42''45'assoc_3332 ::
  T_Nearring_3292 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_3332 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1988
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
         (coe d_isNearring_3328 (coe v0)))
-- Algebra.Bundles.Nearring._.*-cong
d_'42''45'cong_3334 ::
  T_Nearring_3292 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_3334 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1986
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
         (coe d_isNearring_3328 (coe v0)))
-- Algebra.Bundles.Nearring._.∙-congʳ
d_'8729''45'cong'691'_3336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3336 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3336 v2
du_'8729''45'cong'691'_3336 ::
  T_Nearring_3292 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3336 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.Nearring._.∙-congˡ
d_'8729''45'cong'737'_3338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3338 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3338 v2
du_'8729''45'cong'737'_3338 ::
  T_Nearring_3292 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3338 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.Nearring._.*-identity
d_'42''45'identity_3340 ::
  T_Nearring_3292 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_3340 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1990
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
         (coe d_isNearring_3328 (coe v0)))
-- Algebra.Bundles.Nearring._.identityʳ
d_identity'691'_3342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> AgdaAny -> AgdaAny
d_identity'691'_3342 ~v0 ~v1 v2 = du_identity'691'_3342 v2
du_identity'691'_3342 :: T_Nearring_3292 -> AgdaAny -> AgdaAny
du_identity'691'_3342 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036 (coe v2))
-- Algebra.Bundles.Nearring._.identityˡ
d_identity'737'_3344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> AgdaAny -> AgdaAny
d_identity'737'_3344 ~v0 ~v1 v2 = du_identity'737'_3344 v2
du_identity'737'_3344 :: T_Nearring_3292 -> AgdaAny -> AgdaAny
du_identity'737'_3344 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036 (coe v2))
-- Algebra.Bundles.Nearring._.*-isMagma
d_'42''45'isMagma_3346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_3346 ~v0 ~v1 v2 = du_'42''45'isMagma_3346 v2
du_'42''45'isMagma_3346 ::
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_3346 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2032
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1))
-- Algebra.Bundles.Nearring._.*-isMonoid
d_'42''45'isMonoid_3348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_3348 ~v0 ~v1 v2 = du_'42''45'isMonoid_3348 v2
du_'42''45'isMonoid_3348 ::
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_3348 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2036
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1))
-- Algebra.Bundles.Nearring._.*-isSemigroup
d_'42''45'isSemigroup_3350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_3350 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_3350 v2
du_'42''45'isSemigroup_3350 ::
  T_Nearring_3292 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_3350 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_2034
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1))
-- Algebra.Bundles.Nearring._.assoc
d_assoc_3352 ::
  T_Nearring_3292 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_3352 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
               (coe d_isNearring_3328 (coe v0)))))
-- Algebra.Bundles.Nearring._.∙-cong
d_'8729''45'cong_3354 ::
  T_Nearring_3292 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3354 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
                  (coe d_isNearring_3328 (coe v0))))))
-- Algebra.Bundles.Nearring._.∙-congʳ
d_'8729''45'cong'691'_3356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3356 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3356 v2
du_'8729''45'cong'691'_3356 ::
  T_Nearring_3292 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3356 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.Nearring._.∙-congˡ
d_'8729''45'cong'737'_3358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3358 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3358 v2
du_'8729''45'cong'737'_3358 ::
  T_Nearring_3292 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3358 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.Nearring._.identity
d_identity_3360 ::
  T_Nearring_3292 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3360 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
            (coe d_isNearring_3328 (coe v0))))
-- Algebra.Bundles.Nearring._.identityʳ
d_identity'691'_3362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> AgdaAny -> AgdaAny
d_identity'691'_3362 ~v0 ~v1 v2 = du_identity'691'_3362 v2
du_identity'691'_3362 :: T_Nearring_3292 -> AgdaAny -> AgdaAny
du_identity'691'_3362 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v2))
-- Algebra.Bundles.Nearring._.identityˡ
d_identity'737'_3364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> AgdaAny -> AgdaAny
d_identity'737'_3364 ~v0 ~v1 v2 = du_identity'737'_3364 v2
du_identity'737'_3364 :: T_Nearring_3292 -> AgdaAny -> AgdaAny
du_identity'737'_3364 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v2))
-- Algebra.Bundles.Nearring._.+-inverse
d_'43''45'inverse_3366 ::
  T_Nearring_3292 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_3366 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'inverse_2314
      (coe d_isNearring_3328 (coe v0))
-- Algebra.Bundles.Nearring._.+-inverseʳ
d_'43''45'inverse'691'_3368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> AgdaAny -> AgdaAny
d_'43''45'inverse'691'_3368 ~v0 ~v1 v2
  = du_'43''45'inverse'691'_3368 v2
du_'43''45'inverse'691'_3368 ::
  T_Nearring_3292 -> AgdaAny -> AgdaAny
du_'43''45'inverse'691'_3368 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'43''45'inverse'691'_2382
      (coe d_isNearring_3328 (coe v0))
-- Algebra.Bundles.Nearring._.+-inverseˡ
d_'43''45'inverse'737'_3370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> AgdaAny -> AgdaAny
d_'43''45'inverse'737'_3370 ~v0 ~v1 v2
  = du_'43''45'inverse'737'_3370 v2
du_'43''45'inverse'737'_3370 ::
  T_Nearring_3292 -> AgdaAny -> AgdaAny
du_'43''45'inverse'737'_3370 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'43''45'inverse'737'_2380
      (coe d_isNearring_3328 (coe v0))
-- Algebra.Bundles.Nearring._.isMagma
d_isMagma_3372 ::
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_3372 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
               (coe d_isNearring_3328 (coe v0)))))
-- Algebra.Bundles.Nearring._.+-isMonoid
d_'43''45'isMonoid_3374 ::
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_3374 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
         (coe d_isNearring_3328 (coe v0)))
-- Algebra.Bundles.Nearring._.isSemigroup
d_isSemigroup_3376 ::
  T_Nearring_3292 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_3376 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
            (coe d_isNearring_3328 (coe v0))))
-- Algebra.Bundles.Nearring._.isUnitalMagma
d_isUnitalMagma_3378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_3378 ~v0 ~v1 v2 = du_isUnitalMagma_3378 v2
du_isUnitalMagma_3378 ::
  T_Nearring_3292 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_3378 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984 (coe v2))
-- Algebra.Bundles.Nearring._.distrib
d_distrib_3380 ::
  T_Nearring_3292 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_3380 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1992
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
         (coe d_isNearring_3328 (coe v0)))
-- Algebra.Bundles.Nearring._.isEquivalence
d_isEquivalence_3382 ::
  T_Nearring_3292 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3382 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
                  (coe d_isNearring_3328 (coe v0))))))
-- Algebra.Bundles.Nearring._.isPartialEquivalence
d_isPartialEquivalence_3384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3384 ~v0 ~v1 v2
  = du_isPartialEquivalence_3384 v2
du_isPartialEquivalence_3384 ::
  T_Nearring_3292 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3384 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Bundles.Nearring._.isQuasiring
d_isQuasiring_3386 ::
  T_Nearring_3292 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasiring_1962
d_isQuasiring_3386 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
      (coe d_isNearring_3328 (coe v0))
-- Algebra.Bundles.Nearring._.refl
d_refl_3388 :: T_Nearring_3292 -> AgdaAny -> AgdaAny
d_refl_3388 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
                     (coe d_isNearring_3328 (coe v0)))))))
-- Algebra.Bundles.Nearring._.reflexive
d_reflexive_3390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3390 ~v0 ~v1 v2 = du_reflexive_3390 v2
du_reflexive_3390 ::
  T_Nearring_3292 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3390 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Bundles.Nearring._.setoid
d_setoid_3392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3392 ~v0 ~v1 v2 = du_setoid_3392 v2
du_setoid_3392 ::
  T_Nearring_3292 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3392 v0
  = let v1 = d_isNearring_3328 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.Nearring._.sym
d_sym_3394 ::
  T_Nearring_3292 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3394 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
                     (coe d_isNearring_3328 (coe v0)))))))
-- Algebra.Bundles.Nearring._.trans
d_trans_3396 ::
  T_Nearring_3292 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3396 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isMonoid_1984
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
                     (coe d_isNearring_3328 (coe v0)))))))
-- Algebra.Bundles.Nearring._.zero
d_zero_3398 ::
  T_Nearring_3292 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_3398 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1994
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
         (coe d_isNearring_3328 (coe v0)))
-- Algebra.Bundles.Nearring._.⁻¹-cong
d_'8315''185''45'cong_3400 ::
  T_Nearring_3292 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_3400 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_2316
      (coe d_isNearring_3328 (coe v0))
-- Algebra.Bundles.Nearring.quasiring
d_quasiring_3402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> T_Quasiring_2882
d_quasiring_3402 ~v0 ~v1 v2 = du_quasiring_3402 v2
du_quasiring_3402 :: T_Nearring_3292 -> T_Quasiring_2882
du_quasiring_3402 v0
  = coe
      C_Quasiring'46'constructor_50409 (d__'43'__3318 (coe v0))
      (d__'42'__3320 (coe v0)) (d_0'35'_3324 (coe v0))
      (d_1'35'_3326 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isQuasiring_2312
         (coe d_isNearring_3328 (coe v0)))
-- Algebra.Bundles.Nearring._._≉_
d__'8777'__3406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> AgdaAny -> AgdaAny -> ()
d__'8777'__3406 = erased
-- Algebra.Bundles.Nearring._.magma
d_magma_3408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> T_Magma_8
d_magma_3408 ~v0 ~v1 v2 = du_magma_3408 v2
du_magma_3408 :: T_Nearring_3292 -> T_Magma_8
du_magma_3408 v0
  = let v1 = coe du_quasiring_3402 (coe v0) in
    let v2 = coe du_'42''45'monoid_2994 (coe v1) in
    coe du_magma_524 (coe du_semigroup_802 (coe v2))
-- Algebra.Bundles.Nearring._.*-monoid
d_'42''45'monoid_3410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> T_Monoid_740
d_'42''45'monoid_3410 ~v0 ~v1 v2 = du_'42''45'monoid_3410 v2
du_'42''45'monoid_3410 :: T_Nearring_3292 -> T_Monoid_740
du_'42''45'monoid_3410 v0
  = coe du_'42''45'monoid_2994 (coe du_quasiring_3402 (coe v0))
-- Algebra.Bundles.Nearring._.rawMagma
d_rawMagma_3412 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_3412 ~v0 ~v1 v2 = du_rawMagma_3412 v2
du_rawMagma_3412 ::
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_3412 v0
  = let v1 = coe du_quasiring_3402 (coe v0) in
    let v2 = coe du_'42''45'monoid_2994 (coe v1) in
    let v3 = coe du_semigroup_802 (coe v2) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v3))
-- Algebra.Bundles.Nearring._.semigroup
d_semigroup_3414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> T_Semigroup_476
d_semigroup_3414 ~v0 ~v1 v2 = du_semigroup_3414 v2
du_semigroup_3414 :: T_Nearring_3292 -> T_Semigroup_476
du_semigroup_3414 v0
  = let v1 = coe du_quasiring_3402 (coe v0) in
    coe du_semigroup_802 (coe du_'42''45'monoid_2994 (coe v1))
-- Algebra.Bundles.Nearring._.magma
d_magma_3416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> T_Magma_8
d_magma_3416 ~v0 ~v1 v2 = du_magma_3416 v2
du_magma_3416 :: T_Nearring_3292 -> T_Magma_8
du_magma_3416 v0
  = let v1 = coe du_quasiring_3402 (coe v0) in
    let v2 = coe du_'43''45'monoid_2978 (coe v1) in
    coe du_magma_524 (coe du_semigroup_802 (coe v2))
-- Algebra.Bundles.Nearring._.+-monoid
d_'43''45'monoid_3418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> T_Monoid_740
d_'43''45'monoid_3418 ~v0 ~v1 v2 = du_'43''45'monoid_3418 v2
du_'43''45'monoid_3418 :: T_Nearring_3292 -> T_Monoid_740
du_'43''45'monoid_3418 v0
  = coe du_'43''45'monoid_2978 (coe du_quasiring_3402 (coe v0))
-- Algebra.Bundles.Nearring._.rawMagma
d_rawMagma_3420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_3420 ~v0 ~v1 v2 = du_rawMagma_3420 v2
du_rawMagma_3420 ::
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_3420 v0
  = let v1 = coe du_quasiring_3402 (coe v0) in
    let v2 = coe du_'43''45'monoid_2978 (coe v1) in
    let v3 = coe du_semigroup_802 (coe v2) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v3))
-- Algebra.Bundles.Nearring._.rawMonoid
d_rawMonoid_3422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_3422 ~v0 ~v1 v2 = du_rawMonoid_3422 v2
du_rawMonoid_3422 ::
  T_Nearring_3292 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_3422 v0
  = let v1 = coe du_quasiring_3402 (coe v0) in
    coe du_rawMonoid_812 (coe du_'43''45'monoid_2978 (coe v1))
-- Algebra.Bundles.Nearring._.semigroup
d_semigroup_3424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> T_Semigroup_476
d_semigroup_3424 ~v0 ~v1 v2 = du_semigroup_3424 v2
du_semigroup_3424 :: T_Nearring_3292 -> T_Semigroup_476
du_semigroup_3424 v0
  = let v1 = coe du_quasiring_3402 (coe v0) in
    coe du_semigroup_802 (coe du_'43''45'monoid_2978 (coe v1))
-- Algebra.Bundles.Nearring._.unitalMagma
d_unitalMagma_3426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Nearring_3292 -> T_UnitalMagma_672
d_unitalMagma_3426 ~v0 ~v1 v2 = du_unitalMagma_3426 v2
du_unitalMagma_3426 :: T_Nearring_3292 -> T_UnitalMagma_672
du_unitalMagma_3426 v0
  = let v1 = coe du_quasiring_3402 (coe v0) in
    coe du_unitalMagma_814 (coe du_'43''45'monoid_2978 (coe v1))
-- Algebra.Bundles.Ring
d_Ring_3432 a0 a1 = ()
data T_Ring_3432
  = C_Ring'46'constructor_60565 (AgdaAny -> AgdaAny -> AgdaAny)
                                (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny) AgdaAny
                                AgdaAny MAlonzo.Code.Algebra.Structures.T_IsRing_2394
-- Algebra.Bundles.Ring.Carrier
d_Carrier_3454 :: T_Ring_3432 -> ()
d_Carrier_3454 = erased
-- Algebra.Bundles.Ring._≈_
d__'8776'__3456 :: T_Ring_3432 -> AgdaAny -> AgdaAny -> ()
d__'8776'__3456 = erased
-- Algebra.Bundles.Ring._+_
d__'43'__3458 :: T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__3458 v0
  = case coe v0 of
      C_Ring'46'constructor_60565 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Ring._*_
d__'42'__3460 :: T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__3460 v0
  = case coe v0 of
      C_Ring'46'constructor_60565 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Ring.-_
d_'45'__3462 :: T_Ring_3432 -> AgdaAny -> AgdaAny
d_'45'__3462 v0
  = case coe v0 of
      C_Ring'46'constructor_60565 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Ring.0#
d_0'35'_3464 :: T_Ring_3432 -> AgdaAny
d_0'35'_3464 v0
  = case coe v0 of
      C_Ring'46'constructor_60565 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Ring.1#
d_1'35'_3466 :: T_Ring_3432 -> AgdaAny
d_1'35'_3466 v0
  = case coe v0 of
      C_Ring'46'constructor_60565 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Ring.isRing
d_isRing_3468 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_3468 v0
  = case coe v0 of
      C_Ring'46'constructor_60565 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Ring._._-_
d__'45'__3472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__3472 ~v0 ~v1 v2 = du__'45'__3472 v2
du__'45'__3472 :: T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__3472 v0
  = let v1 = d__'43'__3458 (coe v0) in
    let v2 = d_'45'__3462 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v1) (coe v2)
-- Algebra.Bundles.Ring._.*-assoc
d_'42''45'assoc_3474 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_3474 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_2422
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.*-cong
d_'42''45'cong_3476 ::
  T_Ring_3432 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_3476 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_2420
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.∙-congʳ
d_'8729''45'cong'691'_3478 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3478 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3478 v2
du_'8729''45'cong'691'_3478 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3478 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Ring._.∙-congˡ
d_'8729''45'cong'737'_3480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3480 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3480 v2
du_'8729''45'cong'737'_3480 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3480 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Bundles.Ring._.*-identity
d_'42''45'identity_3482 ::
  T_Ring_3432 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_3482 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.identityʳ
d_identity'691'_3484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny
d_identity'691'_3484 ~v0 ~v1 v2 = du_identity'691'_3484 v2
du_identity'691'_3484 :: T_Ring_3432 -> AgdaAny -> AgdaAny
du_identity'691'_3484 v0
  = let v1 = d_isRing_3468 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500 (coe v1))
-- Algebra.Bundles.Ring._.identityˡ
d_identity'737'_3486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny
d_identity'737'_3486 ~v0 ~v1 v2 = du_identity'737'_3486 v2
du_identity'737'_3486 :: T_Ring_3432 -> AgdaAny -> AgdaAny
du_identity'737'_3486 v0
  = let v1 = d_isRing_3468 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500 (coe v1))
-- Algebra.Bundles.Ring._.*-isMagma
d_'42''45'isMagma_3488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_3488 ~v0 ~v1 v2 = du_'42''45'isMagma_3488 v2
du_'42''45'isMagma_3488 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_3488 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2496
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.*-isMonoid
d_'42''45'isMonoid_3490 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_3490 ~v0 ~v1 v2 = du_'42''45'isMonoid_3490 v2
du_'42''45'isMonoid_3490 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_3490 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.*-isSemigroup
d_'42''45'isSemigroup_3492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_3492 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_3492 v2
du_'42''45'isSemigroup_3492 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_3492 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_2498
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.assoc
d_assoc_3494 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_3494 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                  (coe d_isRing_3468 (coe v0))))))
-- Algebra.Bundles.Ring._.comm
d_comm_3496 :: T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_3496 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_990
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe d_isRing_3468 (coe v0)))
-- Algebra.Bundles.Ring._.∙-cong
d_'8729''45'cong_3498 ::
  T_Ring_3432 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3498 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                     (coe d_isRing_3468 (coe v0)))))))
-- Algebra.Bundles.Ring._.∙-congʳ
d_'8729''45'cong'691'_3500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3500 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3500 v2
du_'8729''45'cong'691'_3500 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3500 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.Ring._.∙-congˡ
d_'8729''45'cong'737'_3502 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3502 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3502 v2
du_'8729''45'cong'737'_3502 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3502 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.Ring._.identity
d_identity_3504 ::
  T_Ring_3432 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3504 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe d_isRing_3468 (coe v0)))))
-- Algebra.Bundles.Ring._.identityʳ
d_identity'691'_3506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny
d_identity'691'_3506 ~v0 ~v1 v2 = du_identity'691'_3506 v2
du_identity'691'_3506 :: T_Ring_3432 -> AgdaAny -> AgdaAny
du_identity'691'_3506 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3))
-- Algebra.Bundles.Ring._.identityˡ
d_identity'737'_3508 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny
d_identity'737'_3508 ~v0 ~v1 v2 = du_identity'737'_3508 v2
du_identity'737'_3508 :: T_Ring_3432 -> AgdaAny -> AgdaAny
du_identity'737'_3508 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3))
-- Algebra.Bundles.Ring._.+-isAbelianGroup
d_'43''45'isAbelianGroup_3510 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_3510 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.isCommutativeMagma
d_isCommutativeMagma_3512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_3512 ~v0 ~v1 v2
  = du_isCommutativeMagma_3512 v2
du_isCommutativeMagma_3512 ::
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_3512 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v3))
-- Algebra.Bundles.Ring._.isCommutativeMonoid
d_isCommutativeMonoid_3514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_3514 ~v0 ~v1 v2
  = du_isCommutativeMonoid_3514 v2
du_isCommutativeMonoid_3514 ::
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_3514 v0
  = let v1 = d_isRing_3468 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe v1))
-- Algebra.Bundles.Ring._.isCommutativeSemigroup
d_isCommutativeSemigroup_3516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_3516 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_3516 v2
du_isCommutativeSemigroup_3516 ::
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_3516 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
         (coe v2))
-- Algebra.Bundles.Ring._.isGroup
d_isGroup_3518 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_3518 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe d_isRing_3468 (coe v0)))
-- Algebra.Bundles.Ring._.isInvertibleMagma
d_isInvertibleMagma_3520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_3520 ~v0 ~v1 v2 = du_isInvertibleMagma_3520 v2
du_isInvertibleMagma_3520 ::
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
du_isInvertibleMagma_3520 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleMagma_966
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.Ring._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_3522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_3522 ~v0 ~v1 v2
  = du_isInvertibleUnitalMagma_3522 v2
du_isInvertibleUnitalMagma_3522 ::
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_3522 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleUnitalMagma_968
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.Ring._.isMagma
d_isMagma_3524 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_3524 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                  (coe d_isRing_3468 (coe v0))))))
-- Algebra.Bundles.Ring._.isMonoid
d_isMonoid_3526 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_3526 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe d_isRing_3468 (coe v0))))
-- Algebra.Bundles.Ring._.isSemigroup
d_isSemigroup_3528 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_3528 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe d_isRing_3468 (coe v0)))))
-- Algebra.Bundles.Ring._.isUnitalMagma
d_isUnitalMagma_3530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_3530 ~v0 ~v1 v2 = du_isUnitalMagma_3530 v2
du_isUnitalMagma_3530 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_3530 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3))
-- Algebra.Bundles.Ring._.⁻¹-cong
d_'8315''185''45'cong_3532 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_3532 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe d_isRing_3468 (coe v0))))
-- Algebra.Bundles.Ring._.inverse
d_inverse_3534 ::
  T_Ring_3432 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_3534 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe d_isRing_3468 (coe v0))))
-- Algebra.Bundles.Ring._.inverseʳ
d_inverse'691'_3536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny
d_inverse'691'_3536 ~v0 ~v1 v2 = du_inverse'691'_3536 v2
du_inverse'691'_3536 :: T_Ring_3432 -> AgdaAny -> AgdaAny
du_inverse'691'_3536 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.Ring._.inverseˡ
d_inverse'737'_3538 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny
d_inverse'737'_3538 ~v0 ~v1 v2 = du_inverse'737'_3538 v2
du_inverse'737'_3538 :: T_Ring_3432 -> AgdaAny -> AgdaAny
du_inverse'737'_3538 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2))
-- Algebra.Bundles.Ring._.distrib
d_distrib_3540 ::
  T_Ring_3432 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_3540 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_2426
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.distribʳ
d_distrib'691'_3542 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_3542 ~v0 ~v1 v2 = du_distrib'691'_3542 v2
du_distrib'691'_3542 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_3542 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.Ring._.distribˡ
d_distrib'737'_3544 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_3544 ~v0 ~v1 v2 = du_distrib'737'_3544 v2
du_distrib'737'_3544 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_3544 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Algebra.Bundles.Ring._.isEquivalence
d_isEquivalence_3546 ::
  T_Ring_3432 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3546 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                     (coe d_isRing_3468 (coe v0)))))))
-- Algebra.Bundles.Ring._.isNearSemiring
d_isNearSemiring_3548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_3548 ~v0 ~v1 v2 = du_isNearSemiring_3548 v2
du_isNearSemiring_3548 ::
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_3548 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Algebra.Bundles.Ring._.isPartialEquivalence
d_isPartialEquivalence_3550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3550 ~v0 ~v1 v2
  = du_isPartialEquivalence_3550 v2
du_isPartialEquivalence_3550 ::
  T_Ring_3432 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3550 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v6))
-- Algebra.Bundles.Ring._.isSemiring
d_isSemiring_3552 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_3552 ~v0 ~v1 v2 = du_isSemiring_3552 v2
du_isSemiring_3552 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
du_isSemiring_3552 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isSemiring_2518
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_3554 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_3554 ~v0 ~v1 v2
  = du_isSemiringWithoutAnnihilatingZero_3554 v2
du_isSemiringWithoutAnnihilatingZero_3554 ::
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
du_isSemiringWithoutAnnihilatingZero_3554 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutAnnihilatingZero_2516
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.isSemiringWithoutOne
d_isSemiringWithoutOne_3556 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_3556 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_3556 v2
du_isSemiringWithoutOne_3556 ::
  T_Ring_3432 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_3556 v0
  = let v1 = d_isRing_3468 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v1))
-- Algebra.Bundles.Ring._.refl
d_refl_3558 :: T_Ring_3432 -> AgdaAny -> AgdaAny
d_refl_3558 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                        (coe d_isRing_3468 (coe v0))))))))
-- Algebra.Bundles.Ring._.reflexive
d_reflexive_3560 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3560 ~v0 ~v1 v2 = du_reflexive_3560 v2
du_reflexive_3560 ::
  T_Ring_3432 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3560 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v6))
        v7
-- Algebra.Bundles.Ring._.setoid
d_setoid_3562 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3562 ~v0 ~v1 v2 = du_setoid_3562 v2
du_setoid_3562 ::
  T_Ring_3432 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3562 v0
  = let v1 = d_isRing_3468 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
-- Algebra.Bundles.Ring._.sym
d_sym_3564 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3564 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                        (coe d_isRing_3468 (coe v0))))))))
-- Algebra.Bundles.Ring._.trans
d_trans_3566 ::
  T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3566 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                        (coe d_isRing_3468 (coe v0))))))))
-- Algebra.Bundles.Ring._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_3568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_3568 ~v0 ~v1 v2
  = du_unique'691''45''8315''185'_3568 v2
du_unique'691''45''8315''185'_3568 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_3568 v0
  = let v1 = d__'43'__3458 (coe v0) in
    let v2 = d_'45'__3462 (coe v0) in
    let v3 = d_0'35'_3464 (coe v0) in
    let v4 = d_isRing_3468 (coe v0) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'691''45''8315''185'_964
      (coe v1) (coe v3) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5))
-- Algebra.Bundles.Ring._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_3570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_3570 ~v0 ~v1 v2
  = du_unique'737''45''8315''185'_3570 v2
du_unique'737''45''8315''185'_3570 ::
  T_Ring_3432 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_3570 v0
  = let v1 = d__'43'__3458 (coe v0) in
    let v2 = d_'45'__3462 (coe v0) in
    let v3 = d_0'35'_3464 (coe v0) in
    let v4 = d_isRing_3468 (coe v0) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'737''45''8315''185'_958
      (coe v1) (coe v3) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5))
-- Algebra.Bundles.Ring._.zero
d_zero_3572 ::
  T_Ring_3432 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_3572 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_2428
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.zeroʳ
d_zero'691'_3574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny
d_zero'691'_3574 ~v0 ~v1 v2 = du_zero'691'_3574 v2
du_zero'691'_3574 :: T_Ring_3432 -> AgdaAny -> AgdaAny
du_zero'691'_3574 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_2514
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring._.zeroˡ
d_zero'737'_3576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny
d_zero'737'_3576 ~v0 ~v1 v2 = du_zero'737'_3576 v2
du_zero'737'_3576 :: T_Ring_3432 -> AgdaAny -> AgdaAny
du_zero'737'_3576 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_2512
      (coe d_isRing_3468 (coe v0))
-- Algebra.Bundles.Ring.+-abelianGroup
d_'43''45'abelianGroup_3578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_AbelianGroup_1378
d_'43''45'abelianGroup_3578 ~v0 ~v1 v2
  = du_'43''45'abelianGroup_3578 v2
du_'43''45'abelianGroup_3578 :: T_Ring_3432 -> T_AbelianGroup_1378
du_'43''45'abelianGroup_3578 v0
  = coe
      C_AbelianGroup'46'constructor_24425 (d__'43'__3458 (coe v0))
      (d_0'35'_3464 (coe v0)) (d_'45'__3462 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe d_isRing_3468 (coe v0)))
-- Algebra.Bundles.Ring.semiring
d_semiring_3580 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_Semiring_1986
d_semiring_3580 ~v0 ~v1 v2 = du_semiring_3580 v2
du_semiring_3580 :: T_Ring_3432 -> T_Semiring_1986
du_semiring_3580 v0
  = coe
      C_Semiring'46'constructor_35691 (d__'43'__3458 (coe v0))
      (d__'42'__3460 (coe v0)) (d_0'35'_3464 (coe v0))
      (d_1'35'_3466 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiring_2518
         (coe d_isRing_3468 (coe v0)))
-- Algebra.Bundles.Ring._._≉_
d__'8777'__3584 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> AgdaAny -> AgdaAny -> ()
d__'8777'__3584 = erased
-- Algebra.Bundles.Ring._.magma
d_magma_3586 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Ring_3432 -> T_Magma_8
d_magma_3586 ~v0 ~v1 v2 = du_magma_3586 v2
du_magma_3586 :: T_Ring_3432 -> T_Magma_8
du_magma_3586 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'42''45'monoid_1970 (coe v2) in
    coe du_magma_524 (coe du_semigroup_802 (coe v3))
-- Algebra.Bundles.Ring._.*-monoid
d_'42''45'monoid_3588 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_Monoid_740
d_'42''45'monoid_3588 ~v0 ~v1 v2 = du_'42''45'monoid_3588 v2
du_'42''45'monoid_3588 :: T_Ring_3432 -> T_Monoid_740
du_'42''45'monoid_3588 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    coe
      du_'42''45'monoid_1970
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v1))
-- Algebra.Bundles.Ring._.rawMagma
d_rawMagma_3590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_3590 ~v0 ~v1 v2 = du_rawMagma_3590 v2
du_rawMagma_3590 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_3590 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'42''45'monoid_1970 (coe v2) in
    let v4 = coe du_semigroup_802 (coe v3) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v4))
-- Algebra.Bundles.Ring._.rawMonoid
d_rawMonoid_3592 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_3592 ~v0 ~v1 v2 = du_rawMonoid_3592 v2
du_rawMonoid_3592 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_3592 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe du_rawMonoid_812 (coe du_'42''45'monoid_1970 (coe v2))
-- Algebra.Bundles.Ring._.semigroup
d_semigroup_3594 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_Semigroup_476
d_semigroup_3594 ~v0 ~v1 v2 = du_semigroup_3594 v2
du_semigroup_3594 :: T_Ring_3432 -> T_Semigroup_476
du_semigroup_3594 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe du_semigroup_802 (coe du_'42''45'monoid_1970 (coe v2))
-- Algebra.Bundles.Ring._.commutativeMagma
d_commutativeMagma_3596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_CommutativeMagma_120
d_commutativeMagma_3596 ~v0 ~v1 v2 = du_commutativeMagma_3596 v2
du_commutativeMagma_3596 :: T_Ring_3432 -> T_CommutativeMagma_120
du_commutativeMagma_3596 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v3))
-- Algebra.Bundles.Ring._.+-commutativeMonoid
d_'43''45'commutativeMonoid_3598 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_3598 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_3598 v2
du_'43''45'commutativeMonoid_3598 ::
  T_Ring_3432 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_3598 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    coe
      du_'43''45'commutativeMonoid_1948
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v1))
-- Algebra.Bundles.Ring._.commutativeSemigroup
d_commutativeSemigroup_3600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_3600 ~v0 ~v1 v2
  = du_commutativeSemigroup_3600 v2
du_commutativeSemigroup_3600 ::
  T_Ring_3432 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_3600 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1948 (coe v2))
-- Algebra.Bundles.Ring._.magma
d_magma_3602 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Ring_3432 -> T_Magma_8
d_magma_3602 ~v0 ~v1 v2 = du_magma_3602 v2
du_magma_3602 :: T_Ring_3432 -> T_Magma_8
du_magma_3602 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    let v4 = coe du_monoid_890 (coe v3) in
    coe du_magma_524 (coe du_semigroup_802 (coe v4))
-- Algebra.Bundles.Ring._.monoid
d_monoid_3604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_Monoid_740
d_monoid_3604 ~v0 ~v1 v2 = du_monoid_3604 v2
du_monoid_3604 :: T_Ring_3432 -> T_Monoid_740
du_monoid_3604 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    coe du_monoid_890 (coe du_'43''45'commutativeMonoid_1948 (coe v2))
-- Algebra.Bundles.Ring._.rawMagma
d_rawMagma_3606 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_3606 ~v0 ~v1 v2 = du_rawMagma_3606 v2
du_rawMagma_3606 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_3606 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    let v4 = coe du_monoid_890 (coe v3) in
    let v5 = coe du_semigroup_802 (coe v4) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v5))
-- Algebra.Bundles.Ring._.rawMonoid
d_rawMonoid_3608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_3608 ~v0 ~v1 v2 = du_rawMonoid_3608 v2
du_rawMonoid_3608 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_3608 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v3))
-- Algebra.Bundles.Ring._.semigroup
d_semigroup_3610 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_Semigroup_476
d_semigroup_3610 ~v0 ~v1 v2 = du_semigroup_3610 v2
du_semigroup_3610 :: T_Ring_3432 -> T_Semigroup_476
du_semigroup_3610 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v3))
-- Algebra.Bundles.Ring._.unitalMagma
d_unitalMagma_3612 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_UnitalMagma_672
d_unitalMagma_3612 ~v0 ~v1 v2 = du_unitalMagma_3612 v2
du_unitalMagma_3612 :: T_Ring_3432 -> T_UnitalMagma_672
du_unitalMagma_3612 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    let v2 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v1) in
    let v3 = coe du_'43''45'commutativeMonoid_1948 (coe v2) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v3))
-- Algebra.Bundles.Ring._.nearSemiring
d_nearSemiring_3614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_NearSemiring_1508
d_nearSemiring_3614 ~v0 ~v1 v2 = du_nearSemiring_3614 v2
du_nearSemiring_3614 :: T_Ring_3432 -> T_NearSemiring_1508
du_nearSemiring_3614 v0
  = let v1 = coe du_semiring_3580 (coe v0) in
    coe du_nearSemiring_1688 (coe du_semiringWithoutOne_2142 (coe v1))
-- Algebra.Bundles.Ring._.semiringWithoutAnnihilatingZero
d_semiringWithoutAnnihilatingZero_3616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_SemiringWithoutAnnihilatingZero_1836
d_semiringWithoutAnnihilatingZero_3616 ~v0 ~v1 v2
  = du_semiringWithoutAnnihilatingZero_3616 v2
du_semiringWithoutAnnihilatingZero_3616 ::
  T_Ring_3432 -> T_SemiringWithoutAnnihilatingZero_1836
du_semiringWithoutAnnihilatingZero_3616 v0
  = coe
      du_semiringWithoutAnnihilatingZero_2104
      (coe du_semiring_3580 (coe v0))
-- Algebra.Bundles.Ring._.semiringWithoutOne
d_semiringWithoutOne_3618 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_SemiringWithoutOne_1622
d_semiringWithoutOne_3618 ~v0 ~v1 v2
  = du_semiringWithoutOne_3618 v2
du_semiringWithoutOne_3618 ::
  T_Ring_3432 -> T_SemiringWithoutOne_1622
du_semiringWithoutOne_3618 v0
  = coe du_semiringWithoutOne_2142 (coe du_semiring_3580 (coe v0))
-- Algebra.Bundles.Ring._.group
d_group_3622 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_Group_1266
d_group_3622 ~v0 ~v1 v2 = du_group_3622 v2
du_group_3622 :: T_Ring_3432 -> T_Group_1266
du_group_3622 v0
  = coe du_group_1474 (coe du_'43''45'abelianGroup_3578 (coe v0))
-- Algebra.Bundles.Ring._.invertibleMagma
d_invertibleMagma_3624 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_InvertibleMagma_1106
d_invertibleMagma_3624 ~v0 ~v1 v2 = du_invertibleMagma_3624 v2
du_invertibleMagma_3624 :: T_Ring_3432 -> T_InvertibleMagma_1106
du_invertibleMagma_3624 v0
  = let v1 = coe du_'43''45'abelianGroup_3578 (coe v0) in
    coe du_invertibleMagma_1370 (coe du_group_1474 (coe v1))
-- Algebra.Bundles.Ring._.invertibleUnitalMagma
d_invertibleUnitalMagma_3626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> T_InvertibleUnitalMagma_1180
d_invertibleUnitalMagma_3626 ~v0 ~v1 v2
  = du_invertibleUnitalMagma_3626 v2
du_invertibleUnitalMagma_3626 ::
  T_Ring_3432 -> T_InvertibleUnitalMagma_1180
du_invertibleUnitalMagma_3626 v0
  = let v1 = coe du_'43''45'abelianGroup_3578 (coe v0) in
    coe du_invertibleUnitalMagma_1372 (coe du_group_1474 (coe v1))
-- Algebra.Bundles.Ring.rawRing
d_rawRing_3628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_3628 ~v0 ~v1 v2 = du_rawRing_3628 v2
du_rawRing_3628 ::
  T_Ring_3432 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_3628 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawRing'46'constructor_3463
      (d__'43'__3458 (coe v0)) (d__'42'__3460 (coe v0))
      (d_'45'__3462 (coe v0)) (d_0'35'_3464 (coe v0))
      (d_1'35'_3466 (coe v0))
-- Algebra.Bundles.CommutativeRing
d_CommutativeRing_3634 a0 a1 = ()
data T_CommutativeRing_3634
  = C_CommutativeRing'46'constructor_64147 (AgdaAny ->
                                            AgdaAny -> AgdaAny)
                                           (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                           AgdaAny AgdaAny
                                           MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
-- Algebra.Bundles.CommutativeRing.Carrier
d_Carrier_3656 :: T_CommutativeRing_3634 -> ()
d_Carrier_3656 = erased
-- Algebra.Bundles.CommutativeRing._≈_
d__'8776'__3658 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> ()
d__'8776'__3658 = erased
-- Algebra.Bundles.CommutativeRing._+_
d__'43'__3660 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__3660 v0
  = case coe v0 of
      C_CommutativeRing'46'constructor_64147 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeRing._*_
d__'42'__3662 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__3662 v0
  = case coe v0 of
      C_CommutativeRing'46'constructor_64147 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeRing.-_
d_'45'__3664 :: T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_'45'__3664 v0
  = case coe v0 of
      C_CommutativeRing'46'constructor_64147 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeRing.0#
d_0'35'_3666 :: T_CommutativeRing_3634 -> AgdaAny
d_0'35'_3666 v0
  = case coe v0 of
      C_CommutativeRing'46'constructor_64147 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeRing.1#
d_1'35'_3668 :: T_CommutativeRing_3634 -> AgdaAny
d_1'35'_3668 v0
  = case coe v0 of
      C_CommutativeRing'46'constructor_64147 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeRing.isCommutativeRing
d_isCommutativeRing_3670 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
d_isCommutativeRing_3670 v0
  = case coe v0 of
      C_CommutativeRing'46'constructor_64147 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.CommutativeRing._._-_
d__'45'__3674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__3674 ~v0 ~v1 v2 = du__'45'__3674 v2
du__'45'__3674 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__3674 v0
  = let v1 = d__'43'__3660 (coe v0) in
    let v2 = d_'45'__3664 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v1) (coe v2)
-- Algebra.Bundles.CommutativeRing._.*-assoc
d_'42''45'assoc_3676 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_3676 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_2422
      (coe
         MAlonzo.Code.Algebra.Structures.d_isRing_2556
         (coe d_isCommutativeRing_3670 (coe v0)))
-- Algebra.Bundles.CommutativeRing._.*-comm
d_'42''45'comm_3678 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_3678 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_2558
      (coe d_isCommutativeRing_3670 (coe v0))
-- Algebra.Bundles.CommutativeRing._.*-cong
d_'42''45'cong_3680 ::
  T_CommutativeRing_3634 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_3680 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_2420
      (coe
         MAlonzo.Code.Algebra.Structures.d_isRing_2556
         (coe d_isCommutativeRing_3670 (coe v0)))
-- Algebra.Bundles.CommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_3682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3682 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3682 v2
du_'8729''45'cong'691'_3682 ::
  T_CommutativeRing_3634 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3682 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.CommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_3684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3684 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3684 v2
du_'8729''45'cong'737'_3684 ::
  T_CommutativeRing_3634 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3684 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Bundles.CommutativeRing._.*-identity
d_'42''45'identity_3686 ::
  T_CommutativeRing_3634 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_3686 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424
      (coe
         MAlonzo.Code.Algebra.Structures.d_isRing_2556
         (coe d_isCommutativeRing_3670 (coe v0)))
-- Algebra.Bundles.CommutativeRing._.identityʳ
d_identity'691'_3688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_identity'691'_3688 ~v0 ~v1 v2 = du_identity'691'_3688 v2
du_identity'691'_3688 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
du_identity'691'_3688 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500 (coe v2))
-- Algebra.Bundles.CommutativeRing._.identityˡ
d_identity'737'_3690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_identity'737'_3690 ~v0 ~v1 v2 = du_identity'737'_3690 v2
du_identity'737'_3690 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
du_identity'737'_3690 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500 (coe v2))
-- Algebra.Bundles.CommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_3692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_3692 ~v0 ~v1 v2
  = du_isCommutativeMagma_3692 v2
du_isCommutativeMagma_3692 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_3692 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiring_2668
              (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v3))
-- Algebra.Bundles.CommutativeRing._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_3694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_3694 ~v0 ~v1 v2
  = du_'42''45'isCommutativeMonoid_3694 v2
du_'42''45'isCommutativeMonoid_3694 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_3694 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiring_2668
         (coe v1))
-- Algebra.Bundles.CommutativeRing._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_3696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_3696 ~v0 ~v1 v2
  = du_'42''45'isCommutativeSemigroup_3696 v2
du_'42''45'isCommutativeSemigroup_3696 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_3696 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiring_2668
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Algebra.Bundles.CommutativeRing._.*-isMagma
d_'42''45'isMagma_3698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_3698 ~v0 ~v1 v2 = du_'42''45'isMagma_3698 v2
du_'42''45'isMagma_3698 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_3698 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2496
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1))
-- Algebra.Bundles.CommutativeRing._.*-isMonoid
d_'42''45'isMonoid_3700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_3700 ~v0 ~v1 v2 = du_'42''45'isMonoid_3700 v2
du_'42''45'isMonoid_3700 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_3700 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1))
-- Algebra.Bundles.CommutativeRing._.*-isSemigroup
d_'42''45'isSemigroup_3702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_3702 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_3702 v2
du_'42''45'isSemigroup_3702 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_3702 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_2498
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1))
-- Algebra.Bundles.CommutativeRing._.assoc
d_assoc_3704 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_3704 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isRing_2556
                     (coe d_isCommutativeRing_3670 (coe v0)))))))
-- Algebra.Bundles.CommutativeRing._.comm
d_comm_3706 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_3706 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_990
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe
            MAlonzo.Code.Algebra.Structures.d_isRing_2556
            (coe d_isCommutativeRing_3670 (coe v0))))
-- Algebra.Bundles.CommutativeRing._.∙-cong
d_'8729''45'cong_3708 ::
  T_CommutativeRing_3634 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3708 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isRing_2556
                        (coe d_isCommutativeRing_3670 (coe v0))))))))
-- Algebra.Bundles.CommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_3710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3710 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3710 v2
du_'8729''45'cong'691'_3710 ::
  T_CommutativeRing_3634 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3710 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.CommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_3712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3712 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3712 v2
du_'8729''45'cong'737'_3712 ::
  T_CommutativeRing_3634 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3712 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.CommutativeRing._.identity
d_identity_3714 ::
  T_CommutativeRing_3634 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3714 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isRing_2556
                  (coe d_isCommutativeRing_3670 (coe v0))))))
-- Algebra.Bundles.CommutativeRing._.identityʳ
d_identity'691'_3716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_identity'691'_3716 ~v0 ~v1 v2 = du_identity'691'_3716 v2
du_identity'691'_3716 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
du_identity'691'_3716 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4))
-- Algebra.Bundles.CommutativeRing._.identityˡ
d_identity'737'_3718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_identity'737'_3718 ~v0 ~v1 v2 = du_identity'737'_3718 v2
du_identity'737'_3718 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
du_identity'737'_3718 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4))
-- Algebra.Bundles.CommutativeRing._.+-isAbelianGroup
d_'43''45'isAbelianGroup_3720 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_3720 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
      (coe
         MAlonzo.Code.Algebra.Structures.d_isRing_2556
         (coe d_isCommutativeRing_3670 (coe v0)))
-- Algebra.Bundles.CommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_3722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_3722 ~v0 ~v1 v2
  = du_isCommutativeMagma_3722 v2
du_isCommutativeMagma_3722 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_3722 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v4))
-- Algebra.Bundles.CommutativeRing._.isCommutativeMonoid
d_isCommutativeMonoid_3724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_3724 ~v0 ~v1 v2
  = du_isCommutativeMonoid_3724 v2
du_isCommutativeMonoid_3724 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_3724 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe v2))
-- Algebra.Bundles.CommutativeRing._.isCommutativeSemigroup
d_isCommutativeSemigroup_3726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_3726 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_3726 v2
du_isCommutativeSemigroup_3726 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_3726 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
         (coe v3))
-- Algebra.Bundles.CommutativeRing._.isGroup
d_isGroup_3728 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_3728 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
         (coe
            MAlonzo.Code.Algebra.Structures.d_isRing_2556
            (coe d_isCommutativeRing_3670 (coe v0))))
-- Algebra.Bundles.CommutativeRing._.isInvertibleMagma
d_isInvertibleMagma_3730 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_3730 ~v0 ~v1 v2 = du_isInvertibleMagma_3730 v2
du_isInvertibleMagma_3730 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
du_isInvertibleMagma_3730 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleMagma_966
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3))
-- Algebra.Bundles.CommutativeRing._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_3732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_3732 ~v0 ~v1 v2
  = du_isInvertibleUnitalMagma_3732 v2
du_isInvertibleUnitalMagma_3732 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_3732 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleUnitalMagma_968
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3))
-- Algebra.Bundles.CommutativeRing._.isMagma
d_isMagma_3734 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_3734 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isRing_2556
                     (coe d_isCommutativeRing_3670 (coe v0)))))))
-- Algebra.Bundles.CommutativeRing._.isMonoid
d_isMonoid_3736 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_3736 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe
               MAlonzo.Code.Algebra.Structures.d_isRing_2556
               (coe d_isCommutativeRing_3670 (coe v0)))))
-- Algebra.Bundles.CommutativeRing._.isSemigroup
d_isSemigroup_3738 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_3738 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isRing_2556
                  (coe d_isCommutativeRing_3670 (coe v0))))))
-- Algebra.Bundles.CommutativeRing._.isUnitalMagma
d_isUnitalMagma_3740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_3740 ~v0 ~v1 v2 = du_isUnitalMagma_3740 v2
du_isUnitalMagma_3740 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_3740 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4))
-- Algebra.Bundles.CommutativeRing._.⁻¹-cong
d_'8315''185''45'cong_3742 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_3742 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe
               MAlonzo.Code.Algebra.Structures.d_isRing_2556
               (coe d_isCommutativeRing_3670 (coe v0)))))
-- Algebra.Bundles.CommutativeRing._.inverse
d_inverse_3744 ::
  T_CommutativeRing_3634 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_3744 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe
               MAlonzo.Code.Algebra.Structures.d_isRing_2556
               (coe d_isCommutativeRing_3670 (coe v0)))))
-- Algebra.Bundles.CommutativeRing._.inverseʳ
d_inverse'691'_3746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_inverse'691'_3746 ~v0 ~v1 v2 = du_inverse'691'_3746 v2
du_inverse'691'_3746 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
du_inverse'691'_3746 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3))
-- Algebra.Bundles.CommutativeRing._.inverseˡ
d_inverse'737'_3748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_inverse'737'_3748 ~v0 ~v1 v2 = du_inverse'737'_3748 v2
du_inverse'737'_3748 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
du_inverse'737'_3748 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3))
-- Algebra.Bundles.CommutativeRing._.distrib
d_distrib_3750 ::
  T_CommutativeRing_3634 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_3750 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_2426
      (coe
         MAlonzo.Code.Algebra.Structures.d_isRing_2556
         (coe d_isCommutativeRing_3670 (coe v0)))
-- Algebra.Bundles.CommutativeRing._.distribʳ
d_distrib'691'_3752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_3752 ~v0 ~v1 v2 = du_distrib'691'_3752 v2
du_distrib'691'_3752 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_3752 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.CommutativeRing._.distribˡ
d_distrib'737'_3754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_3754 ~v0 ~v1 v2 = du_distrib'737'_3754 v2
du_distrib'737'_3754 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_3754 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Bundles.CommutativeRing._.isCommutativeSemiring
d_isCommutativeSemiring_3756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_3756 ~v0 ~v1 v2
  = du_isCommutativeSemiring_3756 v2
du_isCommutativeSemiring_3756 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_isCommutativeSemiring_3756 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiring_2668
      (coe d_isCommutativeRing_3670 (coe v0))
-- Algebra.Bundles.CommutativeRing._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_3758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_3758 ~v0 ~v1 v2
  = du_isCommutativeSemiringWithoutOne_3758 v2
du_isCommutativeSemiringWithoutOne_3758 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_3758 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiring_2668
         (coe v1))
-- Algebra.Bundles.CommutativeRing._.isEquivalence
d_isEquivalence_3760 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3760 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isRing_2556
                        (coe d_isCommutativeRing_3670 (coe v0))))))))
-- Algebra.Bundles.CommutativeRing._.isNearSemiring
d_isNearSemiring_3762 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_3762 ~v0 ~v1 v2 = du_isNearSemiring_3762 v2
du_isNearSemiring_3762 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_3762 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Bundles.CommutativeRing._.isPartialEquivalence
d_isPartialEquivalence_3764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3764 ~v0 ~v1 v2
  = du_isPartialEquivalence_3764 v2
du_isPartialEquivalence_3764 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3764 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v7))
-- Algebra.Bundles.CommutativeRing._.isRing
d_isRing_3766 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_3766 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isRing_2556
      (coe d_isCommutativeRing_3670 (coe v0))
-- Algebra.Bundles.CommutativeRing._.isSemiring
d_isSemiring_3768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_3768 ~v0 ~v1 v2 = du_isSemiring_3768 v2
du_isSemiring_3768 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
du_isSemiring_3768 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiring_2518
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1))
-- Algebra.Bundles.CommutativeRing._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_3770 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_3770 ~v0 ~v1 v2
  = du_isSemiringWithoutAnnihilatingZero_3770 v2
du_isSemiringWithoutAnnihilatingZero_3770 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
du_isSemiringWithoutAnnihilatingZero_3770 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutAnnihilatingZero_2516
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1))
-- Algebra.Bundles.CommutativeRing._.isSemiringWithoutOne
d_isSemiringWithoutOne_3772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_3772 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_3772 v2
du_isSemiringWithoutOne_3772 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_3772 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v2))
-- Algebra.Bundles.CommutativeRing._.refl
d_refl_3774 :: T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_refl_3774 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isRing_2556
                           (coe d_isCommutativeRing_3670 (coe v0)))))))))
-- Algebra.Bundles.CommutativeRing._.reflexive
d_reflexive_3776 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3776 ~v0 ~v1 v2 = du_reflexive_3776 v2
du_reflexive_3776 ::
  T_CommutativeRing_3634 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3776 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6) in
    \ v8 v9 v10 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v7))
        v8
-- Algebra.Bundles.CommutativeRing._.setoid
d_setoid_3778 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3778 ~v0 ~v1 v2 = du_setoid_3778 v2
du_setoid_3778 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3778 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
-- Algebra.Bundles.CommutativeRing._.sym
d_sym_3780 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3780 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isRing_2556
                           (coe d_isCommutativeRing_3670 (coe v0)))))))))
-- Algebra.Bundles.CommutativeRing._.trans
d_trans_3782 ::
  T_CommutativeRing_3634 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3782 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isRing_2556
                           (coe d_isCommutativeRing_3670 (coe v0)))))))))
-- Algebra.Bundles.CommutativeRing._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_3784 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_3784 ~v0 ~v1 v2
  = du_unique'691''45''8315''185'_3784 v2
du_unique'691''45''8315''185'_3784 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_3784 v0
  = let v1 = d__'43'__3660 (coe v0) in
    let v2 = d_'45'__3664 (coe v0) in
    let v3 = d_0'35'_3666 (coe v0) in
    let v4 = d_isCommutativeRing_3670 (coe v0) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'691''45''8315''185'_964
      (coe v1) (coe v3) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v6))
-- Algebra.Bundles.CommutativeRing._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_3786 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_3786 ~v0 ~v1 v2
  = du_unique'737''45''8315''185'_3786 v2
du_unique'737''45''8315''185'_3786 ::
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_3786 v0
  = let v1 = d__'43'__3660 (coe v0) in
    let v2 = d_'45'__3664 (coe v0) in
    let v3 = d_0'35'_3666 (coe v0) in
    let v4 = d_isCommutativeRing_3670 (coe v0) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
              (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'737''45''8315''185'_958
      (coe v1) (coe v3) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v6))
-- Algebra.Bundles.CommutativeRing._.zero
d_zero_3788 ::
  T_CommutativeRing_3634 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_3788 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_2428
      (coe
         MAlonzo.Code.Algebra.Structures.d_isRing_2556
         (coe d_isCommutativeRing_3670 (coe v0)))
-- Algebra.Bundles.CommutativeRing._.zeroʳ
d_zero'691'_3790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_zero'691'_3790 ~v0 ~v1 v2 = du_zero'691'_3790 v2
du_zero'691'_3790 :: T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
du_zero'691'_3790 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_2514
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1))
-- Algebra.Bundles.CommutativeRing._.zeroˡ
d_zero'737'_3792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
d_zero'737'_3792 ~v0 ~v1 v2 = du_zero'737'_3792 v2
du_zero'737'_3792 :: T_CommutativeRing_3634 -> AgdaAny -> AgdaAny
du_zero'737'_3792 v0
  = let v1 = d_isCommutativeRing_3670 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_2512
      (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v1))
-- Algebra.Bundles.CommutativeRing.ring
d_ring_3794 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_Ring_3432
d_ring_3794 ~v0 ~v1 v2 = du_ring_3794 v2
du_ring_3794 :: T_CommutativeRing_3634 -> T_Ring_3432
du_ring_3794 v0
  = coe
      C_Ring'46'constructor_60565 (d__'43'__3660 (coe v0))
      (d__'42'__3662 (coe v0)) (d_'45'__3664 (coe v0))
      (d_0'35'_3666 (coe v0)) (d_1'35'_3668 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isRing_2556
         (coe d_isCommutativeRing_3670 (coe v0)))
-- Algebra.Bundles.CommutativeRing._._≉_
d__'8777'__3798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> AgdaAny -> AgdaAny -> ()
d__'8777'__3798 = erased
-- Algebra.Bundles.CommutativeRing._.+-abelianGroup
d_'43''45'abelianGroup_3800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_AbelianGroup_1378
d_'43''45'abelianGroup_3800 ~v0 ~v1 v2
  = du_'43''45'abelianGroup_3800 v2
du_'43''45'abelianGroup_3800 ::
  T_CommutativeRing_3634 -> T_AbelianGroup_1378
du_'43''45'abelianGroup_3800 v0
  = coe du_'43''45'abelianGroup_3578 (coe du_ring_3794 (coe v0))
-- Algebra.Bundles.CommutativeRing._.group
d_group_3802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_Group_1266
d_group_3802 ~v0 ~v1 v2 = du_group_3802 v2
du_group_3802 :: T_CommutativeRing_3634 -> T_Group_1266
du_group_3802 v0
  = let v1 = coe du_ring_3794 (coe v0) in
    coe du_group_1474 (coe du_'43''45'abelianGroup_3578 (coe v1))
-- Algebra.Bundles.CommutativeRing._.invertibleMagma
d_invertibleMagma_3804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_InvertibleMagma_1106
d_invertibleMagma_3804 ~v0 ~v1 v2 = du_invertibleMagma_3804 v2
du_invertibleMagma_3804 ::
  T_CommutativeRing_3634 -> T_InvertibleMagma_1106
du_invertibleMagma_3804 v0
  = let v1 = coe du_ring_3794 (coe v0) in
    let v2 = coe du_'43''45'abelianGroup_3578 (coe v1) in
    coe du_invertibleMagma_1370 (coe du_group_1474 (coe v2))
-- Algebra.Bundles.CommutativeRing._.invertibleUnitalMagma
d_invertibleUnitalMagma_3806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_InvertibleUnitalMagma_1180
d_invertibleUnitalMagma_3806 ~v0 ~v1 v2
  = du_invertibleUnitalMagma_3806 v2
du_invertibleUnitalMagma_3806 ::
  T_CommutativeRing_3634 -> T_InvertibleUnitalMagma_1180
du_invertibleUnitalMagma_3806 v0
  = let v1 = coe du_ring_3794 (coe v0) in
    let v2 = coe du_'43''45'abelianGroup_3578 (coe v1) in
    coe du_invertibleUnitalMagma_1372 (coe du_group_1474 (coe v2))
-- Algebra.Bundles.CommutativeRing._.rawRing
d_rawRing_3808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_3808 ~v0 ~v1 v2 = du_rawRing_3808 v2
du_rawRing_3808 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_3808 v0
  = coe du_rawRing_3628 (coe du_ring_3794 (coe v0))
-- Algebra.Bundles.CommutativeRing.commutativeSemiring
d_commutativeSemiring_3810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_CommutativeSemiring_2152
d_commutativeSemiring_3810 ~v0 ~v1 v2
  = du_commutativeSemiring_3810 v2
du_commutativeSemiring_3810 ::
  T_CommutativeRing_3634 -> T_CommutativeSemiring_2152
du_commutativeSemiring_3810 v0
  = coe
      C_CommutativeSemiring'46'constructor_38603 (d__'43'__3660 (coe v0))
      (d__'42'__3662 (coe v0)) (d_0'35'_3666 (coe v0))
      (d_1'35'_3668 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiring_2668
         (coe d_isCommutativeRing_3670 (coe v0)))
-- Algebra.Bundles.CommutativeRing._.commutativeMagma
d_commutativeMagma_3814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_CommutativeMagma_120
d_commutativeMagma_3814 ~v0 ~v1 v2 = du_commutativeMagma_3814 v2
du_commutativeMagma_3814 ::
  T_CommutativeRing_3634 -> T_CommutativeMagma_120
du_commutativeMagma_3814 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_'42''45'commutativeMonoid_2324 (coe v1) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v2))
-- Algebra.Bundles.CommutativeRing._.*-commutativeMonoid
d_'42''45'commutativeMonoid_3816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_3816 ~v0 ~v1 v2
  = du_'42''45'commutativeMonoid_3816 v2
du_'42''45'commutativeMonoid_3816 ::
  T_CommutativeRing_3634 -> T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_3816 v0
  = coe
      du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_3810 (coe v0))
-- Algebra.Bundles.CommutativeRing._.commutativeSemigroup
d_commutativeSemigroup_3818 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_3818 ~v0 ~v1 v2
  = du_commutativeSemigroup_3818 v2
du_commutativeSemigroup_3818 ::
  T_CommutativeRing_3634 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_3818 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    coe
      du_commutativeSemigroup_906
      (coe du_'42''45'commutativeMonoid_2324 (coe v1))
-- Algebra.Bundles.CommutativeRing._.magma
d_magma_3820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_Magma_8
d_magma_3820 ~v0 ~v1 v2 = du_magma_3820 v2
du_magma_3820 :: T_CommutativeRing_3634 -> T_Magma_8
du_magma_3820 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'42''45'monoid_1970 (coe v3) in
    coe du_magma_524 (coe du_semigroup_802 (coe v4))
-- Algebra.Bundles.CommutativeRing._.*-monoid
d_'42''45'monoid_3822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_Monoid_740
d_'42''45'monoid_3822 ~v0 ~v1 v2 = du_'42''45'monoid_3822 v2
du_'42''45'monoid_3822 :: T_CommutativeRing_3634 -> T_Monoid_740
du_'42''45'monoid_3822 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    coe
      du_'42''45'monoid_1970
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v2))
-- Algebra.Bundles.CommutativeRing._.rawMagma
d_rawMagma_3824 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_3824 ~v0 ~v1 v2 = du_rawMagma_3824 v2
du_rawMagma_3824 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_3824 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'42''45'monoid_1970 (coe v3) in
    let v5 = coe du_semigroup_802 (coe v4) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v5))
-- Algebra.Bundles.CommutativeRing._.rawMonoid
d_rawMonoid_3826 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_3826 ~v0 ~v1 v2 = du_rawMonoid_3826 v2
du_rawMonoid_3826 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_3826 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe du_rawMonoid_812 (coe du_'42''45'monoid_1970 (coe v3))
-- Algebra.Bundles.CommutativeRing._.semigroup
d_semigroup_3828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_Semigroup_476
d_semigroup_3828 ~v0 ~v1 v2 = du_semigroup_3828 v2
du_semigroup_3828 :: T_CommutativeRing_3634 -> T_Semigroup_476
du_semigroup_3828 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe du_semigroup_802 (coe du_'42''45'monoid_1970 (coe v3))
-- Algebra.Bundles.CommutativeRing._.commutativeMagma
d_commutativeMagma_3830 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_CommutativeMagma_120
d_commutativeMagma_3830 ~v0 ~v1 v2 = du_commutativeMagma_3830 v2
du_commutativeMagma_3830 ::
  T_CommutativeRing_3634 -> T_CommutativeMagma_120
du_commutativeMagma_3830 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe
      du_commutativeMagma_666 (coe du_commutativeSemigroup_906 (coe v4))
-- Algebra.Bundles.CommutativeRing._.+-commutativeMonoid
d_'43''45'commutativeMonoid_3832 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_3832 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_3832 v2
du_'43''45'commutativeMonoid_3832 ::
  T_CommutativeRing_3634 -> T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_3832 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    coe
      du_'43''45'commutativeMonoid_1948
      (coe du_semiringWithoutAnnihilatingZero_2104 (coe v2))
-- Algebra.Bundles.CommutativeRing._.commutativeSemigroup
d_commutativeSemigroup_3834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_CommutativeSemigroup_602
d_commutativeSemigroup_3834 ~v0 ~v1 v2
  = du_commutativeSemigroup_3834 v2
du_commutativeSemigroup_3834 ::
  T_CommutativeRing_3634 -> T_CommutativeSemigroup_602
du_commutativeSemigroup_3834 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe
      du_commutativeSemigroup_906
      (coe du_'43''45'commutativeMonoid_1948 (coe v3))
-- Algebra.Bundles.CommutativeRing._.magma
d_magma_3836 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_Magma_8
d_magma_3836 ~v0 ~v1 v2 = du_magma_3836 v2
du_magma_3836 :: T_CommutativeRing_3634 -> T_Magma_8
du_magma_3836 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    let v5 = coe du_monoid_890 (coe v4) in
    coe du_magma_524 (coe du_semigroup_802 (coe v5))
-- Algebra.Bundles.CommutativeRing._.monoid
d_monoid_3838 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_Monoid_740
d_monoid_3838 ~v0 ~v1 v2 = du_monoid_3838 v2
du_monoid_3838 :: T_CommutativeRing_3634 -> T_Monoid_740
du_monoid_3838 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    coe du_monoid_890 (coe du_'43''45'commutativeMonoid_1948 (coe v3))
-- Algebra.Bundles.CommutativeRing._.rawMagma
d_rawMagma_3840 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_3840 ~v0 ~v1 v2 = du_rawMagma_3840 v2
du_rawMagma_3840 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_3840 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    let v5 = coe du_monoid_890 (coe v4) in
    let v6 = coe du_semigroup_802 (coe v5) in
    coe du_rawMagma_52 (coe du_magma_524 (coe v6))
-- Algebra.Bundles.CommutativeRing._.rawMonoid
d_rawMonoid_3842 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_rawMonoid_3842 ~v0 ~v1 v2 = du_rawMonoid_3842 v2
du_rawMonoid_3842 ::
  T_CommutativeRing_3634 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_rawMonoid_3842 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe du_rawMonoid_812 (coe du_monoid_890 (coe v4))
-- Algebra.Bundles.CommutativeRing._.semigroup
d_semigroup_3844 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_Semigroup_476
d_semigroup_3844 ~v0 ~v1 v2 = du_semigroup_3844 v2
du_semigroup_3844 :: T_CommutativeRing_3634 -> T_Semigroup_476
du_semigroup_3844 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe du_semigroup_802 (coe du_monoid_890 (coe v4))
-- Algebra.Bundles.CommutativeRing._.unitalMagma
d_unitalMagma_3846 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_UnitalMagma_672
d_unitalMagma_3846 ~v0 ~v1 v2 = du_unitalMagma_3846 v2
du_unitalMagma_3846 :: T_CommutativeRing_3634 -> T_UnitalMagma_672
du_unitalMagma_3846 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    let v3 = coe du_semiringWithoutAnnihilatingZero_2104 (coe v2) in
    let v4 = coe du_'43''45'commutativeMonoid_1948 (coe v3) in
    coe du_unitalMagma_814 (coe du_monoid_890 (coe v4))
-- Algebra.Bundles.CommutativeRing._.commutativeSemiringWithoutOne
d_commutativeSemiringWithoutOne_3848 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_CommutativeSemiringWithoutOne_1726
d_commutativeSemiringWithoutOne_3848 ~v0 ~v1 v2
  = du_commutativeSemiringWithoutOne_3848 v2
du_commutativeSemiringWithoutOne_3848 ::
  T_CommutativeRing_3634 -> T_CommutativeSemiringWithoutOne_1726
du_commutativeSemiringWithoutOne_3848 v0
  = coe
      du_commutativeSemiringWithoutOne_2332
      (coe du_commutativeSemiring_3810 (coe v0))
-- Algebra.Bundles.CommutativeRing._.nearSemiring
d_nearSemiring_3850 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_NearSemiring_1508
d_nearSemiring_3850 ~v0 ~v1 v2 = du_nearSemiring_3850 v2
du_nearSemiring_3850 ::
  T_CommutativeRing_3634 -> T_NearSemiring_1508
du_nearSemiring_3850 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    let v2 = coe du_semiring_2282 (coe v1) in
    coe du_nearSemiring_1688 (coe du_semiringWithoutOne_2142 (coe v2))
-- Algebra.Bundles.CommutativeRing._.semiring
d_semiring_3852 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_Semiring_1986
d_semiring_3852 ~v0 ~v1 v2 = du_semiring_3852 v2
du_semiring_3852 :: T_CommutativeRing_3634 -> T_Semiring_1986
du_semiring_3852 v0
  = coe du_semiring_2282 (coe du_commutativeSemiring_3810 (coe v0))
-- Algebra.Bundles.CommutativeRing._.semiringWithoutAnnihilatingZero
d_semiringWithoutAnnihilatingZero_3854 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_SemiringWithoutAnnihilatingZero_1836
d_semiringWithoutAnnihilatingZero_3854 ~v0 ~v1 v2
  = du_semiringWithoutAnnihilatingZero_3854 v2
du_semiringWithoutAnnihilatingZero_3854 ::
  T_CommutativeRing_3634 -> T_SemiringWithoutAnnihilatingZero_1836
du_semiringWithoutAnnihilatingZero_3854 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    coe
      du_semiringWithoutAnnihilatingZero_2104
      (coe du_semiring_2282 (coe v1))
-- Algebra.Bundles.CommutativeRing._.semiringWithoutOne
d_semiringWithoutOne_3856 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_CommutativeRing_3634 -> T_SemiringWithoutOne_1622
d_semiringWithoutOne_3856 ~v0 ~v1 v2
  = du_semiringWithoutOne_3856 v2
du_semiringWithoutOne_3856 ::
  T_CommutativeRing_3634 -> T_SemiringWithoutOne_1622
du_semiringWithoutOne_3856 v0
  = let v1 = coe du_commutativeSemiring_3810 (coe v0) in
    coe du_semiringWithoutOne_2142 (coe du_semiring_2282 (coe v1))
-- Algebra.Bundles.Quasigroup
d_Quasigroup_3862 a0 a1 = ()
data T_Quasigroup_3862
  = C_Quasigroup'46'constructor_67847 (AgdaAny -> AgdaAny -> AgdaAny)
                                      (AgdaAny -> AgdaAny -> AgdaAny)
                                      (AgdaAny -> AgdaAny -> AgdaAny)
                                      MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
-- Algebra.Bundles.Quasigroup.Carrier
d_Carrier_3880 :: T_Quasigroup_3862 -> ()
d_Carrier_3880 = erased
-- Algebra.Bundles.Quasigroup._≈_
d__'8776'__3882 :: T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> ()
d__'8776'__3882 = erased
-- Algebra.Bundles.Quasigroup._∙_
d__'8729'__3884 ::
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__3884 v0
  = case coe v0 of
      C_Quasigroup'46'constructor_67847 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Quasigroup._\\_
d__'92''92'__3886 ::
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
d__'92''92'__3886 v0
  = case coe v0 of
      C_Quasigroup'46'constructor_67847 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Quasigroup._//_
d__'47''47'__3888 ::
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
d__'47''47'__3888 v0
  = case coe v0 of
      C_Quasigroup'46'constructor_67847 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Quasigroup.isQuasigroup
d_isQuasigroup_3890 ::
  T_Quasigroup_3862 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_3890 v0
  = case coe v0 of
      C_Quasigroup'46'constructor_67847 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Quasigroup._.//-cong
d_'47''47''45'cong_3894 ::
  T_Quasigroup_3862 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_3894 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'47''47''45'cong_2708
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.//-congʳ
d_'47''47''45'cong'691'_3896 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_3896 ~v0 ~v1 v2
  = du_'47''47''45'cong'691'_3896 v2
du_'47''47''45'cong'691'_3896 ::
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_3896 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'691'_2748
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.//-congˡ
d_'47''47''45'cong'737'_3898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_3898 ~v0 ~v1 v2
  = du_'47''47''45'cong'737'_3898 v2
du_'47''47''45'cong'737'_3898 ::
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_3898 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'737'_2744
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.\\-cong
d_'92''92''45'cong_3900 ::
  T_Quasigroup_3862 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_3900 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'92''92''45'cong_2706
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.\\-congʳ
d_'92''92''45'cong'691'_3902 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_3902 ~v0 ~v1 v2
  = du_'92''92''45'cong'691'_3902 v2
du_'92''92''45'cong'691'_3902 ::
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_3902 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'691'_2740
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.\\-congˡ
d_'92''92''45'cong'737'_3904 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_3904 ~v0 ~v1 v2
  = du_'92''92''45'cong'737'_3904 v2
du_'92''92''45'cong'737'_3904 ::
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_3904 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'737'_2736
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.isEquivalence
d_isEquivalence_3906 ::
  T_Quasigroup_3862 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3906 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe d_isQuasigroup_3890 (coe v0)))
-- Algebra.Bundles.Quasigroup._.isMagma
d_isMagma_3908 ::
  T_Quasigroup_3862 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_3908 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.isPartialEquivalence
d_isPartialEquivalence_3910 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3910 ~v0 ~v1 v2
  = du_isPartialEquivalence_3910 v2
du_isPartialEquivalence_3910 ::
  T_Quasigroup_3862 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3910 v0
  = let v1 = d_isQuasigroup_3890 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Bundles.Quasigroup._.leftDivides
d_leftDivides_3912 ::
  T_Quasigroup_3862 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_3912 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.leftDividesʳ
d_leftDivides'691'_3914 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_3914 ~v0 ~v1 v2 = du_leftDivides'691'_3914 v2
du_leftDivides'691'_3914 ::
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_3914 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'691'_2754
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.leftDividesˡ
d_leftDivides'737'_3916 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_3916 ~v0 ~v1 v2 = du_leftDivides'737'_3916 v2
du_leftDivides'737'_3916 ::
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_3916 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'737'_2752
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.refl
d_refl_3918 :: T_Quasigroup_3862 -> AgdaAny -> AgdaAny
d_refl_3918 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe d_isQuasigroup_3890 (coe v0))))
-- Algebra.Bundles.Quasigroup._.reflexive
d_reflexive_3920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3920 ~v0 ~v1 v2 = du_reflexive_3920 v2
du_reflexive_3920 ::
  T_Quasigroup_3862 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3920 v0
  = let v1 = d_isQuasigroup_3890 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Bundles.Quasigroup._.rightDivides
d_rightDivides_3922 ::
  T_Quasigroup_3862 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_3922 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.rightDividesʳ
d_rightDivides'691'_3924 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_3924 ~v0 ~v1 v2 = du_rightDivides'691'_3924 v2
du_rightDivides'691'_3924 ::
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_3924 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'691'_2758
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.rightDividesˡ
d_rightDivides'737'_3926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_3926 ~v0 ~v1 v2 = du_rightDivides'737'_3926 v2
du_rightDivides'737'_3926 ::
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_3926 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'737'_2756
      (coe d_isQuasigroup_3890 (coe v0))
-- Algebra.Bundles.Quasigroup._.setoid
d_setoid_3928 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3928 ~v0 ~v1 v2 = du_setoid_3928 v2
du_setoid_3928 ::
  T_Quasigroup_3862 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3928 v0
  = let v1 = d_isQuasigroup_3890 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v1))
-- Algebra.Bundles.Quasigroup._.sym
d_sym_3930 ::
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3930 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe d_isQuasigroup_3890 (coe v0))))
-- Algebra.Bundles.Quasigroup._.trans
d_trans_3932 ::
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3932 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe d_isQuasigroup_3890 (coe v0))))
-- Algebra.Bundles.Quasigroup._.∙-cong
d_'8729''45'cong_3934 ::
  T_Quasigroup_3862 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3934 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe d_isQuasigroup_3890 (coe v0)))
-- Algebra.Bundles.Quasigroup._.∙-congʳ
d_'8729''45'cong'691'_3936 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3936 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_3936 v2
du_'8729''45'cong'691'_3936 ::
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3936 v0
  = let v1 = d_isQuasigroup_3890 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v1))
-- Algebra.Bundles.Quasigroup._.∙-congˡ
d_'8729''45'cong'737'_3938 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3938 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_3938 v2
du_'8729''45'cong'737'_3938 ::
  T_Quasigroup_3862 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3938 v0
  = let v1 = d_isQuasigroup_3890 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v1))
-- Algebra.Bundles.Quasigroup.magma
d_magma_3940 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> T_Magma_8
d_magma_3940 ~v0 ~v1 v2 = du_magma_3940 v2
du_magma_3940 :: T_Quasigroup_3862 -> T_Magma_8
du_magma_3940 v0
  = coe
      C_Magma'46'constructor_187 (d__'8729'__3884 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe d_isQuasigroup_3890 (coe v0)))
-- Algebra.Bundles.Quasigroup._._≉_
d__'8777'__3944 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> AgdaAny -> AgdaAny -> ()
d__'8777'__3944 = erased
-- Algebra.Bundles.Quasigroup._.rawMagma
d_rawMagma_3946 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_3946 ~v0 ~v1 v2 = du_rawMagma_3946 v2
du_rawMagma_3946 ::
  T_Quasigroup_3862 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_3946 v0
  = coe du_rawMagma_52 (coe du_magma_3940 (coe v0))
-- Algebra.Bundles.Quasigroup.rawQuasigroup
d_rawQuasigroup_3948 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296
d_rawQuasigroup_3948 ~v0 ~v1 v2 = du_rawQuasigroup_3948 v2
du_rawQuasigroup_3948 ::
  T_Quasigroup_3862 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296
du_rawQuasigroup_3948 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawQuasigroup'46'constructor_4237
      (d__'8729'__3884 (coe v0)) (d__'92''92'__3886 (coe v0))
      (d__'47''47'__3888 (coe v0))
-- Algebra.Bundles.Quasigroup._.//-rawMagma
d_'47''47''45'rawMagma_3952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'47''47''45'rawMagma_3952 ~v0 ~v1 v2
  = du_'47''47''45'rawMagma_3952 v2
du_'47''47''45'rawMagma_3952 ::
  T_Quasigroup_3862 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'47''47''45'rawMagma_3952 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'47''47''45'rawMagma_326
      (coe du_rawQuasigroup_3948 (coe v0))
-- Algebra.Bundles.Quasigroup._.\\-rawMagma
d_'92''92''45'rawMagma_3954 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'92''92''45'rawMagma_3954 ~v0 ~v1 v2
  = du_'92''92''45'rawMagma_3954 v2
du_'92''92''45'rawMagma_3954 ::
  T_Quasigroup_3862 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'92''92''45'rawMagma_3954 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'92''92''45'rawMagma_324
      (coe du_rawQuasigroup_3948 (coe v0))
-- Algebra.Bundles.Quasigroup._.∙-rawMagma
d_'8729''45'rawMagma_3956 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Quasigroup_3862 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8729''45'rawMagma_3956 ~v0 ~v1 v2
  = du_'8729''45'rawMagma_3956 v2
du_'8729''45'rawMagma_3956 ::
  T_Quasigroup_3862 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8729''45'rawMagma_3956 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'8729''45'rawMagma_322
      (coe du_rawQuasigroup_3948 (coe v0))
-- Algebra.Bundles.Loop
d_Loop_3962 a0 a1 = ()
data T_Loop_3962
  = C_Loop'46'constructor_69795 (AgdaAny -> AgdaAny -> AgdaAny)
                                (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny -> AgdaAny)
                                AgdaAny MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
-- Algebra.Bundles.Loop.Carrier
d_Carrier_3982 :: T_Loop_3962 -> ()
d_Carrier_3982 = erased
-- Algebra.Bundles.Loop._≈_
d__'8776'__3984 :: T_Loop_3962 -> AgdaAny -> AgdaAny -> ()
d__'8776'__3984 = erased
-- Algebra.Bundles.Loop._∙_
d__'8729'__3986 :: T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__3986 v0
  = case coe v0 of
      C_Loop'46'constructor_69795 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Loop._\\_
d__'92''92'__3988 :: T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
d__'92''92'__3988 v0
  = case coe v0 of
      C_Loop'46'constructor_69795 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Loop._//_
d__'47''47'__3990 :: T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
d__'47''47'__3990 v0
  = case coe v0 of
      C_Loop'46'constructor_69795 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Loop.ε
d_ε_3992 :: T_Loop_3962 -> AgdaAny
d_ε_3992 v0
  = case coe v0 of
      C_Loop'46'constructor_69795 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Loop.isLoop
d_isLoop_3994 ::
  T_Loop_3962 -> MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_3994 v0
  = case coe v0 of
      C_Loop'46'constructor_69795 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Loop._.//-cong
d_'47''47''45'cong_3998 ::
  T_Loop_3962 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_3998 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'47''47''45'cong_2708
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe d_isLoop_3994 (coe v0)))
-- Algebra.Bundles.Loop._.//-congʳ
d_'47''47''45'cong'691'_4000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_4000 ~v0 ~v1 v2
  = du_'47''47''45'cong'691'_4000 v2
du_'47''47''45'cong'691'_4000 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_4000 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'691'_2748
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1))
-- Algebra.Bundles.Loop._.//-congˡ
d_'47''47''45'cong'737'_4002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_4002 ~v0 ~v1 v2
  = du_'47''47''45'cong'737'_4002 v2
du_'47''47''45'cong'737'_4002 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_4002 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'737'_2744
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1))
-- Algebra.Bundles.Loop._.\\-cong
d_'92''92''45'cong_4004 ::
  T_Loop_3962 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_4004 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'92''92''45'cong_2706
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe d_isLoop_3994 (coe v0)))
-- Algebra.Bundles.Loop._.\\-congʳ
d_'92''92''45'cong'691'_4006 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_4006 ~v0 ~v1 v2
  = du_'92''92''45'cong'691'_4006 v2
du_'92''92''45'cong'691'_4006 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_4006 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'691'_2740
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1))
-- Algebra.Bundles.Loop._.\\-congˡ
d_'92''92''45'cong'737'_4008 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_4008 ~v0 ~v1 v2
  = du_'92''92''45'cong'737'_4008 v2
du_'92''92''45'cong'737'_4008 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_4008 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'737'_2736
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1))
-- Algebra.Bundles.Loop._.identity
d_identity_4010 ::
  T_Loop_3962 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_4010 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe d_isLoop_3994 (coe v0))
-- Algebra.Bundles.Loop._.identityʳ
d_identity'691'_4012 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny
d_identity'691'_4012 ~v0 ~v1 v2 = du_identity'691'_4012 v2
du_identity'691'_4012 :: T_Loop_3962 -> AgdaAny -> AgdaAny
du_identity'691'_4012 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_2836
      (coe d_isLoop_3994 (coe v0))
-- Algebra.Bundles.Loop._.identityˡ
d_identity'737'_4014 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny
d_identity'737'_4014 ~v0 ~v1 v2 = du_identity'737'_4014 v2
du_identity'737'_4014 :: T_Loop_3962 -> AgdaAny -> AgdaAny
du_identity'737'_4014 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_2834
      (coe d_isLoop_3994 (coe v0))
-- Algebra.Bundles.Loop._.isEquivalence
d_isEquivalence_4016 ::
  T_Loop_3962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_4016 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe d_isLoop_3994 (coe v0))))
-- Algebra.Bundles.Loop._.isMagma
d_isMagma_4018 ::
  T_Loop_3962 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_4018 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe d_isLoop_3994 (coe v0)))
-- Algebra.Bundles.Loop._.isPartialEquivalence
d_isPartialEquivalence_4020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_4020 ~v0 ~v1 v2
  = du_isPartialEquivalence_4020 v2
du_isPartialEquivalence_4020 ::
  T_Loop_3962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_4020 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Bundles.Loop._.isQuasigroup
d_isQuasigroup_4022 ::
  T_Loop_3962 -> MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_4022 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe d_isLoop_3994 (coe v0))
-- Algebra.Bundles.Loop._.leftDivides
d_leftDivides_4024 ::
  T_Loop_3962 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_4024 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe d_isLoop_3994 (coe v0)))
-- Algebra.Bundles.Loop._.leftDividesʳ
d_leftDivides'691'_4026 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_4026 ~v0 ~v1 v2 = du_leftDivides'691'_4026 v2
du_leftDivides'691'_4026 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_4026 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'691'_2754
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1))
-- Algebra.Bundles.Loop._.leftDividesˡ
d_leftDivides'737'_4028 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_4028 ~v0 ~v1 v2 = du_leftDivides'737'_4028 v2
du_leftDivides'737'_4028 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_4028 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'737'_2752
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1))
-- Algebra.Bundles.Loop._.refl
d_refl_4030 :: T_Loop_3962 -> AgdaAny -> AgdaAny
d_refl_4030 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe d_isLoop_3994 (coe v0)))))
-- Algebra.Bundles.Loop._.reflexive
d_reflexive_4032 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_4032 ~v0 ~v1 v2 = du_reflexive_4032 v2
du_reflexive_4032 ::
  T_Loop_3962 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_4032 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Bundles.Loop._.rightDivides
d_rightDivides_4034 ::
  T_Loop_3962 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_4034 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe d_isLoop_3994 (coe v0)))
-- Algebra.Bundles.Loop._.rightDividesʳ
d_rightDivides'691'_4036 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_4036 ~v0 ~v1 v2 = du_rightDivides'691'_4036 v2
du_rightDivides'691'_4036 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_4036 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'691'_2758
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1))
-- Algebra.Bundles.Loop._.rightDividesˡ
d_rightDivides'737'_4038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_4038 ~v0 ~v1 v2 = du_rightDivides'737'_4038 v2
du_rightDivides'737'_4038 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_4038 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'737'_2756
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1))
-- Algebra.Bundles.Loop._.setoid
d_setoid_4040 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_4040 ~v0 ~v1 v2 = du_setoid_4040 v2
du_setoid_4040 ::
  T_Loop_3962 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_4040 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v2))
-- Algebra.Bundles.Loop._.sym
d_sym_4042 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_4042 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe d_isLoop_3994 (coe v0)))))
-- Algebra.Bundles.Loop._.trans
d_trans_4044 ::
  T_Loop_3962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_4044 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe d_isLoop_3994 (coe v0)))))
-- Algebra.Bundles.Loop._.∙-cong
d_'8729''45'cong_4046 ::
  T_Loop_3962 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_4046 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe d_isLoop_3994 (coe v0))))
-- Algebra.Bundles.Loop._.∙-congʳ
d_'8729''45'cong'691'_4048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_4048 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_4048 v2
du_'8729''45'cong'691'_4048 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_4048 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v2))
-- Algebra.Bundles.Loop._.∙-congˡ
d_'8729''45'cong'737'_4050 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_4050 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_4050 v2
du_'8729''45'cong'737'_4050 ::
  T_Loop_3962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_4050 v0
  = let v1 = d_isLoop_3994 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v2))
-- Algebra.Bundles.Loop.rawLoop
d_rawLoop_4052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336
d_rawLoop_4052 ~v0 ~v1 v2 = du_rawLoop_4052 v2
du_rawLoop_4052 ::
  T_Loop_3962 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336
du_rawLoop_4052 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawLoop'46'constructor_4949
      (d__'8729'__3986 (coe v0)) (d__'92''92'__3988 (coe v0))
      (d__'47''47'__3990 (coe v0)) (d_ε_3992 (coe v0))
-- Algebra.Bundles.Loop.quasigroup
d_quasigroup_4054 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> T_Quasigroup_3862
d_quasigroup_4054 ~v0 ~v1 v2 = du_quasigroup_4054 v2
du_quasigroup_4054 :: T_Loop_3962 -> T_Quasigroup_3862
du_quasigroup_4054 v0
  = coe
      C_Quasigroup'46'constructor_67847 (d__'8729'__3986 (coe v0))
      (d__'92''92'__3988 (coe v0)) (d__'47''47'__3990 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe d_isLoop_3994 (coe v0)))
-- Algebra.Bundles.Loop._._≉_
d__'8777'__4058 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> AgdaAny -> AgdaAny -> ()
d__'8777'__4058 = erased
-- Algebra.Bundles.Loop._.//-rawMagma
d_'47''47''45'rawMagma_4060 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'47''47''45'rawMagma_4060 ~v0 ~v1 v2
  = du_'47''47''45'rawMagma_4060 v2
du_'47''47''45'rawMagma_4060 ::
  T_Loop_3962 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'47''47''45'rawMagma_4060 v0
  = let v1 = coe du_quasigroup_4054 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'47''47''45'rawMagma_326
      (coe du_rawQuasigroup_3948 (coe v1))
-- Algebra.Bundles.Loop._.\\-rawMagma
d_'92''92''45'rawMagma_4062 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'92''92''45'rawMagma_4062 ~v0 ~v1 v2
  = du_'92''92''45'rawMagma_4062 v2
du_'92''92''45'rawMagma_4062 ::
  T_Loop_3962 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'92''92''45'rawMagma_4062 v0
  = let v1 = coe du_quasigroup_4054 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'92''92''45'rawMagma_324
      (coe du_rawQuasigroup_3948 (coe v1))
-- Algebra.Bundles.Loop._.∙-rawMagma
d_'8729''45'rawMagma_4064 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Loop_3962 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8729''45'rawMagma_4064 ~v0 ~v1 v2
  = du_'8729''45'rawMagma_4064 v2
du_'8729''45'rawMagma_4064 ::
  T_Loop_3962 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8729''45'rawMagma_4064 v0
  = let v1 = coe du_quasigroup_4054 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'8729''45'rawMagma_322
      (coe du_rawQuasigroup_3948 (coe v1))
-- Algebra.Bundles.LeftBolLoop
d_LeftBolLoop_4070 a0 a1 = ()
data T_LeftBolLoop_4070
  = C_LeftBolLoop'46'constructor_72005 (AgdaAny ->
                                        AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
                                       MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846
-- Algebra.Bundles.LeftBolLoop.Carrier
d_Carrier_4090 :: T_LeftBolLoop_4070 -> ()
d_Carrier_4090 = erased
-- Algebra.Bundles.LeftBolLoop._≈_
d__'8776'__4092 :: T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> ()
d__'8776'__4092 = erased
-- Algebra.Bundles.LeftBolLoop._∙_
d__'8729'__4094 ::
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__4094 v0
  = case coe v0 of
      C_LeftBolLoop'46'constructor_72005 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.LeftBolLoop._\\_
d__'92''92'__4096 ::
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
d__'92''92'__4096 v0
  = case coe v0 of
      C_LeftBolLoop'46'constructor_72005 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.LeftBolLoop._//_
d__'47''47'__4098 ::
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
d__'47''47'__4098 v0
  = case coe v0 of
      C_LeftBolLoop'46'constructor_72005 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.LeftBolLoop.ε
d_ε_4100 :: T_LeftBolLoop_4070 -> AgdaAny
d_ε_4100 v0
  = case coe v0 of
      C_LeftBolLoop'46'constructor_72005 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.LeftBolLoop.isLeftBolLoop
d_isLeftBolLoop_4102 ::
  T_LeftBolLoop_4070 ->
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846
d_isLeftBolLoop_4102 v0
  = case coe v0 of
      C_LeftBolLoop'46'constructor_72005 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.LeftBolLoop._.//-cong
d_'47''47''45'cong_4106 ::
  T_LeftBolLoop_4070 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_4106 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'47''47''45'cong_2708
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe d_isLeftBolLoop_4102 (coe v0))))
-- Algebra.Bundles.LeftBolLoop._.//-congʳ
d_'47''47''45'cong'691'_4108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_4108 ~v0 ~v1 v2
  = du_'47''47''45'cong'691'_4108 v2
du_'47''47''45'cong'691'_4108 ::
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_4108 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'691'_2748
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.LeftBolLoop._.//-congˡ
d_'47''47''45'cong'737'_4110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_4110 ~v0 ~v1 v2
  = du_'47''47''45'cong'737'_4110 v2
du_'47''47''45'cong'737'_4110 ::
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_4110 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'737'_2744
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.LeftBolLoop._.\\-cong
d_'92''92''45'cong_4112 ::
  T_LeftBolLoop_4070 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_4112 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'92''92''45'cong_2706
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe d_isLeftBolLoop_4102 (coe v0))))
-- Algebra.Bundles.LeftBolLoop._.\\-congʳ
d_'92''92''45'cong'691'_4114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_4114 ~v0 ~v1 v2
  = du_'92''92''45'cong'691'_4114 v2
du_'92''92''45'cong'691'_4114 ::
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_4114 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'691'_2740
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.LeftBolLoop._.\\-congˡ
d_'92''92''45'cong'737'_4116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_4116 ~v0 ~v1 v2
  = du_'92''92''45'cong'737'_4116 v2
du_'92''92''45'cong'737'_4116 ::
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_4116 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'737'_2736
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.LeftBolLoop._.identity
d_identity_4118 ::
  T_LeftBolLoop_4070 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_4118 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe d_isLeftBolLoop_4102 (coe v0)))
-- Algebra.Bundles.LeftBolLoop._.identityʳ
d_identity'691'_4120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny
d_identity'691'_4120 ~v0 ~v1 v2 = du_identity'691'_4120 v2
du_identity'691'_4120 :: T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny
du_identity'691'_4120 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_2836
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1))
-- Algebra.Bundles.LeftBolLoop._.identityˡ
d_identity'737'_4122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny
d_identity'737'_4122 ~v0 ~v1 v2 = du_identity'737'_4122 v2
du_identity'737'_4122 :: T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny
du_identity'737'_4122 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_2834
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1))
-- Algebra.Bundles.LeftBolLoop._.isEquivalence
d_isEquivalence_4124 ::
  T_LeftBolLoop_4070 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_4124 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_2860
               (coe d_isLeftBolLoop_4102 (coe v0)))))
-- Algebra.Bundles.LeftBolLoop._.isLoop
d_isLoop_4126 ::
  T_LeftBolLoop_4070 -> MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_4126 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLoop_2860
      (coe d_isLeftBolLoop_4102 (coe v0))
-- Algebra.Bundles.LeftBolLoop._.isMagma
d_isMagma_4128 ::
  T_LeftBolLoop_4070 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_4128 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe d_isLeftBolLoop_4102 (coe v0))))
-- Algebra.Bundles.LeftBolLoop._.isPartialEquivalence
d_isPartialEquivalence_4130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_4130 ~v0 ~v1 v2
  = du_isPartialEquivalence_4130 v2
du_isPartialEquivalence_4130 ::
  T_LeftBolLoop_4070 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_4130 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Bundles.LeftBolLoop._.isQuasigroup
d_isQuasigroup_4132 ::
  T_LeftBolLoop_4070 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_4132 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe d_isLeftBolLoop_4102 (coe v0)))
-- Algebra.Bundles.LeftBolLoop._.leftBol
d_leftBol_4134 ::
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_leftBol_4134 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftBol_2862
      (coe d_isLeftBolLoop_4102 (coe v0))
-- Algebra.Bundles.LeftBolLoop._.leftDivides
d_leftDivides_4136 ::
  T_LeftBolLoop_4070 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_4136 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe d_isLeftBolLoop_4102 (coe v0))))
-- Algebra.Bundles.LeftBolLoop._.leftDividesʳ
d_leftDivides'691'_4138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_4138 ~v0 ~v1 v2 = du_leftDivides'691'_4138 v2
du_leftDivides'691'_4138 ::
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_4138 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'691'_2754
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.LeftBolLoop._.leftDividesˡ
d_leftDivides'737'_4140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_4140 ~v0 ~v1 v2 = du_leftDivides'737'_4140 v2
du_leftDivides'737'_4140 ::
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_4140 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'737'_2752
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.LeftBolLoop._.refl
d_refl_4142 :: T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny
d_refl_4142 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_2860
                  (coe d_isLeftBolLoop_4102 (coe v0))))))
-- Algebra.Bundles.LeftBolLoop._.reflexive
d_reflexive_4144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_4144 ~v0 ~v1 v2 = du_reflexive_4144 v2
du_reflexive_4144 ::
  T_LeftBolLoop_4070 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_4144 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Bundles.LeftBolLoop._.rightDivides
d_rightDivides_4146 ::
  T_LeftBolLoop_4070 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_4146 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe d_isLeftBolLoop_4102 (coe v0))))
-- Algebra.Bundles.LeftBolLoop._.rightDividesʳ
d_rightDivides'691'_4148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_4148 ~v0 ~v1 v2 = du_rightDivides'691'_4148 v2
du_rightDivides'691'_4148 ::
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_4148 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'691'_2758
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.LeftBolLoop._.rightDividesˡ
d_rightDivides'737'_4150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_4150 ~v0 ~v1 v2 = du_rightDivides'737'_4150 v2
du_rightDivides'737'_4150 ::
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_4150 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'737'_2756
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.LeftBolLoop._.setoid
d_setoid_4152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_4152 ~v0 ~v1 v2 = du_setoid_4152 v2
du_setoid_4152 ::
  T_LeftBolLoop_4070 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_4152 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3))
-- Algebra.Bundles.LeftBolLoop._.sym
d_sym_4154 ::
  T_LeftBolLoop_4070 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_4154 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_2860
                  (coe d_isLeftBolLoop_4102 (coe v0))))))
-- Algebra.Bundles.LeftBolLoop._.trans
d_trans_4156 ::
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_4156 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_2860
                  (coe d_isLeftBolLoop_4102 (coe v0))))))
-- Algebra.Bundles.LeftBolLoop._.∙-cong
d_'8729''45'cong_4158 ::
  T_LeftBolLoop_4070 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_4158 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_2860
               (coe d_isLeftBolLoop_4102 (coe v0)))))
-- Algebra.Bundles.LeftBolLoop._.∙-congʳ
d_'8729''45'cong'691'_4160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_4160 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_4160 v2
du_'8729''45'cong'691'_4160 ::
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_4160 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3))
-- Algebra.Bundles.LeftBolLoop._.∙-congˡ
d_'8729''45'cong'737'_4162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_4162 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_4162 v2
du_'8729''45'cong'737'_4162 ::
  T_LeftBolLoop_4070 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_4162 v0
  = let v1 = d_isLeftBolLoop_4102 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3))
-- Algebra.Bundles.LeftBolLoop.loop
d_loop_4164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 -> T_Loop_3962
d_loop_4164 ~v0 ~v1 v2 = du_loop_4164 v2
du_loop_4164 :: T_LeftBolLoop_4070 -> T_Loop_3962
du_loop_4164 v0
  = coe
      C_Loop'46'constructor_69795 (d__'8729'__4094 (coe v0))
      (d__'92''92'__4096 (coe v0)) (d__'47''47'__4098 (coe v0))
      (d_ε_4100 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe d_isLeftBolLoop_4102 (coe v0)))
-- Algebra.Bundles.LeftBolLoop._.quasigroup
d_quasigroup_4168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_LeftBolLoop_4070 -> T_Quasigroup_3862
d_quasigroup_4168 ~v0 ~v1 v2 = du_quasigroup_4168 v2
du_quasigroup_4168 :: T_LeftBolLoop_4070 -> T_Quasigroup_3862
du_quasigroup_4168 v0
  = coe du_quasigroup_4054 (coe du_loop_4164 (coe v0))
-- Algebra.Bundles.RightBolLoop
d_RightBolLoop_4174 a0 a1 = ()
data T_RightBolLoop_4174
  = C_RightBolLoop'46'constructor_74149 (AgdaAny ->
                                         AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
                                        MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928
-- Algebra.Bundles.RightBolLoop.Carrier
d_Carrier_4194 :: T_RightBolLoop_4174 -> ()
d_Carrier_4194 = erased
-- Algebra.Bundles.RightBolLoop._≈_
d__'8776'__4196 :: T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> ()
d__'8776'__4196 = erased
-- Algebra.Bundles.RightBolLoop._∙_
d__'8729'__4198 ::
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__4198 v0
  = case coe v0 of
      C_RightBolLoop'46'constructor_74149 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RightBolLoop._\\_
d__'92''92'__4200 ::
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
d__'92''92'__4200 v0
  = case coe v0 of
      C_RightBolLoop'46'constructor_74149 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RightBolLoop._//_
d__'47''47'__4202 ::
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
d__'47''47'__4202 v0
  = case coe v0 of
      C_RightBolLoop'46'constructor_74149 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RightBolLoop.ε
d_ε_4204 :: T_RightBolLoop_4174 -> AgdaAny
d_ε_4204 v0
  = case coe v0 of
      C_RightBolLoop'46'constructor_74149 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RightBolLoop.isRightBolLoop
d_isRightBolLoop_4206 ::
  T_RightBolLoop_4174 ->
  MAlonzo.Code.Algebra.Structures.T_IsRightBolLoop_2928
d_isRightBolLoop_4206 v0
  = case coe v0 of
      C_RightBolLoop'46'constructor_74149 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.RightBolLoop._.//-cong
d_'47''47''45'cong_4210 ::
  T_RightBolLoop_4174 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_4210 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'47''47''45'cong_2708
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2942
            (coe d_isRightBolLoop_4206 (coe v0))))
-- Algebra.Bundles.RightBolLoop._.//-congʳ
d_'47''47''45'cong'691'_4212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_4212 ~v0 ~v1 v2
  = du_'47''47''45'cong'691'_4212 v2
du_'47''47''45'cong'691'_4212 ::
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_4212 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'691'_2748
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.RightBolLoop._.//-congˡ
d_'47''47''45'cong'737'_4214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_4214 ~v0 ~v1 v2
  = du_'47''47''45'cong'737'_4214 v2
du_'47''47''45'cong'737'_4214 ::
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_4214 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'737'_2744
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.RightBolLoop._.\\-cong
d_'92''92''45'cong_4216 ::
  T_RightBolLoop_4174 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_4216 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'92''92''45'cong_2706
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2942
            (coe d_isRightBolLoop_4206 (coe v0))))
-- Algebra.Bundles.RightBolLoop._.\\-congʳ
d_'92''92''45'cong'691'_4218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_4218 ~v0 ~v1 v2
  = du_'92''92''45'cong'691'_4218 v2
du_'92''92''45'cong'691'_4218 ::
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_4218 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'691'_2740
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.RightBolLoop._.\\-congˡ
d_'92''92''45'cong'737'_4220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_4220 ~v0 ~v1 v2
  = du_'92''92''45'cong'737'_4220 v2
du_'92''92''45'cong'737'_4220 ::
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_4220 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'737'_2736
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.RightBolLoop._.identity
d_identity_4222 ::
  T_RightBolLoop_4174 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_4222 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2942
         (coe d_isRightBolLoop_4206 (coe v0)))
-- Algebra.Bundles.RightBolLoop._.identityʳ
d_identity'691'_4224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny
d_identity'691'_4224 ~v0 ~v1 v2 = du_identity'691'_4224 v2
du_identity'691'_4224 :: T_RightBolLoop_4174 -> AgdaAny -> AgdaAny
du_identity'691'_4224 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_2836
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1))
-- Algebra.Bundles.RightBolLoop._.identityˡ
d_identity'737'_4226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny
d_identity'737'_4226 ~v0 ~v1 v2 = du_identity'737'_4226 v2
du_identity'737'_4226 :: T_RightBolLoop_4174 -> AgdaAny -> AgdaAny
du_identity'737'_4226 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_2834
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1))
-- Algebra.Bundles.RightBolLoop._.isEquivalence
d_isEquivalence_4228 ::
  T_RightBolLoop_4174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_4228 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_2942
               (coe d_isRightBolLoop_4206 (coe v0)))))
-- Algebra.Bundles.RightBolLoop._.isLoop
d_isLoop_4230 ::
  T_RightBolLoop_4174 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_4230 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLoop_2942
      (coe d_isRightBolLoop_4206 (coe v0))
-- Algebra.Bundles.RightBolLoop._.isMagma
d_isMagma_4232 ::
  T_RightBolLoop_4174 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_4232 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2942
            (coe d_isRightBolLoop_4206 (coe v0))))
-- Algebra.Bundles.RightBolLoop._.isPartialEquivalence
d_isPartialEquivalence_4234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_4234 ~v0 ~v1 v2
  = du_isPartialEquivalence_4234 v2
du_isPartialEquivalence_4234 ::
  T_RightBolLoop_4174 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_4234 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Bundles.RightBolLoop._.isQuasigroup
d_isQuasigroup_4236 ::
  T_RightBolLoop_4174 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_4236 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2942
         (coe d_isRightBolLoop_4206 (coe v0)))
-- Algebra.Bundles.RightBolLoop._.leftDivides
d_leftDivides_4238 ::
  T_RightBolLoop_4174 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_4238 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2942
            (coe d_isRightBolLoop_4206 (coe v0))))
-- Algebra.Bundles.RightBolLoop._.leftDividesʳ
d_leftDivides'691'_4240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_4240 ~v0 ~v1 v2 = du_leftDivides'691'_4240 v2
du_leftDivides'691'_4240 ::
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_4240 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'691'_2754
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.RightBolLoop._.leftDividesˡ
d_leftDivides'737'_4242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_4242 ~v0 ~v1 v2 = du_leftDivides'737'_4242 v2
du_leftDivides'737'_4242 ::
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_4242 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'737'_2752
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.RightBolLoop._.refl
d_refl_4244 :: T_RightBolLoop_4174 -> AgdaAny -> AgdaAny
d_refl_4244 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_2942
                  (coe d_isRightBolLoop_4206 (coe v0))))))
-- Algebra.Bundles.RightBolLoop._.reflexive
d_reflexive_4246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_4246 ~v0 ~v1 v2 = du_reflexive_4246 v2
du_reflexive_4246 ::
  T_RightBolLoop_4174 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_4246 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Bundles.RightBolLoop._.rightBol
d_rightBol_4248 ::
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_rightBol_4248 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightBol_2944
      (coe d_isRightBolLoop_4206 (coe v0))
-- Algebra.Bundles.RightBolLoop._.rightDivides
d_rightDivides_4250 ::
  T_RightBolLoop_4174 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_4250 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2942
            (coe d_isRightBolLoop_4206 (coe v0))))
-- Algebra.Bundles.RightBolLoop._.rightDividesʳ
d_rightDivides'691'_4252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_4252 ~v0 ~v1 v2 = du_rightDivides'691'_4252 v2
du_rightDivides'691'_4252 ::
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_4252 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'691'_2758
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.RightBolLoop._.rightDividesˡ
d_rightDivides'737'_4254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_4254 ~v0 ~v1 v2 = du_rightDivides'737'_4254 v2
du_rightDivides'737'_4254 ::
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_4254 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'737'_2756
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.RightBolLoop._.setoid
d_setoid_4256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_4256 ~v0 ~v1 v2 = du_setoid_4256 v2
du_setoid_4256 ::
  T_RightBolLoop_4174 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_4256 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3))
-- Algebra.Bundles.RightBolLoop._.sym
d_sym_4258 ::
  T_RightBolLoop_4174 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_4258 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_2942
                  (coe d_isRightBolLoop_4206 (coe v0))))))
-- Algebra.Bundles.RightBolLoop._.trans
d_trans_4260 ::
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_4260 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_2942
                  (coe d_isRightBolLoop_4206 (coe v0))))))
-- Algebra.Bundles.RightBolLoop._.∙-cong
d_'8729''45'cong_4262 ::
  T_RightBolLoop_4174 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_4262 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_2942
               (coe d_isRightBolLoop_4206 (coe v0)))))
-- Algebra.Bundles.RightBolLoop._.∙-congʳ
d_'8729''45'cong'691'_4264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_4264 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_4264 v2
du_'8729''45'cong'691'_4264 ::
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_4264 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3))
-- Algebra.Bundles.RightBolLoop._.∙-congˡ
d_'8729''45'cong'737'_4266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_4266 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_4266 v2
du_'8729''45'cong'737'_4266 ::
  T_RightBolLoop_4174 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_4266 v0
  = let v1 = d_isRightBolLoop_4206 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_2942 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3))
-- Algebra.Bundles.RightBolLoop.loop
d_loop_4268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 -> T_Loop_3962
d_loop_4268 ~v0 ~v1 v2 = du_loop_4268 v2
du_loop_4268 :: T_RightBolLoop_4174 -> T_Loop_3962
du_loop_4268 v0
  = coe
      C_Loop'46'constructor_69795 (d__'8729'__4198 (coe v0))
      (d__'92''92'__4200 (coe v0)) (d__'47''47'__4202 (coe v0))
      (d_ε_4204 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isLoop_2942
         (coe d_isRightBolLoop_4206 (coe v0)))
-- Algebra.Bundles.RightBolLoop._.quasigroup
d_quasigroup_4272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RightBolLoop_4174 -> T_Quasigroup_3862
d_quasigroup_4272 ~v0 ~v1 v2 = du_quasigroup_4272 v2
du_quasigroup_4272 :: T_RightBolLoop_4174 -> T_Quasigroup_3862
du_quasigroup_4272 v0
  = coe du_quasigroup_4054 (coe du_loop_4268 (coe v0))
-- Algebra.Bundles.MoufangLoop
d_MoufangLoop_4278 a0 a1 = ()
data T_MoufangLoop_4278
  = C_MoufangLoop'46'constructor_76293 (AgdaAny ->
                                        AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
                                       MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010
-- Algebra.Bundles.MoufangLoop.Carrier
d_Carrier_4298 :: T_MoufangLoop_4278 -> ()
d_Carrier_4298 = erased
-- Algebra.Bundles.MoufangLoop._≈_
d__'8776'__4300 :: T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> ()
d__'8776'__4300 = erased
-- Algebra.Bundles.MoufangLoop._∙_
d__'8729'__4302 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__4302 v0
  = case coe v0 of
      C_MoufangLoop'46'constructor_76293 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MoufangLoop._\\_
d__'92''92'__4304 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
d__'92''92'__4304 v0
  = case coe v0 of
      C_MoufangLoop'46'constructor_76293 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MoufangLoop._//_
d__'47''47'__4306 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
d__'47''47'__4306 v0
  = case coe v0 of
      C_MoufangLoop'46'constructor_76293 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MoufangLoop.ε
d_ε_4308 :: T_MoufangLoop_4278 -> AgdaAny
d_ε_4308 v0
  = case coe v0 of
      C_MoufangLoop'46'constructor_76293 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MoufangLoop.isMoufangLoop
d_isMoufangLoop_4310 ::
  T_MoufangLoop_4278 ->
  MAlonzo.Code.Algebra.Structures.T_IsMoufangLoop_3010
d_isMoufangLoop_4310 v0
  = case coe v0 of
      C_MoufangLoop'46'constructor_76293 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MoufangLoop._.//-cong
d_'47''47''45'cong_4314 ::
  T_MoufangLoop_4278 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_4314 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'47''47''45'cong_2708
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
               (coe d_isMoufangLoop_4310 (coe v0)))))
-- Algebra.Bundles.MoufangLoop._.//-congʳ
d_'47''47''45'cong'691'_4316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_4316 ~v0 ~v1 v2
  = du_'47''47''45'cong'691'_4316 v2
du_'47''47''45'cong'691'_4316 ::
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_4316 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'691'_2748
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3))
-- Algebra.Bundles.MoufangLoop._.//-congˡ
d_'47''47''45'cong'737'_4318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_4318 ~v0 ~v1 v2
  = du_'47''47''45'cong'737'_4318 v2
du_'47''47''45'cong'737'_4318 ::
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_4318 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'737'_2744
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3))
-- Algebra.Bundles.MoufangLoop._.\\-cong
d_'92''92''45'cong_4320 ::
  T_MoufangLoop_4278 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_4320 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'92''92''45'cong_2706
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
               (coe d_isMoufangLoop_4310 (coe v0)))))
-- Algebra.Bundles.MoufangLoop._.\\-congʳ
d_'92''92''45'cong'691'_4322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_4322 ~v0 ~v1 v2
  = du_'92''92''45'cong'691'_4322 v2
du_'92''92''45'cong'691'_4322 ::
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_4322 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'691'_2740
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3))
-- Algebra.Bundles.MoufangLoop._.\\-congˡ
d_'92''92''45'cong'737'_4324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_4324 ~v0 ~v1 v2
  = du_'92''92''45'cong'737'_4324 v2
du_'92''92''45'cong'737'_4324 ::
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_4324 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'737'_2736
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3))
-- Algebra.Bundles.MoufangLoop._.identical
d_identical_4326 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identical_4326 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identical_3030
      (coe d_isMoufangLoop_4310 (coe v0))
-- Algebra.Bundles.MoufangLoop._.identity
d_identity_4328 ::
  T_MoufangLoop_4278 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_4328 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
            (coe d_isMoufangLoop_4310 (coe v0))))
-- Algebra.Bundles.MoufangLoop._.identityʳ
d_identity'691'_4330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny
d_identity'691'_4330 ~v0 ~v1 v2 = du_identity'691'_4330 v2
du_identity'691'_4330 :: T_MoufangLoop_4278 -> AgdaAny -> AgdaAny
du_identity'691'_4330 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_2836
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2))
-- Algebra.Bundles.MoufangLoop._.identityˡ
d_identity'737'_4332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny
d_identity'737'_4332 ~v0 ~v1 v2 = du_identity'737'_4332 v2
du_identity'737'_4332 :: T_MoufangLoop_4278 -> AgdaAny -> AgdaAny
du_identity'737'_4332 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_2834
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2))
-- Algebra.Bundles.MoufangLoop._.isEquivalence
d_isEquivalence_4334 ::
  T_MoufangLoop_4278 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_4334 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_2860
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
                  (coe d_isMoufangLoop_4310 (coe v0))))))
-- Algebra.Bundles.MoufangLoop._.isLeftBolLoop
d_isLeftBolLoop_4336 ::
  T_MoufangLoop_4278 ->
  MAlonzo.Code.Algebra.Structures.T_IsLeftBolLoop_2846
d_isLeftBolLoop_4336 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
      (coe d_isMoufangLoop_4310 (coe v0))
-- Algebra.Bundles.MoufangLoop._.isLoop
d_isLoop_4338 ::
  T_MoufangLoop_4278 -> MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_4338 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLoop_2860
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
         (coe d_isMoufangLoop_4310 (coe v0)))
-- Algebra.Bundles.MoufangLoop._.isMagma
d_isMagma_4340 ::
  T_MoufangLoop_4278 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_4340 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
               (coe d_isMoufangLoop_4310 (coe v0)))))
-- Algebra.Bundles.MoufangLoop._.isPartialEquivalence
d_isPartialEquivalence_4342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_4342 ~v0 ~v1 v2
  = du_isPartialEquivalence_4342 v2
du_isPartialEquivalence_4342 ::
  T_MoufangLoop_4278 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_4342 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Bundles.MoufangLoop._.isQuasigroup
d_isQuasigroup_4344 ::
  T_MoufangLoop_4278 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_4344 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_2860
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
            (coe d_isMoufangLoop_4310 (coe v0))))
-- Algebra.Bundles.MoufangLoop._.leftBol
d_leftBol_4346 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_leftBol_4346 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftBol_2862
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
         (coe d_isMoufangLoop_4310 (coe v0)))
-- Algebra.Bundles.MoufangLoop._.leftDivides
d_leftDivides_4348 ::
  T_MoufangLoop_4278 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_4348 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
               (coe d_isMoufangLoop_4310 (coe v0)))))
-- Algebra.Bundles.MoufangLoop._.leftDividesʳ
d_leftDivides'691'_4350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_4350 ~v0 ~v1 v2 = du_leftDivides'691'_4350 v2
du_leftDivides'691'_4350 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_4350 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'691'_2754
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3))
-- Algebra.Bundles.MoufangLoop._.leftDividesˡ
d_leftDivides'737'_4352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_4352 ~v0 ~v1 v2 = du_leftDivides'737'_4352 v2
du_leftDivides'737'_4352 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_4352 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'737'_2752
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3))
-- Algebra.Bundles.MoufangLoop._.refl
d_refl_4354 :: T_MoufangLoop_4278 -> AgdaAny -> AgdaAny
d_refl_4354 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_2860
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
                     (coe d_isMoufangLoop_4310 (coe v0)))))))
-- Algebra.Bundles.MoufangLoop._.reflexive
d_reflexive_4356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_4356 ~v0 ~v1 v2 = du_reflexive_4356 v2
du_reflexive_4356 ::
  T_MoufangLoop_4278 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_4356 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Bundles.MoufangLoop._.rightBol
d_rightBol_4358 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_rightBol_4358 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightBol_3028
      (coe d_isMoufangLoop_4310 (coe v0))
-- Algebra.Bundles.MoufangLoop._.rightDivides
d_rightDivides_4360 ::
  T_MoufangLoop_4278 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_4360 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_2860
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
               (coe d_isMoufangLoop_4310 (coe v0)))))
-- Algebra.Bundles.MoufangLoop._.rightDividesʳ
d_rightDivides'691'_4362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_4362 ~v0 ~v1 v2 = du_rightDivides'691'_4362 v2
du_rightDivides'691'_4362 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_4362 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'691'_2758
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3))
-- Algebra.Bundles.MoufangLoop._.rightDividesˡ
d_rightDivides'737'_4364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_4364 ~v0 ~v1 v2 = du_rightDivides'737'_4364 v2
du_rightDivides'737'_4364 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_4364 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'737'_2756
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3))
-- Algebra.Bundles.MoufangLoop._.setoid
d_setoid_4366 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_4366 ~v0 ~v1 v2 = du_setoid_4366 v2
du_setoid_4366 ::
  T_MoufangLoop_4278 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_4366 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v4))
-- Algebra.Bundles.MoufangLoop._.sym
d_sym_4368 ::
  T_MoufangLoop_4278 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_4368 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_2860
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
                     (coe d_isMoufangLoop_4310 (coe v0)))))))
-- Algebra.Bundles.MoufangLoop._.trans
d_trans_4370 ::
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_4370 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_2860
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
                     (coe d_isMoufangLoop_4310 (coe v0)))))))
-- Algebra.Bundles.MoufangLoop._.∙-cong
d_'8729''45'cong_4372 ::
  T_MoufangLoop_4278 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_4372 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_2860
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
                  (coe d_isMoufangLoop_4310 (coe v0))))))
-- Algebra.Bundles.MoufangLoop._.∙-congʳ
d_'8729''45'cong'691'_4374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_4374 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_4374 v2
du_'8729''45'cong'691'_4374 ::
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_4374 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v4))
-- Algebra.Bundles.MoufangLoop._.∙-congˡ
d_'8729''45'cong'737'_4376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_4376 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_4376 v2
du_'8729''45'cong'737'_4376 ::
  T_MoufangLoop_4278 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_4376 v0
  = let v1 = d_isMoufangLoop_4310 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isLoop_2860 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v4))
-- Algebra.Bundles.MoufangLoop.leftBolLoop
d_leftBolLoop_4378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 -> T_LeftBolLoop_4070
d_leftBolLoop_4378 ~v0 ~v1 v2 = du_leftBolLoop_4378 v2
du_leftBolLoop_4378 :: T_MoufangLoop_4278 -> T_LeftBolLoop_4070
du_leftBolLoop_4378 v0
  = coe
      C_LeftBolLoop'46'constructor_72005 (d__'8729'__4302 (coe v0))
      (d__'92''92'__4304 (coe v0)) (d__'47''47'__4306 (coe v0))
      (d_ε_4308 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isLeftBolLoop_3026
         (coe d_isMoufangLoop_4310 (coe v0)))
-- Algebra.Bundles.MoufangLoop._.loop
d_loop_4382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MoufangLoop_4278 -> T_Loop_3962
d_loop_4382 ~v0 ~v1 v2 = du_loop_4382 v2
du_loop_4382 :: T_MoufangLoop_4278 -> T_Loop_3962
du_loop_4382 v0
  = coe du_loop_4164 (coe du_leftBolLoop_4378 (coe v0))
-- Algebra.Bundles.MiddleBolLoop
d_MiddleBolLoop_4388 a0 a1 = ()
data T_MiddleBolLoop_4388
  = C_MiddleBolLoop'46'constructor_78527 (AgdaAny ->
                                          AgdaAny -> AgdaAny)
                                         (AgdaAny -> AgdaAny -> AgdaAny)
                                         (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
                                         MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100
-- Algebra.Bundles.MiddleBolLoop.Carrier
d_Carrier_4408 :: T_MiddleBolLoop_4388 -> ()
d_Carrier_4408 = erased
-- Algebra.Bundles.MiddleBolLoop._≈_
d__'8776'__4410 :: T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> ()
d__'8776'__4410 = erased
-- Algebra.Bundles.MiddleBolLoop._∙_
d__'8729'__4412 ::
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__4412 v0
  = case coe v0 of
      C_MiddleBolLoop'46'constructor_78527 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MiddleBolLoop._\\_
d__'92''92'__4414 ::
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
d__'92''92'__4414 v0
  = case coe v0 of
      C_MiddleBolLoop'46'constructor_78527 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MiddleBolLoop._//_
d__'47''47'__4416 ::
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
d__'47''47'__4416 v0
  = case coe v0 of
      C_MiddleBolLoop'46'constructor_78527 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MiddleBolLoop.ε
d_ε_4418 :: T_MiddleBolLoop_4388 -> AgdaAny
d_ε_4418 v0
  = case coe v0 of
      C_MiddleBolLoop'46'constructor_78527 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MiddleBolLoop.isMiddleBolLoop
d_isMiddleBolLoop_4420 ::
  T_MiddleBolLoop_4388 ->
  MAlonzo.Code.Algebra.Structures.T_IsMiddleBolLoop_3100
d_isMiddleBolLoop_4420 v0
  = case coe v0 of
      C_MiddleBolLoop'46'constructor_78527 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.MiddleBolLoop._.//-cong
d_'47''47''45'cong_4424 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_4424 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'47''47''45'cong_2708
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_3114
            (coe d_isMiddleBolLoop_4420 (coe v0))))
-- Algebra.Bundles.MiddleBolLoop._.//-congʳ
d_'47''47''45'cong'691'_4426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_4426 ~v0 ~v1 v2
  = du_'47''47''45'cong'691'_4426 v2
du_'47''47''45'cong'691'_4426 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_4426 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'691'_2748
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.MiddleBolLoop._.//-congˡ
d_'47''47''45'cong'737'_4428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_4428 ~v0 ~v1 v2
  = du_'47''47''45'cong'737'_4428 v2
du_'47''47''45'cong'737'_4428 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_4428 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'47''47''45'cong'737'_2744
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.MiddleBolLoop._.\\-cong
d_'92''92''45'cong_4430 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_4430 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'92''92''45'cong_2706
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_3114
            (coe d_isMiddleBolLoop_4420 (coe v0))))
-- Algebra.Bundles.MiddleBolLoop._.\\-congʳ
d_'92''92''45'cong'691'_4432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_4432 ~v0 ~v1 v2
  = du_'92''92''45'cong'691'_4432 v2
du_'92''92''45'cong'691'_4432 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_4432 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'691'_2740
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.MiddleBolLoop._.\\-congˡ
d_'92''92''45'cong'737'_4434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_4434 ~v0 ~v1 v2
  = du_'92''92''45'cong'737'_4434 v2
du_'92''92''45'cong'737'_4434 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_4434 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'92''92''45'cong'737'_2736
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.MiddleBolLoop._.identity
d_identity_4436 ::
  T_MiddleBolLoop_4388 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_4436 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_2784
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_3114
         (coe d_isMiddleBolLoop_4420 (coe v0)))
-- Algebra.Bundles.MiddleBolLoop._.identityʳ
d_identity'691'_4438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny
d_identity'691'_4438 ~v0 ~v1 v2 = du_identity'691'_4438 v2
du_identity'691'_4438 :: T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny
du_identity'691'_4438 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_2836
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1))
-- Algebra.Bundles.MiddleBolLoop._.identityˡ
d_identity'737'_4440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny
d_identity'737'_4440 ~v0 ~v1 v2 = du_identity'737'_4440 v2
du_identity'737'_4440 :: T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny
du_identity'737'_4440 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_2834
      (coe MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1))
-- Algebra.Bundles.MiddleBolLoop._.isEquivalence
d_isEquivalence_4442 ::
  T_MiddleBolLoop_4388 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_4442 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_3114
               (coe d_isMiddleBolLoop_4420 (coe v0)))))
-- Algebra.Bundles.MiddleBolLoop._.isLoop
d_isLoop_4444 ::
  T_MiddleBolLoop_4388 ->
  MAlonzo.Code.Algebra.Structures.T_IsLoop_2768
d_isLoop_4444 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isLoop_3114
      (coe d_isMiddleBolLoop_4420 (coe v0))
-- Algebra.Bundles.MiddleBolLoop._.isMagma
d_isMagma_4446 ::
  T_MiddleBolLoop_4388 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_4446 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_2704
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_3114
            (coe d_isMiddleBolLoop_4420 (coe v0))))
-- Algebra.Bundles.MiddleBolLoop._.isPartialEquivalence
d_isPartialEquivalence_4448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_4448 ~v0 ~v1 v2
  = du_isPartialEquivalence_4448 v2
du_isPartialEquivalence_4448 ::
  T_MiddleBolLoop_4388 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_4448 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Bundles.MiddleBolLoop._.isQuasigroup
d_isQuasigroup_4450 ::
  T_MiddleBolLoop_4388 ->
  MAlonzo.Code.Algebra.Structures.T_IsQuasigroup_2686
d_isQuasigroup_4450 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
      (coe
         MAlonzo.Code.Algebra.Structures.d_isLoop_3114
         (coe d_isMiddleBolLoop_4420 (coe v0)))
-- Algebra.Bundles.MiddleBolLoop._.leftDivides
d_leftDivides_4452 ::
  T_MiddleBolLoop_4388 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_4452 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_leftDivides_2710
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_3114
            (coe d_isMiddleBolLoop_4420 (coe v0))))
-- Algebra.Bundles.MiddleBolLoop._.leftDividesʳ
d_leftDivides'691'_4454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_4454 ~v0 ~v1 v2 = du_leftDivides'691'_4454 v2
du_leftDivides'691'_4454 ::
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_4454 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'691'_2754
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.MiddleBolLoop._.leftDividesˡ
d_leftDivides'737'_4456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_4456 ~v0 ~v1 v2 = du_leftDivides'737'_4456 v2
du_leftDivides'737'_4456 ::
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_4456 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_leftDivides'737'_2752
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.MiddleBolLoop._.middleBol
d_middleBol_4458 ::
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_middleBol_4458 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_middleBol_3116
      (coe d_isMiddleBolLoop_4420 (coe v0))
-- Algebra.Bundles.MiddleBolLoop._.refl
d_refl_4460 :: T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny
d_refl_4460 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_3114
                  (coe d_isMiddleBolLoop_4420 (coe v0))))))
-- Algebra.Bundles.MiddleBolLoop._.reflexive
d_reflexive_4462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_4462 ~v0 ~v1 v2 = du_reflexive_4462 v2
du_reflexive_4462 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_4462 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Bundles.MiddleBolLoop._.rightDivides
d_rightDivides_4464 ::
  T_MiddleBolLoop_4388 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_4464 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_rightDivides_2712
      (coe
         MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
         (coe
            MAlonzo.Code.Algebra.Structures.d_isLoop_3114
            (coe d_isMiddleBolLoop_4420 (coe v0))))
-- Algebra.Bundles.MiddleBolLoop._.rightDividesʳ
d_rightDivides'691'_4466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_4466 ~v0 ~v1 v2 = du_rightDivides'691'_4466 v2
du_rightDivides'691'_4466 ::
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_4466 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'691'_2758
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.MiddleBolLoop._.rightDividesˡ
d_rightDivides'737'_4468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_4468 ~v0 ~v1 v2 = du_rightDivides'737'_4468 v2
du_rightDivides'737'_4468 ::
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_4468 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_rightDivides'737'_2756
      (coe MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2))
-- Algebra.Bundles.MiddleBolLoop._.setoid
d_setoid_4470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_4470 ~v0 ~v1 v2 = du_setoid_4470 v2
du_setoid_4470 ::
  T_MiddleBolLoop_4388 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_4470 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3))
-- Algebra.Bundles.MiddleBolLoop._.sym
d_sym_4472 ::
  T_MiddleBolLoop_4388 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_4472 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_3114
                  (coe d_isMiddleBolLoop_4420 (coe v0))))))
-- Algebra.Bundles.MiddleBolLoop._.trans
d_trans_4474 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_4474 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_2704
            (coe
               MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isLoop_3114
                  (coe d_isMiddleBolLoop_4420 (coe v0))))))
-- Algebra.Bundles.MiddleBolLoop._.∙-cong
d_'8729''45'cong_4476 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_4476 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_2704
         (coe
            MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782
            (coe
               MAlonzo.Code.Algebra.Structures.d_isLoop_3114
               (coe d_isMiddleBolLoop_4420 (coe v0)))))
-- Algebra.Bundles.MiddleBolLoop._.∙-congʳ
d_'8729''45'cong'691'_4478 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_4478 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_4478 v2
du_'8729''45'cong'691'_4478 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_4478 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3))
-- Algebra.Bundles.MiddleBolLoop._.∙-congˡ
d_'8729''45'cong'737'_4480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_4480 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_4480 v2
du_'8729''45'cong'737'_4480 ::
  T_MiddleBolLoop_4388 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_4480 v0
  = let v1 = d_isMiddleBolLoop_4420 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isLoop_3114 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isQuasigroup_2782 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_2704 (coe v3))
-- Algebra.Bundles.MiddleBolLoop.loop
d_loop_4482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 -> T_Loop_3962
d_loop_4482 ~v0 ~v1 v2 = du_loop_4482 v2
du_loop_4482 :: T_MiddleBolLoop_4388 -> T_Loop_3962
du_loop_4482 v0
  = coe
      C_Loop'46'constructor_69795 (d__'8729'__4412 (coe v0))
      (d__'92''92'__4414 (coe v0)) (d__'47''47'__4416 (coe v0))
      (d_ε_4418 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d_isLoop_3114
         (coe d_isMiddleBolLoop_4420 (coe v0)))
-- Algebra.Bundles.MiddleBolLoop._.quasigroup
d_quasigroup_4486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MiddleBolLoop_4388 -> T_Quasigroup_3862
d_quasigroup_4486 ~v0 ~v1 v2 = du_quasigroup_4486 v2
du_quasigroup_4486 :: T_MiddleBolLoop_4388 -> T_Quasigroup_3862
du_quasigroup_4486 v0
  = coe du_quasigroup_4054 (coe du_loop_4482 (coe v0))
