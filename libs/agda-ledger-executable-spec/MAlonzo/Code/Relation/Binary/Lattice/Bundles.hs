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

module MAlonzo.Code.Relation.Binary.Lattice.Bundles where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Lattice.Structures
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Relation.Binary.Lattice.Bundles.JoinSemilattice
d_JoinSemilattice_14 a0 a1 a2 = ()
data T_JoinSemilattice_14
  = C_JoinSemilattice'46'constructor_363 (AgdaAny ->
                                          AgdaAny -> AgdaAny)
                                         MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
-- Relation.Binary.Lattice.Bundles.JoinSemilattice.Carrier
d_Carrier_32 :: T_JoinSemilattice_14 -> ()
d_Carrier_32 = erased
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._≈_
d__'8776'__34 :: T_JoinSemilattice_14 -> AgdaAny -> AgdaAny -> ()
d__'8776'__34 = erased
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._≤_
d__'8804'__36 :: T_JoinSemilattice_14 -> AgdaAny -> AgdaAny -> ()
d__'8804'__36 = erased
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._∨_
d__'8744'__38 ::
  T_JoinSemilattice_14 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__38 v0
  = case coe v0 of
      C_JoinSemilattice'46'constructor_363 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.JoinSemilattice.isJoinSemilattice
d_isJoinSemilattice_40 ::
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_isJoinSemilattice_40 v0
  = case coe v0 of
      C_JoinSemilattice'46'constructor_363 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.antisym
d_antisym_44 ::
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_44 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
         (coe d_isJoinSemilattice_40 (coe v0)))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.isEquivalence
d_isEquivalence_46 ::
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_46 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
            (coe d_isJoinSemilattice_40 (coe v0))))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.isPartialOrder
d_isPartialOrder_48 ::
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_48 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
      (coe d_isJoinSemilattice_40 (coe v0))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.isPreorder
d_isPreorder_50 ::
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_50 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
         (coe d_isJoinSemilattice_40 (coe v0)))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.refl
d_refl_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 -> AgdaAny -> AgdaAny
d_refl_52 ~v0 ~v1 ~v2 v3 = du_refl_52 v3
du_refl_52 :: T_JoinSemilattice_14 -> AgdaAny -> AgdaAny
du_refl_52 v0
  = let v1 = d_isJoinSemilattice_40 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.reflexive
d_reflexive_54 ::
  T_JoinSemilattice_14 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_54 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
            (coe d_isJoinSemilattice_40 (coe v0))))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.supremum
d_supremum_56 ::
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_56 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_supremum_32
      (coe d_isJoinSemilattice_40 (coe v0))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.trans
d_trans_58 ::
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_58 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
            (coe d_isJoinSemilattice_40 (coe v0))))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.x≤x∨y
d_x'8804'x'8744'y_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_60 ~v0 ~v1 ~v2 v3 = du_x'8804'x'8744'y_60 v3
du_x'8804'x'8744'y_60 ::
  T_JoinSemilattice_14 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_60 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8804'x'8744'y_38
      (coe d_isJoinSemilattice_40 (coe v0))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.y≤x∨y
d_y'8804'x'8744'y_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_62 ~v0 ~v1 ~v2 v3 = du_y'8804'x'8744'y_62 v3
du_y'8804'x'8744'y_62 ::
  T_JoinSemilattice_14 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_62 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_y'8804'x'8744'y_50
      (coe d_isJoinSemilattice_40 (coe v0))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.∨-least
d_'8744''45'least_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_64 ~v0 ~v1 ~v2 v3 = du_'8744''45'least_64 v3
du_'8744''45'least_64 ::
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_64 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8744''45'least_64
      (coe d_isJoinSemilattice_40 (coe v0))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_66 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_66 v3
du_'8764''45'resp'45''8776'_66 ::
  T_JoinSemilattice_14 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_66 v0
  = let v1 = d_isJoinSemilattice_40 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_68 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_68 v3
du_'8764''45'resp'691''45''8776'_68 ::
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_68 v0
  = let v1 = d_isJoinSemilattice_40 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_70 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_70 v3
du_'8764''45'resp'737''45''8776'_70 ::
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_70 v0
  = let v1 = d_isJoinSemilattice_40 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_74 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_74 v3
du_isPartialEquivalence_74 ::
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_74 v0
  = let v1 = d_isJoinSemilattice_40 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v3))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.Eq.refl
d_refl_76 :: T_JoinSemilattice_14 -> AgdaAny -> AgdaAny
d_refl_76 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
               (coe d_isJoinSemilattice_40 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.Eq.reflexive
d_reflexive_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_78 ~v0 ~v1 ~v2 v3 = du_reflexive_78 v3
du_reflexive_78 ::
  T_JoinSemilattice_14 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_78 v0
  = let v1 = d_isJoinSemilattice_40 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
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
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.Eq.sym
d_sym_80 ::
  T_JoinSemilattice_14 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_80 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
               (coe d_isJoinSemilattice_40 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.Eq.trans
d_trans_82 ::
  T_JoinSemilattice_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_82 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
               (coe d_isJoinSemilattice_40 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice.poset
d_poset_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_84 ~v0 ~v1 ~v2 v3 = du_poset_84 v3
du_poset_84 ::
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_84 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      (MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
         (coe d_isJoinSemilattice_40 (coe v0)))
-- Relation.Binary.Lattice.Bundles.JoinSemilattice._.preorder
d_preorder_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_88 ~v0 ~v1 ~v2 v3 = du_preorder_88 v3
du_preorder_88 ::
  T_JoinSemilattice_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_88 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326
      (coe du_poset_84 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice
d_BoundedJoinSemilattice_96 a0 a1 a2 = ()
data T_BoundedJoinSemilattice_96
  = C_BoundedJoinSemilattice'46'constructor_2251 (AgdaAny ->
                                                  AgdaAny -> AgdaAny)
                                                 AgdaAny
                                                 MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice.Carrier
d_Carrier_116 :: T_BoundedJoinSemilattice_96 -> ()
d_Carrier_116 = erased
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._≈_
d__'8776'__118 ::
  T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny -> ()
d__'8776'__118 = erased
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._≤_
d__'8804'__120 ::
  T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny -> ()
d__'8804'__120 = erased
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._∨_
d__'8744'__122 ::
  T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__122 v0
  = case coe v0 of
      C_BoundedJoinSemilattice'46'constructor_2251 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice.⊥
d_'8869'_124 :: T_BoundedJoinSemilattice_96 -> AgdaAny
d_'8869'_124 v0
  = case coe v0 of
      C_BoundedJoinSemilattice'46'constructor_2251 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice.isBoundedJoinSemilattice
d_isBoundedJoinSemilattice_126 ::
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
d_isBoundedJoinSemilattice_126 v0
  = case coe v0 of
      C_BoundedJoinSemilattice'46'constructor_2251 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.antisym
d_antisym_130 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_130 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
            (coe d_isBoundedJoinSemilattice_126 (coe v0))))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.isEquivalence
d_isEquivalence_132 ::
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_132 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
               (coe d_isBoundedJoinSemilattice_126 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.isJoinSemilattice
d_isJoinSemilattice_134 ::
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_isJoinSemilattice_134 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
      (coe d_isBoundedJoinSemilattice_126 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.isPartialOrder
d_isPartialOrder_136 ::
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_136 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
         (coe d_isBoundedJoinSemilattice_126 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.isPreorder
d_isPreorder_138 ::
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_138 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
            (coe d_isBoundedJoinSemilattice_126 (coe v0))))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.minimum
d_minimum_140 :: T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny
d_minimum_140 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_minimum_122
      (coe d_isBoundedJoinSemilattice_126 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.refl
d_refl_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny
d_refl_142 ~v0 ~v1 ~v2 v3 = du_refl_142 v3
du_refl_142 :: T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny
du_refl_142 v0
  = let v1 = d_isBoundedJoinSemilattice_126 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.reflexive
d_reflexive_144 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_144 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
               (coe d_isBoundedJoinSemilattice_126 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.supremum
d_supremum_146 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_146 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_supremum_32
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
         (coe d_isBoundedJoinSemilattice_126 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.trans
d_trans_148 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_148 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
               (coe d_isBoundedJoinSemilattice_126 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.x≤x∨y
d_x'8804'x'8744'y_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_150 ~v0 ~v1 ~v2 v3 = du_x'8804'x'8744'y_150 v3
du_x'8804'x'8744'y_150 ::
  T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_150 v0
  = let v1 = d_isBoundedJoinSemilattice_126 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8804'x'8744'y_38
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
         (coe v1))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.y≤x∨y
d_y'8804'x'8744'y_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_152 ~v0 ~v1 ~v2 v3 = du_y'8804'x'8744'y_152 v3
du_y'8804'x'8744'y_152 ::
  T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_152 v0
  = let v1 = d_isBoundedJoinSemilattice_126 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_y'8804'x'8744'y_50
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
         (coe v1))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.∨-least
d_'8744''45'least_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_154 ~v0 ~v1 ~v2 v3 = du_'8744''45'least_154 v3
du_'8744''45'least_154 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_154 v0
  = let v1 = d_isBoundedJoinSemilattice_126 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8744''45'least_64
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
         (coe v1))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_156 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_156 v3
du_'8764''45'resp'45''8776'_156 ::
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_156 v0
  = let v1 = d_isBoundedJoinSemilattice_126 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_158 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_158 v3
du_'8764''45'resp'691''45''8776'_158 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_158 v0
  = let v1 = d_isBoundedJoinSemilattice_126 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_160 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_160 v3
du_'8764''45'resp'737''45''8776'_160 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_160 v0
  = let v1 = d_isBoundedJoinSemilattice_126 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_164 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_164 v3
du_isPartialEquivalence_164 ::
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_164 v0
  = let v1 = d_isBoundedJoinSemilattice_126 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v4))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.Eq.refl
d_refl_166 :: T_BoundedJoinSemilattice_96 -> AgdaAny -> AgdaAny
d_refl_166 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
                  (coe d_isBoundedJoinSemilattice_126 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.Eq.reflexive
d_reflexive_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_168 ~v0 ~v1 ~v2 v3 = du_reflexive_168 v3
du_reflexive_168 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_168 v0
  = let v1 = d_isBoundedJoinSemilattice_126 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
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
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.Eq.sym
d_sym_170 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_170 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
                  (coe d_isBoundedJoinSemilattice_126 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.Eq.trans
d_trans_172 ::
  T_BoundedJoinSemilattice_96 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_172 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_30
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
                  (coe d_isBoundedJoinSemilattice_126 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice.joinSemilattice
d_joinSemilattice_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 -> T_JoinSemilattice_14
d_joinSemilattice_174 ~v0 ~v1 ~v2 v3 = du_joinSemilattice_174 v3
du_joinSemilattice_174 ::
  T_BoundedJoinSemilattice_96 -> T_JoinSemilattice_14
du_joinSemilattice_174 v0
  = coe
      C_JoinSemilattice'46'constructor_363 (d__'8744'__122 (coe v0))
      (MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isJoinSemilattice_120
         (coe d_isBoundedJoinSemilattice_126 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.poset
d_poset_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_178 ~v0 ~v1 ~v2 v3 = du_poset_178 v3
du_poset_178 ::
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_178 v0
  = coe du_poset_84 (coe du_joinSemilattice_174 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedJoinSemilattice._.preorder
d_preorder_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_180 ~v0 ~v1 ~v2 v3 = du_preorder_180 v3
du_preorder_180 ::
  T_BoundedJoinSemilattice_96 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_180 v0
  = let v1 = coe du_joinSemilattice_174 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326
      (coe du_poset_84 (coe v1))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice
d_MeetSemilattice_188 a0 a1 a2 = ()
data T_MeetSemilattice_188
  = C_MeetSemilattice'46'constructor_4329 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
-- Relation.Binary.Lattice.Bundles.MeetSemilattice.Carrier
d_Carrier_206 :: T_MeetSemilattice_188 -> ()
d_Carrier_206 = erased
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._≈_
d__'8776'__208 :: T_MeetSemilattice_188 -> AgdaAny -> AgdaAny -> ()
d__'8776'__208 = erased
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._≤_
d__'8804'__210 :: T_MeetSemilattice_188 -> AgdaAny -> AgdaAny -> ()
d__'8804'__210 = erased
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._∧_
d__'8743'__212 ::
  T_MeetSemilattice_188 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__212 v0
  = case coe v0 of
      C_MeetSemilattice'46'constructor_4329 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.MeetSemilattice.isMeetSemilattice
d_isMeetSemilattice_214 ::
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_isMeetSemilattice_214 v0
  = case coe v0 of
      C_MeetSemilattice'46'constructor_4329 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.antisym
d_antisym_218 ::
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_218 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
         (coe d_isMeetSemilattice_214 (coe v0)))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.infimum
d_infimum_220 ::
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_220 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_infimum_178
      (coe d_isMeetSemilattice_214 (coe v0))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.isEquivalence
d_isEquivalence_222 ::
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_222 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
            (coe d_isMeetSemilattice_214 (coe v0))))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.isPartialOrder
d_isPartialOrder_224 ::
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_224 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
      (coe d_isMeetSemilattice_214 (coe v0))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.isPreorder
d_isPreorder_226 ::
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_226 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
         (coe d_isMeetSemilattice_214 (coe v0)))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.refl
d_refl_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 -> AgdaAny -> AgdaAny
d_refl_228 ~v0 ~v1 ~v2 v3 = du_refl_228 v3
du_refl_228 :: T_MeetSemilattice_188 -> AgdaAny -> AgdaAny
du_refl_228 v0
  = let v1 = d_isMeetSemilattice_214 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.reflexive
d_reflexive_230 ::
  T_MeetSemilattice_188 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_230 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
            (coe d_isMeetSemilattice_214 (coe v0))))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.trans
d_trans_232 ::
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_232 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
            (coe d_isMeetSemilattice_214 (coe v0))))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.x∧y≤x
d_x'8743'y'8804'x_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_234 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'x_234 v3
du_x'8743'y'8804'x_234 ::
  T_MeetSemilattice_188 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_234 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'x_184
      (coe d_isMeetSemilattice_214 (coe v0))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.x∧y≤y
d_x'8743'y'8804'y_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_236 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'y_236 v3
du_x'8743'y'8804'y_236 ::
  T_MeetSemilattice_188 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_236 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'y_196
      (coe d_isMeetSemilattice_214 (coe v0))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.∧-greatest
d_'8743''45'greatest_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_238 ~v0 ~v1 ~v2 v3
  = du_'8743''45'greatest_238 v3
du_'8743''45'greatest_238 ::
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_238 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8743''45'greatest_210
      (coe d_isMeetSemilattice_214 (coe v0))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_240 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_240 v3
du_'8764''45'resp'45''8776'_240 ::
  T_MeetSemilattice_188 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_240 v0
  = let v1 = d_isMeetSemilattice_214 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_242 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_242 v3
du_'8764''45'resp'691''45''8776'_242 ::
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_242 v0
  = let v1 = d_isMeetSemilattice_214 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_244 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_244 v3
du_'8764''45'resp'737''45''8776'_244 ::
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_244 v0
  = let v1 = d_isMeetSemilattice_214 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_248 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_248 v3
du_isPartialEquivalence_248 ::
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_248 v0
  = let v1 = d_isMeetSemilattice_214 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v3))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.Eq.refl
d_refl_250 :: T_MeetSemilattice_188 -> AgdaAny -> AgdaAny
d_refl_250 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
               (coe d_isMeetSemilattice_214 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.Eq.reflexive
d_reflexive_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_252 ~v0 ~v1 ~v2 v3 = du_reflexive_252 v3
du_reflexive_252 ::
  T_MeetSemilattice_188 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_252 v0
  = let v1 = d_isMeetSemilattice_214 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
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
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.Eq.sym
d_sym_254 ::
  T_MeetSemilattice_188 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_254 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
               (coe d_isMeetSemilattice_214 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.Eq.trans
d_trans_256 ::
  T_MeetSemilattice_188 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_256 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
               (coe d_isMeetSemilattice_214 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice.poset
d_poset_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_258 ~v0 ~v1 ~v2 v3 = du_poset_258 v3
du_poset_258 ::
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_258 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      (MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
         (coe d_isMeetSemilattice_214 (coe v0)))
-- Relation.Binary.Lattice.Bundles.MeetSemilattice._.preorder
d_preorder_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_262 ~v0 ~v1 ~v2 v3 = du_preorder_262 v3
du_preorder_262 ::
  T_MeetSemilattice_188 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_262 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326
      (coe du_poset_258 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice
d_BoundedMeetSemilattice_270 a0 a1 a2 = ()
data T_BoundedMeetSemilattice_270
  = C_BoundedMeetSemilattice'46'constructor_6217 (AgdaAny ->
                                                  AgdaAny -> AgdaAny)
                                                 AgdaAny
                                                 MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedMeetSemilattice_256
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice.Carrier
d_Carrier_290 :: T_BoundedMeetSemilattice_270 -> ()
d_Carrier_290 = erased
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._≈_
d__'8776'__292 ::
  T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny -> ()
d__'8776'__292 = erased
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._≤_
d__'8804'__294 ::
  T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny -> ()
d__'8804'__294 = erased
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._∧_
d__'8743'__296 ::
  T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__296 v0
  = case coe v0 of
      C_BoundedMeetSemilattice'46'constructor_6217 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice.⊤
d_'8868'_298 :: T_BoundedMeetSemilattice_270 -> AgdaAny
d_'8868'_298 v0
  = case coe v0 of
      C_BoundedMeetSemilattice'46'constructor_6217 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice.isBoundedMeetSemilattice
d_isBoundedMeetSemilattice_300 ::
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedMeetSemilattice_256
d_isBoundedMeetSemilattice_300 v0
  = case coe v0 of
      C_BoundedMeetSemilattice'46'constructor_6217 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.antisym
d_antisym_304 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_304 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
            (coe d_isBoundedMeetSemilattice_300 (coe v0))))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.infimum
d_infimum_306 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_306 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_infimum_178
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
         (coe d_isBoundedMeetSemilattice_300 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.isEquivalence
d_isEquivalence_308 ::
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_308 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
               (coe d_isBoundedMeetSemilattice_300 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.isMeetSemilattice
d_isMeetSemilattice_310 ::
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_isMeetSemilattice_310 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
      (coe d_isBoundedMeetSemilattice_300 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.isPartialOrder
d_isPartialOrder_312 ::
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_312 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
         (coe d_isBoundedMeetSemilattice_300 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.isPreorder
d_isPreorder_314 ::
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_314 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
            (coe d_isBoundedMeetSemilattice_300 (coe v0))))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.maximum
d_maximum_316 :: T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny
d_maximum_316 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_maximum_268
      (coe d_isBoundedMeetSemilattice_300 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.refl
d_refl_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny
d_refl_318 ~v0 ~v1 ~v2 v3 = du_refl_318 v3
du_refl_318 :: T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny
du_refl_318 v0
  = let v1 = d_isBoundedMeetSemilattice_300 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.reflexive
d_reflexive_320 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_320 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
               (coe d_isBoundedMeetSemilattice_300 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.trans
d_trans_322 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_322 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
               (coe d_isBoundedMeetSemilattice_300 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.x∧y≤x
d_x'8743'y'8804'x_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_324 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'x_324 v3
du_x'8743'y'8804'x_324 ::
  T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_324 v0
  = let v1 = d_isBoundedMeetSemilattice_300 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'x_184
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
         (coe v1))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.x∧y≤y
d_x'8743'y'8804'y_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_326 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'y_326 v3
du_x'8743'y'8804'y_326 ::
  T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_326 v0
  = let v1 = d_isBoundedMeetSemilattice_300 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'y_196
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
         (coe v1))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.∧-greatest
d_'8743''45'greatest_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_328 ~v0 ~v1 ~v2 v3
  = du_'8743''45'greatest_328 v3
du_'8743''45'greatest_328 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_328 v0
  = let v1 = d_isBoundedMeetSemilattice_300 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8743''45'greatest_210
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
         (coe v1))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_330 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_330 v3
du_'8764''45'resp'45''8776'_330 ::
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_330 v0
  = let v1 = d_isBoundedMeetSemilattice_300 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_332 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_332 v3
du_'8764''45'resp'691''45''8776'_332 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_332 v0
  = let v1 = d_isBoundedMeetSemilattice_300 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_334 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_334 v3
du_'8764''45'resp'737''45''8776'_334 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_334 v0
  = let v1 = d_isBoundedMeetSemilattice_300 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_338 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_338 v3
du_isPartialEquivalence_338 ::
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_338 v0
  = let v1 = d_isBoundedMeetSemilattice_300 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v4))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.Eq.refl
d_refl_340 :: T_BoundedMeetSemilattice_270 -> AgdaAny -> AgdaAny
d_refl_340 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
                  (coe d_isBoundedMeetSemilattice_300 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.Eq.reflexive
d_reflexive_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_342 ~v0 ~v1 ~v2 v3 = du_reflexive_342 v3
du_reflexive_342 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_342 v0
  = let v1 = d_isBoundedMeetSemilattice_300 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
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
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.Eq.sym
d_sym_344 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_344 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
                  (coe d_isBoundedMeetSemilattice_300 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.Eq.trans
d_trans_346 ::
  T_BoundedMeetSemilattice_270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_346 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_176
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
                  (coe d_isBoundedMeetSemilattice_300 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice.meetSemilattice
d_meetSemilattice_348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 -> T_MeetSemilattice_188
d_meetSemilattice_348 ~v0 ~v1 ~v2 v3 = du_meetSemilattice_348 v3
du_meetSemilattice_348 ::
  T_BoundedMeetSemilattice_270 -> T_MeetSemilattice_188
du_meetSemilattice_348 v0
  = coe
      C_MeetSemilattice'46'constructor_4329 (d__'8743'__296 (coe v0))
      (MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isMeetSemilattice_266
         (coe d_isBoundedMeetSemilattice_300 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.poset
d_poset_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_352 ~v0 ~v1 ~v2 v3 = du_poset_352 v3
du_poset_352 ::
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_352 v0
  = coe du_poset_258 (coe du_meetSemilattice_348 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedMeetSemilattice._.preorder
d_preorder_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_354 ~v0 ~v1 ~v2 v3 = du_preorder_354 v3
du_preorder_354 ::
  T_BoundedMeetSemilattice_270 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_354 v0
  = let v1 = coe du_meetSemilattice_348 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326
      (coe du_poset_258 (coe v1))
-- Relation.Binary.Lattice.Bundles.Lattice
d_Lattice_362 a0 a1 a2 = ()
data T_Lattice_362
  = C_Lattice'46'constructor_8383 (AgdaAny -> AgdaAny -> AgdaAny)
                                  (AgdaAny -> AgdaAny -> AgdaAny)
                                  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
-- Relation.Binary.Lattice.Bundles.Lattice.Carrier
d_Carrier_382 :: T_Lattice_362 -> ()
d_Carrier_382 = erased
-- Relation.Binary.Lattice.Bundles.Lattice._≈_
d__'8776'__384 :: T_Lattice_362 -> AgdaAny -> AgdaAny -> ()
d__'8776'__384 = erased
-- Relation.Binary.Lattice.Bundles.Lattice._≤_
d__'8804'__386 :: T_Lattice_362 -> AgdaAny -> AgdaAny -> ()
d__'8804'__386 = erased
-- Relation.Binary.Lattice.Bundles.Lattice._∨_
d__'8744'__388 :: T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__388 v0
  = case coe v0 of
      C_Lattice'46'constructor_8383 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.Lattice._∧_
d__'8743'__390 :: T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__390 v0
  = case coe v0 of
      C_Lattice'46'constructor_8383 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.Lattice.isLattice
d_isLattice_392 ::
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
d_isLattice_392 v0
  = case coe v0 of
      C_Lattice'46'constructor_8383 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.Lattice._.antisym
d_antisym_396 ::
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_396 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe d_isLattice_392 (coe v0)))
-- Relation.Binary.Lattice.Bundles.Lattice._.infimum
d_infimum_398 ::
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_398 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_infimum_332
      (coe d_isLattice_392 (coe v0))
-- Relation.Binary.Lattice.Bundles.Lattice._.isEquivalence
d_isEquivalence_400 ::
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_400 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe d_isLattice_392 (coe v0))))
-- Relation.Binary.Lattice.Bundles.Lattice._.isJoinSemilattice
d_isJoinSemilattice_402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_isJoinSemilattice_402 ~v0 ~v1 ~v2 v3
  = du_isJoinSemilattice_402 v3
du_isJoinSemilattice_402 ::
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_isJoinSemilattice_402 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
      (coe d_isLattice_392 (coe v0))
-- Relation.Binary.Lattice.Bundles.Lattice._.isMeetSemilattice
d_isMeetSemilattice_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_isMeetSemilattice_404 ~v0 ~v1 ~v2 v3
  = du_isMeetSemilattice_404 v3
du_isMeetSemilattice_404 ::
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_isMeetSemilattice_404 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
      (coe d_isLattice_392 (coe v0))
-- Relation.Binary.Lattice.Bundles.Lattice._.isPartialOrder
d_isPartialOrder_406 ::
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_406 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
      (coe d_isLattice_392 (coe v0))
-- Relation.Binary.Lattice.Bundles.Lattice._.isPreorder
d_isPreorder_408 ::
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_408 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe d_isLattice_392 (coe v0)))
-- Relation.Binary.Lattice.Bundles.Lattice._.refl
d_refl_410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> AgdaAny -> AgdaAny
d_refl_410 ~v0 ~v1 ~v2 v3 = du_refl_410 v3
du_refl_410 :: T_Lattice_362 -> AgdaAny -> AgdaAny
du_refl_410 v0
  = let v1 = d_isLattice_392 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.Lattice._.reflexive
d_reflexive_412 ::
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_412 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe d_isLattice_392 (coe v0))))
-- Relation.Binary.Lattice.Bundles.Lattice._.supremum
d_supremum_414 ::
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_414 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_supremum_330
      (coe d_isLattice_392 (coe v0))
-- Relation.Binary.Lattice.Bundles.Lattice._.trans
d_trans_416 ::
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_416 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe d_isLattice_392 (coe v0))))
-- Relation.Binary.Lattice.Bundles.Lattice._.x∧y≤x
d_x'8743'y'8804'x_418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_418 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'x_418 v3
du_x'8743'y'8804'x_418 ::
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_418 v0
  = let v1 = d_isLattice_392 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'x_184
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v1))
-- Relation.Binary.Lattice.Bundles.Lattice._.x∧y≤y
d_x'8743'y'8804'y_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_420 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'y_420 v3
du_x'8743'y'8804'y_420 ::
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_420 v0
  = let v1 = d_isLattice_392 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'y_196
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v1))
-- Relation.Binary.Lattice.Bundles.Lattice._.x≤x∨y
d_x'8804'x'8744'y_422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_422 ~v0 ~v1 ~v2 v3 = du_x'8804'x'8744'y_422 v3
du_x'8804'x'8744'y_422 ::
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_422 v0
  = let v1 = d_isLattice_392 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8804'x'8744'y_38
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v1))
-- Relation.Binary.Lattice.Bundles.Lattice._.y≤x∨y
d_y'8804'x'8744'y_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_424 ~v0 ~v1 ~v2 v3 = du_y'8804'x'8744'y_424 v3
du_y'8804'x'8744'y_424 ::
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_424 v0
  = let v1 = d_isLattice_392 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_y'8804'x'8744'y_50
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v1))
-- Relation.Binary.Lattice.Bundles.Lattice._.∧-greatest
d_'8743''45'greatest_426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_426 ~v0 ~v1 ~v2 v3
  = du_'8743''45'greatest_426 v3
du_'8743''45'greatest_426 ::
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_426 v0
  = let v1 = d_isLattice_392 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8743''45'greatest_210
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v1))
-- Relation.Binary.Lattice.Bundles.Lattice._.∨-least
d_'8744''45'least_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_428 ~v0 ~v1 ~v2 v3 = du_'8744''45'least_428 v3
du_'8744''45'least_428 ::
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_428 v0
  = let v1 = d_isLattice_392 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8744''45'least_64
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v1))
-- Relation.Binary.Lattice.Bundles.Lattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_430 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_430 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_430 v3
du_'8764''45'resp'45''8776'_430 ::
  T_Lattice_362 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_430 v0
  = let v1 = d_isLattice_392 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.Lattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_432 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_432 v3
du_'8764''45'resp'691''45''8776'_432 ::
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_432 v0
  = let v1 = d_isLattice_392 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.Lattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_434 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_434 v3
du_'8764''45'resp'737''45''8776'_434 ::
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_434 v0
  = let v1 = d_isLattice_392 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Bundles.Lattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_438 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_438 v3
du_isPartialEquivalence_438 ::
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_438 v0
  = let v1 = d_isLattice_392 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v3))
-- Relation.Binary.Lattice.Bundles.Lattice._.Eq.refl
d_refl_440 :: T_Lattice_362 -> AgdaAny -> AgdaAny
d_refl_440 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe d_isLattice_392 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.Lattice._.Eq.reflexive
d_reflexive_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_442 ~v0 ~v1 ~v2 v3 = du_reflexive_442 v3
du_reflexive_442 ::
  T_Lattice_362 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_442 v0
  = let v1 = d_isLattice_392 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
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
-- Relation.Binary.Lattice.Bundles.Lattice._.Eq.sym
d_sym_444 ::
  T_Lattice_362 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_444 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe d_isLattice_392 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.Lattice._.Eq.trans
d_trans_446 ::
  T_Lattice_362 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_446 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe d_isLattice_392 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.Lattice.setoid
d_setoid_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_448 ~v0 ~v1 ~v2 v3 = du_setoid_448 v3
du_setoid_448 ::
  T_Lattice_362 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_448 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe d_isLattice_392 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.Lattice.joinSemilattice
d_joinSemilattice_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> T_JoinSemilattice_14
d_joinSemilattice_450 ~v0 ~v1 ~v2 v3 = du_joinSemilattice_450 v3
du_joinSemilattice_450 :: T_Lattice_362 -> T_JoinSemilattice_14
du_joinSemilattice_450 v0
  = coe
      C_JoinSemilattice'46'constructor_363 (d__'8744'__388 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe d_isLattice_392 (coe v0)))
-- Relation.Binary.Lattice.Bundles.Lattice.meetSemilattice
d_meetSemilattice_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> T_MeetSemilattice_188
d_meetSemilattice_452 ~v0 ~v1 ~v2 v3 = du_meetSemilattice_452 v3
du_meetSemilattice_452 :: T_Lattice_362 -> T_MeetSemilattice_188
du_meetSemilattice_452 v0
  = coe
      C_MeetSemilattice'46'constructor_4329 (d__'8743'__390 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe d_isLattice_392 (coe v0)))
-- Relation.Binary.Lattice.Bundles.Lattice._.poset
d_poset_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 -> MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_456 ~v0 ~v1 ~v2 v3 = du_poset_456 v3
du_poset_456 ::
  T_Lattice_362 -> MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_456 v0
  = coe du_poset_84 (coe du_joinSemilattice_450 (coe v0))
-- Relation.Binary.Lattice.Bundles.Lattice._.preorder
d_preorder_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_458 ~v0 ~v1 ~v2 v3 = du_preorder_458 v3
du_preorder_458 ::
  T_Lattice_362 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_458 v0
  = let v1 = coe du_joinSemilattice_450 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326
      (coe du_poset_84 (coe v1))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice
d_DistributiveLattice_466 a0 a1 a2 = ()
data T_DistributiveLattice_466
  = C_DistributiveLattice'46'constructor_11121 (AgdaAny ->
                                                AgdaAny -> AgdaAny)
                                               (AgdaAny -> AgdaAny -> AgdaAny)
                                               MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsDistributiveLattice_390
-- Relation.Binary.Lattice.Bundles.DistributiveLattice.Carrier
d_Carrier_486 :: T_DistributiveLattice_466 -> ()
d_Carrier_486 = erased
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._≈_
d__'8776'__488 ::
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> ()
d__'8776'__488 = erased
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._≤_
d__'8804'__490 ::
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> ()
d__'8804'__490 = erased
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._∨_
d__'8744'__492 ::
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__492 v0
  = case coe v0 of
      C_DistributiveLattice'46'constructor_11121 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._∧_
d__'8743'__494 ::
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__494 v0
  = case coe v0 of
      C_DistributiveLattice'46'constructor_11121 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.DistributiveLattice.isDistributiveLattice
d_isDistributiveLattice_496 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsDistributiveLattice_390
d_isDistributiveLattice_496 v0
  = case coe v0 of
      C_DistributiveLattice'46'constructor_11121 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_500 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8744'_500 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_'8743''45'distrib'737''45''8744'_402
      (coe d_isDistributiveLattice_496 (coe v0))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice.lattice
d_lattice_506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> T_Lattice_362
d_lattice_506 ~v0 ~v1 ~v2 v3 = du_lattice_506 v3
du_lattice_506 :: T_DistributiveLattice_466 -> T_Lattice_362
du_lattice_506 v0
  = coe
      C_Lattice'46'constructor_8383 (d__'8744'__492 (coe v0))
      (d__'8743'__494 (coe v0))
      (MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
         (coe d_isDistributiveLattice_496 (coe v0)))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.antisym
d_antisym_510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_510 ~v0 ~v1 ~v2 v3 = du_antisym_510 v3
du_antisym_510 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antisym_510 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
            (coe d_isDistributiveLattice_496 (coe v0))))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.infimum
d_infimum_512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_512 ~v0 ~v1 ~v2 v3 = du_infimum_512 v3
du_infimum_512 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_infimum_512 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_infimum_332
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
         (coe d_isDistributiveLattice_496 (coe v0)))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.isEquivalence
d_isEquivalence_514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_514 ~v0 ~v1 ~v2 v3 = du_isEquivalence_514 v3
du_isEquivalence_514 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_514 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
               (coe d_isDistributiveLattice_496 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.isJoinSemilattice
d_isJoinSemilattice_516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_isJoinSemilattice_516 ~v0 ~v1 ~v2 v3
  = du_isJoinSemilattice_516 v3
du_isJoinSemilattice_516 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_isJoinSemilattice_516 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
      (coe d_isLattice_392 (coe v1))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.isLattice
d_isLattice_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
d_isLattice_518 ~v0 ~v1 ~v2 v3 = du_isLattice_518 v3
du_isLattice_518 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
du_isLattice_518 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
      (coe d_isDistributiveLattice_496 (coe v0))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.isMeetSemilattice
d_isMeetSemilattice_520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_isMeetSemilattice_520 ~v0 ~v1 ~v2 v3
  = du_isMeetSemilattice_520 v3
du_isMeetSemilattice_520 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_isMeetSemilattice_520 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
      (coe d_isLattice_392 (coe v1))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.isPartialOrder
d_isPartialOrder_522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_522 ~v0 ~v1 ~v2 v3 = du_isPartialOrder_522 v3
du_isPartialOrder_522 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_isPartialOrder_522 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
         (coe d_isDistributiveLattice_496 (coe v0)))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.isPreorder
d_isPreorder_524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_524 ~v0 ~v1 ~v2 v3 = du_isPreorder_524 v3
du_isPreorder_524 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_isPreorder_524 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
            (coe d_isDistributiveLattice_496 (coe v0))))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.joinSemilattice
d_joinSemilattice_526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> T_JoinSemilattice_14
d_joinSemilattice_526 ~v0 ~v1 ~v2 v3 = du_joinSemilattice_526 v3
du_joinSemilattice_526 ::
  T_DistributiveLattice_466 -> T_JoinSemilattice_14
du_joinSemilattice_526 v0
  = coe du_joinSemilattice_450 (coe du_lattice_506 (coe v0))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.meetSemilattice
d_meetSemilattice_528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> T_MeetSemilattice_188
d_meetSemilattice_528 ~v0 ~v1 ~v2 v3 = du_meetSemilattice_528 v3
du_meetSemilattice_528 ::
  T_DistributiveLattice_466 -> T_MeetSemilattice_188
du_meetSemilattice_528 v0
  = coe du_meetSemilattice_452 (coe du_lattice_506 (coe v0))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.poset
d_poset_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_530 ~v0 ~v1 ~v2 v3 = du_poset_530 v3
du_poset_530 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_530 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    coe du_poset_84 (coe du_joinSemilattice_450 (coe v1))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.preorder
d_preorder_532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_532 ~v0 ~v1 ~v2 v3 = du_preorder_532 v3
du_preorder_532 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_532 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = coe du_joinSemilattice_450 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326
      (coe du_poset_84 (coe v2))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.refl
d_refl_534 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny
d_refl_534 ~v0 ~v1 ~v2 v3 = du_refl_534 v3
du_refl_534 :: T_DistributiveLattice_466 -> AgdaAny -> AgdaAny
du_refl_534 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.reflexive
d_reflexive_536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_536 ~v0 ~v1 ~v2 v3 = du_reflexive_536 v3
du_reflexive_536 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_reflexive_536 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
               (coe d_isDistributiveLattice_496 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.setoid
d_setoid_538 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_538 ~v0 ~v1 ~v2 v3 = du_setoid_538 v3
du_setoid_538 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_538 v0 = coe du_setoid_448 (coe du_lattice_506 (coe v0))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.supremum
d_supremum_540 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_540 ~v0 ~v1 ~v2 v3 = du_supremum_540 v3
du_supremum_540 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_supremum_540 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_supremum_330
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
         (coe d_isDistributiveLattice_496 (coe v0)))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.trans
d_trans_542 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_542 ~v0 ~v1 ~v2 v3 = du_trans_542 v3
du_trans_542 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_542 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
               (coe d_isDistributiveLattice_496 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.x∧y≤x
d_x'8743'y'8804'x_544 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_544 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'x_544 v3
du_x'8743'y'8804'x_544 ::
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_544 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'x_184
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v2))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.x∧y≤y
d_x'8743'y'8804'y_546 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_546 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'y_546 v3
du_x'8743'y'8804'y_546 ::
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_546 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'y_196
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v2))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.x≤x∨y
d_x'8804'x'8744'y_548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_548 ~v0 ~v1 ~v2 v3 = du_x'8804'x'8744'y_548 v3
du_x'8804'x'8744'y_548 ::
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_548 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8804'x'8744'y_38
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v2))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.y≤x∨y
d_y'8804'x'8744'y_550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_550 ~v0 ~v1 ~v2 v3 = du_y'8804'x'8744'y_550 v3
du_y'8804'x'8744'y_550 ::
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_550 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_y'8804'x'8744'y_50
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v2))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.∧-greatest
d_'8743''45'greatest_552 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_552 ~v0 ~v1 ~v2 v3
  = du_'8743''45'greatest_552 v3
du_'8743''45'greatest_552 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_552 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8743''45'greatest_210
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v2))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.∨-least
d_'8744''45'least_554 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_554 ~v0 ~v1 ~v2 v3 = du_'8744''45'least_554 v3
du_'8744''45'least_554 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_554 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8744''45'least_64
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v2))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_556 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_556 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_556 v3
du_'8764''45'resp'45''8776'_556 ::
  T_DistributiveLattice_466 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_556 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_558 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_558 v3
du_'8764''45'resp'691''45''8776'_558 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_558 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_560 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_560 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_560 v3
du_'8764''45'resp'737''45''8776'_560 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_560 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_564 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_564 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_564 v3
du_isPartialEquivalence_564 ::
  T_DistributiveLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_564 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v4))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.Eq.refl
d_refl_566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 -> AgdaAny -> AgdaAny
d_refl_566 ~v0 ~v1 ~v2 v3 = du_refl_566 v3
du_refl_566 :: T_DistributiveLattice_466 -> AgdaAny -> AgdaAny
du_refl_566 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
                  (coe d_isDistributiveLattice_496 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.Eq.reflexive
d_reflexive_568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_568 ~v0 ~v1 ~v2 v3 = du_reflexive_568 v3
du_reflexive_568 ::
  T_DistributiveLattice_466 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_568 v0
  = let v1 = coe du_lattice_506 (coe v0) in
    let v2 = d_isLattice_392 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
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
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.Eq.sym
d_sym_570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_570 ~v0 ~v1 ~v2 v3 = du_sym_570 v3
du_sym_570 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_570 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
                  (coe d_isDistributiveLattice_496 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.DistributiveLattice._.Eq.trans
d_trans_572 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_572 ~v0 ~v1 ~v2 v3 = du_trans_572 v3
du_trans_572 ::
  T_DistributiveLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_572 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_400
                  (coe d_isDistributiveLattice_496 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedLattice
d_BoundedLattice_580 a0 a1 a2 = ()
data T_BoundedLattice_580
  = C_BoundedLattice'46'constructor_14009 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
                                          MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedLattice_466
-- Relation.Binary.Lattice.Bundles.BoundedLattice.Carrier
d_Carrier_604 :: T_BoundedLattice_580 -> ()
d_Carrier_604 = erased
-- Relation.Binary.Lattice.Bundles.BoundedLattice._≈_
d__'8776'__606 :: T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> ()
d__'8776'__606 = erased
-- Relation.Binary.Lattice.Bundles.BoundedLattice._≤_
d__'8804'__608 :: T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> ()
d__'8804'__608 = erased
-- Relation.Binary.Lattice.Bundles.BoundedLattice._∨_
d__'8744'__610 ::
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__610 v0
  = case coe v0 of
      C_BoundedLattice'46'constructor_14009 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedLattice._∧_
d__'8743'__612 ::
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__612 v0
  = case coe v0 of
      C_BoundedLattice'46'constructor_14009 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedLattice.⊤
d_'8868'_614 :: T_BoundedLattice_580 -> AgdaAny
d_'8868'_614 v0
  = case coe v0 of
      C_BoundedLattice'46'constructor_14009 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedLattice.⊥
d_'8869'_616 :: T_BoundedLattice_580 -> AgdaAny
d_'8869'_616 v0
  = case coe v0 of
      C_BoundedLattice'46'constructor_14009 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedLattice.isBoundedLattice
d_isBoundedLattice_618 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedLattice_466
d_isBoundedLattice_618 v0
  = case coe v0 of
      C_BoundedLattice'46'constructor_14009 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.antisym
d_antisym_622 ::
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_622 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
            (coe d_isBoundedLattice_618 (coe v0))))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.infimum
d_infimum_624 ::
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_624 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_infimum_332
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe d_isBoundedLattice_618 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.isBoundedJoinSemilattice
d_isBoundedJoinSemilattice_626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
d_isBoundedJoinSemilattice_626 ~v0 ~v1 ~v2 v3
  = du_isBoundedJoinSemilattice_626 v3
du_isBoundedJoinSemilattice_626 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
du_isBoundedJoinSemilattice_626 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isBoundedJoinSemilattice_542
      (coe d_isBoundedLattice_618 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.isBoundedMeetSemilattice
d_isBoundedMeetSemilattice_628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedMeetSemilattice_256
d_isBoundedMeetSemilattice_628 ~v0 ~v1 ~v2 v3
  = du_isBoundedMeetSemilattice_628 v3
du_isBoundedMeetSemilattice_628 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedMeetSemilattice_256
du_isBoundedMeetSemilattice_628 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isBoundedMeetSemilattice_544
      (coe d_isBoundedLattice_618 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.isEquivalence
d_isEquivalence_630 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_630 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
               (coe d_isBoundedLattice_618 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.isJoinSemilattice
d_isJoinSemilattice_632 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_isJoinSemilattice_632 ~v0 ~v1 ~v2 v3
  = du_isJoinSemilattice_632 v3
du_isJoinSemilattice_632 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_isJoinSemilattice_632 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe v1))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.isLattice
d_isLattice_634 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
d_isLattice_634 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
      (coe d_isBoundedLattice_618 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.isMeetSemilattice
d_isMeetSemilattice_636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_isMeetSemilattice_636 ~v0 ~v1 ~v2 v3
  = du_isMeetSemilattice_636 v3
du_isMeetSemilattice_636 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_isMeetSemilattice_636 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe v1))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.isPartialOrder
d_isPartialOrder_638 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_638 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe d_isBoundedLattice_618 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.isPreorder
d_isPreorder_640 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_640 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
            (coe d_isBoundedLattice_618 (coe v0))))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.maximum
d_maximum_642 :: T_BoundedLattice_580 -> AgdaAny -> AgdaAny
d_maximum_642 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_maximum_484
      (coe d_isBoundedLattice_618 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.minimum
d_minimum_644 :: T_BoundedLattice_580 -> AgdaAny -> AgdaAny
d_minimum_644 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_minimum_486
      (coe d_isBoundedLattice_618 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.refl
d_refl_646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny
d_refl_646 ~v0 ~v1 ~v2 v3 = du_refl_646 v3
du_refl_646 :: T_BoundedLattice_580 -> AgdaAny -> AgdaAny
du_refl_646 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.reflexive
d_reflexive_648 ::
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_648 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
               (coe d_isBoundedLattice_618 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.supremum
d_supremum_650 ::
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_650 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_supremum_330
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe d_isBoundedLattice_618 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.trans
d_trans_652 ::
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_652 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
               (coe d_isBoundedLattice_618 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.x∧y≤x
d_x'8743'y'8804'x_654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_654 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'x_654 v3
du_x'8743'y'8804'x_654 ::
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_654 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'x_184
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v2))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.x∧y≤y
d_x'8743'y'8804'y_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_656 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'y_656 v3
du_x'8743'y'8804'y_656 ::
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_656 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'y_196
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v2))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.x≤x∨y
d_x'8804'x'8744'y_658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_658 ~v0 ~v1 ~v2 v3 = du_x'8804'x'8744'y_658 v3
du_x'8804'x'8744'y_658 ::
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_658 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8804'x'8744'y_38
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v2))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.y≤x∨y
d_y'8804'x'8744'y_660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_660 ~v0 ~v1 ~v2 v3 = du_y'8804'x'8744'y_660 v3
du_y'8804'x'8744'y_660 ::
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_660 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_y'8804'x'8744'y_50
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v2))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.∧-greatest
d_'8743''45'greatest_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_662 ~v0 ~v1 ~v2 v3
  = du_'8743''45'greatest_662 v3
du_'8743''45'greatest_662 ::
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_662 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8743''45'greatest_210
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v2))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.∨-least
d_'8744''45'least_664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_664 ~v0 ~v1 ~v2 v3 = du_'8744''45'least_664 v3
du_'8744''45'least_664 ::
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_664 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8744''45'least_64
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v2))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_666 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_666 v3
du_'8764''45'resp'45''8776'_666 ::
  T_BoundedLattice_580 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_666 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_668 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_668 v3
du_'8764''45'resp'691''45''8776'_668 ::
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_668 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_670 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_670 v3
du_'8764''45'resp'737''45''8776'_670 ::
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_670 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_674 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_674 v3
du_isPartialEquivalence_674 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_674 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v4))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.Eq.refl
d_refl_676 :: T_BoundedLattice_580 -> AgdaAny -> AgdaAny
d_refl_676 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
                  (coe d_isBoundedLattice_618 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.Eq.reflexive
d_reflexive_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_678 ~v0 ~v1 ~v2 v3 = du_reflexive_678 v3
du_reflexive_678 ::
  T_BoundedLattice_580 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_678 v0
  = let v1 = d_isBoundedLattice_618 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
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
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.Eq.sym
d_sym_680 ::
  T_BoundedLattice_580 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_680 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
                  (coe d_isBoundedLattice_618 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.Eq.trans
d_trans_682 ::
  T_BoundedLattice_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_682 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
                  (coe d_isBoundedLattice_618 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.BoundedLattice.boundedJoinSemilattice
d_boundedJoinSemilattice_684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> T_BoundedJoinSemilattice_96
d_boundedJoinSemilattice_684 ~v0 ~v1 ~v2 v3
  = du_boundedJoinSemilattice_684 v3
du_boundedJoinSemilattice_684 ::
  T_BoundedLattice_580 -> T_BoundedJoinSemilattice_96
du_boundedJoinSemilattice_684 v0
  = coe
      C_BoundedJoinSemilattice'46'constructor_2251
      (d__'8744'__610 (coe v0)) (d_'8869'_616 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isBoundedJoinSemilattice_542
         (coe d_isBoundedLattice_618 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedLattice.boundedMeetSemilattice
d_boundedMeetSemilattice_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> T_BoundedMeetSemilattice_270
d_boundedMeetSemilattice_686 ~v0 ~v1 ~v2 v3
  = du_boundedMeetSemilattice_686 v3
du_boundedMeetSemilattice_686 ::
  T_BoundedLattice_580 -> T_BoundedMeetSemilattice_270
du_boundedMeetSemilattice_686 v0
  = coe
      C_BoundedMeetSemilattice'46'constructor_6217
      (d__'8743'__612 (coe v0)) (d_'8868'_614 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isBoundedMeetSemilattice_544
         (coe d_isBoundedLattice_618 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedLattice.lattice
d_lattice_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> T_Lattice_362
d_lattice_688 ~v0 ~v1 ~v2 v3 = du_lattice_688 v3
du_lattice_688 :: T_BoundedLattice_580 -> T_Lattice_362
du_lattice_688 v0
  = coe
      C_Lattice'46'constructor_8383 (d__'8744'__610 (coe v0))
      (d__'8743'__612 (coe v0))
      (MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe d_isBoundedLattice_618 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.joinSemilattice
d_joinSemilattice_692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> T_JoinSemilattice_14
d_joinSemilattice_692 ~v0 ~v1 ~v2 v3 = du_joinSemilattice_692 v3
du_joinSemilattice_692 ::
  T_BoundedLattice_580 -> T_JoinSemilattice_14
du_joinSemilattice_692 v0
  = coe du_joinSemilattice_450 (coe du_lattice_688 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.meetSemilattice
d_meetSemilattice_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 -> T_MeetSemilattice_188
d_meetSemilattice_694 ~v0 ~v1 ~v2 v3 = du_meetSemilattice_694 v3
du_meetSemilattice_694 ::
  T_BoundedLattice_580 -> T_MeetSemilattice_188
du_meetSemilattice_694 v0
  = coe du_meetSemilattice_452 (coe du_lattice_688 (coe v0))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.poset
d_poset_696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_696 ~v0 ~v1 ~v2 v3 = du_poset_696 v3
du_poset_696 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_696 v0
  = let v1 = coe du_lattice_688 (coe v0) in
    coe du_poset_84 (coe du_joinSemilattice_450 (coe v1))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.preorder
d_preorder_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_698 ~v0 ~v1 ~v2 v3 = du_preorder_698 v3
du_preorder_698 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_698 v0
  = let v1 = coe du_lattice_688 (coe v0) in
    let v2 = coe du_joinSemilattice_450 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326
      (coe du_poset_84 (coe v2))
-- Relation.Binary.Lattice.Bundles.BoundedLattice._.setoid
d_setoid_700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_700 ~v0 ~v1 ~v2 v3 = du_setoid_700 v3
du_setoid_700 ::
  T_BoundedLattice_580 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_700 v0 = coe du_setoid_448 (coe du_lattice_688 (coe v0))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra
d_HeytingAlgebra_708 a0 a1 a2 = ()
data T_HeytingAlgebra_708
  = C_HeytingAlgebra'46'constructor_17569 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
                                          MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsHeytingAlgebra_556
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra.Carrier
d_Carrier_734 :: T_HeytingAlgebra_708 -> ()
d_Carrier_734 = erased
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._≈_
d__'8776'__736 :: T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> ()
d__'8776'__736 = erased
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._≤_
d__'8804'__738 :: T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> ()
d__'8804'__738 = erased
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._∨_
d__'8744'__740 ::
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__740 v0
  = case coe v0 of
      C_HeytingAlgebra'46'constructor_17569 v4 v5 v6 v7 v8 v9 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._∧_
d__'8743'__742 ::
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__742 v0
  = case coe v0 of
      C_HeytingAlgebra'46'constructor_17569 v4 v5 v6 v7 v8 v9 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._⇨_
d__'8680'__744 ::
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8680'__744 v0
  = case coe v0 of
      C_HeytingAlgebra'46'constructor_17569 v4 v5 v6 v7 v8 v9 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra.⊤
d_'8868'_746 :: T_HeytingAlgebra_708 -> AgdaAny
d_'8868'_746 v0
  = case coe v0 of
      C_HeytingAlgebra'46'constructor_17569 v4 v5 v6 v7 v8 v9 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra.⊥
d_'8869'_748 :: T_HeytingAlgebra_708 -> AgdaAny
d_'8869'_748 v0
  = case coe v0 of
      C_HeytingAlgebra'46'constructor_17569 v4 v5 v6 v7 v8 v9 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra.isHeytingAlgebra
d_isHeytingAlgebra_750 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsHeytingAlgebra_556
d_isHeytingAlgebra_750 v0
  = case coe v0 of
      C_HeytingAlgebra'46'constructor_17569 v4 v5 v6 v7 v8 v9 -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra.boundedLattice
d_boundedLattice_752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> T_BoundedLattice_580
d_boundedLattice_752 ~v0 ~v1 ~v2 v3 = du_boundedLattice_752 v3
du_boundedLattice_752 ::
  T_HeytingAlgebra_708 -> T_BoundedLattice_580
du_boundedLattice_752 v0
  = coe
      C_BoundedLattice'46'constructor_14009 (d__'8744'__740 (coe v0))
      (d__'8743'__742 (coe v0)) (d_'8868'_746 (coe v0))
      (d_'8869'_748 (coe v0))
      (MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
         (coe d_isHeytingAlgebra_750 (coe v0)))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.exponential
d_exponential_756 ::
  T_HeytingAlgebra_708 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_exponential_756 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_exponential_574
      (coe d_isHeytingAlgebra_750 (coe v0))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.transpose-⇨
d_transpose'45''8680'_758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_transpose'45''8680'_758 ~v0 ~v1 ~v2 v3
  = du_transpose'45''8680'_758 v3
du_transpose'45''8680'_758 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_transpose'45''8680'_758 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_transpose'45''8680'_582
      (coe d_isHeytingAlgebra_750 (coe v0))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.transpose-∧
d_transpose'45''8743'_760 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_transpose'45''8743'_760 ~v0 ~v1 ~v2 v3
  = du_transpose'45''8743'_760 v3
du_transpose'45''8743'_760 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_transpose'45''8743'_760 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_transpose'45''8743'_598
      (coe d_isHeytingAlgebra_750 (coe v0))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.antisym
d_antisym_764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_764 ~v0 ~v1 ~v2 v3 = du_antisym_764 v3
du_antisym_764 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antisym_764 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
               (coe d_isHeytingAlgebra_750 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.boundedJoinSemilattice
d_boundedJoinSemilattice_766 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> T_BoundedJoinSemilattice_96
d_boundedJoinSemilattice_766 ~v0 ~v1 ~v2 v3
  = du_boundedJoinSemilattice_766 v3
du_boundedJoinSemilattice_766 ::
  T_HeytingAlgebra_708 -> T_BoundedJoinSemilattice_96
du_boundedJoinSemilattice_766 v0
  = coe
      du_boundedJoinSemilattice_684 (coe du_boundedLattice_752 (coe v0))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.boundedMeetSemilattice
d_boundedMeetSemilattice_768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> T_BoundedMeetSemilattice_270
d_boundedMeetSemilattice_768 ~v0 ~v1 ~v2 v3
  = du_boundedMeetSemilattice_768 v3
du_boundedMeetSemilattice_768 ::
  T_HeytingAlgebra_708 -> T_BoundedMeetSemilattice_270
du_boundedMeetSemilattice_768 v0
  = coe
      du_boundedMeetSemilattice_686 (coe du_boundedLattice_752 (coe v0))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.infimum
d_infimum_770 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_770 ~v0 ~v1 ~v2 v3 = du_infimum_770 v3
du_infimum_770 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_infimum_770 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_infimum_332
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
            (coe d_isHeytingAlgebra_750 (coe v0))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.isBoundedJoinSemilattice
d_isBoundedJoinSemilattice_772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
d_isBoundedJoinSemilattice_772 ~v0 ~v1 ~v2 v3
  = du_isBoundedJoinSemilattice_772 v3
du_isBoundedJoinSemilattice_772 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
du_isBoundedJoinSemilattice_772 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isBoundedJoinSemilattice_542
      (coe d_isBoundedLattice_618 (coe v1))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.isBoundedLattice
d_isBoundedLattice_774 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedLattice_466
d_isBoundedLattice_774 ~v0 ~v1 ~v2 v3 = du_isBoundedLattice_774 v3
du_isBoundedLattice_774 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedLattice_466
du_isBoundedLattice_774 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
      (coe d_isHeytingAlgebra_750 (coe v0))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.isBoundedMeetSemilattice
d_isBoundedMeetSemilattice_776 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedMeetSemilattice_256
d_isBoundedMeetSemilattice_776 ~v0 ~v1 ~v2 v3
  = du_isBoundedMeetSemilattice_776 v3
du_isBoundedMeetSemilattice_776 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedMeetSemilattice_256
du_isBoundedMeetSemilattice_776 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isBoundedMeetSemilattice_544
      (coe d_isBoundedLattice_618 (coe v1))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.isEquivalence
d_isEquivalence_778 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_778 ~v0 ~v1 ~v2 v3 = du_isEquivalence_778 v3
du_isEquivalence_778 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_778 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                  (coe d_isHeytingAlgebra_750 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.isJoinSemilattice
d_isJoinSemilattice_780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_isJoinSemilattice_780 ~v0 ~v1 ~v2 v3
  = du_isJoinSemilattice_780 v3
du_isJoinSemilattice_780 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_isJoinSemilattice_780 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe v2))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.isLattice
d_isLattice_782 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
d_isLattice_782 ~v0 ~v1 ~v2 v3 = du_isLattice_782 v3
du_isLattice_782 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
du_isLattice_782 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
         (coe d_isHeytingAlgebra_750 (coe v0)))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.isMeetSemilattice
d_isMeetSemilattice_784 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_isMeetSemilattice_784 ~v0 ~v1 ~v2 v3
  = du_isMeetSemilattice_784 v3
du_isMeetSemilattice_784 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_isMeetSemilattice_784 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe v2))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.isPartialOrder
d_isPartialOrder_786 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_786 ~v0 ~v1 ~v2 v3 = du_isPartialOrder_786 v3
du_isPartialOrder_786 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_isPartialOrder_786 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
            (coe d_isHeytingAlgebra_750 (coe v0))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.isPreorder
d_isPreorder_788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_788 ~v0 ~v1 ~v2 v3 = du_isPreorder_788 v3
du_isPreorder_788 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_isPreorder_788 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
               (coe d_isHeytingAlgebra_750 (coe v0)))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.joinSemilattice
d_joinSemilattice_790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> T_JoinSemilattice_14
d_joinSemilattice_790 ~v0 ~v1 ~v2 v3 = du_joinSemilattice_790 v3
du_joinSemilattice_790 ::
  T_HeytingAlgebra_708 -> T_JoinSemilattice_14
du_joinSemilattice_790 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    coe du_joinSemilattice_450 (coe du_lattice_688 (coe v1))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.lattice
d_lattice_792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> T_Lattice_362
d_lattice_792 ~v0 ~v1 ~v2 v3 = du_lattice_792 v3
du_lattice_792 :: T_HeytingAlgebra_708 -> T_Lattice_362
du_lattice_792 v0
  = coe du_lattice_688 (coe du_boundedLattice_752 (coe v0))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.maximum
d_maximum_794 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny
d_maximum_794 ~v0 ~v1 ~v2 v3 = du_maximum_794 v3
du_maximum_794 :: T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny
du_maximum_794 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_maximum_484
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
         (coe d_isHeytingAlgebra_750 (coe v0)))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.meetSemilattice
d_meetSemilattice_796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> T_MeetSemilattice_188
d_meetSemilattice_796 ~v0 ~v1 ~v2 v3 = du_meetSemilattice_796 v3
du_meetSemilattice_796 ::
  T_HeytingAlgebra_708 -> T_MeetSemilattice_188
du_meetSemilattice_796 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    coe du_meetSemilattice_452 (coe du_lattice_688 (coe v1))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.minimum
d_minimum_798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny
d_minimum_798 ~v0 ~v1 ~v2 v3 = du_minimum_798 v3
du_minimum_798 :: T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny
du_minimum_798 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_minimum_486
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
         (coe d_isHeytingAlgebra_750 (coe v0)))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.poset
d_poset_800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_800 ~v0 ~v1 ~v2 v3 = du_poset_800 v3
du_poset_800 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_800 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = coe du_lattice_688 (coe v1) in
    coe du_poset_84 (coe du_joinSemilattice_450 (coe v2))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.preorder
d_preorder_802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_802 ~v0 ~v1 ~v2 v3 = du_preorder_802 v3
du_preorder_802 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_802 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = coe du_lattice_688 (coe v1) in
    let v3 = coe du_joinSemilattice_450 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326
      (coe du_poset_84 (coe v3))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.refl
d_refl_804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny
d_refl_804 ~v0 ~v1 ~v2 v3 = du_refl_804 v3
du_refl_804 :: T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny
du_refl_804 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.reflexive
d_reflexive_806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_806 ~v0 ~v1 ~v2 v3 = du_reflexive_806 v3
du_reflexive_806 ::
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_reflexive_806 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                  (coe d_isHeytingAlgebra_750 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.setoid
d_setoid_808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_808 ~v0 ~v1 ~v2 v3 = du_setoid_808 v3
du_setoid_808 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_808 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    coe du_setoid_448 (coe du_lattice_688 (coe v1))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.supremum
d_supremum_810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_810 ~v0 ~v1 ~v2 v3 = du_supremum_810 v3
du_supremum_810 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_supremum_810 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_supremum_330
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
            (coe d_isHeytingAlgebra_750 (coe v0))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.trans
d_trans_812 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_812 ~v0 ~v1 ~v2 v3 = du_trans_812 v3
du_trans_812 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_812 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                  (coe d_isHeytingAlgebra_750 (coe v0))))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.x∧y≤x
d_x'8743'y'8804'x_814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_814 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'x_814 v3
du_x'8743'y'8804'x_814 ::
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_814 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'x_184
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v3))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.x∧y≤y
d_x'8743'y'8804'y_816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_816 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'y_816 v3
du_x'8743'y'8804'y_816 ::
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_816 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'y_196
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v3))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.x≤x∨y
d_x'8804'x'8744'y_818 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_818 ~v0 ~v1 ~v2 v3 = du_x'8804'x'8744'y_818 v3
du_x'8804'x'8744'y_818 ::
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_818 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8804'x'8744'y_38
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v3))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.y≤x∨y
d_y'8804'x'8744'y_820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_820 ~v0 ~v1 ~v2 v3 = du_y'8804'x'8744'y_820 v3
du_y'8804'x'8744'y_820 ::
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_820 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_y'8804'x'8744'y_50
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v3))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.∧-greatest
d_'8743''45'greatest_822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_822 ~v0 ~v1 ~v2 v3
  = du_'8743''45'greatest_822 v3
du_'8743''45'greatest_822 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_822 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8743''45'greatest_210
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v3))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.∨-least
d_'8744''45'least_824 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_824 ~v0 ~v1 ~v2 v3 = du_'8744''45'least_824 v3
du_'8744''45'least_824 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_824 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8744''45'least_64
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v3))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.∼-resp-≈
d_'8764''45'resp'45''8776'_826 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_826 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_826 v3
du_'8764''45'resp'45''8776'_826 ::
  T_HeytingAlgebra_708 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_826 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_828 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_828 v3
du_'8764''45'resp'691''45''8776'_828 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_828 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_830 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_830 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_830 v3
du_'8764''45'resp'737''45''8776'_830 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_830 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.Eq.isPartialEquivalence
d_isPartialEquivalence_834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_834 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_834 v3
du_isPartialEquivalence_834 ::
  T_HeytingAlgebra_708 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_834 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v5))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.Eq.refl
d_refl_836 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny
d_refl_836 ~v0 ~v1 ~v2 v3 = du_refl_836 v3
du_refl_836 :: T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny
du_refl_836 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
                  (coe
                     MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                     (coe d_isHeytingAlgebra_750 (coe v0)))))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.Eq.reflexive
d_reflexive_838 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_838 ~v0 ~v1 ~v2 v3 = du_reflexive_838 v3
du_reflexive_838 ::
  T_HeytingAlgebra_708 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_838 v0
  = let v1 = coe du_boundedLattice_752 (coe v0) in
    let v2 = d_isBoundedLattice_618 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
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
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.Eq.sym
d_sym_840 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_840 ~v0 ~v1 ~v2 v3 = du_sym_840 v3
du_sym_840 ::
  T_HeytingAlgebra_708 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_840 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
                  (coe
                     MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                     (coe d_isHeytingAlgebra_750 (coe v0)))))))
-- Relation.Binary.Lattice.Bundles.HeytingAlgebra._.Eq.trans
d_trans_842 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_842 ~v0 ~v1 ~v2 v3 = du_trans_842 v3
du_trans_842 ::
  T_HeytingAlgebra_708 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_842 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
                  (coe
                     MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                     (coe d_isHeytingAlgebra_750 (coe v0)))))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra
d_BooleanAlgebra_850 a0 a1 a2 = ()
data T_BooleanAlgebra_850
  = C_BooleanAlgebra'46'constructor_21397 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                          AgdaAny AgdaAny
                                          MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBooleanAlgebra_682
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra.Carrier
d_Carrier_876 :: T_BooleanAlgebra_850 -> ()
d_Carrier_876 = erased
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._≈_
d__'8776'__878 :: T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> ()
d__'8776'__878 = erased
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._≤_
d__'8804'__880 :: T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> ()
d__'8804'__880 = erased
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._∨_
d__'8744'__882 ::
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__882 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_21397 v4 v5 v6 v7 v8 v9 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._∧_
d__'8743'__884 ::
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__884 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_21397 v4 v5 v6 v7 v8 v9 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra.¬_
d_'172'__886 :: T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny
d_'172'__886 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_21397 v4 v5 v6 v7 v8 v9 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra.⊤
d_'8868'_888 :: T_BooleanAlgebra_850 -> AgdaAny
d_'8868'_888 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_21397 v4 v5 v6 v7 v8 v9 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra.⊥
d_'8869'_890 :: T_BooleanAlgebra_850 -> AgdaAny
d_'8869'_890 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_21397 v4 v5 v6 v7 v8 v9 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra.isBooleanAlgebra
d_isBooleanAlgebra_892 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBooleanAlgebra_682
d_isBooleanAlgebra_892 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_21397 v4 v5 v6 v7 v8 v9 -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra.heytingAlgebra
d_heytingAlgebra_898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> T_HeytingAlgebra_708
d_heytingAlgebra_898 ~v0 ~v1 ~v2 v3 = du_heytingAlgebra_898 v3
du_heytingAlgebra_898 ::
  T_BooleanAlgebra_850 -> T_HeytingAlgebra_708
du_heytingAlgebra_898 v0
  = coe
      C_HeytingAlgebra'46'constructor_17569 (d__'8744'__882 (coe v0))
      (d__'8743'__884 (coe v0))
      (\ v1 -> coe d__'8744'__882 v0 (coe d_'172'__886 v0 v1))
      (d_'8868'_888 (coe v0)) (d_'8869'_890 (coe v0))
      (MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isHeytingAlgebra_708
         (coe d_isBooleanAlgebra_892 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._._⇨_
d__'8680'__902 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8680'__902 ~v0 ~v1 ~v2 v3 v4 = du__'8680'__902 v3 v4
du__'8680'__902 ::
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
du__'8680'__902 v0 v1
  = coe d__'8744'__882 v0 (coe d_'172'__886 v0 v1)
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.antisym
d_antisym_904 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_904 ~v0 ~v1 ~v2 v3 = du_antisym_904 v3
du_antisym_904 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antisym_904 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
               (coe d_isHeytingAlgebra_750 (coe v1)))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.boundedJoinSemilattice
d_boundedJoinSemilattice_906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> T_BoundedJoinSemilattice_96
d_boundedJoinSemilattice_906 ~v0 ~v1 ~v2 v3
  = du_boundedJoinSemilattice_906 v3
du_boundedJoinSemilattice_906 ::
  T_BooleanAlgebra_850 -> T_BoundedJoinSemilattice_96
du_boundedJoinSemilattice_906 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      du_boundedJoinSemilattice_684 (coe du_boundedLattice_752 (coe v1))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.boundedLattice
d_boundedLattice_908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> T_BoundedLattice_580
d_boundedLattice_908 ~v0 ~v1 ~v2 v3 = du_boundedLattice_908 v3
du_boundedLattice_908 ::
  T_BooleanAlgebra_850 -> T_BoundedLattice_580
du_boundedLattice_908 v0
  = coe du_boundedLattice_752 (coe du_heytingAlgebra_898 (coe v0))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.boundedMeetSemilattice
d_boundedMeetSemilattice_910 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> T_BoundedMeetSemilattice_270
d_boundedMeetSemilattice_910 ~v0 ~v1 ~v2 v3
  = du_boundedMeetSemilattice_910 v3
du_boundedMeetSemilattice_910 ::
  T_BooleanAlgebra_850 -> T_BoundedMeetSemilattice_270
du_boundedMeetSemilattice_910 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      du_boundedMeetSemilattice_686 (coe du_boundedLattice_752 (coe v1))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.exponential
d_exponential_912 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_exponential_912 ~v0 ~v1 ~v2 v3 = du_exponential_912 v3
du_exponential_912 ::
  T_BooleanAlgebra_850 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_exponential_912 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_exponential_574
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isHeytingAlgebra_708
         (coe d_isBooleanAlgebra_892 (coe v0)))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.infimum
d_infimum_914 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_914 ~v0 ~v1 ~v2 v3 = du_infimum_914 v3
du_infimum_914 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_infimum_914 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_infimum_332
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
            (coe d_isHeytingAlgebra_750 (coe v1))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isBoundedJoinSemilattice
d_isBoundedJoinSemilattice_916 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
d_isBoundedJoinSemilattice_916 ~v0 ~v1 ~v2 v3
  = du_isBoundedJoinSemilattice_916 v3
du_isBoundedJoinSemilattice_916 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
du_isBoundedJoinSemilattice_916 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isBoundedJoinSemilattice_542
      (coe d_isBoundedLattice_618 (coe v2))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isBoundedLattice
d_isBoundedLattice_918 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedLattice_466
d_isBoundedLattice_918 ~v0 ~v1 ~v2 v3 = du_isBoundedLattice_918 v3
du_isBoundedLattice_918 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedLattice_466
du_isBoundedLattice_918 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
      (coe d_isHeytingAlgebra_750 (coe v1))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isBoundedMeetSemilattice
d_isBoundedMeetSemilattice_920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedMeetSemilattice_256
d_isBoundedMeetSemilattice_920 ~v0 ~v1 ~v2 v3
  = du_isBoundedMeetSemilattice_920 v3
du_isBoundedMeetSemilattice_920 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedMeetSemilattice_256
du_isBoundedMeetSemilattice_920 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isBoundedMeetSemilattice_544
      (coe d_isBoundedLattice_618 (coe v2))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isEquivalence
d_isEquivalence_922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_922 ~v0 ~v1 ~v2 v3 = du_isEquivalence_922 v3
du_isEquivalence_922 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_922 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                  (coe d_isHeytingAlgebra_750 (coe v1))))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isHeytingAlgebra
d_isHeytingAlgebra_924 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsHeytingAlgebra_556
d_isHeytingAlgebra_924 ~v0 ~v1 ~v2 v3 = du_isHeytingAlgebra_924 v3
du_isHeytingAlgebra_924 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsHeytingAlgebra_556
du_isHeytingAlgebra_924 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isHeytingAlgebra_708
      (coe d_isBooleanAlgebra_892 (coe v0))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isJoinSemilattice
d_isJoinSemilattice_926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_isJoinSemilattice_926 ~v0 ~v1 ~v2 v3
  = du_isJoinSemilattice_926 v3
du_isJoinSemilattice_926 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_isJoinSemilattice_926 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe v3))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isLattice
d_isLattice_928 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
d_isLattice_928 ~v0 ~v1 ~v2 v3 = du_isLattice_928 v3
du_isLattice_928 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
du_isLattice_928 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
         (coe d_isHeytingAlgebra_750 (coe v1)))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isMeetSemilattice
d_isMeetSemilattice_930 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_isMeetSemilattice_930 ~v0 ~v1 ~v2 v3
  = du_isMeetSemilattice_930 v3
du_isMeetSemilattice_930 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_isMeetSemilattice_930 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe v3))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isPartialOrder
d_isPartialOrder_932 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_932 ~v0 ~v1 ~v2 v3 = du_isPartialOrder_932 v3
du_isPartialOrder_932 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_isPartialOrder_932 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
            (coe d_isHeytingAlgebra_750 (coe v1))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.isPreorder
d_isPreorder_934 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_934 ~v0 ~v1 ~v2 v3 = du_isPreorder_934 v3
du_isPreorder_934 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_isPreorder_934 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
               (coe d_isHeytingAlgebra_750 (coe v1)))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.joinSemilattice
d_joinSemilattice_936 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> T_JoinSemilattice_14
d_joinSemilattice_936 ~v0 ~v1 ~v2 v3 = du_joinSemilattice_936 v3
du_joinSemilattice_936 ::
  T_BooleanAlgebra_850 -> T_JoinSemilattice_14
du_joinSemilattice_936 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    coe du_joinSemilattice_450 (coe du_lattice_688 (coe v2))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.lattice
d_lattice_938 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> T_Lattice_362
d_lattice_938 ~v0 ~v1 ~v2 v3 = du_lattice_938 v3
du_lattice_938 :: T_BooleanAlgebra_850 -> T_Lattice_362
du_lattice_938 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe du_lattice_688 (coe du_boundedLattice_752 (coe v1))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.maximum
d_maximum_940 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny
d_maximum_940 ~v0 ~v1 ~v2 v3 = du_maximum_940 v3
du_maximum_940 :: T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny
du_maximum_940 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_maximum_484
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
         (coe d_isHeytingAlgebra_750 (coe v1)))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.meetSemilattice
d_meetSemilattice_942 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> T_MeetSemilattice_188
d_meetSemilattice_942 ~v0 ~v1 ~v2 v3 = du_meetSemilattice_942 v3
du_meetSemilattice_942 ::
  T_BooleanAlgebra_850 -> T_MeetSemilattice_188
du_meetSemilattice_942 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    coe du_meetSemilattice_452 (coe du_lattice_688 (coe v2))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.minimum
d_minimum_944 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny
d_minimum_944 ~v0 ~v1 ~v2 v3 = du_minimum_944 v3
du_minimum_944 :: T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny
du_minimum_944 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_minimum_486
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
         (coe d_isHeytingAlgebra_750 (coe v1)))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.poset
d_poset_946 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_946 ~v0 ~v1 ~v2 v3 = du_poset_946 v3
du_poset_946 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_946 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = coe du_lattice_688 (coe v2) in
    coe du_poset_84 (coe du_joinSemilattice_450 (coe v3))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.preorder
d_preorder_948 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_948 ~v0 ~v1 ~v2 v3 = du_preorder_948 v3
du_preorder_948 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_948 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = coe du_lattice_688 (coe v2) in
    let v4 = coe du_joinSemilattice_450 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326
      (coe du_poset_84 (coe v4))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.refl
d_refl_950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny
d_refl_950 ~v0 ~v1 ~v2 v3 = du_refl_950 v3
du_refl_950 :: T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny
du_refl_950 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.reflexive
d_reflexive_952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_952 ~v0 ~v1 ~v2 v3 = du_reflexive_952 v3
du_reflexive_952 ::
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_reflexive_952 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                  (coe d_isHeytingAlgebra_750 (coe v1))))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.setoid
d_setoid_954 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_954 ~v0 ~v1 ~v2 v3 = du_setoid_954 v3
du_setoid_954 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_954 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    coe du_setoid_448 (coe du_lattice_688 (coe v2))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.supremum
d_supremum_956 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_956 ~v0 ~v1 ~v2 v3 = du_supremum_956 v3
du_supremum_956 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_supremum_956 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.d_supremum_330
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
            (coe d_isHeytingAlgebra_750 (coe v1))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.trans
d_trans_958 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_958 ~v0 ~v1 ~v2 v3 = du_trans_958 v3
du_trans_958 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_958 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                  (coe d_isHeytingAlgebra_750 (coe v1))))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.transpose-⇨
d_transpose'45''8680'_960 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_transpose'45''8680'_960 ~v0 ~v1 ~v2 v3
  = du_transpose'45''8680'_960 v3
du_transpose'45''8680'_960 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_transpose'45''8680'_960 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_transpose'45''8680'_582
      (coe d_isHeytingAlgebra_750 (coe v1))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.transpose-∧
d_transpose'45''8743'_962 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_transpose'45''8743'_962 ~v0 ~v1 ~v2 v3
  = du_transpose'45''8743'_962 v3
du_transpose'45''8743'_962 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_transpose'45''8743'_962 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_transpose'45''8743'_598
      (coe d_isHeytingAlgebra_750 (coe v1))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.x∧y≤x
d_x'8743'y'8804'x_964 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_964 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'x_964 v3
du_x'8743'y'8804'x_964 ::
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_964 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'x_184
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v4))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.x∧y≤y
d_x'8743'y'8804'y_966 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_966 ~v0 ~v1 ~v2 v3 = du_x'8743'y'8804'y_966 v3
du_x'8743'y'8804'y_966 ::
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_966 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8743'y'8804'y_196
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v4))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.x≤x∨y
d_x'8804'x'8744'y_968 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_968 ~v0 ~v1 ~v2 v3 = du_x'8804'x'8744'y_968 v3
du_x'8804'x'8744'y_968 ::
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_968 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8804'x'8744'y_38
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v4))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.y≤x∨y
d_y'8804'x'8744'y_970 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_970 ~v0 ~v1 ~v2 v3 = du_y'8804'x'8744'y_970 v3
du_y'8804'x'8744'y_970 ::
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_970 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_y'8804'x'8744'y_50
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v4))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.∧-greatest
d_'8743''45'greatest_972 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_972 ~v0 ~v1 ~v2 v3
  = du_'8743''45'greatest_972 v3
du_'8743''45'greatest_972 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_972 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8743''45'greatest_210
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isMeetSemilattice_336
         (coe v4))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.∨-least
d_'8744''45'least_974 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_974 ~v0 ~v1 ~v2 v3 = du_'8744''45'least_974 v3
du_'8744''45'least_974 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_974 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8744''45'least_64
      (coe
         MAlonzo.Code.Relation.Binary.Lattice.Structures.du_isJoinSemilattice_334
         (coe v4))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.∼-resp-≈
d_'8764''45'resp'45''8776'_976 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_976 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_976 v3
du_'8764''45'resp'45''8776'_976 ::
  T_BooleanAlgebra_850 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_976 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_978 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_978 v3
du_'8764''45'resp'691''45''8776'_978 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_978 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_980 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_980 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_980 v3
du_'8764''45'resp'737''45''8776'_980 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_980 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v5))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.Eq.isPartialEquivalence
d_isPartialEquivalence_984 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_984 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_984 v3
du_isPartialEquivalence_984 ::
  T_BooleanAlgebra_850 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_984 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
              (coe v4) in
    let v6
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v6))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.Eq.refl
d_refl_986 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny
d_refl_986 ~v0 ~v1 ~v2 v3 = du_refl_986 v3
du_refl_986 :: T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny
du_refl_986 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
                  (coe
                     MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                     (coe d_isHeytingAlgebra_750 (coe v1)))))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.Eq.reflexive
d_reflexive_988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_988 ~v0 ~v1 ~v2 v3 = du_reflexive_988 v3
du_reflexive_988 ::
  T_BooleanAlgebra_850 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_988 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    let v2 = coe du_boundedLattice_752 (coe v1) in
    let v3 = d_isBoundedLattice_618 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
              (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
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
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.Eq.sym
d_sym_990 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_990 ~v0 ~v1 ~v2 v3 = du_sym_990 v3
du_sym_990 ::
  T_BooleanAlgebra_850 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_990 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
                  (coe
                     MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                     (coe d_isHeytingAlgebra_750 (coe v1)))))))
-- Relation.Binary.Lattice.Bundles.BooleanAlgebra._.Eq.trans
d_trans_992 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_992 ~v0 ~v1 ~v2 v3 = du_trans_992 v3
du_trans_992 ::
  T_BooleanAlgebra_850 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_992 v0
  = let v1 = coe du_heytingAlgebra_898 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isPartialOrder_328
               (coe
                  MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isLattice_482
                  (coe
                     MAlonzo.Code.Relation.Binary.Lattice.Structures.d_isBoundedLattice_572
                     (coe d_isHeytingAlgebra_750 (coe v1)))))))
