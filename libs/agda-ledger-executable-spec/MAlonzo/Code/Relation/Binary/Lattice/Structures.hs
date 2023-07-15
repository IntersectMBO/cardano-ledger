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

module MAlonzo.Code.Relation.Binary.Lattice.Structures where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Relation.Binary.Lattice.Structures.IsJoinSemilattice
d_IsJoinSemilattice_22 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsJoinSemilattice_22
  = C_IsJoinSemilattice'46'constructor_527 MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
                                           (AgdaAny ->
                                            AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice.isPartialOrder
d_isPartialOrder_30 ::
  T_IsJoinSemilattice_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_30 v0
  = case coe v0 of
      C_IsJoinSemilattice'46'constructor_527 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice.supremum
d_supremum_32 ::
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_32 v0
  = case coe v0 of
      C_IsJoinSemilattice'46'constructor_527 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice.x≤x∨y
d_x'8804'x'8744'y_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsJoinSemilattice_22 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_38 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9
  = du_x'8804'x'8744'y_38 v7 v8 v9
du_x'8804'x'8744'y_38 ::
  T_IsJoinSemilattice_22 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_38 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_supremum_32 v0 v1 v2)
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice.y≤x∨y
d_y'8804'x'8744'y_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsJoinSemilattice_22 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_50 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9
  = du_y'8804'x'8744'y_50 v7 v8 v9
du_y'8804'x'8744'y_50 ::
  T_IsJoinSemilattice_22 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_50 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
         (coe d_supremum_32 v0 v1 v2))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice.∨-least
d_'8744''45'least_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_64 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9 v10
  = du_'8744''45'least_64 v7 v8 v9 v10
du_'8744''45'least_64 ::
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_64 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
         (coe d_supremum_32 v0 v1 v2))
      v3
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.antisym
d_antisym_76 ::
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_76 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_30 (coe v0))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.isEquivalence
d_isEquivalence_78 ::
  T_IsJoinSemilattice_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_78 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_30 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.isPreorder
d_isPreorder_80 ::
  T_IsJoinSemilattice_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_80 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_30 (coe v0))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.refl
d_refl_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsJoinSemilattice_22 -> AgdaAny -> AgdaAny
d_refl_82 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_refl_82 v7
du_refl_82 :: T_IsJoinSemilattice_22 -> AgdaAny -> AgdaAny
du_refl_82 v0
  = let v1 = d_isPartialOrder_30 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.reflexive
d_reflexive_84 ::
  T_IsJoinSemilattice_22 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_84 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_30 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.trans
d_trans_86 ::
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_86 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_30 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsJoinSemilattice_22 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_88 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8764''45'resp'45''8776'_88 v7
du_'8764''45'resp'45''8776'_88 ::
  T_IsJoinSemilattice_22 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_88 v0
  = let v1 = d_isPartialOrder_30 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_90 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8764''45'resp'691''45''8776'_90 v7
du_'8764''45'resp'691''45''8776'_90 ::
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_90 v0
  = let v1 = d_isPartialOrder_30 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_92 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8764''45'resp'737''45''8776'_92 v7
du_'8764''45'resp'737''45''8776'_92 ::
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_92 v0
  = let v1 = d_isPartialOrder_30 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsJoinSemilattice_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_96 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_96 v7
du_isPartialEquivalence_96 ::
  T_IsJoinSemilattice_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_96 v0
  = let v1 = d_isPartialOrder_30 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v2))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.Eq.refl
d_refl_98 :: T_IsJoinSemilattice_22 -> AgdaAny -> AgdaAny
d_refl_98 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_30 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.Eq.reflexive
d_reflexive_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsJoinSemilattice_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_100 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_100 v7
du_reflexive_100 ::
  T_IsJoinSemilattice_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_100 v0
  = let v1 = d_isPartialOrder_30 (coe v0) in
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
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.Eq.sym
d_sym_102 ::
  T_IsJoinSemilattice_22 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_102 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_30 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsJoinSemilattice._.Eq.trans
d_trans_104 ::
  T_IsJoinSemilattice_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_104 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_30 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice
d_IsBoundedJoinSemilattice_110 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsBoundedJoinSemilattice_110
  = C_IsBoundedJoinSemilattice'46'constructor_5169 T_IsJoinSemilattice_22
                                                   (AgdaAny -> AgdaAny)
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice.isJoinSemilattice
d_isJoinSemilattice_120 ::
  T_IsBoundedJoinSemilattice_110 -> T_IsJoinSemilattice_22
d_isJoinSemilattice_120 v0
  = case coe v0 of
      C_IsBoundedJoinSemilattice'46'constructor_5169 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice.minimum
d_minimum_122 ::
  T_IsBoundedJoinSemilattice_110 -> AgdaAny -> AgdaAny
d_minimum_122 v0
  = case coe v0 of
      C_IsBoundedJoinSemilattice'46'constructor_5169 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.antisym
d_antisym_126 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_126 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_30 (coe d_isJoinSemilattice_120 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.isEquivalence
d_isEquivalence_128 ::
  T_IsBoundedJoinSemilattice_110 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_128 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_30 (coe d_isJoinSemilattice_120 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.isPartialOrder
d_isPartialOrder_130 ::
  T_IsBoundedJoinSemilattice_110 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_130 v0
  = coe d_isPartialOrder_30 (coe d_isJoinSemilattice_120 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.isPreorder
d_isPreorder_132 ::
  T_IsBoundedJoinSemilattice_110 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_132 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_30 (coe d_isJoinSemilattice_120 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.refl
d_refl_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsBoundedJoinSemilattice_110 -> AgdaAny -> AgdaAny
d_refl_134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_refl_134 v8
du_refl_134 :: T_IsBoundedJoinSemilattice_110 -> AgdaAny -> AgdaAny
du_refl_134 v0
  = let v1 = d_isJoinSemilattice_120 (coe v0) in
    let v2 = d_isPartialOrder_30 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.reflexive
d_reflexive_136 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_136 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_30 (coe d_isJoinSemilattice_120 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.supremum
d_supremum_138 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_138 v0
  = coe d_supremum_32 (coe d_isJoinSemilattice_120 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.trans
d_trans_140 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_140 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_30 (coe d_isJoinSemilattice_120 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.x≤x∨y
d_x'8804'x'8744'y_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedJoinSemilattice_110 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_142 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_x'8804'x'8744'y_142 v8
du_x'8804'x'8744'y_142 ::
  T_IsBoundedJoinSemilattice_110 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_142 v0
  = coe du_x'8804'x'8744'y_38 (coe d_isJoinSemilattice_120 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.y≤x∨y
d_y'8804'x'8744'y_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedJoinSemilattice_110 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_144 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_y'8804'x'8744'y_144 v8
du_y'8804'x'8744'y_144 ::
  T_IsBoundedJoinSemilattice_110 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_144 v0
  = coe du_y'8804'x'8744'y_50 (coe d_isJoinSemilattice_120 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.∨-least
d_'8744''45'least_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_146 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8744''45'least_146 v8
du_'8744''45'least_146 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_146 v0
  = coe du_'8744''45'least_64 (coe d_isJoinSemilattice_120 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedJoinSemilattice_110 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_148 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8764''45'resp'45''8776'_148 v8
du_'8764''45'resp'45''8776'_148 ::
  T_IsBoundedJoinSemilattice_110 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_148 v0
  = let v1 = d_isJoinSemilattice_120 (coe v0) in
    let v2 = d_isPartialOrder_30 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_150 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    v8
  = du_'8764''45'resp'691''45''8776'_150 v8
du_'8764''45'resp'691''45''8776'_150 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_150 v0
  = let v1 = d_isJoinSemilattice_120 (coe v0) in
    let v2 = d_isPartialOrder_30 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_152 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    v8
  = du_'8764''45'resp'737''45''8776'_152 v8
du_'8764''45'resp'737''45''8776'_152 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_152 v0
  = let v1 = d_isJoinSemilattice_120 (coe v0) in
    let v2 = d_isPartialOrder_30 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedJoinSemilattice_110 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_156 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_156 v8
du_isPartialEquivalence_156 ::
  T_IsBoundedJoinSemilattice_110 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_156 v0
  = let v1 = d_isJoinSemilattice_120 (coe v0) in
    let v2 = d_isPartialOrder_30 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v3))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.Eq.refl
d_refl_158 :: T_IsBoundedJoinSemilattice_110 -> AgdaAny -> AgdaAny
d_refl_158 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_30 (coe d_isJoinSemilattice_120 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.Eq.reflexive
d_reflexive_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_160 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_160 v8
du_reflexive_160 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_160 v0
  = let v1 = d_isJoinSemilattice_120 (coe v0) in
    let v2 = d_isPartialOrder_30 (coe v1) in
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
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.Eq.sym
d_sym_162 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_162 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_30 (coe d_isJoinSemilattice_120 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBoundedJoinSemilattice._.Eq.trans
d_trans_164 ::
  T_IsBoundedJoinSemilattice_110 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_164 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_30 (coe d_isJoinSemilattice_120 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice
d_IsMeetSemilattice_168 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsMeetSemilattice_168
  = C_IsMeetSemilattice'46'constructor_7327 MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
                                            (AgdaAny ->
                                             AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice.isPartialOrder
d_isPartialOrder_176 ::
  T_IsMeetSemilattice_168 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_176 v0
  = case coe v0 of
      C_IsMeetSemilattice'46'constructor_7327 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice.infimum
d_infimum_178 ::
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_178 v0
  = case coe v0 of
      C_IsMeetSemilattice'46'constructor_7327 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice.x∧y≤x
d_x'8743'y'8804'x_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMeetSemilattice_168 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_184 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9
  = du_x'8743'y'8804'x_184 v7 v8 v9
du_x'8743'y'8804'x_184 ::
  T_IsMeetSemilattice_168 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_184 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_infimum_178 v0 v1 v2)
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice.x∧y≤y
d_x'8743'y'8804'y_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMeetSemilattice_168 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9
  = du_x'8743'y'8804'y_196 v7 v8 v9
du_x'8743'y'8804'y_196 ::
  T_IsMeetSemilattice_168 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_196 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
         (coe d_infimum_178 v0 v1 v2))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice.∧-greatest
d_'8743''45'greatest_210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_210 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9 v10
  = du_'8743''45'greatest_210 v7 v8 v9 v10
du_'8743''45'greatest_210 ::
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_210 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
         (coe d_infimum_178 v0 v2 v3))
      v1
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.antisym
d_antisym_222 ::
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_222 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_176 (coe v0))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.isEquivalence
d_isEquivalence_224 ::
  T_IsMeetSemilattice_168 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_224 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_176 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.isPreorder
d_isPreorder_226 ::
  T_IsMeetSemilattice_168 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_226 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_176 (coe v0))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.refl
d_refl_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMeetSemilattice_168 -> AgdaAny -> AgdaAny
d_refl_228 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_refl_228 v7
du_refl_228 :: T_IsMeetSemilattice_168 -> AgdaAny -> AgdaAny
du_refl_228 v0
  = let v1 = d_isPartialOrder_176 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.reflexive
d_reflexive_230 ::
  T_IsMeetSemilattice_168 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_230 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_176 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.trans
d_trans_232 ::
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_232 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_176 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMeetSemilattice_168 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_234 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8764''45'resp'45''8776'_234 v7
du_'8764''45'resp'45''8776'_234 ::
  T_IsMeetSemilattice_168 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_234 v0
  = let v1 = d_isPartialOrder_176 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_236 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8764''45'resp'691''45''8776'_236 v7
du_'8764''45'resp'691''45''8776'_236 ::
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_236 v0
  = let v1 = d_isPartialOrder_176 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_238 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8764''45'resp'737''45''8776'_238 v7
du_'8764''45'resp'737''45''8776'_238 ::
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_238 v0
  = let v1 = d_isPartialOrder_176 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMeetSemilattice_168 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_242 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_242 v7
du_isPartialEquivalence_242 ::
  T_IsMeetSemilattice_168 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_242 v0
  = let v1 = d_isPartialOrder_176 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v2))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.Eq.refl
d_refl_244 :: T_IsMeetSemilattice_168 -> AgdaAny -> AgdaAny
d_refl_244 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_176 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.Eq.reflexive
d_reflexive_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMeetSemilattice_168 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_246 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_246 v7
du_reflexive_246 ::
  T_IsMeetSemilattice_168 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_246 v0
  = let v1 = d_isPartialOrder_176 (coe v0) in
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
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.Eq.sym
d_sym_248 ::
  T_IsMeetSemilattice_168 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_248 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_176 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsMeetSemilattice._.Eq.trans
d_trans_250 ::
  T_IsMeetSemilattice_168 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_250 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_176 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice
d_IsBoundedMeetSemilattice_256 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsBoundedMeetSemilattice_256
  = C_IsBoundedMeetSemilattice'46'constructor_11969 T_IsMeetSemilattice_168
                                                    (AgdaAny -> AgdaAny)
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice.isMeetSemilattice
d_isMeetSemilattice_266 ::
  T_IsBoundedMeetSemilattice_256 -> T_IsMeetSemilattice_168
d_isMeetSemilattice_266 v0
  = case coe v0 of
      C_IsBoundedMeetSemilattice'46'constructor_11969 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice.maximum
d_maximum_268 ::
  T_IsBoundedMeetSemilattice_256 -> AgdaAny -> AgdaAny
d_maximum_268 v0
  = case coe v0 of
      C_IsBoundedMeetSemilattice'46'constructor_11969 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.antisym
d_antisym_272 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_272 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_176 (coe d_isMeetSemilattice_266 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.infimum
d_infimum_274 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_274 v0
  = coe d_infimum_178 (coe d_isMeetSemilattice_266 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.isEquivalence
d_isEquivalence_276 ::
  T_IsBoundedMeetSemilattice_256 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_276 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_176 (coe d_isMeetSemilattice_266 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.isPartialOrder
d_isPartialOrder_278 ::
  T_IsBoundedMeetSemilattice_256 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_278 v0
  = coe d_isPartialOrder_176 (coe d_isMeetSemilattice_266 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.isPreorder
d_isPreorder_280 ::
  T_IsBoundedMeetSemilattice_256 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_280 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_176 (coe d_isMeetSemilattice_266 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.refl
d_refl_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsBoundedMeetSemilattice_256 -> AgdaAny -> AgdaAny
d_refl_282 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_refl_282 v8
du_refl_282 :: T_IsBoundedMeetSemilattice_256 -> AgdaAny -> AgdaAny
du_refl_282 v0
  = let v1 = d_isMeetSemilattice_266 (coe v0) in
    let v2 = d_isPartialOrder_176 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.reflexive
d_reflexive_284 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_284 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_176 (coe d_isMeetSemilattice_266 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.trans
d_trans_286 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_286 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_176 (coe d_isMeetSemilattice_266 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.x∧y≤x
d_x'8743'y'8804'x_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedMeetSemilattice_256 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_288 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_x'8743'y'8804'x_288 v8
du_x'8743'y'8804'x_288 ::
  T_IsBoundedMeetSemilattice_256 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_288 v0
  = coe du_x'8743'y'8804'x_184 (coe d_isMeetSemilattice_266 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.x∧y≤y
d_x'8743'y'8804'y_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedMeetSemilattice_256 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_290 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_x'8743'y'8804'y_290 v8
du_x'8743'y'8804'y_290 ::
  T_IsBoundedMeetSemilattice_256 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_290 v0
  = coe du_x'8743'y'8804'y_196 (coe d_isMeetSemilattice_266 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.∧-greatest
d_'8743''45'greatest_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_292 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8743''45'greatest_292 v8
du_'8743''45'greatest_292 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_292 v0
  = coe
      du_'8743''45'greatest_210 (coe d_isMeetSemilattice_266 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedMeetSemilattice_256 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_294 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8764''45'resp'45''8776'_294 v8
du_'8764''45'resp'45''8776'_294 ::
  T_IsBoundedMeetSemilattice_256 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_294 v0
  = let v1 = d_isMeetSemilattice_266 (coe v0) in
    let v2 = d_isPartialOrder_176 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_296 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    v8
  = du_'8764''45'resp'691''45''8776'_296 v8
du_'8764''45'resp'691''45''8776'_296 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_296 v0
  = let v1 = d_isMeetSemilattice_266 (coe v0) in
    let v2 = d_isPartialOrder_176 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_298 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    v8
  = du_'8764''45'resp'737''45''8776'_298 v8
du_'8764''45'resp'737''45''8776'_298 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_298 v0
  = let v1 = d_isMeetSemilattice_266 (coe v0) in
    let v2 = d_isPartialOrder_176 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedMeetSemilattice_256 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_302 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_302 v8
du_isPartialEquivalence_302 ::
  T_IsBoundedMeetSemilattice_256 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_302 v0
  = let v1 = d_isMeetSemilattice_266 (coe v0) in
    let v2 = d_isPartialOrder_176 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v3))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.Eq.refl
d_refl_304 :: T_IsBoundedMeetSemilattice_256 -> AgdaAny -> AgdaAny
d_refl_304 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_176 (coe d_isMeetSemilattice_266 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.Eq.reflexive
d_reflexive_306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_306 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_306 v8
du_reflexive_306 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_306 v0
  = let v1 = d_isMeetSemilattice_266 (coe v0) in
    let v2 = d_isPartialOrder_176 (coe v1) in
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
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.Eq.sym
d_sym_308 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_308 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_176 (coe d_isMeetSemilattice_266 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBoundedMeetSemilattice._.Eq.trans
d_trans_310 ::
  T_IsBoundedMeetSemilattice_256 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_310 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_176 (coe d_isMeetSemilattice_266 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsLattice
d_IsLattice_316 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsLattice_316
  = C_IsLattice'46'constructor_14441 MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
                                     (AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
                                     (AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Relation.Binary.Lattice.Structures.IsLattice.isPartialOrder
d_isPartialOrder_328 ::
  T_IsLattice_316 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_328 v0
  = case coe v0 of
      C_IsLattice'46'constructor_14441 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsLattice.supremum
d_supremum_330 ::
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_330 v0
  = case coe v0 of
      C_IsLattice'46'constructor_14441 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsLattice.infimum
d_infimum_332 ::
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_332 v0
  = case coe v0 of
      C_IsLattice'46'constructor_14441 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsLattice.isJoinSemilattice
d_isJoinSemilattice_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 -> T_IsJoinSemilattice_22
d_isJoinSemilattice_334 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isJoinSemilattice_334 v8
du_isJoinSemilattice_334 ::
  T_IsLattice_316 -> T_IsJoinSemilattice_22
du_isJoinSemilattice_334 v0
  = coe
      C_IsJoinSemilattice'46'constructor_527
      (coe d_isPartialOrder_328 (coe v0)) (coe d_supremum_330 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice.isMeetSemilattice
d_isMeetSemilattice_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 -> T_IsMeetSemilattice_168
d_isMeetSemilattice_336 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isMeetSemilattice_336 v8
du_isMeetSemilattice_336 ::
  T_IsLattice_316 -> T_IsMeetSemilattice_168
du_isMeetSemilattice_336 v0
  = coe
      C_IsMeetSemilattice'46'constructor_7327
      (coe d_isPartialOrder_328 (coe v0)) (coe d_infimum_332 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice._.x≤x∨y
d_x'8804'x'8744'y_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_340 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_x'8804'x'8744'y_340 v8
du_x'8804'x'8744'y_340 ::
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_340 v0
  = coe du_x'8804'x'8744'y_38 (coe du_isJoinSemilattice_334 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice._.y≤x∨y
d_y'8804'x'8744'y_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_342 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_y'8804'x'8744'y_342 v8
du_y'8804'x'8744'y_342 ::
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_342 v0
  = coe du_y'8804'x'8744'y_50 (coe du_isJoinSemilattice_334 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice._.∨-least
d_'8744''45'least_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_344 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8744''45'least_344 v8
du_'8744''45'least_344 ::
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_344 v0
  = coe du_'8744''45'least_64 (coe du_isJoinSemilattice_334 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice._.x∧y≤x
d_x'8743'y'8804'x_348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_348 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_x'8743'y'8804'x_348 v8
du_x'8743'y'8804'x_348 ::
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_348 v0
  = coe
      du_x'8743'y'8804'x_184 (coe du_isMeetSemilattice_336 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice._.x∧y≤y
d_x'8743'y'8804'y_350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_350 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_x'8743'y'8804'y_350 v8
du_x'8743'y'8804'y_350 ::
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_350 v0
  = coe
      du_x'8743'y'8804'y_196 (coe du_isMeetSemilattice_336 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice._.∧-greatest
d_'8743''45'greatest_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_352 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8743''45'greatest_352 v8
du_'8743''45'greatest_352 ::
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_352 v0
  = coe
      du_'8743''45'greatest_210 (coe du_isMeetSemilattice_336 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice._.antisym
d_antisym_356 ::
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_356 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_328 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice._.isEquivalence
d_isEquivalence_358 ::
  T_IsLattice_316 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_358 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_328 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsLattice._.isPreorder
d_isPreorder_360 ::
  T_IsLattice_316 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_360 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_328 (coe v0))
-- Relation.Binary.Lattice.Structures.IsLattice._.refl
d_refl_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 -> AgdaAny -> AgdaAny
d_refl_362 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_refl_362 v8
du_refl_362 :: T_IsLattice_316 -> AgdaAny -> AgdaAny
du_refl_362 v0
  = let v1 = d_isPartialOrder_328 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsLattice._.reflexive
d_reflexive_364 ::
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_364 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_328 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsLattice._.trans
d_trans_366 ::
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_366 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_328 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsLattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_368 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8764''45'resp'45''8776'_368 v8
du_'8764''45'resp'45''8776'_368 ::
  T_IsLattice_316 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_368 v0
  = let v1 = d_isPartialOrder_328 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsLattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_370 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    v8
  = du_'8764''45'resp'691''45''8776'_370 v8
du_'8764''45'resp'691''45''8776'_370 ::
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_370 v0
  = let v1 = d_isPartialOrder_328 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsLattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_372 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    v8
  = du_'8764''45'resp'737''45''8776'_372 v8
du_'8764''45'resp'737''45''8776'_372 ::
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_372 v0
  = let v1 = d_isPartialOrder_328 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Lattice.Structures.IsLattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_376 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_376 v8
du_isPartialEquivalence_376 ::
  T_IsLattice_316 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_376 v0
  = let v1 = d_isPartialOrder_328 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v2))
-- Relation.Binary.Lattice.Structures.IsLattice._.Eq.refl
d_refl_378 :: T_IsLattice_316 -> AgdaAny -> AgdaAny
d_refl_378 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_328 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsLattice._.Eq.reflexive
d_reflexive_380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_316 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_380 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_380 v8
du_reflexive_380 ::
  T_IsLattice_316 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_380 v0
  = let v1 = d_isPartialOrder_328 (coe v0) in
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
-- Relation.Binary.Lattice.Structures.IsLattice._.Eq.sym
d_sym_382 ::
  T_IsLattice_316 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_382 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_328 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsLattice._.Eq.trans
d_trans_384 ::
  T_IsLattice_316 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_384 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_328 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice
d_IsDistributiveLattice_390 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsDistributiveLattice_390
  = C_IsDistributiveLattice'46'constructor_17485 T_IsLattice_316
                                                 (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice.isLattice
d_isLattice_400 :: T_IsDistributiveLattice_390 -> T_IsLattice_316
d_isLattice_400 v0
  = case coe v0 of
      C_IsDistributiveLattice'46'constructor_17485 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_402 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8744'_402 v0
  = case coe v0 of
      C_IsDistributiveLattice'46'constructor_17485 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.antisym
d_antisym_406 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_406 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_328 (coe d_isLattice_400 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.infimum
d_infimum_408 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_408 v0 = coe d_infimum_332 (coe d_isLattice_400 (coe v0))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.isEquivalence
d_isEquivalence_410 ::
  T_IsDistributiveLattice_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_410 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_328 (coe d_isLattice_400 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.isJoinSemilattice
d_isJoinSemilattice_412 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 -> T_IsJoinSemilattice_22
d_isJoinSemilattice_412 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isJoinSemilattice_412 v8
du_isJoinSemilattice_412 ::
  T_IsDistributiveLattice_390 -> T_IsJoinSemilattice_22
du_isJoinSemilattice_412 v0
  = coe du_isJoinSemilattice_334 (coe d_isLattice_400 (coe v0))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.isMeetSemilattice
d_isMeetSemilattice_414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 -> T_IsMeetSemilattice_168
d_isMeetSemilattice_414 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isMeetSemilattice_414 v8
du_isMeetSemilattice_414 ::
  T_IsDistributiveLattice_390 -> T_IsMeetSemilattice_168
du_isMeetSemilattice_414 v0
  = coe du_isMeetSemilattice_336 (coe d_isLattice_400 (coe v0))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.isPartialOrder
d_isPartialOrder_416 ::
  T_IsDistributiveLattice_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_416 v0
  = coe d_isPartialOrder_328 (coe d_isLattice_400 (coe v0))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.isPreorder
d_isPreorder_418 ::
  T_IsDistributiveLattice_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_418 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_328 (coe d_isLattice_400 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.refl
d_refl_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny
d_refl_420 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_refl_420 v8
du_refl_420 :: T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny
du_refl_420 v0
  = let v1 = d_isLattice_400 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.reflexive
d_reflexive_422 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_422 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_328 (coe d_isLattice_400 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.supremum
d_supremum_424 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_424 v0
  = coe d_supremum_330 (coe d_isLattice_400 (coe v0))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.trans
d_trans_426 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_426 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_328 (coe d_isLattice_400 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.x∧y≤x
d_x'8743'y'8804'x_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_428 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_x'8743'y'8804'x_428 v8
du_x'8743'y'8804'x_428 ::
  T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_428 v0
  = let v1 = d_isLattice_400 (coe v0) in
    coe du_x'8743'y'8804'x_184 (coe du_isMeetSemilattice_336 (coe v1))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.x∧y≤y
d_x'8743'y'8804'y_430 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_430 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_x'8743'y'8804'y_430 v8
du_x'8743'y'8804'y_430 ::
  T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_430 v0
  = let v1 = d_isLattice_400 (coe v0) in
    coe du_x'8743'y'8804'y_196 (coe du_isMeetSemilattice_336 (coe v1))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.x≤x∨y
d_x'8804'x'8744'y_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_432 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_x'8804'x'8744'y_432 v8
du_x'8804'x'8744'y_432 ::
  T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_432 v0
  = let v1 = d_isLattice_400 (coe v0) in
    coe du_x'8804'x'8744'y_38 (coe du_isJoinSemilattice_334 (coe v1))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.y≤x∨y
d_y'8804'x'8744'y_434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_434 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_y'8804'x'8744'y_434 v8
du_y'8804'x'8744'y_434 ::
  T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_434 v0
  = let v1 = d_isLattice_400 (coe v0) in
    coe du_y'8804'x'8744'y_50 (coe du_isJoinSemilattice_334 (coe v1))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.∧-greatest
d_'8743''45'greatest_436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_436 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8743''45'greatest_436 v8
du_'8743''45'greatest_436 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_436 v0
  = let v1 = d_isLattice_400 (coe v0) in
    coe
      du_'8743''45'greatest_210 (coe du_isMeetSemilattice_336 (coe v1))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.∨-least
d_'8744''45'least_438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_438 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8744''45'least_438 v8
du_'8744''45'least_438 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_438 v0
  = let v1 = d_isLattice_400 (coe v0) in
    coe du_'8744''45'least_64 (coe du_isJoinSemilattice_334 (coe v1))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_440 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8764''45'resp'45''8776'_440 v8
du_'8764''45'resp'45''8776'_440 ::
  T_IsDistributiveLattice_390 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_440 v0
  = let v1 = d_isLattice_400 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_442 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    v8
  = du_'8764''45'resp'691''45''8776'_442 v8
du_'8764''45'resp'691''45''8776'_442 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_442 v0
  = let v1 = d_isLattice_400 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_444 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    v8
  = du_'8764''45'resp'737''45''8776'_444 v8
du_'8764''45'resp'737''45''8776'_444 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_444 v0
  = let v1 = d_isLattice_400 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_448 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_448 v8
du_isPartialEquivalence_448 ::
  T_IsDistributiveLattice_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_448 v0
  = let v1 = d_isLattice_400 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v3))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.Eq.refl
d_refl_450 :: T_IsDistributiveLattice_390 -> AgdaAny -> AgdaAny
d_refl_450 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_328 (coe d_isLattice_400 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.Eq.reflexive
d_reflexive_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_390 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_452 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_452 v8
du_reflexive_452 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_452 v0
  = let v1 = d_isLattice_400 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
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
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.Eq.sym
d_sym_454 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_454 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_328 (coe d_isLattice_400 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsDistributiveLattice._.Eq.trans
d_trans_456 ::
  T_IsDistributiveLattice_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_456 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_328 (coe d_isLattice_400 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice
d_IsBoundedLattice_466 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 = ()
data T_IsBoundedLattice_466
  = C_IsBoundedLattice'46'constructor_20407 T_IsLattice_316
                                            (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
-- Relation.Binary.Lattice.Structures.IsBoundedLattice.isLattice
d_isLattice_482 :: T_IsBoundedLattice_466 -> T_IsLattice_316
d_isLattice_482 v0
  = case coe v0 of
      C_IsBoundedLattice'46'constructor_20407 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsBoundedLattice.maximum
d_maximum_484 :: T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny
d_maximum_484 v0
  = case coe v0 of
      C_IsBoundedLattice'46'constructor_20407 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsBoundedLattice.minimum
d_minimum_486 :: T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny
d_minimum_486 v0
  = case coe v0 of
      C_IsBoundedLattice'46'constructor_20407 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.antisym
d_antisym_490 ::
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_490 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_328 (coe d_isLattice_482 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.infimum
d_infimum_492 ::
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_492 v0 = coe d_infimum_332 (coe d_isLattice_482 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.isEquivalence
d_isEquivalence_494 ::
  T_IsBoundedLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_494 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_328 (coe d_isLattice_482 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.isJoinSemilattice
d_isJoinSemilattice_496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBoundedLattice_466 -> T_IsJoinSemilattice_22
d_isJoinSemilattice_496 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isJoinSemilattice_496 v10
du_isJoinSemilattice_496 ::
  T_IsBoundedLattice_466 -> T_IsJoinSemilattice_22
du_isJoinSemilattice_496 v0
  = coe du_isJoinSemilattice_334 (coe d_isLattice_482 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.isMeetSemilattice
d_isMeetSemilattice_498 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBoundedLattice_466 -> T_IsMeetSemilattice_168
d_isMeetSemilattice_498 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isMeetSemilattice_498 v10
du_isMeetSemilattice_498 ::
  T_IsBoundedLattice_466 -> T_IsMeetSemilattice_168
du_isMeetSemilattice_498 v0
  = coe du_isMeetSemilattice_336 (coe d_isLattice_482 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.isPartialOrder
d_isPartialOrder_500 ::
  T_IsBoundedLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_500 v0
  = coe d_isPartialOrder_328 (coe d_isLattice_482 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.isPreorder
d_isPreorder_502 ::
  T_IsBoundedLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_502 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_328 (coe d_isLattice_482 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.refl
d_refl_504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny
d_refl_504 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_refl_504 v10
du_refl_504 :: T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny
du_refl_504 v0
  = let v1 = d_isLattice_482 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.reflexive
d_reflexive_506 ::
  T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_506 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_328 (coe d_isLattice_482 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.supremum
d_supremum_508 ::
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_508 v0
  = coe d_supremum_330 (coe d_isLattice_482 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.trans
d_trans_510 ::
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_510 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_328 (coe d_isLattice_482 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.x∧y≤x
d_x'8743'y'8804'x_512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_512 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_x'8743'y'8804'x_512 v10
du_x'8743'y'8804'x_512 ::
  T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_512 v0
  = let v1 = d_isLattice_482 (coe v0) in
    coe du_x'8743'y'8804'x_184 (coe du_isMeetSemilattice_336 (coe v1))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.x∧y≤y
d_x'8743'y'8804'y_514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_514 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_x'8743'y'8804'y_514 v10
du_x'8743'y'8804'y_514 ::
  T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_514 v0
  = let v1 = d_isLattice_482 (coe v0) in
    coe du_x'8743'y'8804'y_196 (coe du_isMeetSemilattice_336 (coe v1))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.x≤x∨y
d_x'8804'x'8744'y_516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_516 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_x'8804'x'8744'y_516 v10
du_x'8804'x'8744'y_516 ::
  T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_516 v0
  = let v1 = d_isLattice_482 (coe v0) in
    coe du_x'8804'x'8744'y_38 (coe du_isJoinSemilattice_334 (coe v1))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.y≤x∨y
d_y'8804'x'8744'y_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_518 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_y'8804'x'8744'y_518 v10
du_y'8804'x'8744'y_518 ::
  T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_518 v0
  = let v1 = d_isLattice_482 (coe v0) in
    coe du_y'8804'x'8744'y_50 (coe du_isJoinSemilattice_334 (coe v1))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.∧-greatest
d_'8743''45'greatest_520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_520 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                         v10
  = du_'8743''45'greatest_520 v10
du_'8743''45'greatest_520 ::
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_520 v0
  = let v1 = d_isLattice_482 (coe v0) in
    coe
      du_'8743''45'greatest_210 (coe du_isMeetSemilattice_336 (coe v1))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.∨-least
d_'8744''45'least_522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_522 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_'8744''45'least_522 v10
du_'8744''45'least_522 ::
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_522 v0
  = let v1 = d_isLattice_482 (coe v0) in
    coe du_'8744''45'least_64 (coe du_isJoinSemilattice_334 (coe v1))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.∼-resp-≈
d_'8764''45'resp'45''8776'_524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBoundedLattice_466 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_524 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 v10
  = du_'8764''45'resp'45''8776'_524 v10
du_'8764''45'resp'45''8776'_524 ::
  T_IsBoundedLattice_466 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_524 v0
  = let v1 = d_isLattice_482 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_526 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 v10
  = du_'8764''45'resp'691''45''8776'_526 v10
du_'8764''45'resp'691''45''8776'_526 ::
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_526 v0
  = let v1 = d_isLattice_482 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_528 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 v10
  = du_'8764''45'resp'737''45''8776'_528 v10
du_'8764''45'resp'737''45''8776'_528 ::
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_528 v0
  = let v1 = d_isLattice_482 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.Eq.isPartialEquivalence
d_isPartialEquivalence_532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBoundedLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_532 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           v10
  = du_isPartialEquivalence_532 v10
du_isPartialEquivalence_532 ::
  T_IsBoundedLattice_466 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_532 v0
  = let v1 = d_isLattice_482 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v3))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.Eq.refl
d_refl_534 :: T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny
d_refl_534 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_328 (coe d_isLattice_482 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.Eq.reflexive
d_reflexive_536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBoundedLattice_466 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_536 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_reflexive_536 v10
du_reflexive_536 ::
  T_IsBoundedLattice_466 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_536 v0
  = let v1 = d_isLattice_482 (coe v0) in
    let v2 = d_isPartialOrder_328 (coe v1) in
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
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.Eq.sym
d_sym_538 ::
  T_IsBoundedLattice_466 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_538 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_328 (coe d_isLattice_482 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice._.Eq.trans
d_trans_540 ::
  T_IsBoundedLattice_466 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_540 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe d_isPartialOrder_328 (coe d_isLattice_482 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice.isBoundedJoinSemilattice
d_isBoundedJoinSemilattice_542 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBoundedLattice_466 -> T_IsBoundedJoinSemilattice_110
d_isBoundedJoinSemilattice_542 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 v10
  = du_isBoundedJoinSemilattice_542 v10
du_isBoundedJoinSemilattice_542 ::
  T_IsBoundedLattice_466 -> T_IsBoundedJoinSemilattice_110
du_isBoundedJoinSemilattice_542 v0
  = coe
      C_IsBoundedJoinSemilattice'46'constructor_5169
      (coe du_isJoinSemilattice_334 (coe d_isLattice_482 (coe v0)))
      (coe d_minimum_486 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBoundedLattice.isBoundedMeetSemilattice
d_isBoundedMeetSemilattice_544 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBoundedLattice_466 -> T_IsBoundedMeetSemilattice_256
d_isBoundedMeetSemilattice_544 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 v10
  = du_isBoundedMeetSemilattice_544 v10
du_isBoundedMeetSemilattice_544 ::
  T_IsBoundedLattice_466 -> T_IsBoundedMeetSemilattice_256
du_isBoundedMeetSemilattice_544 v0
  = coe
      C_IsBoundedMeetSemilattice'46'constructor_11969
      (coe du_isMeetSemilattice_336 (coe d_isLattice_482 (coe v0)))
      (coe d_maximum_484 (coe v0))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra
d_IsHeytingAlgebra_556 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
data T_IsHeytingAlgebra_556
  = C_IsHeytingAlgebra'46'constructor_24147 T_IsBoundedLattice_466
                                            (AgdaAny ->
                                             AgdaAny ->
                                             AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra.isBoundedLattice
d_isBoundedLattice_572 ::
  T_IsHeytingAlgebra_556 -> T_IsBoundedLattice_466
d_isBoundedLattice_572 v0
  = case coe v0 of
      C_IsHeytingAlgebra'46'constructor_24147 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra.exponential
d_exponential_574 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_exponential_574 v0
  = case coe v0 of
      C_IsHeytingAlgebra'46'constructor_24147 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra.transpose-⇨
d_transpose'45''8680'_582 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_transpose'45''8680'_582 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                          ~v10 v11 v12 v13 v14
  = du_transpose'45''8680'_582 v11 v12 v13 v14
du_transpose'45''8680'_582 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_transpose'45''8680'_582 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_exponential_574 v0 v1 v2 v3)
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra.transpose-∧
d_transpose'45''8743'_598 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_transpose'45''8743'_598 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                          ~v10 v11 v12 v13 v14
  = du_transpose'45''8743'_598 v11 v12 v13 v14
du_transpose'45''8743'_598 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_transpose'45''8743'_598 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_exponential_574 v0 v1 v2 v3)
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.antisym
d_antisym_610 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_610 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         d_isPartialOrder_328
         (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.infimum
d_infimum_612 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_612 v0
  = coe
      d_infimum_332
      (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.isBoundedJoinSemilattice
d_isBoundedJoinSemilattice_614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsHeytingAlgebra_556 -> T_IsBoundedJoinSemilattice_110
d_isBoundedJoinSemilattice_614 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 v11
  = du_isBoundedJoinSemilattice_614 v11
du_isBoundedJoinSemilattice_614 ::
  T_IsHeytingAlgebra_556 -> T_IsBoundedJoinSemilattice_110
du_isBoundedJoinSemilattice_614 v0
  = coe
      du_isBoundedJoinSemilattice_542
      (coe d_isBoundedLattice_572 (coe v0))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.isBoundedMeetSemilattice
d_isBoundedMeetSemilattice_616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsHeytingAlgebra_556 -> T_IsBoundedMeetSemilattice_256
d_isBoundedMeetSemilattice_616 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 v11
  = du_isBoundedMeetSemilattice_616 v11
du_isBoundedMeetSemilattice_616 ::
  T_IsHeytingAlgebra_556 -> T_IsBoundedMeetSemilattice_256
du_isBoundedMeetSemilattice_616 v0
  = coe
      du_isBoundedMeetSemilattice_544
      (coe d_isBoundedLattice_572 (coe v0))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.isEquivalence
d_isEquivalence_618 ::
  T_IsHeytingAlgebra_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_618 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_328
            (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.isJoinSemilattice
d_isJoinSemilattice_620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsHeytingAlgebra_556 -> T_IsJoinSemilattice_22
d_isJoinSemilattice_620 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                        ~v10 v11
  = du_isJoinSemilattice_620 v11
du_isJoinSemilattice_620 ::
  T_IsHeytingAlgebra_556 -> T_IsJoinSemilattice_22
du_isJoinSemilattice_620 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    coe du_isJoinSemilattice_334 (coe d_isLattice_482 (coe v1))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.isLattice
d_isLattice_622 :: T_IsHeytingAlgebra_556 -> T_IsLattice_316
d_isLattice_622 v0
  = coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.isMeetSemilattice
d_isMeetSemilattice_624 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsHeytingAlgebra_556 -> T_IsMeetSemilattice_168
d_isMeetSemilattice_624 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                        ~v10 v11
  = du_isMeetSemilattice_624 v11
du_isMeetSemilattice_624 ::
  T_IsHeytingAlgebra_556 -> T_IsMeetSemilattice_168
du_isMeetSemilattice_624 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    coe du_isMeetSemilattice_336 (coe d_isLattice_482 (coe v1))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.isPartialOrder
d_isPartialOrder_626 ::
  T_IsHeytingAlgebra_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_626 v0
  = coe
      d_isPartialOrder_328
      (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.isPreorder
d_isPreorder_628 ::
  T_IsHeytingAlgebra_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_628 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         d_isPartialOrder_328
         (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.maximum
d_maximum_630 :: T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny
d_maximum_630 v0
  = coe d_maximum_484 (coe d_isBoundedLattice_572 (coe v0))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.minimum
d_minimum_632 :: T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny
d_minimum_632 v0
  = coe d_minimum_486 (coe d_isBoundedLattice_572 (coe v0))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.refl
d_refl_634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny
d_refl_634 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_refl_634 v11
du_refl_634 :: T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny
du_refl_634 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    let v3 = d_isPartialOrder_328 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.reflexive
d_reflexive_636 ::
  T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_636 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_328
            (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.supremum
d_supremum_638 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_638 v0
  = coe
      d_supremum_330
      (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.trans
d_trans_640 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_640 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_328
            (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.x∧y≤x
d_x'8743'y'8804'x_642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_642 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_x'8743'y'8804'x_642 v11
du_x'8743'y'8804'x_642 ::
  T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_642 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    coe du_x'8743'y'8804'x_184 (coe du_isMeetSemilattice_336 (coe v2))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.x∧y≤y
d_x'8743'y'8804'y_644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_644 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_x'8743'y'8804'y_644 v11
du_x'8743'y'8804'y_644 ::
  T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_644 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    coe du_x'8743'y'8804'y_196 (coe du_isMeetSemilattice_336 (coe v2))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.x≤x∨y
d_x'8804'x'8744'y_646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_646 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_x'8804'x'8744'y_646 v11
du_x'8804'x'8744'y_646 ::
  T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_646 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    coe du_x'8804'x'8744'y_38 (coe du_isJoinSemilattice_334 (coe v2))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.y≤x∨y
d_y'8804'x'8744'y_648 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_648 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_y'8804'x'8744'y_648 v11
du_y'8804'x'8744'y_648 ::
  T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_648 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    coe du_y'8804'x'8744'y_50 (coe du_isJoinSemilattice_334 (coe v2))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.∧-greatest
d_'8743''45'greatest_650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_650 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                         ~v10 v11
  = du_'8743''45'greatest_650 v11
du_'8743''45'greatest_650 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_650 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    coe
      du_'8743''45'greatest_210 (coe du_isMeetSemilattice_336 (coe v2))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.∨-least
d_'8744''45'least_652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_652 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_'8744''45'least_652 v11
du_'8744''45'least_652 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_652 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    coe du_'8744''45'least_64 (coe du_isJoinSemilattice_334 (coe v2))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.∼-resp-≈
d_'8764''45'resp'45''8776'_654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsHeytingAlgebra_556 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_654 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 v11
  = du_'8764''45'resp'45''8776'_654 v11
du_'8764''45'resp'45''8776'_654 ::
  T_IsHeytingAlgebra_556 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_654 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    let v3 = d_isPartialOrder_328 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_656 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 v11
  = du_'8764''45'resp'691''45''8776'_656 v11
du_'8764''45'resp'691''45''8776'_656 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_656 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    let v3 = d_isPartialOrder_328 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_658 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 v11
  = du_'8764''45'resp'737''45''8776'_658 v11
du_'8764''45'resp'737''45''8776'_658 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_658 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    let v3 = d_isPartialOrder_328 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.Eq.isPartialEquivalence
d_isPartialEquivalence_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsHeytingAlgebra_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_662 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 v11
  = du_isPartialEquivalence_662 v11
du_isPartialEquivalence_662 ::
  T_IsHeytingAlgebra_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_662 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    let v3 = d_isPartialOrder_328 (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v4))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.Eq.refl
d_refl_664 :: T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny
d_refl_664 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_328
               (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0))))))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.Eq.reflexive
d_reflexive_666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsHeytingAlgebra_556 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_666 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_reflexive_666 v11
du_reflexive_666 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_666 v0
  = let v1 = d_isBoundedLattice_572 (coe v0) in
    let v2 = d_isLattice_482 (coe v1) in
    let v3 = d_isPartialOrder_328 (coe v2) in
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
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.Eq.sym
d_sym_668 ::
  T_IsHeytingAlgebra_556 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_668 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_328
               (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0))))))
-- Relation.Binary.Lattice.Structures.IsHeytingAlgebra._.Eq.trans
d_trans_670 ::
  T_IsHeytingAlgebra_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_670 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_328
               (coe d_isLattice_482 (coe d_isBoundedLattice_572 (coe v0))))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra
d_IsBooleanAlgebra_682 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
newtype T_IsBooleanAlgebra_682
  = C_IsBooleanAlgebra'46'constructor_30293 T_IsHeytingAlgebra_556
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._⇨_
d__'8680'__702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8680'__702 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 v8 ~v9 ~v10 ~v11 v12
               v13
  = du__'8680'__702 v6 v8 v12 v13
du__'8680'__702 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8680'__702 v0 v1 v2 v3 = coe v0 (coe v1 v2) v3
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra.isHeytingAlgebra
d_isHeytingAlgebra_708 ::
  T_IsBooleanAlgebra_682 -> T_IsHeytingAlgebra_556
d_isHeytingAlgebra_708 v0
  = case coe v0 of
      C_IsBooleanAlgebra'46'constructor_30293 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.antisym
d_antisym_712 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_712 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         d_isPartialOrder_328
         (coe
            d_isLattice_482
            (coe
               d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.exponential
d_exponential_714 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_exponential_714 v0
  = coe d_exponential_574 (coe d_isHeytingAlgebra_708 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.infimum
d_infimum_716 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infimum_716 v0
  = coe
      d_infimum_332
      (coe
         d_isLattice_482
         (coe d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.isBoundedJoinSemilattice
d_isBoundedJoinSemilattice_718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_682 -> T_IsBoundedJoinSemilattice_110
d_isBoundedJoinSemilattice_718 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 v11
  = du_isBoundedJoinSemilattice_718 v11
du_isBoundedJoinSemilattice_718 ::
  T_IsBooleanAlgebra_682 -> T_IsBoundedJoinSemilattice_110
du_isBoundedJoinSemilattice_718 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    coe
      du_isBoundedJoinSemilattice_542
      (coe d_isBoundedLattice_572 (coe v1))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.isBoundedLattice
d_isBoundedLattice_720 ::
  T_IsBooleanAlgebra_682 -> T_IsBoundedLattice_466
d_isBoundedLattice_720 v0
  = coe d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.isBoundedMeetSemilattice
d_isBoundedMeetSemilattice_722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_682 -> T_IsBoundedMeetSemilattice_256
d_isBoundedMeetSemilattice_722 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 v11
  = du_isBoundedMeetSemilattice_722 v11
du_isBoundedMeetSemilattice_722 ::
  T_IsBooleanAlgebra_682 -> T_IsBoundedMeetSemilattice_256
du_isBoundedMeetSemilattice_722 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    coe
      du_isBoundedMeetSemilattice_544
      (coe d_isBoundedLattice_572 (coe v1))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.isEquivalence
d_isEquivalence_724 ::
  T_IsBooleanAlgebra_682 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_724 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_328
            (coe
               d_isLattice_482
               (coe
                  d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0))))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.isJoinSemilattice
d_isJoinSemilattice_726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_682 -> T_IsJoinSemilattice_22
d_isJoinSemilattice_726 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                        ~v10 v11
  = du_isJoinSemilattice_726 v11
du_isJoinSemilattice_726 ::
  T_IsBooleanAlgebra_682 -> T_IsJoinSemilattice_22
du_isJoinSemilattice_726 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    coe du_isJoinSemilattice_334 (coe d_isLattice_482 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.isLattice
d_isLattice_728 :: T_IsBooleanAlgebra_682 -> T_IsLattice_316
d_isLattice_728 v0
  = coe
      d_isLattice_482
      (coe d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.isMeetSemilattice
d_isMeetSemilattice_730 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_682 -> T_IsMeetSemilattice_168
d_isMeetSemilattice_730 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                        ~v10 v11
  = du_isMeetSemilattice_730 v11
du_isMeetSemilattice_730 ::
  T_IsBooleanAlgebra_682 -> T_IsMeetSemilattice_168
du_isMeetSemilattice_730 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    coe du_isMeetSemilattice_336 (coe d_isLattice_482 (coe v2))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.isPartialOrder
d_isPartialOrder_732 ::
  T_IsBooleanAlgebra_682 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_732 v0
  = coe
      d_isPartialOrder_328
      (coe
         d_isLattice_482
         (coe d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.isPreorder
d_isPreorder_734 ::
  T_IsBooleanAlgebra_682 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_734 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         d_isPartialOrder_328
         (coe
            d_isLattice_482
            (coe
               d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0)))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.maximum
d_maximum_736 :: T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny
d_maximum_736 v0
  = coe
      d_maximum_484
      (coe d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.minimum
d_minimum_738 :: T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny
d_minimum_738 v0
  = coe
      d_minimum_486
      (coe d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0)))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.refl
d_refl_740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny
d_refl_740 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_refl_740 v11
du_refl_740 :: T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny
du_refl_740 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    let v4 = d_isPartialOrder_328 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.reflexive
d_reflexive_742 ::
  T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_742 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_328
            (coe
               d_isLattice_482
               (coe
                  d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0))))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.supremum
d_supremum_744 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_744 v0
  = coe
      d_supremum_330
      (coe
         d_isLattice_482
         (coe d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.trans
d_trans_746 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_746 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            d_isPartialOrder_328
            (coe
               d_isLattice_482
               (coe
                  d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0))))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.transpose-⇨
d_transpose'45''8680'_748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_transpose'45''8680'_748 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                          ~v10 v11
  = du_transpose'45''8680'_748 v11
du_transpose'45''8680'_748 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_transpose'45''8680'_748 v0
  = coe
      du_transpose'45''8680'_582 (coe d_isHeytingAlgebra_708 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.transpose-∧
d_transpose'45''8743'_750 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_transpose'45''8743'_750 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                          ~v10 v11
  = du_transpose'45''8743'_750 v11
du_transpose'45''8743'_750 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_transpose'45''8743'_750 v0
  = coe
      du_transpose'45''8743'_598 (coe d_isHeytingAlgebra_708 (coe v0))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.x∧y≤x
d_x'8743'y'8804'x_752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'x_752 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_x'8743'y'8804'x_752 v11
du_x'8743'y'8804'x_752 ::
  T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'x_752 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    coe du_x'8743'y'8804'x_184 (coe du_isMeetSemilattice_336 (coe v3))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.x∧y≤y
d_x'8743'y'8804'y_754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8743'y'8804'y_754 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_x'8743'y'8804'y_754 v11
du_x'8743'y'8804'y_754 ::
  T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8743'y'8804'y_754 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    coe du_x'8743'y'8804'y_196 (coe du_isMeetSemilattice_336 (coe v3))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.x≤x∨y
d_x'8804'x'8744'y_756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'x'8744'y_756 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_x'8804'x'8744'y_756 v11
du_x'8804'x'8744'y_756 ::
  T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'x'8744'y_756 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    coe du_x'8804'x'8744'y_38 (coe du_isJoinSemilattice_334 (coe v3))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.y≤x∨y
d_y'8804'x'8744'y_758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny
d_y'8804'x'8744'y_758 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_y'8804'x'8744'y_758 v11
du_y'8804'x'8744'y_758 ::
  T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny
du_y'8804'x'8744'y_758 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    coe du_y'8804'x'8744'y_50 (coe du_isJoinSemilattice_334 (coe v3))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.∧-greatest
d_'8743''45'greatest_760 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'greatest_760 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                         ~v10 v11
  = du_'8743''45'greatest_760 v11
du_'8743''45'greatest_760 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'greatest_760 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    coe
      du_'8743''45'greatest_210 (coe du_isMeetSemilattice_336 (coe v3))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.∨-least
d_'8744''45'least_762 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'least_762 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      v11
  = du_'8744''45'least_762 v11
du_'8744''45'least_762 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'least_762 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    coe du_'8744''45'least_64 (coe du_isJoinSemilattice_334 (coe v3))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.∼-resp-≈
d_'8764''45'resp'45''8776'_764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_682 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_764 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               ~v9 ~v10 v11
  = du_'8764''45'resp'45''8776'_764 v11
du_'8764''45'resp'45''8776'_764 ::
  T_IsBooleanAlgebra_682 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_764 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    let v4 = d_isPartialOrder_328 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_766 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_766 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 v11
  = du_'8764''45'resp'691''45''8776'_766 v11
du_'8764''45'resp'691''45''8776'_766 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_766 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    let v4 = d_isPartialOrder_328 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_768 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                    ~v8 ~v9 ~v10 v11
  = du_'8764''45'resp'737''45''8776'_768 v11
du_'8764''45'resp'737''45''8776'_768 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_768 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    let v4 = d_isPartialOrder_328 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.Eq.isPartialEquivalence
d_isPartialEquivalence_772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_682 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_772 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 v11
  = du_isPartialEquivalence_772 v11
du_isPartialEquivalence_772 ::
  T_IsBooleanAlgebra_682 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_772 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    let v4 = d_isPartialOrder_328 (coe v3) in
    let v5
          = MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
              (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v5))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.Eq.refl
d_refl_774 :: T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny
d_refl_774 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_328
               (coe
                  d_isLattice_482
                  (coe
                     d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0)))))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.Eq.reflexive
d_reflexive_776 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_682 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_776 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_reflexive_776 v11
du_reflexive_776 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_776 v0
  = let v1 = d_isHeytingAlgebra_708 (coe v0) in
    let v2 = d_isBoundedLattice_572 (coe v1) in
    let v3 = d_isLattice_482 (coe v2) in
    let v4 = d_isPartialOrder_328 (coe v3) in
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
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.Eq.sym
d_sym_778 ::
  T_IsBooleanAlgebra_682 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_778 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_328
               (coe
                  d_isLattice_482
                  (coe
                     d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0)))))))
-- Relation.Binary.Lattice.Structures.IsBooleanAlgebra._.Eq.trans
d_trans_780 ::
  T_IsBooleanAlgebra_682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_780 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               d_isPartialOrder_328
               (coe
                  d_isLattice_482
                  (coe
                     d_isBoundedLattice_572 (coe d_isHeytingAlgebra_708 (coe v0)))))))
