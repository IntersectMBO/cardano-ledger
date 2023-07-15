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

module MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Definitions.RawMonoid
import qualified MAlonzo.Code.Algebra.Properties.AbelianGroup
import qualified MAlonzo.Code.Algebra.Properties.Ring
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing
d_IsAlmostCommutativeRing_26 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsAlmostCommutativeRing_26
  = C_IsAlmostCommutativeRing'46'constructor_1169 MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
                                                  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                                  (AgdaAny -> AgdaAny -> AgdaAny)
                                                  (AgdaAny -> AgdaAny -> AgdaAny)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing.isCommutativeSemiring
d_isCommutativeSemiring_62 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_62 v0
  = case coe v0 of
      C_IsAlmostCommutativeRing'46'constructor_1169 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing.-‿cong
d_'45''8255'cong_64 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_64 v0
  = case coe v0 of
      C_IsAlmostCommutativeRing'46'constructor_1169 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_70 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_70 v0
  = case coe v0 of
      C_IsAlmostCommutativeRing'46'constructor_1169 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing.-‿+-comm
d_'45''8255''43''45'comm_76 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_76 v0
  = case coe v0 of
      C_IsAlmostCommutativeRing'46'constructor_1169 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-assoc
d_'42''45'assoc_80 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_80 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_62 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-comm
d_'42''45'comm_82 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_82 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe d_isCommutativeSemiring_62 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-cong
d_'42''45'cong_84 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_84 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_62 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_86 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_86 v9
du_'8729''45'cong'691'_86 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_86 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_88 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_88 v9
du_'8729''45'cong'737'_88 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_88 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-identity
d_'42''45'identity_90 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_90 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_62 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.identityʳ
d_identity'691'_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
d_identity'691'_92 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_92 v9
du_identity'691'_92 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
du_identity'691'_92 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.identityˡ
d_identity'737'_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
d_identity'737'_94 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_94 v9
du_identity'737'_94 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
du_identity'737'_94 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_96 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMagma_96 v9
du_isCommutativeMagma_96 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_96 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_98 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                 ~v8 v9
  = du_'42''45'isCommutativeMonoid_98 v9
du_'42''45'isCommutativeMonoid_98 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_98 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_100 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                     ~v7 ~v8 v9
  = du_'42''45'isCommutativeSemigroup_100 v9
du_'42''45'isCommutativeSemigroup_100 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_100 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isMagma
d_'42''45'isMagma_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_102 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMagma_102 v9
du_'42''45'isMagma_102 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_102 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isMonoid
d_'42''45'isMonoid_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMonoid_104 v9
du_'42''45'isMonoid_104 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_104 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isSemigroup
d_'42''45'isSemigroup_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_106 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isSemigroup_106 v9
du_'42''45'isSemigroup_106 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_106 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.assoc
d_assoc_108 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_108 v0
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
                     (coe d_isCommutativeSemiring_62 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.comm
d_comm_110 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_110 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe d_isCommutativeSemiring_62 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-cong
d_'8729''45'cong_112 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_112 v0
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
                        (coe d_isCommutativeSemiring_62 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_114 v9
du_'8729''45'cong'691'_114 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_114 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_116 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_116 v9
du_'8729''45'cong'737'_116 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_116 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.identity
d_identity_118 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_118 v0
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
                  (coe d_isCommutativeSemiring_62 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.identityʳ
d_identity'691'_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
d_identity'691'_120 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_120 v9
du_identity'691'_120 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
du_identity'691'_120 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.identityˡ
d_identity'737'_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
d_identity'737'_122 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_122 v9
du_identity'737'_122 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
du_identity'737'_122 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_124 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMagma_124 v9
du_isCommutativeMagma_124 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_124 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_126 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_126 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_62 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isCommutativeSemigroup
d_isCommutativeSemigroup_128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_128 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeSemigroup_128 v9
du_isCommutativeSemigroup_128 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_128 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isMagma
d_isMagma_130 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_130 v0
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
                     (coe d_isCommutativeSemiring_62 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isMonoid
d_isMonoid_132 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_132 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe d_isCommutativeSemiring_62 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isSemigroup
d_isSemigroup_134 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_134 v0
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
                  (coe d_isCommutativeSemiring_62 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isUnitalMagma
d_isUnitalMagma_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_136 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isUnitalMagma_136 v9
du_isUnitalMagma_136 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_136 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.distrib
d_distrib_138 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_138 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe d_isCommutativeSemiring_62 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.distribʳ
d_distrib'691'_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_140 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_distrib'691'_140 v9
du_distrib'691'_140 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_140 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.distribˡ
d_distrib'737'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_142 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_distrib'737'_142 v9
du_distrib'737'_142 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_142 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_144 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                      ~v7 ~v8 v9
  = du_isCommutativeSemiringWithoutOne_144 v9
du_isCommutativeSemiringWithoutOne_144 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_144 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isEquivalence
d_isEquivalence_146 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_146 v0
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
                        (coe d_isCommutativeSemiring_62 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isNearSemiring
d_isNearSemiring_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_148 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isNearSemiring_148 v9
du_isNearSemiring_148 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_148 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isPartialEquivalence
d_isPartialEquivalence_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_150 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_150 v9
du_isPartialEquivalence_150 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_150 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isSemiring
d_isSemiring_152 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_152 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe d_isCommutativeSemiring_62 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_154 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_154 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe d_isCommutativeSemiring_62 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.isSemiringWithoutOne
d_isSemiringWithoutOne_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_156 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isSemiringWithoutOne_156 v9
du_isSemiringWithoutOne_156 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_156 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.refl
d_refl_158 :: T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
d_refl_158 v0
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
                           (coe d_isCommutativeSemiring_62 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.reflexive
d_reflexive_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_160 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_160 v9
du_reflexive_160 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_160 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.setoid
d_setoid_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_162 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_162 v9
du_setoid_162 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_162 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.sym
d_sym_164 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_164 v0
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
                           (coe d_isCommutativeSemiring_62 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.trans
d_trans_166 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_166 v0
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
                           (coe d_isCommutativeSemiring_62 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.zero
d_zero_168 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_168 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe d_isCommutativeSemiring_62 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.zeroʳ
d_zero'691'_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
d_zero'691'_170 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_zero'691'_170 v9
du_zero'691'_170 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
du_zero'691'_170 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.IsAlmostCommutativeRing._.zeroˡ
d_zero'737'_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
d_zero'737'_172 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_zero'737'_172 v9
du_zero'737'_172 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny
du_zero'737'_172 v0
  = let v1 = d_isCommutativeSemiring_62 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing
d_AlmostCommutativeRing_178 a0 a1 = ()
data T_AlmostCommutativeRing_178
  = C_AlmostCommutativeRing'46'constructor_6199 (AgdaAny ->
                                                 AgdaAny -> AgdaAny)
                                                (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                                AgdaAny (AgdaAny -> Maybe AgdaAny) AgdaAny
                                                T_IsAlmostCommutativeRing_26
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.Carrier
d_Carrier_204 :: T_AlmostCommutativeRing_178 -> ()
d_Carrier_204 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._≈_
d__'8776'__206 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> ()
d__'8776'__206 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._+_
d__'43'__208 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__208 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_6199 v3 v4 v5 v6 v7 v8 v9
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._*_
d__'42'__210 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__210 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_6199 v3 v4 v5 v6 v7 v8 v9
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.-_
d_'45'__212 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_'45'__212 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_6199 v3 v4 v5 v6 v7 v8 v9
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.0#
d_0'35'_214 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_0'35'_214 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_6199 v3 v4 v5 v6 v7 v8 v9
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.0≟_
d_0'8799'__218 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Maybe AgdaAny
d_0'8799'__218 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_6199 v3 v4 v5 v6 v7 v8 v9
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.1#
d_1'35'_220 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_1'35'_220 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_6199 v3 v4 v5 v6 v7 v8 v9
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.isAlmostCommutativeRing
d_isAlmostCommutativeRing_222 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_222 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_6199 v3 v4 v5 v6 v7 v8 v9
        -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-assoc
d_'42''45'assoc_226 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_226 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-comm
d_'42''45'comm_228 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_228 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-cong
d_'42''45'cong_230 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_230 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_232 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_232 v2
du_'8729''45'cong'691'_232 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_232 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_234 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_234 v2
du_'8729''45'cong'737'_234 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_234 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-identity
d_'42''45'identity_236 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_236 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.identityʳ
d_identity'691'_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_238 ~v0 ~v1 v2 = du_identity'691'_238 v2
du_identity'691'_238 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_238 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.identityˡ
d_identity'737'_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_240 ~v0 ~v1 v2 = du_identity'737'_240 v2
du_identity'737'_240 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_240 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_242 ~v0 ~v1 v2 = du_isCommutativeMagma_242 v2
du_isCommutativeMagma_242 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_242 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_244 ~v0 ~v1 v2
  = du_'42''45'isCommutativeMonoid_244 v2
du_'42''45'isCommutativeMonoid_244 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_244 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_246 ~v0 ~v1 v2
  = du_'42''45'isCommutativeSemigroup_246 v2
du_'42''45'isCommutativeSemigroup_246 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_246 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-isMagma
d_'42''45'isMagma_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_248 ~v0 ~v1 v2 = du_'42''45'isMagma_248 v2
du_'42''45'isMagma_248 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_248 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-isMonoid
d_'42''45'isMonoid_250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_250 ~v0 ~v1 v2 = du_'42''45'isMonoid_250 v2
du_'42''45'isMonoid_250 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_250 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-isSemigroup
d_'42''45'isSemigroup_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_252 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_252 v2
du_'42''45'isSemigroup_252 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_252 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.assoc
d_assoc_254 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_254 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.comm
d_comm_256 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_256 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.∙-cong
d_'8729''45'cong_258 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_258 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_260 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_260 v2
du_'8729''45'cong'691'_260 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_260 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_262 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_262 v2
du_'8729''45'cong'737'_262 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_262 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.identity
d_identity_264 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_264 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.identityʳ
d_identity'691'_266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_266 ~v0 ~v1 v2 = du_identity'691'_266 v2
du_identity'691'_266 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_266 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.identityˡ
d_identity'737'_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_268 ~v0 ~v1 v2 = du_identity'737'_268 v2
du_identity'737'_268 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_268 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_270 ~v0 ~v1 v2 = du_isCommutativeMagma_270 v2
du_isCommutativeMagma_270 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_270 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_272 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_272 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeSemigroup
d_isCommutativeSemigroup_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_274 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_274 v2
du_isCommutativeSemigroup_274 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_274 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isMagma
d_isMagma_276 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_276 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isMonoid
d_isMonoid_278 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_278 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isSemigroup
d_isSemigroup_280 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_280 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isUnitalMagma
d_isUnitalMagma_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_282 ~v0 ~v1 v2 = du_isUnitalMagma_282 v2
du_isUnitalMagma_282 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_282 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_284 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_284 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.-‿+-comm
d_'45''8255''43''45'comm_286 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_286 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.-‿cong
d_'45''8255'cong_288 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_288 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.distrib
d_distrib_290 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_290 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.distribʳ
d_distrib'691'_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_292 ~v0 ~v1 v2 = du_distrib'691'_292 v2
du_distrib'691'_292 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_292 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.distribˡ
d_distrib'737'_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_294 ~v0 ~v1 v2 = du_distrib'737'_294 v2
du_distrib'737'_294 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_294 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeSemiring
d_isCommutativeSemiring_296 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_296 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_298 ~v0 ~v1 v2
  = du_isCommutativeSemiringWithoutOne_298 v2
du_isCommutativeSemiringWithoutOne_298 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_298 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isEquivalence
d_isEquivalence_300 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_300 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isNearSemiring
d_isNearSemiring_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_302 ~v0 ~v1 v2 = du_isNearSemiring_302 v2
du_isNearSemiring_302 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_302 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isPartialEquivalence
d_isPartialEquivalence_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_304 ~v0 ~v1 v2
  = du_isPartialEquivalence_304 v2
du_isPartialEquivalence_304 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_304 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isSemiring
d_isSemiring_306 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_306 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_308 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_308 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.isSemiringWithoutOne
d_isSemiringWithoutOne_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_310 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_310 v2
du_isSemiringWithoutOne_310 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_310 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.reflexive
d_reflexive_312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_312 ~v0 ~v1 v2 = du_reflexive_312 v2
du_reflexive_312 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_312 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.setoid
d_setoid_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_314 ~v0 ~v1 v2 = du_setoid_314 v2
du_setoid_314 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_314 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.sym
d_sym_316 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_316 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.trans
d_trans_318 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_318 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.zero
d_zero_320 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_320 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.zeroʳ
d_zero'691'_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'691'_322 ~v0 ~v1 v2 = du_zero'691'_322 v2
du_zero'691'_322 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_322 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.zeroˡ
d_zero'737'_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'737'_324 ~v0 ~v1 v2 = du_zero'737'_324 v2
du_zero'737'_324 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_324 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.commutativeSemiring
d_commutativeSemiring_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_326 ~v0 ~v1 v2
  = du_commutativeSemiring_326 v2
du_commutativeSemiring_326 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_326 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      (d__'43'__208 (coe v0)) (d__'42'__210 (coe v0))
      (d_0'35'_214 (coe v0)) (d_1'35'_220 (coe v0))
      (d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-commutativeMonoid
d_'42''45'commutativeMonoid_330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_330 ~v0 ~v1 v2
  = du_'42''45'commutativeMonoid_330 v2
du_'42''45'commutativeMonoid_330 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_330 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.*-monoid
d_'42''45'monoid_332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_332 ~v0 ~v1 v2 = du_'42''45'monoid_332 v2
du_'42''45'monoid_332 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_332 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.semigroup
d_semigroup_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_334 ~v0 ~v1 v2 = du_semigroup_334 v2
du_semigroup_334 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_334 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.+-commutativeMonoid
d_'43''45'commutativeMonoid_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_336 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_336 v2
du_'43''45'commutativeMonoid_336 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_336 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.monoid
d_monoid_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_338 ~v0 ~v1 v2 = du_monoid_338 v2
du_monoid_338 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_338 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_monoid_890
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.semigroup
d_semigroup_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_340 ~v0 ~v1 v2 = du_semigroup_340 v2
du_semigroup_340 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_340 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.rawSemiring
d_rawSemiring_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_342 ~v0 ~v1 v2 = du_rawSemiring_342 v2
du_rawSemiring_342 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_342 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.semiring
d_semiring_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_344 ~v0 ~v1 v2 = du_semiring_344 v2
du_semiring_344 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_344 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.rawRing
d_rawRing_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_346 ~v0 ~v1 v2 = du_rawRing_346 v2
du_rawRing_346 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_346 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawRing'46'constructor_3463
      (d__'43'__208 (coe v0)) (d__'42'__210 (coe v0))
      (d_'45'__212 (coe v0)) (d_0'35'_214 (coe v0))
      (d_1'35'_220 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._^_
d__'94'__348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> Integer -> AgdaAny
d__'94'__348 ~v0 ~v1 v2 v3 v4 = du__'94'__348 v2 v3 v4
du__'94'__348 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Integer -> AgdaAny
du__'94'__348 v0 v1 v2
  = let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (let v3
                     = coe
                         MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                         (coe du_commutativeSemiring_326 (coe v0)) in
               coe
                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                 (coe
                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                    (coe v3))) in
    let v4 = subInt (coe v2) (coe (1 :: Integer)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
              (coe
                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                 (coe v3) (coe v4) (coe v1))
              v1 in
    case coe v2 of
      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
      1 -> coe v1
      _ -> coe v5
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._-_
d__'45'__350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__350 ~v0 ~v1 v2 v3 v4 = du__'45'__350 v2 v3 v4
du__'45'__350 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__350 v0 v1 v2
  = coe d__'43'__208 v0 v1 (coe d_'45'__212 v0 v2)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.refl
d_refl_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_refl_358 ~v0 ~v1 v2 v3 = du_refl_358 v2 v3
du_refl_358 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_refl_358 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
      v1
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_
d__'45'Raw'45'AlmostCommutative'10230'__372 a0 a1 a2 a3 a4 a5 = ()
data T__'45'Raw'45'AlmostCommutative'10230'__372
  = C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_10401 (AgdaAny ->
                                                                  AgdaAny)
                                                                 (AgdaAny -> AgdaAny -> AgdaAny)
                                                                 (AgdaAny -> AgdaAny -> AgdaAny)
                                                                 (AgdaAny -> AgdaAny) AgdaAny
                                                                 AgdaAny
-- Tactic.RingSolver.Core.AlmostCommutativeRing.F._*_
d__'42'__388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__388 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'42'__388 v4
du__'42'__388 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'42'__388 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.F._+_
d__'43'__390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__390 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'43'__390 v4
du__'43'__390 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'43'__390 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.F.-_
d_'45'__406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_'45'__406 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_'45'__406 v4
du_'45'__406 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny
du_'45'__406 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.F.0#
d_0'35'_408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny
d_0'35'_408 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_0'35'_408 v4
du_0'35'_408 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 -> AgdaAny
du_0'35'_408 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.F.1#
d_1'35'_410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny
d_1'35'_410 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_1'35'_410 v4
du_1'35'_410 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 -> AgdaAny
du_1'35'_410 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.F.Carrier
d_Carrier_412 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> ()
d_Carrier_412 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T._*_
d__'42'__418 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__418 v0 = coe d__'42'__210 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T._+_
d__'43'__420 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__420 v0 = coe d__'43'__208 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T._-_
d__'45'__422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__422 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'45'__422 v5
du__'45'__422 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__422 v0 = coe du__'45'__350 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T._^_
d__'94'__424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> Integer -> AgdaAny
d__'94'__424 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'94'__424 v5
du__'94'__424 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Integer -> AgdaAny
du__'94'__424 v0 = coe du__'94'__348 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T._≈_
d__'8776'__426 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> ()
d__'8776'__426 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-assoc
d_'42''45'assoc_428 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_428 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-comm
d_'42''45'comm_430 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_430 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-commutativeMonoid
d_'42''45'commutativeMonoid_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_432 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'commutativeMonoid_432 v5
du_'42''45'commutativeMonoid_432 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_432 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-cong
d_'42''45'cong_434 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_434 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.∙-congʳ
d_'8729''45'cong'691'_436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_436 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_436 v5
du_'8729''45'cong'691'_436 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_436 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.∙-congˡ
d_'8729''45'cong'737'_438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_438 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_438 v5
du_'8729''45'cong'737'_438 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_438 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-identity
d_'42''45'identity_440 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_440 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.identityʳ
d_identity'691'_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_442 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_identity'691'_442 v5
du_identity'691'_442 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_442 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.identityˡ
d_identity'737'_444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_444 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_identity'737'_444 v5
du_identity'737'_444 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_444 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isCommutativeMagma
d_isCommutativeMagma_446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_446 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isCommutativeMagma_446 v5
du_isCommutativeMagma_446 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_446 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_448 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isCommutativeMonoid_448 v5
du_'42''45'isCommutativeMonoid_448 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_448 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_450 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isCommutativeSemigroup_450 v5
du_'42''45'isCommutativeSemigroup_450 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_450 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-isMagma
d_'42''45'isMagma_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_452 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isMagma_452 v5
du_'42''45'isMagma_452 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_452 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-isMonoid
d_'42''45'isMonoid_454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_454 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isMonoid_454 v5
du_'42''45'isMonoid_454 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_454 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-isSemigroup
d_'42''45'isSemigroup_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_456 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isSemigroup_456 v5
du_'42''45'isSemigroup_456 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_456 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.*-monoid
d_'42''45'monoid_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_458 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'monoid_458 v5
du_'42''45'monoid_458 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_458 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.semigroup
d_semigroup_460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_460 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_semigroup_460 v5
du_semigroup_460 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_460 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.assoc
d_assoc_462 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_462 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.comm
d_comm_464 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_464 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.+-commutativeMonoid
d_'43''45'commutativeMonoid_466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_466 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'43''45'commutativeMonoid_466 v5
du_'43''45'commutativeMonoid_466 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_466 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.∙-cong
d_'8729''45'cong_468 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_468 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.∙-congʳ
d_'8729''45'cong'691'_470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_470 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_470 v5
du_'8729''45'cong'691'_470 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_470 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.∙-congˡ
d_'8729''45'cong'737'_472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_472 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_472 v5
du_'8729''45'cong'737'_472 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_472 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.identity
d_identity_474 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_474 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.identityʳ
d_identity'691'_476 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_476 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_identity'691'_476 v5
du_identity'691'_476 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_476 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.identityˡ
d_identity'737'_478 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_478 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_identity'737'_478 v5
du_identity'737'_478 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_478 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isCommutativeMagma
d_isCommutativeMagma_480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_480 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isCommutativeMagma_480 v5
du_isCommutativeMagma_480 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_480 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_482 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_482 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isCommutativeSemigroup
d_isCommutativeSemigroup_484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_484 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isCommutativeSemigroup_484 v5
du_isCommutativeSemigroup_484 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_484 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isMagma
d_isMagma_486 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_486 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isMonoid
d_isMonoid_488 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_488 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isSemigroup
d_isSemigroup_490 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_490 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isUnitalMagma
d_isUnitalMagma_492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_492 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isUnitalMagma_492 v5
du_isUnitalMagma_492 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_492 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.monoid
d_monoid_494 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_494 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_monoid_494 v5
du_monoid_494 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_494 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_monoid_890
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.semigroup
d_semigroup_496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_496 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_semigroup_496 v5
du_semigroup_496 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_496 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.-_
d_'45'__498 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_'45'__498 v0 = coe d_'45'__212 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_500 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_500 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.-‿+-comm
d_'45''8255''43''45'comm_502 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_502 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.-‿cong
d_'45''8255'cong_504 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_504 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.0#
d_0'35'_506 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_0'35'_506 v0 = coe d_0'35'_214 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.0≟_
d_0'8799'__508 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Maybe AgdaAny
d_0'8799'__508 v0 = coe d_0'8799'__218 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.1#
d_1'35'_510 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_1'35'_510 v0 = coe d_1'35'_220 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.Carrier
d_Carrier_512 :: T_AlmostCommutativeRing_178 -> ()
d_Carrier_512 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.commutativeSemiring
d_commutativeSemiring_514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_514 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_commutativeSemiring_514 v5
du_commutativeSemiring_514 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_514 v0
  = coe du_commutativeSemiring_326 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.distrib
d_distrib_516 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_516 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.distribʳ
d_distrib'691'_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_518 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_distrib'691'_518 v5
du_distrib'691'_518 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_518 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.distribˡ
d_distrib'737'_520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_520 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_distrib'737'_520 v5
du_distrib'737'_520 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_520 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isAlmostCommutativeRing
d_isAlmostCommutativeRing_522 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_522 v0
  = coe d_isAlmostCommutativeRing_222 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isCommutativeSemiring
d_isCommutativeSemiring_524 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_524 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_526 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isCommutativeSemiringWithoutOne_526 v5
du_isCommutativeSemiringWithoutOne_526 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_526 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isEquivalence
d_isEquivalence_528 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_528 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isNearSemiring
d_isNearSemiring_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_530 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isNearSemiring_530 v5
du_isNearSemiring_530 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_530 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isPartialEquivalence
d_isPartialEquivalence_532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_532 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_532 v5
du_isPartialEquivalence_532 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_532 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isSemiring
d_isSemiring_534 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_534 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_536 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_536 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.isSemiringWithoutOne
d_isSemiringWithoutOne_538 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_538 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isSemiringWithoutOne_538 v5
du_isSemiringWithoutOne_538 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_538 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.rawRing
d_rawRing_540 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_540 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_rawRing_540 v5
du_rawRing_540 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_540 v0 = coe du_rawRing_346 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.rawSemiring
d_rawSemiring_542 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_542 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_rawSemiring_542 v5
du_rawSemiring_542 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_542 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.refl
d_refl_544 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_refl_544 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_refl_544 v5
du_refl_544 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_refl_544 v0 = coe du_refl_358 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.reflexive
d_reflexive_546 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_546 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_546 v5
du_reflexive_546 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_546 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.semiring
d_semiring_548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_548 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_semiring_548 v5
du_semiring_548 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_548 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.setoid
d_setoid_550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_550 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_550 v5
du_setoid_550 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_550 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.sym
d_sym_552 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_552 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.trans
d_trans_554 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_554 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.zero
d_zero_556 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_556 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.zeroʳ
d_zero'691'_558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'691'_558 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_zero'691'_558 v5
du_zero'691'_558 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_558 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.T.zeroˡ
d_zero'737'_560 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'737'_560 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_zero'737'_560 v5
du_zero'737'_560 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_560 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._.Homomorphic₀
d_Homomorphic'8320'_564 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_Homomorphic'8320'_564 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._.Homomorphic₁
d_Homomorphic'8321'_566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8321'_566 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._.Homomorphic₂
d_Homomorphic'8322'_568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8322'_568 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._.Morphism
d_Morphism_570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> ()
d_Morphism_570 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T._*_
d__'42'__616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__616 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du__'42'__616 v5
du__'42'__616 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'42'__616 v0 = coe d__'42'__210 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T._+_
d__'43'__618 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43'__618 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du__'43'__618 v5
du__'43'__618 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'43'__618 v0 = coe d__'43'__208 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T._-_
d__'45'__620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'45'__620 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du__'45'__620 v5
du__'45'__620 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__620 v0 = coe du__'45'__350 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T._^_
d__'94'__622 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> Integer -> AgdaAny
d__'94'__622 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du__'94'__622 v5
du__'94'__622 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Integer -> AgdaAny
du__'94'__622 v0 = coe du__'94'__348 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T._≈_
d__'8776'__624 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__624 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-assoc
d_'42''45'assoc_626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_626 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'assoc_626 v5
du_'42''45'assoc_626 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'assoc_626 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-comm
d_'42''45'comm_628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_628 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'comm_628 v5
du_'42''45'comm_628 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'comm_628 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-commutativeMonoid
d_'42''45'commutativeMonoid_630 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_630 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'commutativeMonoid_630 v5
du_'42''45'commutativeMonoid_630 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_630 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-cong
d_'42''45'cong_632 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_632 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'cong_632 v5
du_'42''45'cong_632 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'cong_632 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-congʳ
d_'8729''45'cong'691'_634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_634 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong'691'_634 v5
du_'8729''45'cong'691'_634 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_634 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-congˡ
d_'8729''45'cong'737'_636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_636 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong'737'_636 v5
du_'8729''45'cong'737'_636 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_636 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-identity
d_'42''45'identity_638 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_638 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'identity_638 v5
du_'42''45'identity_638 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'42''45'identity_638 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identityʳ
d_identity'691'_640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_identity'691'_640 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_identity'691'_640 v5
du_identity'691'_640 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_640 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identityˡ
d_identity'737'_642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_identity'737'_642 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_identity'737'_642 v5
du_identity'737'_642 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_642 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeMagma
d_isCommutativeMagma_644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_644 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeMagma_644 v5
du_isCommutativeMagma_644 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_644 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_646 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isCommutativeMonoid_646 v5
du_'42''45'isCommutativeMonoid_646 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_646 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_648 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_648 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isCommutativeSemigroup_648 v5
du_'42''45'isCommutativeSemigroup_648 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_648 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isMagma
d_'42''45'isMagma_650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_650 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isMagma_650 v5
du_'42''45'isMagma_650 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_650 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isMonoid
d_'42''45'isMonoid_652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_652 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isMonoid_652 v5
du_'42''45'isMonoid_652 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_652 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isSemigroup
d_'42''45'isSemigroup_654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_654 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isSemigroup_654 v5
du_'42''45'isSemigroup_654 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_654 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-monoid
d_'42''45'monoid_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_656 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'monoid_656 v5
du_'42''45'monoid_656 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_656 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.semigroup
d_semigroup_658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_658 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_semigroup_658 v5
du_semigroup_658 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_658 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.assoc
d_assoc_660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_660 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_assoc_660 v5
du_assoc_660 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_660 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.comm
d_comm_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_662 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_comm_662 v5
du_comm_662 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_662 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.+-commutativeMonoid
d_'43''45'commutativeMonoid_664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_664 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'43''45'commutativeMonoid_664 v5
du_'43''45'commutativeMonoid_664 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_664 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-cong
d_'8729''45'cong_666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_666 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong_666 v5
du_'8729''45'cong_666 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong_666 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-congʳ
d_'8729''45'cong'691'_668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_668 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong'691'_668 v5
du_'8729''45'cong'691'_668 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_668 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-congˡ
d_'8729''45'cong'737'_670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_670 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong'737'_670 v5
du_'8729''45'cong'737'_670 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_670 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identity
d_identity_672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_672 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_identity_672 v5
du_identity_672 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_identity_672 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identityʳ
d_identity'691'_674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_identity'691'_674 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_identity'691'_674 v5
du_identity'691'_674 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_674 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identityˡ
d_identity'737'_676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_identity'737'_676 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_identity'737'_676 v5
du_identity'737'_676 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_676 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeMagma
d_isCommutativeMagma_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_678 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeMagma_678 v5
du_isCommutativeMagma_678 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_678 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_680 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'43''45'isCommutativeMonoid_680 v5
du_'43''45'isCommutativeMonoid_680 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'43''45'isCommutativeMonoid_680 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeSemigroup
d_isCommutativeSemigroup_682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_682 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeSemigroup_682 v5
du_isCommutativeSemigroup_682 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_682 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isMagma
d_isMagma_684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_684 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_isMagma_684 v5
du_isMagma_684 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_684 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isMonoid
d_isMonoid_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_686 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_isMonoid_686 v5
du_isMonoid_686 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_isMonoid_686 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isSemigroup
d_isSemigroup_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_688 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isSemigroup_688 v5
du_isSemigroup_688 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_688 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isUnitalMagma
d_isUnitalMagma_690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_690 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isUnitalMagma_690 v5
du_isUnitalMagma_690 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_690 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.monoid
d_monoid_692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_692 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_monoid_692 v5
du_monoid_692 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_692 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_monoid_890
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.semigroup
d_semigroup_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_694 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_semigroup_694 v5
du_semigroup_694 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_694 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.-_
d_'45'__696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_'45'__696 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_'45'__696 v5
du_'45'__696 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_'45'__696 v0 = coe d_'45'__212 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_698 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'45''8255''42''45'distrib'737'_698 v5
du_'45''8255''42''45'distrib'737'_698 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255''42''45'distrib'737'_698 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.-‿+-comm
d_'45''8255''43''45'comm_700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_700 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'45''8255''43''45'comm_700 v5
du_'45''8255''43''45'comm_700 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255''43''45'comm_700 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.-‿cong
d_'45''8255'cong_702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_702 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'45''8255'cong_702 v5
du_'45''8255'cong_702 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255'cong_702 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.0#
d_0'35'_704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny
d_0'35'_704 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_0'35'_704 v5
du_0'35'_704 :: T_AlmostCommutativeRing_178 -> AgdaAny
du_0'35'_704 v0 = coe d_0'35'_214 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.0≟_
d_0'8799'__706 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> Maybe AgdaAny
d_0'8799'__706 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_0'8799'__706 v5
du_0'8799'__706 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Maybe AgdaAny
du_0'8799'__706 v0 = coe d_0'8799'__218 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.1#
d_1'35'_708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny
d_1'35'_708 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_1'35'_708 v5
du_1'35'_708 :: T_AlmostCommutativeRing_178 -> AgdaAny
du_1'35'_708 v0 = coe d_1'35'_220 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.Carrier
d_Carrier_710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> ()
d_Carrier_710 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.commutativeSemiring
d_commutativeSemiring_712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_712 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_commutativeSemiring_712 v5
du_commutativeSemiring_712 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_712 v0
  = coe du_commutativeSemiring_326 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.distrib
d_distrib_714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_714 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_distrib_714 v5
du_distrib_714 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_distrib_714 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.distribʳ
d_distrib'691'_716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_716 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_distrib'691'_716 v5
du_distrib'691'_716 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_716 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.distribˡ
d_distrib'737'_718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_718 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_distrib'737'_718 v5
du_distrib'737'_718 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_718 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isAlmostCommutativeRing
d_isAlmostCommutativeRing_720 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_720 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isAlmostCommutativeRing_720 v5
du_isAlmostCommutativeRing_720 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
du_isAlmostCommutativeRing_720 v0
  = coe d_isAlmostCommutativeRing_222 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeSemiring
d_isCommutativeSemiring_722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_722 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeSemiring_722 v5
du_isCommutativeSemiring_722 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_isCommutativeSemiring_722 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_724 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeSemiringWithoutOne_724 v5
du_isCommutativeSemiringWithoutOne_724 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_724 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isEquivalence
d_isEquivalence_726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_726 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isEquivalence_726 v5
du_isEquivalence_726 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_726 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isNearSemiring
d_isNearSemiring_728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_728 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isNearSemiring_728 v5
du_isNearSemiring_728 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_728 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isPartialEquivalence
d_isPartialEquivalence_730 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_730 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isPartialEquivalence_730 v5
du_isPartialEquivalence_730 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_730 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isSemiring
d_isSemiring_732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_732 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_isSemiring_732 v5
du_isSemiring_732 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
du_isSemiring_732 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_734 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isSemiringWithoutAnnihilatingZero_734 v5
du_isSemiringWithoutAnnihilatingZero_734 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
du_isSemiringWithoutAnnihilatingZero_734 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isSemiringWithoutOne
d_isSemiringWithoutOne_736 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_736 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isSemiringWithoutOne_736 v5
du_isSemiringWithoutOne_736 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_736 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.rawRing
d_rawRing_738 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_738 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_rawRing_738 v5
du_rawRing_738 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_738 v0 = coe du_rawRing_346 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.rawSemiring
d_rawSemiring_740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_740 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_rawSemiring_740 v5
du_rawSemiring_740 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_740 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.refl
d_refl_742 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_refl_742 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_refl_742 v5
du_refl_742 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_refl_742 v0 = coe du_refl_358 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.reflexive
d_reflexive_744 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_744 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_reflexive_744 v5
du_reflexive_744 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_744 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.semiring
d_semiring_746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_746 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_semiring_746 v5
du_semiring_746 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_746 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.setoid
d_setoid_748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_748 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_setoid_748 v5
du_setoid_748 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_748 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.sym
d_sym_750 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_750 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_sym_750 v5
du_sym_750 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_750 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.trans
d_trans_752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_752 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_trans_752 v5
du_trans_752 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_752 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.zero
d_zero_754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_754 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_zero_754 v5
du_zero_754 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_754 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.zeroʳ
d_zero'691'_756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_zero'691'_756 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_zero'691'_756 v5
du_zero'691'_756 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_756 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.zeroˡ
d_zero'737'_758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_zero'737'_758 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_zero'737'_758 v5
du_zero'737'_758 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_758 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.⟦_⟧
d_'10214'_'10215'_770 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_'10214'_'10215'_770 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_10401 v1 v2 v3 v4 v5 v6
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.+-homo
d_'43''45'homo_772 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'43''45'homo_772 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_10401 v1 v2 v3 v4 v5 v6
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.*-homo
d_'42''45'homo_774 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_774 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_10401 v1 v2 v3 v4 v5 v6
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.-‿homo
d_'45''8255'homo_776 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
d_'45''8255'homo_776 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_10401 v1 v2 v3 v4 v5 v6
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.0-homo
d_0'45'homo_778 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny
d_0'45'homo_778 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_10401 v1 v2 v3 v4 v5 v6
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.1-homo
d_1'45'homo_780 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny
d_1'45'homo_780 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_10401 v1 v2 v3 v4 v5 v6
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.AlmostCommutativeRing.-raw-almostCommutative⟶
d_'45'raw'45'almostCommutative'10230'_788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372
d_'45'raw'45'almostCommutative'10230'_788 ~v0 ~v1 v2
  = du_'45'raw'45'almostCommutative'10230'_788 v2
du_'45'raw'45'almostCommutative'10230'_788 ::
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372
du_'45'raw'45'almostCommutative'10230'_788 v0
  = coe
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_10401
      (coe (\ v1 -> v1))
      (coe
         (\ v1 v2 ->
            coe
              du_refl_358 (coe v0)
              (let v3 = coe du_rawRing_346 (coe v0) in
               coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v3 v1 v2)))
      (coe
         (\ v1 v2 ->
            coe
              du_refl_358 (coe v0)
              (let v3 = coe du_rawRing_346 (coe v0) in
               coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v3 v1 v2)))
      (coe
         (\ v1 ->
            coe
              du_refl_358 (coe v0)
              (let v2 = coe du_rawRing_346 (coe v0) in
               coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v2 v1)))
      (coe
         du_refl_358 (coe v0)
         (let v1 = coe du_rawRing_346 (coe v0) in
          coe MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1)))
      (coe
         du_refl_358 (coe v0)
         (let v1 = coe du_rawRing_346 (coe v0) in
          coe MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274 (coe v1)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._*_
d__'42'__798 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__798 v0 = coe d__'42'__210 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._+_
d__'43'__800 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__800 v0 = coe d__'43'__208 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._-_
d__'45'__802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__802 ~v0 ~v1 v2 = du__'45'__802 v2
du__'45'__802 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__802 v0 = coe du__'45'__350 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._^_
d__'94'__804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> Integer -> AgdaAny
d__'94'__804 ~v0 ~v1 v2 = du__'94'__804 v2
du__'94'__804 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Integer -> AgdaAny
du__'94'__804 v0 = coe du__'94'__348 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._≈_
d__'8776'__806 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> ()
d__'8776'__806 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-assoc
d_'42''45'assoc_808 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_808 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-comm
d_'42''45'comm_810 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_810 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-commutativeMonoid
d_'42''45'commutativeMonoid_812 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_812 ~v0 ~v1 v2
  = du_'42''45'commutativeMonoid_812 v2
du_'42''45'commutativeMonoid_812 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_812 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-cong
d_'42''45'cong_814 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_814 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-congʳ
d_'8729''45'cong'691'_816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_816 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_816 v2
du_'8729''45'cong'691'_816 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_816 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-congˡ
d_'8729''45'cong'737'_818 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_818 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_818 v2
du_'8729''45'cong'737'_818 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_818 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-identity
d_'42''45'identity_820 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_820 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identityʳ
d_identity'691'_822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_822 ~v0 ~v1 v2 = du_identity'691'_822 v2
du_identity'691'_822 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_822 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identityˡ
d_identity'737'_824 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_824 ~v0 ~v1 v2 = du_identity'737'_824 v2
du_identity'737'_824 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_824 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeMagma
d_isCommutativeMagma_826 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_826 ~v0 ~v1 v2 = du_isCommutativeMagma_826 v2
du_isCommutativeMagma_826 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_826 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_828 ~v0 ~v1 v2
  = du_'42''45'isCommutativeMonoid_828 v2
du_'42''45'isCommutativeMonoid_828 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_828 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_830 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_830 ~v0 ~v1 v2
  = du_'42''45'isCommutativeSemigroup_830 v2
du_'42''45'isCommutativeSemigroup_830 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_830 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isMagma
d_'42''45'isMagma_832 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_832 ~v0 ~v1 v2 = du_'42''45'isMagma_832 v2
du_'42''45'isMagma_832 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_832 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isMonoid
d_'42''45'isMonoid_834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_834 ~v0 ~v1 v2 = du_'42''45'isMonoid_834 v2
du_'42''45'isMonoid_834 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_834 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isSemigroup
d_'42''45'isSemigroup_836 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_836 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_836 v2
du_'42''45'isSemigroup_836 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_836 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-monoid
d_'42''45'monoid_838 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_838 ~v0 ~v1 v2 = du_'42''45'monoid_838 v2
du_'42''45'monoid_838 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_838 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.semigroup
d_semigroup_840 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_840 ~v0 ~v1 v2 = du_semigroup_840 v2
du_semigroup_840 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_840 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.assoc
d_assoc_842 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_842 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.comm
d_comm_844 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_844 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.+-commutativeMonoid
d_'43''45'commutativeMonoid_846 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_846 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_846 v2
du_'43''45'commutativeMonoid_846 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_846 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-cong
d_'8729''45'cong_848 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_848 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-congʳ
d_'8729''45'cong'691'_850 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_850 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_850 v2
du_'8729''45'cong'691'_850 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_850 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-congˡ
d_'8729''45'cong'737'_852 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_852 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_852 v2
du_'8729''45'cong'737'_852 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_852 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identity
d_identity_854 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_854 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identityʳ
d_identity'691'_856 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_856 ~v0 ~v1 v2 = du_identity'691'_856 v2
du_identity'691'_856 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_856 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identityˡ
d_identity'737'_858 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_858 ~v0 ~v1 v2 = du_identity'737'_858 v2
du_identity'737'_858 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_858 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeMagma
d_isCommutativeMagma_860 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_860 ~v0 ~v1 v2 = du_isCommutativeMagma_860 v2
du_isCommutativeMagma_860 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_860 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_862 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_862 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeSemigroup
d_isCommutativeSemigroup_864 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_864 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_864 v2
du_isCommutativeSemigroup_864 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_864 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isMagma
d_isMagma_866 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_866 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isMonoid
d_isMonoid_868 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_868 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isSemigroup
d_isSemigroup_870 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_870 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isUnitalMagma
d_isUnitalMagma_872 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_872 ~v0 ~v1 v2 = du_isUnitalMagma_872 v2
du_isUnitalMagma_872 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_872 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.monoid
d_monoid_874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_874 ~v0 ~v1 v2 = du_monoid_874 v2
du_monoid_874 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_874 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_monoid_890
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.semigroup
d_semigroup_876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_876 ~v0 ~v1 v2 = du_semigroup_876 v2
du_semigroup_876 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_876 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.-_
d_'45'__878 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_'45'__878 v0 = coe d_'45'__212 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_880 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_880 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.-‿+-comm
d_'45''8255''43''45'comm_882 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_882 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.-‿cong
d_'45''8255'cong_884 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_884 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.0#
d_0'35'_886 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_0'35'_886 v0 = coe d_0'35'_214 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.0≟_
d_0'8799'__888 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Maybe AgdaAny
d_0'8799'__888 v0 = coe d_0'8799'__218 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.1#
d_1'35'_890 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_1'35'_890 v0 = coe d_1'35'_220 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.Carrier
d_Carrier_892 :: T_AlmostCommutativeRing_178 -> ()
d_Carrier_892 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.commutativeSemiring
d_commutativeSemiring_894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_894 ~v0 ~v1 v2
  = du_commutativeSemiring_894 v2
du_commutativeSemiring_894 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_894 v0
  = coe du_commutativeSemiring_326 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.distrib
d_distrib_896 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_896 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.distribʳ
d_distrib'691'_898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_898 ~v0 ~v1 v2 = du_distrib'691'_898 v2
du_distrib'691'_898 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_898 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.distribˡ
d_distrib'737'_900 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_900 ~v0 ~v1 v2 = du_distrib'737'_900 v2
du_distrib'737'_900 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_900 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isAlmostCommutativeRing
d_isAlmostCommutativeRing_902 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_902 v0
  = coe d_isAlmostCommutativeRing_222 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeSemiring
d_isCommutativeSemiring_904 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_904 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_906 ~v0 ~v1 v2
  = du_isCommutativeSemiringWithoutOne_906 v2
du_isCommutativeSemiringWithoutOne_906 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_906 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isEquivalence
d_isEquivalence_908 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_908 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isNearSemiring
d_isNearSemiring_910 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_910 ~v0 ~v1 v2 = du_isNearSemiring_910 v2
du_isNearSemiring_910 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_910 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isPartialEquivalence
d_isPartialEquivalence_912 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_912 ~v0 ~v1 v2
  = du_isPartialEquivalence_912 v2
du_isPartialEquivalence_912 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_912 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isSemiring
d_isSemiring_914 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_914 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_916 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_916 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isSemiringWithoutOne
d_isSemiringWithoutOne_918 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_918 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_918 v2
du_isSemiringWithoutOne_918 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_918 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.rawRing
d_rawRing_920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_920 ~v0 ~v1 v2 = du_rawRing_920 v2
du_rawRing_920 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_920 v0 = coe du_rawRing_346 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.rawSemiring
d_rawSemiring_922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_922 ~v0 ~v1 v2 = du_rawSemiring_922 v2
du_rawSemiring_922 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_922 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.refl
d_refl_924 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_refl_924 ~v0 ~v1 v2 = du_refl_924 v2
du_refl_924 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_refl_924 v0 = coe du_refl_358 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.reflexive
d_reflexive_926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_926 ~v0 ~v1 v2 = du_reflexive_926 v2
du_reflexive_926 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_926 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.semiring
d_semiring_928 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_928 ~v0 ~v1 v2 = du_semiring_928 v2
du_semiring_928 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_928 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.setoid
d_setoid_930 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_930 ~v0 ~v1 v2 = du_setoid_930 v2
du_setoid_930 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_930 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.sym
d_sym_932 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_932 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.trans
d_trans_934 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_934 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.zero
d_zero_936 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_936 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.zeroʳ
d_zero'691'_938 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'691'_938 ~v0 ~v1 v2 = du_zero'691'_938 v2
du_zero'691'_938 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_938 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.zeroˡ
d_zero'737'_940 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'737'_940 ~v0 ~v1 v2 = du_zero'737'_940 v2
du_zero'737'_940 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_940 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.Induced-equivalence
d_Induced'45'equivalence_964 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> ()
d_Induced'45'equivalence_964 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._*_
d__'42'__980 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__980 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du__'42'__980 v5
du__'42'__980 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'42'__980 v0 = coe d__'42'__210 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._+_
d__'43'__982 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__982 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du__'43'__982 v5
du__'43'__982 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'43'__982 v0 = coe d__'43'__208 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._-_
d__'45'__984 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__984 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du__'45'__984 v5
du__'45'__984 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__984 v0 = coe du__'45'__350 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._^_
d__'94'__986 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> Integer -> AgdaAny
d__'94'__986 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du__'94'__986 v5
du__'94'__986 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Integer -> AgdaAny
du__'94'__986 v0 = coe du__'94'__348 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._._≈_
d__'8776'__988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()
d__'8776'__988 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-assoc
d_'42''45'assoc_990 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_990 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'assoc_990 v5
du_'42''45'assoc_990 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'assoc_990 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-comm
d_'42''45'comm_992 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_992 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'comm_992 v5
du_'42''45'comm_992 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'comm_992 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-commutativeMonoid
d_'42''45'commutativeMonoid_994 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_994 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'commutativeMonoid_994 v5
du_'42''45'commutativeMonoid_994 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_994 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-cong
d_'42''45'cong_996 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_996 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'cong_996 v5
du_'42''45'cong_996 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'cong_996 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-congʳ
d_'8729''45'cong'691'_998 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_998 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong'691'_998 v5
du_'8729''45'cong'691'_998 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_998 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-congˡ
d_'8729''45'cong'737'_1000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1000 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong'737'_1000 v5
du_'8729''45'cong'737'_1000 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1000 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-identity
d_'42''45'identity_1002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1002 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'identity_1002 v5
du_'42''45'identity_1002 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'42''45'identity_1002 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identityʳ
d_identity'691'_1004 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'691'_1004 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity'691'_1004 v5
du_identity'691'_1004 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_1004 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identityˡ
d_identity'737'_1006 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'737'_1006 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity'737'_1006 v5
du_identity'737'_1006 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_1006 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeMagma
d_isCommutativeMagma_1008 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_1008 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isCommutativeMagma_1008 v5
du_isCommutativeMagma_1008 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_1008 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_1010 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_1010 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7
                                   ~v8
  = du_'42''45'isCommutativeMonoid_1010 v5
du_'42''45'isCommutativeMonoid_1010 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_1010 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_1012 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_1012 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
                                      ~v7 ~v8
  = du_'42''45'isCommutativeSemigroup_1012 v5
du_'42''45'isCommutativeSemigroup_1012 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_1012 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isMagma
d_'42''45'isMagma_1014 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_1014 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'isMagma_1014 v5
du_'42''45'isMagma_1014 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_1014 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isMonoid
d_'42''45'isMonoid_1016 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_1016 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'isMonoid_1016 v5
du_'42''45'isMonoid_1016 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_1016 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-isSemigroup
d_'42''45'isSemigroup_1018 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_1018 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'isSemigroup_1018 v5
du_'42''45'isSemigroup_1018 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_1018 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-monoid
d_'42''45'monoid_1020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_1020 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'monoid_1020 v5
du_'42''45'monoid_1020 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_1020 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.semigroup
d_semigroup_1022 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_1022 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_semigroup_1022 v5
du_semigroup_1022 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_1022 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.assoc
d_assoc_1024 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1024 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_assoc_1024 v5
du_assoc_1024 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_1024 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.comm
d_comm_1026 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1026 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_comm_1026 v5
du_comm_1026 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_1026 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.+-commutativeMonoid
d_'43''45'commutativeMonoid_1028 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_1028 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'43''45'commutativeMonoid_1028 v5
du_'43''45'commutativeMonoid_1028 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_1028 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-cong
d_'8729''45'cong_1030 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1030 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong_1030 v5
du_'8729''45'cong_1030 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong_1030 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-congʳ
d_'8729''45'cong'691'_1032 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1032 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong'691'_1032 v5
du_'8729''45'cong'691'_1032 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1032 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.∙-congˡ
d_'8729''45'cong'737'_1034 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1034 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong'737'_1034 v5
du_'8729''45'cong'737'_1034 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1034 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identity
d_identity_1036 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1036 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity_1036 v5
du_identity_1036 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_identity_1036 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identityʳ
d_identity'691'_1038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'691'_1038 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity'691'_1038 v5
du_identity'691'_1038 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_1038 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.identityˡ
d_identity'737'_1040 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'737'_1040 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity'737'_1040 v5
du_identity'737'_1040 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_1040 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeMagma
d_isCommutativeMagma_1042 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_1042 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isCommutativeMagma_1042 v5
du_isCommutativeMagma_1042 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_1042 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1044 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1044 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7
                                   ~v8
  = du_'43''45'isCommutativeMonoid_1044 v5
du_'43''45'isCommutativeMonoid_1044 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'43''45'isCommutativeMonoid_1044 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeSemigroup
d_isCommutativeSemigroup_1046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1046 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isCommutativeSemigroup_1046 v5
du_isCommutativeSemigroup_1046 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1046 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isMagma
d_isMagma_1048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1048 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isMagma_1048 v5
du_isMagma_1048 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_1048 v0
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
                        d_isCommutativeSemiring_62
                        (coe d_isAlmostCommutativeRing_222 (coe v0))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isMonoid
d_isMonoid_1050 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1050 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isMonoid_1050 v5
du_isMonoid_1050 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_isMonoid_1050 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
               (coe
                  d_isCommutativeSemiring_62
                  (coe d_isAlmostCommutativeRing_222 (coe v0))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isSemigroup
d_isSemigroup_1052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1052 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isSemigroup_1052 v5
du_isSemigroup_1052 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_1052 v0
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
                     d_isCommutativeSemiring_62
                     (coe d_isAlmostCommutativeRing_222 (coe v0)))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isUnitalMagma
d_isUnitalMagma_1054 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_1054 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isUnitalMagma_1054 v5
du_isUnitalMagma_1054 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_1054 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.monoid
d_monoid_1056 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_1056 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_monoid_1056 v5
du_monoid_1056 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_1056 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_monoid_890
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.semigroup
d_semigroup_1058 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_1058 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_semigroup_1058 v5
du_semigroup_1058 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_1058 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.-_
d_'45'__1060 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45'__1060 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_'45'__1060 v5
du_'45'__1060 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_'45'__1060 v0 = coe d_'45'__212 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_1062 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_1062 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
                                      ~v7 ~v8
  = du_'45''8255''42''45'distrib'737'_1062 v5
du_'45''8255''42''45'distrib'737'_1062 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255''42''45'distrib'737'_1062 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.-‿+-comm
d_'45''8255''43''45'comm_1064 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_1064 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'45''8255''43''45'comm_1064 v5
du_'45''8255''43''45'comm_1064 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255''43''45'comm_1064 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.-‿cong
d_'45''8255'cong_1066 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_1066 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'45''8255'cong_1066 v5
du_'45''8255'cong_1066 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255'cong_1066 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.0#
d_0'35'_1068 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_0'35'_1068 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_0'35'_1068 v5
du_0'35'_1068 :: T_AlmostCommutativeRing_178 -> AgdaAny
du_0'35'_1068 v0 = coe d_0'35'_214 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.0≟_
d_0'8799'__1070 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> Maybe AgdaAny
d_0'8799'__1070 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_0'8799'__1070 v5
du_0'8799'__1070 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> Maybe AgdaAny
du_0'8799'__1070 v0 = coe d_0'8799'__218 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.1#
d_1'35'_1072 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_1'35'_1072 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_1'35'_1072 v5
du_1'35'_1072 :: T_AlmostCommutativeRing_178 -> AgdaAny
du_1'35'_1072 v0 = coe d_1'35'_220 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.Carrier
d_Carrier_1074 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> ()
d_Carrier_1074 = erased
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.commutativeSemiring
d_commutativeSemiring_1076 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_1076 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_commutativeSemiring_1076 v5
du_commutativeSemiring_1076 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_1076 v0
  = coe du_commutativeSemiring_326 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.distrib
d_distrib_1078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1078 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_distrib_1078 v5
du_distrib_1078 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_distrib_1078 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_222 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.distribʳ
d_distrib'691'_1080 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1080 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_distrib'691'_1080 v5
du_distrib'691'_1080 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_1080 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.distribˡ
d_distrib'737'_1082 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_1082 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_distrib'737'_1082 v5
du_distrib'737'_1082 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_1082 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isAlmostCommutativeRing
d_isAlmostCommutativeRing_1084 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_1084 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isAlmostCommutativeRing_1084 v5
du_isAlmostCommutativeRing_1084 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
du_isAlmostCommutativeRing_1084 v0
  = coe d_isAlmostCommutativeRing_222 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeSemiring
d_isCommutativeSemiring_1086 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_1086 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isCommutativeSemiring_1086 v5
du_isCommutativeSemiring_1086 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_isCommutativeSemiring_1086 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_222 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_1088 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_1088 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
                                       ~v7 ~v8
  = du_isCommutativeSemiringWithoutOne_1088 v5
du_isCommutativeSemiringWithoutOne_1088 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_1088 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isEquivalence
d_isEquivalence_1090 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1090 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isEquivalence_1090 v5
du_isEquivalence_1090 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_1090 v0
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
                           d_isCommutativeSemiring_62
                           (coe d_isAlmostCommutativeRing_222 (coe v0)))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isNearSemiring
d_isNearSemiring_1092 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_1092 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isNearSemiring_1092 v5
du_isNearSemiring_1092 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_1092 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isPartialEquivalence
d_isPartialEquivalence_1094 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1094 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isPartialEquivalence_1094 v5
du_isPartialEquivalence_1094 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1094 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isSemiring
d_isSemiring_1096 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1096 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isSemiring_1096 v5
du_isSemiring_1096 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
du_isSemiring_1096 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_222 (coe v0)))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1098 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1098 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
                                         ~v7 ~v8
  = du_isSemiringWithoutAnnihilatingZero_1098 v5
du_isSemiringWithoutAnnihilatingZero_1098 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
du_isSemiringWithoutAnnihilatingZero_1098 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.isSemiringWithoutOne
d_isSemiringWithoutOne_1100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1100 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isSemiringWithoutOne_1100 v5
du_isSemiringWithoutOne_1100 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_1100 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.rawRing
d_rawRing_1102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_1102 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_rawRing_1102 v5
du_rawRing_1102 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_1102 v0 = coe du_rawRing_346 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.rawSemiring
d_rawSemiring_1104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_1104 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_rawSemiring_1104 v5
du_rawSemiring_1104 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_1104 v0
  = let v1 = coe du_commutativeSemiring_326 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.refl
d_refl_1106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_refl_1106 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_refl_1106 v5
du_refl_1106 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_refl_1106 v0 = coe du_refl_358 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.reflexive
d_reflexive_1108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1108 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_reflexive_1108 v5
du_reflexive_1108 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1108 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.semiring
d_semiring_1110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_1110 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_semiring_1110 v5
du_semiring_1110 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_1110 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_326 (coe v0))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.setoid
d_setoid_1112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1112 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_setoid_1112 v5
du_setoid_1112 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1112 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
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
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.sym
d_sym_1114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1114 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_sym_1114 v5
du_sym_1114 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_1114 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.trans
d_trans_1116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1116 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_trans_1116 v5
du_trans_1116 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_1116 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_222 (coe v0))))))))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.zero
d_zero_1118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1118 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_zero_1118 v5
du_zero_1118 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_1118 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_222 (coe v0))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.zeroʳ
d_zero'691'_1120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_zero'691'_1120 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_zero'691'_1120 v5
du_zero'691'_1120 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_1120 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.zeroˡ
d_zero'737'_1122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_zero'737'_1122 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_zero'737'_1122 v5
du_zero'737'_1122 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_1122 v0
  = let v1 = d_isAlmostCommutativeRing_222 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.*-homo
d_'42''45'homo_1126 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1126 v0 ~v1 ~v2 = du_'42''45'homo_1126 v0
du_'42''45'homo_1126 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'homo_1126 v0 = coe d_'42''45'homo_774 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.+-homo
d_'43''45'homo_1128 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'43''45'homo_1128 v0 ~v1 ~v2 = du_'43''45'homo_1128 v0
du_'43''45'homo_1128 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'43''45'homo_1128 v0 = coe d_'43''45'homo_772 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.-‿homo
d_'45''8255'homo_1130 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'homo_1130 v0 ~v1 ~v2 = du_'45''8255'homo_1130 v0
du_'45''8255'homo_1130 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
du_'45''8255'homo_1130 v0 = coe d_'45''8255'homo_776 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.0-homo
d_0'45'homo_1132 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_0'45'homo_1132 v0 ~v1 ~v2 = du_0'45'homo_1132 v0
du_0'45'homo_1132 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny
du_0'45'homo_1132 v0 = coe d_0'45'homo_778 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.1-homo
d_1'45'homo_1134 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_1'45'homo_1134 v0 ~v1 ~v2 = du_1'45'homo_1134 v0
du_1'45'homo_1134 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny
du_1'45'homo_1134 v0 = coe d_1'45'homo_780 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing._._.⟦_⟧
d_'10214'_'10215'_1136 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'10214'_'10215'_1136 v0 ~v1 ~v2 = du_'10214'_'10215'_1136 v0
du_'10214'_'10215'_1136 ::
  T__'45'Raw'45'AlmostCommutative'10230'__372 -> AgdaAny -> AgdaAny
du_'10214'_'10215'_1136 v0 = coe d_'10214'_'10215'_770 (coe v0)
-- Tactic.RingSolver.Core.AlmostCommutativeRing.fromCommutativeRing
d_fromCommutativeRing_1344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634 ->
  (AgdaAny -> Maybe AgdaAny) -> T_AlmostCommutativeRing_178
d_fromCommutativeRing_1344 ~v0 ~v1 v2 v3
  = du_fromCommutativeRing_1344 v2 v3
du_fromCommutativeRing_1344 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634 ->
  (AgdaAny -> Maybe AgdaAny) -> T_AlmostCommutativeRing_178
du_fromCommutativeRing_1344 v0 v1
  = coe
      C_AlmostCommutativeRing'46'constructor_6199
      (MAlonzo.Code.Algebra.Bundles.d__'43'__3660 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d__'42'__3662 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d_'45'__3664 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d_0'35'_3666 (coe v0)) v1
      (MAlonzo.Code.Algebra.Bundles.d_1'35'_3668 (coe v0))
      (coe
         C_IsAlmostCommutativeRing'46'constructor_1169
         (coe
            MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiring_2668
            (coe
               MAlonzo.Code.Algebra.Bundles.d_isCommutativeRing_3670 (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isRing_2556
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_isCommutativeRing_3670 (coe v0))))))
         (coe
            (\ v2 v3 ->
               coe
                 MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                 (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
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
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.d_isCommutativeRing_3670
                                         (coe v0)))))))))
                 (coe
                    MAlonzo.Code.Algebra.Bundles.d_'45'__3664 v0
                    (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3662 v0 v2 v3))
                 (coe
                    MAlonzo.Code.Algebra.Bundles.d__'42'__3662 v0
                    (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3664 v0 v2) v3)
                 (coe
                    MAlonzo.Code.Algebra.Properties.Ring.du_'45''8255'distrib'737''45''42'_368
                    (coe MAlonzo.Code.Algebra.Bundles.du_ring_3794 (coe v0)) (coe v2)
                    (coe v3))))
         (coe
            MAlonzo.Code.Algebra.Properties.AbelianGroup.du_'8315''185''45''8729''45'comm_180
            (coe
               MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
               (coe MAlonzo.Code.Algebra.Bundles.du_ring_3794 (coe v0)))))
-- Tactic.RingSolver.Core.AlmostCommutativeRing.fromCommutativeSemiring
d_fromCommutativeSemiring_1794 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152 ->
  (AgdaAny -> Maybe AgdaAny) -> T_AlmostCommutativeRing_178
d_fromCommutativeSemiring_1794 ~v0 ~v1 v2 v3
  = du_fromCommutativeSemiring_1794 v2 v3
du_fromCommutativeSemiring_1794 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152 ->
  (AgdaAny -> Maybe AgdaAny) -> T_AlmostCommutativeRing_178
du_fromCommutativeSemiring_1794 v0 v1
  = coe
      C_AlmostCommutativeRing'46'constructor_6199
      (MAlonzo.Code.Algebra.Bundles.d__'43'__2176 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d__'42'__2178 (coe v0)) (\ v2 -> v2)
      (MAlonzo.Code.Algebra.Bundles.d_0'35'_2180 (coe v0)) v1
      (MAlonzo.Code.Algebra.Bundles.d_1'35'_2182 (coe v0))
      (coe
         C_IsAlmostCommutativeRing'46'constructor_1169
         (coe
            MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemiring_2184 (coe v0))
         (coe (\ v2 v3 v4 -> v4))
         (coe
            (\ v2 v3 ->
               coe
                 MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                 (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
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
                                         MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemiring_2184
                                         (coe v0)))))))))
                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__2178 v0 v2 v3)))
         (coe
            (\ v2 v3 ->
               coe
                 MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                 (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
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
                                         MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemiring_2184
                                         (coe v0)))))))))
                 (coe MAlonzo.Code.Algebra.Bundles.d__'43'__2176 v0 v2 v3))))
