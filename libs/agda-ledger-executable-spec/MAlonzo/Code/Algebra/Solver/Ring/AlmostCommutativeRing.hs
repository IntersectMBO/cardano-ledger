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

module MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing where

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
import qualified MAlonzo.Code.Algebra.Properties.AbelianGroup
import qualified MAlonzo.Code.Algebra.Properties.Ring
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing
d_IsAlmostCommutativeRing_26 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsAlmostCommutativeRing_26
  = C_IsAlmostCommutativeRing'46'constructor_1019 MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
                                                  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                                  (AgdaAny -> AgdaAny -> AgdaAny)
                                                  (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing.isCommutativeSemiring
d_isCommutativeSemiring_62 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_62 v0
  = case coe v0 of
      C_IsAlmostCommutativeRing'46'constructor_1019 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing.-‿cong
d_'45''8255'cong_64 ::
  T_IsAlmostCommutativeRing_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_64 v0
  = case coe v0 of
      C_IsAlmostCommutativeRing'46'constructor_1019 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_70 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_70 v0
  = case coe v0 of
      C_IsAlmostCommutativeRing'46'constructor_1019 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing.-‿+-comm
d_'45''8255''43''45'comm_76 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_76 v0
  = case coe v0 of
      C_IsAlmostCommutativeRing'46'constructor_1019 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-assoc
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-comm
d_'42''45'comm_82 ::
  T_IsAlmostCommutativeRing_26 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_82 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe d_isCommutativeSemiring_62 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-cong
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-congʳ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-congˡ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-identity
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.identityʳ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.identityˡ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isCommutativeMagma
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isCommutativeMonoid
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isCommutativeSemigroup
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isMagma
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isMonoid
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.*-isSemigroup
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.assoc
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.comm
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-cong
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-congʳ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.∙-congˡ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.identity
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.identityʳ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.identityˡ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isCommutativeMagma
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.+-isCommutativeMonoid
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isCommutativeSemigroup
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isMagma
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isMonoid
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isSemigroup
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isUnitalMagma
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.distrib
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.distribʳ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.distribˡ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isCommutativeSemiringWithoutOne
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isEquivalence
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isNearSemiring
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isPartialEquivalence
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isSemiring
d_isSemiring_152 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_152 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe d_isCommutativeSemiring_62 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_154 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_154 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe d_isCommutativeSemiring_62 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.isSemiringWithoutOne
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.refl
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.reflexive
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.setoid
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.sym
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.trans
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.zero
d_zero_168 ::
  T_IsAlmostCommutativeRing_26 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_168 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe d_isCommutativeSemiring_62 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.zeroʳ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.IsAlmostCommutativeRing._.zeroˡ
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing
d_AlmostCommutativeRing_178 a0 a1 = ()
data T_AlmostCommutativeRing_178
  = C_AlmostCommutativeRing'46'constructor_5939 (AgdaAny ->
                                                 AgdaAny -> AgdaAny)
                                                (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                                AgdaAny AgdaAny T_IsAlmostCommutativeRing_26
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing.Carrier
d_Carrier_200 :: T_AlmostCommutativeRing_178 -> ()
d_Carrier_200 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._≈_
d__'8776'__202 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> ()
d__'8776'__202 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._+_
d__'43'__204 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__204 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_5939 v3 v4 v5 v6 v7 v8
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._*_
d__'42'__206 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__206 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_5939 v3 v4 v5 v6 v7 v8
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing.-_
d_'45'__208 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_'45'__208 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_5939 v3 v4 v5 v6 v7 v8
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing.0#
d_0'35'_210 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_0'35'_210 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_5939 v3 v4 v5 v6 v7 v8
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing.1#
d_1'35'_212 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_1'35'_212 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_5939 v3 v4 v5 v6 v7 v8
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing.isAlmostCommutativeRing
d_isAlmostCommutativeRing_214 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_214 v0
  = case coe v0 of
      C_AlmostCommutativeRing'46'constructor_5939 v3 v4 v5 v6 v7 v8
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-assoc
d_'42''45'assoc_218 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_218 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-comm
d_'42''45'comm_220 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_220 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-cong
d_'42''45'cong_222 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_222 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_224 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_224 v2
du_'8729''45'cong'691'_224 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_224 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_226 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_226 v2
du_'8729''45'cong'737'_226 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_226 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-identity
d_'42''45'identity_228 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_228 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.identityʳ
d_identity'691'_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_230 ~v0 ~v1 v2 = du_identity'691'_230 v2
du_identity'691'_230 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_230 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.identityˡ
d_identity'737'_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_232 ~v0 ~v1 v2 = du_identity'737'_232 v2
du_identity'737'_232 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_232 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_234 ~v0 ~v1 v2 = du_isCommutativeMagma_234 v2
du_isCommutativeMagma_234 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_234 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_236 ~v0 ~v1 v2
  = du_'42''45'isCommutativeMonoid_236 v2
du_'42''45'isCommutativeMonoid_236 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_236 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_238 ~v0 ~v1 v2
  = du_'42''45'isCommutativeSemigroup_238 v2
du_'42''45'isCommutativeSemigroup_238 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_238 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-isMagma
d_'42''45'isMagma_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_240 ~v0 ~v1 v2 = du_'42''45'isMagma_240 v2
du_'42''45'isMagma_240 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_240 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-isMonoid
d_'42''45'isMonoid_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_242 ~v0 ~v1 v2 = du_'42''45'isMonoid_242 v2
du_'42''45'isMonoid_242 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_242 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-isSemigroup
d_'42''45'isSemigroup_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_244 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_244 v2
du_'42''45'isSemigroup_244 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_244 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.assoc
d_assoc_246 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_246 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.comm
d_comm_248 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_248 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.∙-cong
d_'8729''45'cong_250 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_250 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_252 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_252 v2
du_'8729''45'cong'691'_252 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_252 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_254 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_254 v2
du_'8729''45'cong'737'_254 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_254 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.identity
d_identity_256 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_256 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.identityʳ
d_identity'691'_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_258 ~v0 ~v1 v2 = du_identity'691'_258 v2
du_identity'691'_258 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_258 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.identityˡ
d_identity'737'_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_260 ~v0 ~v1 v2 = du_identity'737'_260 v2
du_identity'737'_260 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_260 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_262 ~v0 ~v1 v2 = du_isCommutativeMagma_262 v2
du_isCommutativeMagma_262 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_262 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_264 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_264 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeSemigroup
d_isCommutativeSemigroup_266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_266 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_266 v2
du_isCommutativeSemigroup_266 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_266 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isMagma
d_isMagma_268 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_268 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isMonoid
d_isMonoid_270 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_270 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isSemigroup
d_isSemigroup_272 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_272 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isUnitalMagma
d_isUnitalMagma_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_274 ~v0 ~v1 v2 = du_isUnitalMagma_274 v2
du_isUnitalMagma_274 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_274 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_276 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_276 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.-‿+-comm
d_'45''8255''43''45'comm_278 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_278 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.-‿cong
d_'45''8255'cong_280 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_280 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.distrib
d_distrib_282 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_282 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.distribʳ
d_distrib'691'_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_284 ~v0 ~v1 v2 = du_distrib'691'_284 v2
du_distrib'691'_284 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_284 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.distribˡ
d_distrib'737'_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_286 ~v0 ~v1 v2 = du_distrib'737'_286 v2
du_distrib'737'_286 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_286 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeSemiring
d_isCommutativeSemiring_288 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_288 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_290 ~v0 ~v1 v2
  = du_isCommutativeSemiringWithoutOne_290 v2
du_isCommutativeSemiringWithoutOne_290 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_290 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isEquivalence
d_isEquivalence_292 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_292 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isNearSemiring
d_isNearSemiring_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_294 ~v0 ~v1 v2 = du_isNearSemiring_294 v2
du_isNearSemiring_294 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_294 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isPartialEquivalence
d_isPartialEquivalence_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_296 ~v0 ~v1 v2
  = du_isPartialEquivalence_296 v2
du_isPartialEquivalence_296 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_296 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isSemiring
d_isSemiring_298 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_298 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_300 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_300 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.isSemiringWithoutOne
d_isSemiringWithoutOne_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_302 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_302 v2
du_isSemiringWithoutOne_302 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_302 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.refl
d_refl_304 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_refl_304 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.reflexive
d_reflexive_306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_306 ~v0 ~v1 v2 = du_reflexive_306 v2
du_reflexive_306 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_306 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.setoid
d_setoid_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_308 ~v0 ~v1 v2 = du_setoid_308 v2
du_setoid_308 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_308 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.sym
d_sym_310 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_310 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.trans
d_trans_312 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_312 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.zero
d_zero_314 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_314 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.zeroʳ
d_zero'691'_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'691'_316 ~v0 ~v1 v2 = du_zero'691'_316 v2
du_zero'691'_316 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_316 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.zeroˡ
d_zero'737'_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'737'_318 ~v0 ~v1 v2 = du_zero'737'_318 v2
du_zero'737'_318 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_318 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing.commutativeSemiring
d_commutativeSemiring_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_320 ~v0 ~v1 v2
  = du_commutativeSemiring_320 v2
du_commutativeSemiring_320 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_320 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      (d__'43'__204 (coe v0)) (d__'42'__206 (coe v0))
      (d_0'35'_210 (coe v0)) (d_1'35'_212 (coe v0))
      (d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-commutativeMonoid
d_'42''45'commutativeMonoid_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_324 ~v0 ~v1 v2
  = du_'42''45'commutativeMonoid_324 v2
du_'42''45'commutativeMonoid_324 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_324 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.commutativeSemigroup
d_commutativeSemigroup_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_commutativeSemigroup_326 ~v0 ~v1 v2
  = du_commutativeSemigroup_326 v2
du_commutativeSemigroup_326 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
du_commutativeSemigroup_326 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_commutativeSemigroup_906
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
         (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.magma
d_magma_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_328 ~v0 ~v1 v2 = du_magma_328 v2
du_magma_328 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_328 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v4))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.*-monoid
d_'42''45'monoid_330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_330 ~v0 ~v1 v2 = du_'42''45'monoid_330 v2
du_'42''45'monoid_330 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_330 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.semigroup
d_semigroup_332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_332 ~v0 ~v1 v2 = du_semigroup_332 v2
du_semigroup_332 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_332 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.+-commutativeMonoid
d_'43''45'commutativeMonoid_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_334 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_334 v2
du_'43''45'commutativeMonoid_334 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_334 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.magma
d_magma_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_336 ~v0 ~v1 v2 = du_magma_336 v2
du_magma_336 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_336 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
    let v5 = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v5))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.monoid
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
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.semigroup
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
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing._.semiring
d_semiring_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_342 ~v0 ~v1 v2 = du_semiring_342 v2
du_semiring_342 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_342 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.AlmostCommutativeRing.rawRing
d_rawRing_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_344 ~v0 ~v1 v2 = du_rawRing_344 v2
du_rawRing_344 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_344 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawRing'46'constructor_3463
      (d__'43'__204 (coe v0)) (d__'42'__206 (coe v0))
      (d_'45'__208 (coe v0)) (d_0'35'_210 (coe v0))
      (d_1'35'_212 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_
d__'45'Raw'45'AlmostCommutative'10230'__358 a0 a1 a2 a3 a4 a5 = ()
data T__'45'Raw'45'AlmostCommutative'10230'__358
  = C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_9383 (AgdaAny ->
                                                                 AgdaAny)
                                                                (AgdaAny -> AgdaAny -> AgdaAny)
                                                                (AgdaAny -> AgdaAny -> AgdaAny)
                                                                (AgdaAny -> AgdaAny) AgdaAny AgdaAny
-- Algebra.Solver.Ring.AlmostCommutativeRing.F._*_
d__'42'__374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__374 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'42'__374 v4
du__'42'__374 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'42'__374 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.F._+_
d__'43'__376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__376 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'43'__376 v4
du__'43'__376 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'43'__376 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.F.-_
d_'45'__392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_'45'__392 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_'45'__392 v4
du_'45'__392 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny
du_'45'__392 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.F.0#
d_0'35'_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny
d_0'35'_394 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_0'35'_394 v4
du_0'35'_394 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 -> AgdaAny
du_0'35'_394 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.F.1#
d_1'35'_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny
d_1'35'_396 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_1'35'_396 v4
du_1'35'_396 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 -> AgdaAny
du_1'35'_396 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.F.Carrier
d_Carrier_398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> ()
d_Carrier_398 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing.T._*_
d__'42'__404 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__404 v0 = coe d__'42'__206 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.T._+_
d__'43'__406 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__406 v0 = coe d__'43'__204 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.T._≈_
d__'8776'__408 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> ()
d__'8776'__408 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-assoc
d_'42''45'assoc_410 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_410 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-comm
d_'42''45'comm_412 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_412 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-commutativeMonoid
d_'42''45'commutativeMonoid_414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_414 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'commutativeMonoid_414 v5
du_'42''45'commutativeMonoid_414 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_414 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.commutativeSemigroup
d_commutativeSemigroup_416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_commutativeSemigroup_416 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_commutativeSemigroup_416 v5
du_commutativeSemigroup_416 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
du_commutativeSemigroup_416 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_commutativeSemigroup_906
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
         (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-cong
d_'42''45'cong_418 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_418 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.∙-congʳ
d_'8729''45'cong'691'_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_420 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_420 v5
du_'8729''45'cong'691'_420 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_420 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.∙-congˡ
d_'8729''45'cong'737'_422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_422 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_422 v5
du_'8729''45'cong'737'_422 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_422 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-identity
d_'42''45'identity_424 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_424 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.identityʳ
d_identity'691'_426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_426 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_identity'691'_426 v5
du_identity'691'_426 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_426 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.identityˡ
d_identity'737'_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_428 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_identity'737'_428 v5
du_identity'737'_428 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_428 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isCommutativeMagma
d_isCommutativeMagma_430 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_430 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isCommutativeMagma_430 v5
du_isCommutativeMagma_430 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_430 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_432 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isCommutativeMonoid_432 v5
du_'42''45'isCommutativeMonoid_432 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_432 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_434 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isCommutativeSemigroup_434 v5
du_'42''45'isCommutativeSemigroup_434 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_434 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-isMagma
d_'42''45'isMagma_436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_436 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isMagma_436 v5
du_'42''45'isMagma_436 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_436 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-isMonoid
d_'42''45'isMonoid_438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_438 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isMonoid_438 v5
du_'42''45'isMonoid_438 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_438 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-isSemigroup
d_'42''45'isSemigroup_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_440 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'isSemigroup_440 v5
du_'42''45'isSemigroup_440 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_440 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.magma
d_magma_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_442 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_magma_442 v5
du_magma_442 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_442 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v4))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.*-monoid
d_'42''45'monoid_444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_444 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'42''45'monoid_444 v5
du_'42''45'monoid_444 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_444 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.semigroup
d_semigroup_446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_446 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_semigroup_446 v5
du_semigroup_446 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_446 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.assoc
d_assoc_448 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_448 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.comm
d_comm_450 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_450 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.+-commutativeMonoid
d_'43''45'commutativeMonoid_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_452 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'43''45'commutativeMonoid_452 v5
du_'43''45'commutativeMonoid_452 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_452 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.∙-cong
d_'8729''45'cong_454 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_454 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.∙-congʳ
d_'8729''45'cong'691'_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_456 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_456 v5
du_'8729''45'cong'691'_456 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_456 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.∙-congˡ
d_'8729''45'cong'737'_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_458 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_458 v5
du_'8729''45'cong'737'_458 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_458 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.identity
d_identity_460 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_460 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.identityʳ
d_identity'691'_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_462 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_identity'691'_462 v5
du_identity'691'_462 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_462 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.identityˡ
d_identity'737'_464 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_464 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_identity'737'_464 v5
du_identity'737'_464 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_464 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isCommutativeMagma
d_isCommutativeMagma_466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_466 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isCommutativeMagma_466 v5
du_isCommutativeMagma_466 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_466 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_468 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_468 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isCommutativeSemigroup
d_isCommutativeSemigroup_470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_470 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isCommutativeSemigroup_470 v5
du_isCommutativeSemigroup_470 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_470 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isMagma
d_isMagma_472 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_472 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isMonoid
d_isMonoid_474 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_474 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isSemigroup
d_isSemigroup_476 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_476 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isUnitalMagma
d_isUnitalMagma_478 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_478 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isUnitalMagma_478 v5
du_isUnitalMagma_478 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_478 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.magma
d_magma_480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_480 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_magma_480 v5
du_magma_480 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_480 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
    let v5 = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v5))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.monoid
d_monoid_482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_482 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_monoid_482 v5
du_monoid_482 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_482 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.semigroup
d_semigroup_484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_484 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_semigroup_484 v5
du_semigroup_484 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_484 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.-_
d_'45'__486 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_'45'__486 v0 = coe d_'45'__208 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_488 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_488 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.-‿+-comm
d_'45''8255''43''45'comm_490 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_490 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.-‿cong
d_'45''8255'cong_492 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_492 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.0#
d_0'35'_494 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_0'35'_494 v0 = coe d_0'35'_210 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.1#
d_1'35'_496 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_1'35'_496 v0 = coe d_1'35'_212 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.Carrier
d_Carrier_498 :: T_AlmostCommutativeRing_178 -> ()
d_Carrier_498 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.commutativeSemiring
d_commutativeSemiring_500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_500 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_commutativeSemiring_500 v5
du_commutativeSemiring_500 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_500 v0
  = coe du_commutativeSemiring_320 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.distrib
d_distrib_502 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_502 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.distribʳ
d_distrib'691'_504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_504 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_distrib'691'_504 v5
du_distrib'691'_504 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_504 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.distribˡ
d_distrib'737'_506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_506 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_distrib'737'_506 v5
du_distrib'737'_506 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_506 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isAlmostCommutativeRing
d_isAlmostCommutativeRing_508 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_508 v0
  = coe d_isAlmostCommutativeRing_214 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isCommutativeSemiring
d_isCommutativeSemiring_510 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_510 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_512 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isCommutativeSemiringWithoutOne_512 v5
du_isCommutativeSemiringWithoutOne_512 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_512 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isEquivalence
d_isEquivalence_514 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_514 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isNearSemiring
d_isNearSemiring_516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_516 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isNearSemiring_516 v5
du_isNearSemiring_516 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_516 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isPartialEquivalence
d_isPartialEquivalence_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_518 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_518 v5
du_isPartialEquivalence_518 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_518 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isSemiring
d_isSemiring_520 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_520 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_522 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_522 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.isSemiringWithoutOne
d_isSemiringWithoutOne_524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_524 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isSemiringWithoutOne_524 v5
du_isSemiringWithoutOne_524 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_524 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.rawRing
d_rawRing_526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_526 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_rawRing_526 v5
du_rawRing_526 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_526 v0 = coe du_rawRing_344 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.refl
d_refl_528 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_refl_528 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.reflexive
d_reflexive_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_530 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_530 v5
du_reflexive_530 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_530 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.semiring
d_semiring_532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_532 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_semiring_532 v5
du_semiring_532 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_532 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.setoid
d_setoid_534 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_534 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_534 v5
du_setoid_534 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_534 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.sym
d_sym_536 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_536 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.trans
d_trans_538 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_538 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.zero
d_zero_540 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_540 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.zeroʳ
d_zero'691'_542 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'691'_542 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_zero'691'_542 v5
du_zero'691'_542 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_542 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.T.zeroˡ
d_zero'737'_544 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'737'_544 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_zero'737'_544 v5
du_zero'737'_544 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_544 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._.Homomorphic₀
d_Homomorphic'8320'_548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_Homomorphic'8320'_548 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._.Homomorphic₁
d_Homomorphic'8321'_550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8321'_550 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._.Homomorphic₂
d_Homomorphic'8322'_552 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8322'_552 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._.Morphism
d_Morphism_554 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 -> ()
d_Morphism_554 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T._*_
d__'42'__600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__600 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du__'42'__600 v5
du__'42'__600 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'42'__600 v0 = coe d__'42'__206 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T._+_
d__'43'__602 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43'__602 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du__'43'__602 v5
du__'43'__602 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'43'__602 v0 = coe d__'43'__204 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T._≈_
d__'8776'__604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__604 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-assoc
d_'42''45'assoc_606 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_606 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'assoc_606 v5
du_'42''45'assoc_606 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'assoc_606 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-comm
d_'42''45'comm_608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_608 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'comm_608 v5
du_'42''45'comm_608 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'comm_608 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-commutativeMonoid
d_'42''45'commutativeMonoid_610 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_610 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'commutativeMonoid_610 v5
du_'42''45'commutativeMonoid_610 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_610 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.commutativeSemigroup
d_commutativeSemigroup_612 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_commutativeSemigroup_612 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_commutativeSemigroup_612 v5
du_commutativeSemigroup_612 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
du_commutativeSemigroup_612 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_commutativeSemigroup_906
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
         (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-cong
d_'42''45'cong_614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_614 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'cong_614 v5
du_'42''45'cong_614 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'cong_614 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-congʳ
d_'8729''45'cong'691'_616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_616 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong'691'_616 v5
du_'8729''45'cong'691'_616 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_616 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-congˡ
d_'8729''45'cong'737'_618 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_618 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong'737'_618 v5
du_'8729''45'cong'737'_618 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_618 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-identity
d_'42''45'identity_620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_620 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'identity_620 v5
du_'42''45'identity_620 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'42''45'identity_620 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identityʳ
d_identity'691'_622 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_identity'691'_622 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_identity'691'_622 v5
du_identity'691'_622 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_622 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identityˡ
d_identity'737'_624 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_identity'737'_624 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_identity'737'_624 v5
du_identity'737'_624 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_624 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeMagma
d_isCommutativeMagma_626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_626 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeMagma_626 v5
du_isCommutativeMagma_626 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_626 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_628 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isCommutativeMonoid_628 v5
du_'42''45'isCommutativeMonoid_628 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_628 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_630 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_630 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isCommutativeSemigroup_630 v5
du_'42''45'isCommutativeSemigroup_630 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_630 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isMagma
d_'42''45'isMagma_632 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_632 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isMagma_632 v5
du_'42''45'isMagma_632 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_632 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isMonoid
d_'42''45'isMonoid_634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_634 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isMonoid_634 v5
du_'42''45'isMonoid_634 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_634 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-isSemigroup
d_'42''45'isSemigroup_636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_636 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'isSemigroup_636 v5
du_'42''45'isSemigroup_636 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_636 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.magma
d_magma_638 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_638 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_magma_638 v5
du_magma_638 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_638 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v4))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.*-monoid
d_'42''45'monoid_640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_640 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'42''45'monoid_640 v5
du_'42''45'monoid_640 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_640 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.semigroup
d_semigroup_642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_642 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_semigroup_642 v5
du_semigroup_642 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_642 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.assoc
d_assoc_644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_644 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_assoc_644 v5
du_assoc_644 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_644 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.comm
d_comm_646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_646 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_comm_646 v5
du_comm_646 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_646 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.+-commutativeMonoid
d_'43''45'commutativeMonoid_648 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_648 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'43''45'commutativeMonoid_648 v5
du_'43''45'commutativeMonoid_648 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_648 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-cong
d_'8729''45'cong_650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_650 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong_650 v5
du_'8729''45'cong_650 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong_650 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-congʳ
d_'8729''45'cong'691'_652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_652 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong'691'_652 v5
du_'8729''45'cong'691'_652 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_652 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.∙-congˡ
d_'8729''45'cong'737'_654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_654 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'8729''45'cong'737'_654 v5
du_'8729''45'cong'737'_654 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_654 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identity
d_identity_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_656 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_identity_656 v5
du_identity_656 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_identity_656 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identityʳ
d_identity'691'_658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_identity'691'_658 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_identity'691'_658 v5
du_identity'691'_658 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_658 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.identityˡ
d_identity'737'_660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_identity'737'_660 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_identity'737'_660 v5
du_identity'737'_660 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_660 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeMagma
d_isCommutativeMagma_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_662 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeMagma_662 v5
du_isCommutativeMagma_662 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_662 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_664 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'43''45'isCommutativeMonoid_664 v5
du_'43''45'isCommutativeMonoid_664 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'43''45'isCommutativeMonoid_664 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeSemigroup
d_isCommutativeSemigroup_666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_666 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeSemigroup_666 v5
du_isCommutativeSemigroup_666 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_666 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isMagma
d_isMagma_668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_668 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_isMagma_668 v5
du_isMagma_668 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_668 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isMonoid
d_isMonoid_670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_670 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_isMonoid_670 v5
du_isMonoid_670 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_isMonoid_670 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isSemigroup
d_isSemigroup_672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_672 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isSemigroup_672 v5
du_isSemigroup_672 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_672 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isUnitalMagma
d_isUnitalMagma_674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_674 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isUnitalMagma_674 v5
du_isUnitalMagma_674 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_674 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.magma
d_magma_676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_676 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_magma_676 v5
du_magma_676 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_676 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
    let v5 = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v5))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.monoid
d_monoid_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_678 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_monoid_678 v5
du_monoid_678 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_678 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.semigroup
d_semigroup_680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_680 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_semigroup_680 v5
du_semigroup_680 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_680 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.-_
d_'45'__682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_'45'__682 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_'45'__682 v5
du_'45'__682 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_'45'__682 v0 = coe d_'45'__208 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_684 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'45''8255''42''45'distrib'737'_684 v5
du_'45''8255''42''45'distrib'737'_684 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255''42''45'distrib'737'_684 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.-‿+-comm
d_'45''8255''43''45'comm_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_686 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'45''8255''43''45'comm_686 v5
du_'45''8255''43''45'comm_686 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255''43''45'comm_686 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.-‿cong
d_'45''8255'cong_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_688 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_'45''8255'cong_688 v5
du_'45''8255'cong_688 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255'cong_688 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.0#
d_0'35'_690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny
d_0'35'_690 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_0'35'_690 v5
du_0'35'_690 :: T_AlmostCommutativeRing_178 -> AgdaAny
du_0'35'_690 v0 = coe d_0'35'_210 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.1#
d_1'35'_692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny
d_1'35'_692 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_1'35'_692 v5
du_1'35'_692 :: T_AlmostCommutativeRing_178 -> AgdaAny
du_1'35'_692 v0 = coe d_1'35'_212 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.Carrier
d_Carrier_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> ()
d_Carrier_694 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.commutativeSemiring
d_commutativeSemiring_696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_696 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_commutativeSemiring_696 v5
du_commutativeSemiring_696 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_696 v0
  = coe du_commutativeSemiring_320 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.distrib
d_distrib_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_698 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_distrib_698 v5
du_distrib_698 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_distrib_698 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.distribʳ
d_distrib'691'_700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_700 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_distrib'691'_700 v5
du_distrib'691'_700 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_700 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.distribˡ
d_distrib'737'_702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_702 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_distrib'737'_702 v5
du_distrib'737'_702 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_702 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isAlmostCommutativeRing
d_isAlmostCommutativeRing_704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_704 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isAlmostCommutativeRing_704 v5
du_isAlmostCommutativeRing_704 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
du_isAlmostCommutativeRing_704 v0
  = coe d_isAlmostCommutativeRing_214 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeSemiring
d_isCommutativeSemiring_706 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_706 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeSemiring_706 v5
du_isCommutativeSemiring_706 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_isCommutativeSemiring_706 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_708 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isCommutativeSemiringWithoutOne_708 v5
du_isCommutativeSemiringWithoutOne_708 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_708 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isEquivalence
d_isEquivalence_710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_710 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isEquivalence_710 v5
du_isEquivalence_710 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_710 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isNearSemiring
d_isNearSemiring_712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_712 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isNearSemiring_712 v5
du_isNearSemiring_712 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_712 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isPartialEquivalence
d_isPartialEquivalence_714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_714 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isPartialEquivalence_714 v5
du_isPartialEquivalence_714 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_714 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isSemiring
d_isSemiring_716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_716 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_isSemiring_716 v5
du_isSemiring_716 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
du_isSemiring_716 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_718 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isSemiringWithoutAnnihilatingZero_718 v5
du_isSemiringWithoutAnnihilatingZero_718 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
du_isSemiringWithoutAnnihilatingZero_718 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.isSemiringWithoutOne
d_isSemiringWithoutOne_720 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_720 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
  = du_isSemiringWithoutOne_720 v5
du_isSemiringWithoutOne_720 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_720 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.rawRing
d_rawRing_722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_722 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_rawRing_722 v5
du_rawRing_722 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_722 v0 = coe du_rawRing_344 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.refl
d_refl_724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_refl_724 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_refl_724 v5
du_refl_724 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_refl_724 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.reflexive
d_reflexive_726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_726 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_reflexive_726 v5
du_reflexive_726 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_726 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.semiring
d_semiring_728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_728 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_semiring_728 v5
du_semiring_728 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_728 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.setoid
d_setoid_730 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_730 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_setoid_730 v5
du_setoid_730 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_730 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.sym
d_sym_732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_732 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_sym_732 v5
du_sym_732 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_732 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.trans
d_trans_734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_734 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_trans_734 v5
du_trans_734 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_734 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.zero
d_zero_736 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_736 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_zero_736 v5
du_zero_736 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_736 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.zeroʳ
d_zero'691'_738 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_zero'691'_738 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_zero'691'_738 v5
du_zero'691'_738 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_738 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.T.zeroˡ
d_zero'737'_740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_zero'737'_740 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 = du_zero'737'_740 v5
du_zero'737'_740 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_740 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.⟦_⟧
d_'10214'_'10215'_752 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_'10214'_'10215'_752 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_9383 v1 v2 v3 v4 v5 v6
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.+-homo
d_'43''45'homo_754 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'43''45'homo_754 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_9383 v1 v2 v3 v4 v5 v6
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.*-homo
d_'42''45'homo_756 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_756 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_9383 v1 v2 v3 v4 v5 v6
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.-‿homo
d_'45''8255'homo_758 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
d_'45''8255'homo_758 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_9383 v1 v2 v3 v4 v5 v6
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.0-homo
d_0'45'homo_760 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny
d_0'45'homo_760 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_9383 v1 v2 v3 v4 v5 v6
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing._-Raw-AlmostCommutative⟶_.1-homo
d_1'45'homo_762 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny
d_1'45'homo_762 v0
  = case coe v0 of
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_9383 v1 v2 v3 v4 v5 v6
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.AlmostCommutativeRing.-raw-almostCommutative⟶
d_'45'raw'45'almostCommutative'10230'_770 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358
d_'45'raw'45'almostCommutative'10230'_770 ~v0 ~v1 v2
  = du_'45'raw'45'almostCommutative'10230'_770 v2
du_'45'raw'45'almostCommutative'10230'_770 ::
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358
du_'45'raw'45'almostCommutative'10230'_770 v0
  = coe
      C__'45'Raw'45'AlmostCommutative'10230'_'46'constructor_9383
      (coe (\ v1 -> v1))
      (coe
         (\ v1 v2 ->
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
                                      d_isCommutativeSemiring_62
                                      (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
              (let v3 = coe du_rawRing_344 (coe v0) in
               coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v3 v1 v2)))
      (coe
         (\ v1 v2 ->
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
                                      d_isCommutativeSemiring_62
                                      (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
              (let v3 = coe du_rawRing_344 (coe v0) in
               coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v3 v1 v2)))
      (coe
         (\ v1 ->
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
                                      d_isCommutativeSemiring_62
                                      (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
              (let v2 = coe du_rawRing_344 (coe v0) in
               coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v2 v1)))
      (coe
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
                                 (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
         (let v1 = coe du_rawRing_344 (coe v0) in
          MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1)))
      (coe
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
                                 (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
         (let v1 = coe du_rawRing_344 (coe v0) in
          MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274 (coe v1)))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._._*_
d__'42'__780 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__780 v0 = coe d__'42'__206 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._._+_
d__'43'__782 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__782 v0 = coe d__'43'__204 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._._≈_
d__'8776'__784 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> ()
d__'8776'__784 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-assoc
d_'42''45'assoc_786 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_786 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-comm
d_'42''45'comm_788 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_788 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-commutativeMonoid
d_'42''45'commutativeMonoid_790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_790 ~v0 ~v1 v2
  = du_'42''45'commutativeMonoid_790 v2
du_'42''45'commutativeMonoid_790 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_790 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.commutativeSemigroup
d_commutativeSemigroup_792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_commutativeSemigroup_792 ~v0 ~v1 v2
  = du_commutativeSemigroup_792 v2
du_commutativeSemigroup_792 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
du_commutativeSemigroup_792 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_commutativeSemigroup_906
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
         (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-cong
d_'42''45'cong_794 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_794 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-congʳ
d_'8729''45'cong'691'_796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_796 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_796 v2
du_'8729''45'cong'691'_796 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_796 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-congˡ
d_'8729''45'cong'737'_798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_798 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_798 v2
du_'8729''45'cong'737'_798 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_798 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-identity
d_'42''45'identity_800 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_800 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identityʳ
d_identity'691'_802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_802 ~v0 ~v1 v2 = du_identity'691'_802 v2
du_identity'691'_802 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_802 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identityˡ
d_identity'737'_804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_804 ~v0 ~v1 v2 = du_identity'737'_804 v2
du_identity'737'_804 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_804 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeMagma
d_isCommutativeMagma_806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_806 ~v0 ~v1 v2 = du_isCommutativeMagma_806 v2
du_isCommutativeMagma_806 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_806 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_808 ~v0 ~v1 v2
  = du_'42''45'isCommutativeMonoid_808 v2
du_'42''45'isCommutativeMonoid_808 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_808 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_810 ~v0 ~v1 v2
  = du_'42''45'isCommutativeSemigroup_810 v2
du_'42''45'isCommutativeSemigroup_810 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_810 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isMagma
d_'42''45'isMagma_812 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_812 ~v0 ~v1 v2 = du_'42''45'isMagma_812 v2
du_'42''45'isMagma_812 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_812 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isMonoid
d_'42''45'isMonoid_814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_814 ~v0 ~v1 v2 = du_'42''45'isMonoid_814 v2
du_'42''45'isMonoid_814 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_814 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isSemigroup
d_'42''45'isSemigroup_816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_816 ~v0 ~v1 v2
  = du_'42''45'isSemigroup_816 v2
du_'42''45'isSemigroup_816 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_816 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.magma
d_magma_818 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_818 ~v0 ~v1 v2 = du_magma_818 v2
du_magma_818 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_818 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v4))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-monoid
d_'42''45'monoid_820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_820 ~v0 ~v1 v2 = du_'42''45'monoid_820 v2
du_'42''45'monoid_820 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_820 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.semigroup
d_semigroup_822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_822 ~v0 ~v1 v2 = du_semigroup_822 v2
du_semigroup_822 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_822 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.assoc
d_assoc_824 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_824 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.comm
d_comm_826 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_826 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.+-commutativeMonoid
d_'43''45'commutativeMonoid_828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_828 ~v0 ~v1 v2
  = du_'43''45'commutativeMonoid_828 v2
du_'43''45'commutativeMonoid_828 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_828 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-cong
d_'8729''45'cong_830 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_830 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-congʳ
d_'8729''45'cong'691'_832 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_832 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_832 v2
du_'8729''45'cong'691'_832 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_832 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-congˡ
d_'8729''45'cong'737'_834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_834 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_834 v2
du_'8729''45'cong'737'_834 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_834 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identity
d_identity_836 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_836 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identityʳ
d_identity'691'_838 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'691'_838 ~v0 ~v1 v2 = du_identity'691'_838 v2
du_identity'691'_838 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_838 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identityˡ
d_identity'737'_840 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_identity'737'_840 ~v0 ~v1 v2 = du_identity'737'_840 v2
du_identity'737'_840 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_840 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeMagma
d_isCommutativeMagma_842 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_842 ~v0 ~v1 v2 = du_isCommutativeMagma_842 v2
du_isCommutativeMagma_842 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_842 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_844 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_844 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeSemigroup
d_isCommutativeSemigroup_846 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_846 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_846 v2
du_isCommutativeSemigroup_846 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_846 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isMagma
d_isMagma_848 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_848 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isMonoid
d_isMonoid_850 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_850 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isSemigroup
d_isSemigroup_852 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_852 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isUnitalMagma
d_isUnitalMagma_854 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_854 ~v0 ~v1 v2 = du_isUnitalMagma_854 v2
du_isUnitalMagma_854 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_854 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.magma
d_magma_856 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_856 ~v0 ~v1 v2 = du_magma_856 v2
du_magma_856 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_856 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
    let v5 = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v5))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.monoid
d_monoid_858 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_858 ~v0 ~v1 v2 = du_monoid_858 v2
du_monoid_858 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_858 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.semigroup
d_semigroup_860 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_860 ~v0 ~v1 v2 = du_semigroup_860 v2
du_semigroup_860 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_860 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.-_
d_'45'__862 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_'45'__862 v0 = coe d_'45'__208 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_864 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_864 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.-‿+-comm
d_'45''8255''43''45'comm_866 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_866 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.-‿cong
d_'45''8255'cong_868 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_868 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.0#
d_0'35'_870 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_0'35'_870 v0 = coe d_0'35'_210 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.1#
d_1'35'_872 :: T_AlmostCommutativeRing_178 -> AgdaAny
d_1'35'_872 v0 = coe d_1'35'_212 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.Carrier
d_Carrier_874 :: T_AlmostCommutativeRing_178 -> ()
d_Carrier_874 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.commutativeSemiring
d_commutativeSemiring_876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_876 ~v0 ~v1 v2
  = du_commutativeSemiring_876 v2
du_commutativeSemiring_876 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_876 v0
  = coe du_commutativeSemiring_320 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.distrib
d_distrib_878 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_878 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.distribʳ
d_distrib'691'_880 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_880 ~v0 ~v1 v2 = du_distrib'691'_880 v2
du_distrib'691'_880 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_880 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.distribˡ
d_distrib'737'_882 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_882 ~v0 ~v1 v2 = du_distrib'737'_882 v2
du_distrib'737'_882 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_882 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isAlmostCommutativeRing
d_isAlmostCommutativeRing_884 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_884 v0
  = coe d_isAlmostCommutativeRing_214 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeSemiring
d_isCommutativeSemiring_886 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_886 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_888 ~v0 ~v1 v2
  = du_isCommutativeSemiringWithoutOne_888 v2
du_isCommutativeSemiringWithoutOne_888 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_888 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isEquivalence
d_isEquivalence_890 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_890 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isNearSemiring
d_isNearSemiring_892 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_892 ~v0 ~v1 v2 = du_isNearSemiring_892 v2
du_isNearSemiring_892 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_892 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isPartialEquivalence
d_isPartialEquivalence_894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_894 ~v0 ~v1 v2
  = du_isPartialEquivalence_894 v2
du_isPartialEquivalence_894 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_894 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isSemiring
d_isSemiring_896 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_896 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_898 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_898 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isSemiringWithoutOne
d_isSemiringWithoutOne_900 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_900 ~v0 ~v1 v2
  = du_isSemiringWithoutOne_900 v2
du_isSemiringWithoutOne_900 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_900 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.rawRing
d_rawRing_902 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_902 ~v0 ~v1 v2 = du_rawRing_902 v2
du_rawRing_902 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_902 v0 = coe du_rawRing_344 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.refl
d_refl_904 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_refl_904 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.reflexive
d_reflexive_906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_906 ~v0 ~v1 v2 = du_reflexive_906 v2
du_reflexive_906 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_906 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.semiring
d_semiring_908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_908 ~v0 ~v1 v2 = du_semiring_908 v2
du_semiring_908 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_908 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.setoid
d_setoid_910 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_910 ~v0 ~v1 v2 = du_setoid_910 v2
du_setoid_910 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_910 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.sym
d_sym_912 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_912 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.trans
d_trans_914 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_914 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.zero
d_zero_916 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_916 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.zeroʳ
d_zero'691'_918 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'691'_918 ~v0 ~v1 v2 = du_zero'691'_918 v2
du_zero'691'_918 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_918 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.zeroˡ
d_zero'737'_920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
d_zero'737'_920 ~v0 ~v1 v2 = du_zero'737'_920 v2
du_zero'737'_920 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_920 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing.Induced-equivalence
d_Induced'45'equivalence_944 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> ()
d_Induced'45'equivalence_944 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._._._*_
d__'42'__960 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__960 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du__'42'__960 v5
du__'42'__960 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'42'__960 v0 = coe d__'42'__206 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._._+_
d__'43'__962 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__962 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du__'43'__962 v5
du__'43'__962 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du__'43'__962 v0 = coe d__'43'__204 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._._≈_
d__'8776'__964 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()
d__'8776'__964 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-assoc
d_'42''45'assoc_966 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_966 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'assoc_966 v5
du_'42''45'assoc_966 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'assoc_966 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-comm
d_'42''45'comm_968 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_968 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'comm_968 v5
du_'42''45'comm_968 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'comm_968 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-commutativeMonoid
d_'42''45'commutativeMonoid_970 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_970 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'commutativeMonoid_970 v5
du_'42''45'commutativeMonoid_970 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_970 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.commutativeSemigroup
d_commutativeSemigroup_972 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_commutativeSemigroup_972 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_commutativeSemigroup_972 v5
du_commutativeSemigroup_972 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
du_commutativeSemigroup_972 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_commutativeSemigroup_906
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
         (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-cong
d_'42''45'cong_974 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_974 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'cong_974 v5
du_'42''45'cong_974 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'cong_974 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-congʳ
d_'8729''45'cong'691'_976 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_976 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong'691'_976 v5
du_'8729''45'cong'691'_976 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_976 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-congˡ
d_'8729''45'cong'737'_978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_978 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong'737'_978 v5
du_'8729''45'cong'737'_978 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_978 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-identity
d_'42''45'identity_980 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_980 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'identity_980 v5
du_'42''45'identity_980 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'42''45'identity_980 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identityʳ
d_identity'691'_982 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'691'_982 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity'691'_982 v5
du_identity'691'_982 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_982 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identityˡ
d_identity'737'_984 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'737'_984 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity'737'_984 v5
du_identity'737'_984 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_984 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeMagma
d_isCommutativeMagma_986 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_986 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isCommutativeMagma_986 v5
du_isCommutativeMagma_986 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_986 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_988 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7
                                  ~v8
  = du_'42''45'isCommutativeMonoid_988 v5
du_'42''45'isCommutativeMonoid_988 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_988 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_990 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_990 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7
                                     ~v8
  = du_'42''45'isCommutativeSemigroup_990 v5
du_'42''45'isCommutativeSemigroup_990 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_990 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isMagma
d_'42''45'isMagma_992 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_992 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'isMagma_992 v5
du_'42''45'isMagma_992 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_992 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isMonoid
d_'42''45'isMonoid_994 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_994 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'isMonoid_994 v5
du_'42''45'isMonoid_994 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_994 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-isSemigroup
d_'42''45'isSemigroup_996 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_996 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'isSemigroup_996 v5
du_'42''45'isSemigroup_996 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_996 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.magma
d_magma_998 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_998 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_magma_998 v5
du_magma_998 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_998 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v4))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-monoid
d_'42''45'monoid_1000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_1000 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'42''45'monoid_1000 v5
du_'42''45'monoid_1000 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_1000 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.semigroup
d_semigroup_1002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_1002 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_semigroup_1002 v5
du_semigroup_1002 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_1002 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.assoc
d_assoc_1004 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1004 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_assoc_1004 v5
du_assoc_1004 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_1004 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.comm
d_comm_1006 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1006 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_comm_1006 v5
du_comm_1006 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_1006 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.+-commutativeMonoid
d_'43''45'commutativeMonoid_1008 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_1008 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'43''45'commutativeMonoid_1008 v5
du_'43''45'commutativeMonoid_1008 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_1008 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-cong
d_'8729''45'cong_1010 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1010 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong_1010 v5
du_'8729''45'cong_1010 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong_1010 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-congʳ
d_'8729''45'cong'691'_1012 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1012 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong'691'_1012 v5
du_'8729''45'cong'691'_1012 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1012 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.∙-congˡ
d_'8729''45'cong'737'_1014 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1014 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'8729''45'cong'737'_1014 v5
du_'8729''45'cong'737'_1014 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1014 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identity
d_identity_1016 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1016 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity_1016 v5
du_identity_1016 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_identity_1016 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identityʳ
d_identity'691'_1018 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'691'_1018 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity'691'_1018 v5
du_identity'691'_1018 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'691'_1018 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.identityˡ
d_identity'737'_1020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'737'_1020 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_identity'737'_1020 v5
du_identity'737'_1020 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_identity'737'_1020 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeMagma
d_isCommutativeMagma_1022 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_1022 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isCommutativeMagma_1022 v5
du_isCommutativeMagma_1022 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_1022 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1024 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1024 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7
                                   ~v8
  = du_'43''45'isCommutativeMonoid_1024 v5
du_'43''45'isCommutativeMonoid_1024 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'43''45'isCommutativeMonoid_1024 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeSemigroup
d_isCommutativeSemigroup_1026 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1026 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isCommutativeSemigroup_1026 v5
du_isCommutativeSemigroup_1026 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1026 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isMagma
d_isMagma_1028 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_1028 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isMagma_1028 v5
du_isMagma_1028 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_1028 v0
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
                        (coe d_isAlmostCommutativeRing_214 (coe v0))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isMonoid
d_isMonoid_1030 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_1030 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isMonoid_1030 v5
du_isMonoid_1030 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_isMonoid_1030 v0
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
                  (coe d_isAlmostCommutativeRing_214 (coe v0))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isSemigroup
d_isSemigroup_1032 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_1032 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isSemigroup_1032 v5
du_isSemigroup_1032 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_1032 v0
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
                     (coe d_isAlmostCommutativeRing_214 (coe v0)))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isUnitalMagma
d_isUnitalMagma_1034 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_1034 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isUnitalMagma_1034 v5
du_isUnitalMagma_1034 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_1034 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.magma
d_magma_1036 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_1036 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_magma_1036 v5
du_magma_1036 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_1036 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
    let v5 = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v4) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_802 (coe v5))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.monoid
d_monoid_1038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_1038 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_monoid_1038 v5
du_monoid_1038 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_1038 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.semigroup
d_semigroup_1040 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_1040 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_semigroup_1040 v5
du_semigroup_1040 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_1040 v0
  = let v1 = coe du_commutativeSemiring_320 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.-_
d_'45'__1042 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45'__1042 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_'45'__1042 v5
du_'45'__1042 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_'45'__1042 v0 = coe d_'45'__208 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_1044 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_1044 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
                                      ~v7 ~v8
  = du_'45''8255''42''45'distrib'737'_1044 v5
du_'45''8255''42''45'distrib'737'_1044 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255''42''45'distrib'737'_1044 v0
  = coe
      d_'45''8255''42''45'distrib'737'_70
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.-‿+-comm
d_'45''8255''43''45'comm_1046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_1046 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'45''8255''43''45'comm_1046 v5
du_'45''8255''43''45'comm_1046 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255''43''45'comm_1046 v0
  = coe
      d_'45''8255''43''45'comm_76
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.-‿cong
d_'45''8255'cong_1048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_1048 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_'45''8255'cong_1048 v5
du_'45''8255'cong_1048 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255'cong_1048 v0
  = coe
      d_'45''8255'cong_64 (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.0#
d_0'35'_1050 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_0'35'_1050 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_0'35'_1050 v5
du_0'35'_1050 :: T_AlmostCommutativeRing_178 -> AgdaAny
du_0'35'_1050 v0 = coe d_0'35'_210 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.1#
d_1'35'_1052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_1'35'_1052 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_1'35'_1052 v5
du_1'35'_1052 :: T_AlmostCommutativeRing_178 -> AgdaAny
du_1'35'_1052 v0 = coe d_1'35'_212 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.Carrier
d_Carrier_1054 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> ()
d_Carrier_1054 = erased
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.commutativeSemiring
d_commutativeSemiring_1056 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_1056 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_commutativeSemiring_1056 v5
du_commutativeSemiring_1056 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_1056 v0
  = coe du_commutativeSemiring_320 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.distrib
d_distrib_1058 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1058 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_distrib_1058 v5
du_distrib_1058 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_distrib_1058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               d_isCommutativeSemiring_62
               (coe d_isAlmostCommutativeRing_214 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.distribʳ
d_distrib'691'_1060 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1060 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_distrib'691'_1060 v5
du_distrib'691'_1060 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_1060 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.distribˡ
d_distrib'737'_1062 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_1062 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_distrib'737'_1062 v5
du_distrib'737'_1062 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_1062 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isAlmostCommutativeRing
d_isAlmostCommutativeRing_1064 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_1064 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isAlmostCommutativeRing_1064 v5
du_isAlmostCommutativeRing_1064 ::
  T_AlmostCommutativeRing_178 -> T_IsAlmostCommutativeRing_26
du_isAlmostCommutativeRing_1064 v0
  = coe d_isAlmostCommutativeRing_214 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeSemiring
d_isCommutativeSemiring_1066 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_1066 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isCommutativeSemiring_1066 v5
du_isCommutativeSemiring_1066 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_isCommutativeSemiring_1066 v0
  = coe
      d_isCommutativeSemiring_62
      (coe d_isAlmostCommutativeRing_214 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_1068 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_1068 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
                                       ~v7 ~v8
  = du_isCommutativeSemiringWithoutOne_1068 v5
du_isCommutativeSemiringWithoutOne_1068 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_1068 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_62 (coe v1))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isEquivalence
d_isEquivalence_1070 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1070 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isEquivalence_1070 v5
du_isEquivalence_1070 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_1070 v0
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
                           (coe d_isAlmostCommutativeRing_214 (coe v0)))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isNearSemiring
d_isNearSemiring_1072 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_1072 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isNearSemiring_1072 v5
du_isNearSemiring_1072 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_1072 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isPartialEquivalence
d_isPartialEquivalence_1074 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1074 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isPartialEquivalence_1074 v5
du_isPartialEquivalence_1074 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1074 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isSemiring
d_isSemiring_1076 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_1076 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isSemiring_1076 v5
du_isSemiring_1076 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
du_isSemiring_1076 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         d_isCommutativeSemiring_62
         (coe d_isAlmostCommutativeRing_214 (coe v0)))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1078 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6
                                         ~v7 ~v8
  = du_isSemiringWithoutAnnihilatingZero_1078 v5
du_isSemiringWithoutAnnihilatingZero_1078 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
du_isSemiringWithoutAnnihilatingZero_1078 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.isSemiringWithoutOne
d_isSemiringWithoutOne_1080 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1080 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_isSemiringWithoutOne_1080 v5
du_isSemiringWithoutOne_1080 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_1080 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.rawRing
d_rawRing_1082 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_1082 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_rawRing_1082 v5
du_rawRing_1082 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_1082 v0 = coe du_rawRing_344 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.refl
d_refl_1084 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_refl_1084 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_refl_1084 v5
du_refl_1084 :: T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_refl_1084 v0
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
                              d_isCommutativeSemiring_62
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.reflexive
d_reflexive_1086 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1086 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_reflexive_1086 v5
du_reflexive_1086 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1086 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.semiring
d_semiring_1088 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_1088 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_semiring_1088 v5
du_semiring_1088 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_1088 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe du_commutativeSemiring_320 (coe v0))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.setoid
d_setoid_1090 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1090 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_setoid_1090 v5
du_setoid_1090 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1090 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
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
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.sym
d_sym_1092 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1092 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_sym_1092 v5
du_sym_1092 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_1092 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.trans
d_trans_1094 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1094 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_trans_1094 v5
du_trans_1094 ::
  T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_1094 v0
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
                              (coe d_isAlmostCommutativeRing_214 (coe v0))))))))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.zero
d_zero_1096 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1096 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 = du_zero_1096 v5
du_zero_1096 ::
  T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_1096 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            d_isCommutativeSemiring_62
            (coe d_isAlmostCommutativeRing_214 (coe v0))))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.zeroʳ
d_zero'691'_1098 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_zero'691'_1098 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_zero'691'_1098 v5
du_zero'691'_1098 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'691'_1098 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.zeroˡ
d_zero'737'_1100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_zero'737'_1100 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8
  = du_zero'737'_1100 v5
du_zero'737'_1100 ::
  T_AlmostCommutativeRing_178 -> AgdaAny -> AgdaAny
du_zero'737'_1100 v0
  = let v1 = d_isAlmostCommutativeRing_214 (coe v0) in
    let v2 = d_isCommutativeSemiring_62 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v3))
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.*-homo
d_'42''45'homo_1104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8
  = du_'42''45'homo_1104 v6
du_'42''45'homo_1104 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'42''45'homo_1104 v0 = coe d_'42''45'homo_756 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.+-homo
d_'43''45'homo_1106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'43''45'homo_1106 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8
  = du_'43''45'homo_1106 v6
du_'43''45'homo_1106 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'43''45'homo_1106 v0 = coe d_'43''45'homo_754 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.-‿homo
d_'45''8255'homo_1108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'homo_1108 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8
  = du_'45''8255'homo_1108 v6
du_'45''8255'homo_1108 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
du_'45''8255'homo_1108 v0 = coe d_'45''8255'homo_758 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.0-homo
d_0'45'homo_1110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_0'45'homo_1110 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8
  = du_0'45'homo_1110 v6
du_0'45'homo_1110 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny
du_0'45'homo_1110 v0 = coe d_0'45'homo_760 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.1-homo
d_1'45'homo_1112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_1'45'homo_1112 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8
  = du_1'45'homo_1112 v6
du_1'45'homo_1112 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny
du_1'45'homo_1112 v0 = coe d_1'45'homo_762 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing._._.⟦_⟧
d_'10214'_'10215'_1114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  T_AlmostCommutativeRing_178 ->
  T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'10214'_'10215'_1114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8
  = du_'10214'_'10215'_1114 v6
du_'10214'_'10215'_1114 ::
  T__'45'Raw'45'AlmostCommutative'10230'__358 -> AgdaAny -> AgdaAny
du_'10214'_'10215'_1114 v0 = coe d_'10214'_'10215'_752 (coe v0)
-- Algebra.Solver.Ring.AlmostCommutativeRing.fromCommutativeRing
d_fromCommutativeRing_1120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634 ->
  T_AlmostCommutativeRing_178
d_fromCommutativeRing_1120 ~v0 ~v1 v2
  = du_fromCommutativeRing_1120 v2
du_fromCommutativeRing_1120 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634 ->
  T_AlmostCommutativeRing_178
du_fromCommutativeRing_1120 v0
  = coe
      C_AlmostCommutativeRing'46'constructor_5939
      (MAlonzo.Code.Algebra.Bundles.d__'43'__3660 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d__'42'__3662 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d_'45'__3664 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d_0'35'_3666 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d_1'35'_3668 (coe v0))
      (coe
         C_IsAlmostCommutativeRing'46'constructor_1019
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
            (\ v1 v2 ->
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
                    (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3662 v0 v1 v2))
                 (coe
                    MAlonzo.Code.Algebra.Bundles.d__'42'__3662 v0
                    (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3664 v0 v1) v2)
                 (coe
                    MAlonzo.Code.Algebra.Properties.Ring.du_'45''8255'distrib'737''45''42'_368
                    (coe MAlonzo.Code.Algebra.Bundles.du_ring_3794 (coe v0)) (coe v1)
                    (coe v2))))
         (coe
            MAlonzo.Code.Algebra.Properties.AbelianGroup.du_'8315''185''45''8729''45'comm_180
            (coe
               MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
               (coe MAlonzo.Code.Algebra.Bundles.du_ring_3794 (coe v0)))))
-- Algebra.Solver.Ring.AlmostCommutativeRing.fromCommutativeSemiring
d_fromCommutativeSemiring_1406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152 ->
  T_AlmostCommutativeRing_178
d_fromCommutativeSemiring_1406 ~v0 ~v1 v2
  = du_fromCommutativeSemiring_1406 v2
du_fromCommutativeSemiring_1406 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152 ->
  T_AlmostCommutativeRing_178
du_fromCommutativeSemiring_1406 v0
  = coe
      C_AlmostCommutativeRing'46'constructor_5939
      (MAlonzo.Code.Algebra.Bundles.d__'43'__2176 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d__'42'__2178 (coe v0)) (\ v1 -> v1)
      (MAlonzo.Code.Algebra.Bundles.d_0'35'_2180 (coe v0))
      (MAlonzo.Code.Algebra.Bundles.d_1'35'_2182 (coe v0))
      (coe
         C_IsAlmostCommutativeRing'46'constructor_1019
         (coe
            MAlonzo.Code.Algebra.Bundles.d_isCommutativeSemiring_2184 (coe v0))
         (coe (\ v1 v2 v3 -> v3))
         (coe
            (\ v1 v2 ->
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
                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__2178 v0 v1 v2)))
         (coe
            (\ v1 v2 ->
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
                 (coe MAlonzo.Code.Algebra.Bundles.d__'43'__2176 v0 v1 v2))))
