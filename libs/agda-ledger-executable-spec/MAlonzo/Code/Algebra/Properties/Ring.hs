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

module MAlonzo.Code.Algebra.Properties.Ring where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Properties.AbelianGroup
import qualified MAlonzo.Code.Algebra.Properties.Group
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Properties.Ring._._-_
d__'45'__18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'45'__18 ~v0 ~v1 v2 = du__'45'__18 v2
du__'45'__18 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'45'__18 v0
  = let v1 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v1) (coe v2)
-- Algebra.Properties.Ring._.∙-cancel
d_'8729''45'cancel_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8729''45'cancel_336 ~v0 ~v1 v2 = du_'8729''45'cancel_336 v2
du_'8729''45'cancel_336 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8729''45'cancel_336 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_'8729''45'cancel_292
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.∙-cancelʳ
d_'8729''45'cancel'691'_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cancel'691'_338 ~v0 ~v1 v2
  = du_'8729''45'cancel'691'_338 v2
du_'8729''45'cancel'691'_338 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cancel'691'_338 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_'8729''45'cancel'691'_282
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.∙-cancelˡ
d_'8729''45'cancel'737'_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cancel'737'_340 ~v0 ~v1 v2
  = du_'8729''45'cancel'737'_340 v2
du_'8729''45'cancel'737'_340 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cancel'737'_340 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_'8729''45'cancel'737'_272
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.identity-unique
d_identity'45'unique_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_identity'45'unique_342 ~v0 ~v1 v2 = du_identity'45'unique_342 v2
du_identity'45'unique_342 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_identity'45'unique_342 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_identity'45'unique_348
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.identityʳ-unique
d_identity'691''45'unique_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'691''45'unique_344 ~v0 ~v1 v2
  = du_identity'691''45'unique_344 v2
du_identity'691''45'unique_344 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_identity'691''45'unique_344 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_identity'691''45'unique_338
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.identityˡ-unique
d_identity'737''45'unique_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identity'737''45'unique_346 ~v0 ~v1 v2
  = du_identity'737''45'unique_346 v2
du_identity'737''45'unique_346 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_identity'737''45'unique_346 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_identity'737''45'unique_326
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.inverseʳ-unique
d_inverse'691''45'unique_348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_inverse'691''45'unique_348 ~v0 ~v1 v2
  = du_inverse'691''45'unique_348 v2
du_inverse'691''45'unique_348 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_inverse'691''45'unique_348 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_inverse'691''45'unique_370
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.inverseˡ-unique
d_inverse'737''45'unique_350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_inverse'737''45'unique_350 ~v0 ~v1 v2
  = du_inverse'737''45'unique_350 v2
du_inverse'737''45'unique_350 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_inverse'737''45'unique_350 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_inverse'737''45'unique_358
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.ε⁻¹≈ε
d_ε'8315''185''8776'ε_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 -> AgdaAny
d_ε'8315''185''8776'ε_352 ~v0 ~v1 v2
  = du_ε'8315''185''8776'ε_352 v2
du_ε'8315''185''8776'ε_352 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 -> AgdaAny
du_ε'8315''185''8776'ε_352 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_ε'8315''185''8776'ε_250
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.⁻¹-∙-comm
d_'8315''185''45''8729''45'comm_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45''8729''45'comm_354 ~v0 ~v1 v2
  = du_'8315''185''45''8729''45'comm_354 v2
du_'8315''185''45''8729''45'comm_354 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8315''185''45''8729''45'comm_354 v0
  = coe
      MAlonzo.Code.Algebra.Properties.AbelianGroup.du_'8315''185''45''8729''45'comm_180
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578 (coe v0))
-- Algebra.Properties.Ring._.⁻¹-anti-homo-∙
d_'8315''185''45'anti'45'homo'45''8729'_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'anti'45'homo'45''8729'_356 ~v0 ~v1 v2
  = du_'8315''185''45'anti'45'homo'45''8729'_356 v2
du_'8315''185''45'anti'45'homo'45''8729'_356 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8315''185''45'anti'45'homo'45''8729'_356 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_'8315''185''45'anti'45'homo'45''8729'_316
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.⁻¹-injective
d_'8315''185''45'injective_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'injective_358 ~v0 ~v1 v2
  = du_'8315''185''45'injective_358 v2
du_'8315''185''45'injective_358 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8315''185''45'injective_358 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_'8315''185''45'injective_304
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.⁻¹-involutive
d_'8315''185''45'involutive_360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 -> AgdaAny -> AgdaAny
d_'8315''185''45'involutive_360 ~v0 ~v1 v2
  = du_'8315''185''45'involutive_360 v2
du_'8315''185''45'involutive_360 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 -> AgdaAny -> AgdaAny
du_'8315''185''45'involutive_360 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Properties.Group.du_'8315''185''45'involutive_296
      (coe MAlonzo.Code.Algebra.Bundles.du_group_1474 (coe v1))
-- Algebra.Properties.Ring._.xyx⁻¹≈y
d_xyx'8315''185''8776'y_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_xyx'8315''185''8776'y_362 ~v0 ~v1 v2
  = du_xyx'8315''185''8776'y_362 v2
du_xyx'8315''185''8776'y_362 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_xyx'8315''185''8776'y_362 v0
  = coe
      MAlonzo.Code.Algebra.Properties.AbelianGroup.du_xyx'8315''185''8776'y_170
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578 (coe v0))
-- Algebra.Properties.Ring.-‿distribˡ-*
d_'45''8255'distrib'737''45''42'_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'distrib'737''45''42'_368 ~v0 ~v1 v2 v3 v4
  = du_'45''8255'distrib'737''45''42'_368 v2 v3 v4
du_'45''8255'distrib'737''45''42'_368 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255'distrib'737''45''42'_368 v0 v1 v2
  = coe
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
                        (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
         (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
      (coe
         MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
             let v4
                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                       (coe v3) in
             let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
             let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
             let v7
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
               (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
               (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                let v4
                      = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                          (coe v3) in
                let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                let v7
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                  (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                   let v4
                         = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                             (coe v3) in
                   let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                   let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                   let v7
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                      let v4
                            = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                (coe v3) in
                      let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                      let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                      let v7
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v1)
                           v2)
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                         let v4
                               = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                   (coe v3) in
                         let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                         let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                         let v7
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v1)
                              v2)
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                              (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)) v2)
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                           (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                            let v4
                                  = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                      (coe v3) in
                            let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                            let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                            let v7
                                  = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_setoid_164
                              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                                 (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)) v2)
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                              (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                              (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                               let v4
                                     = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                         (coe v3) in
                               let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                               let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                               let v7
                                     = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_setoid_164
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                                 (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                    (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                       (let v3
                                              = MAlonzo.Code.Algebra.Bundles.d_isRing_3468
                                                  (coe v0) in
                                        let v4
                                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                                  (coe v3) in
                                        let v5
                                              = MAlonzo.Code.Algebra.Structures.d_isGroup_988
                                                  (coe v4) in
                                        let v6
                                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                                                  (coe v5) in
                                        let v7
                                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                  (coe v6) in
                                        coe
                                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                                          (coe
                                             MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                             (coe v7)))))
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                    (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                              (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                               let v4
                                     = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                         (coe v3) in
                               let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_identity'737'_640
                                 (MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                    (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))))
                           (let v3
                                  = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                      (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                            let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
                            let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
                            let v6
                                  = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                                 (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)) v2)
                              (coe MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                              (coe
                                 MAlonzo.Code.Algebra.Structures.du_zero'737'_2512
                                 (MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) v2)))
                        (let v3
                               = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                   (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                         let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
                         let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
                         let v6
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                           (coe
                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                              (\ v7 -> coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v7 v2)
                              (\ v7 v8 -> v7)
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v1)
                              (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
                           (coe
                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                              (\ v7 v8 -> v8)
                              (\ v7 -> coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v7 v2)
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v1)
                              (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
                           (let v7
                                  = coe
                                      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500
                                      (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                            let v8
                                  = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8))
                              (coe v2)
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v1)
                              (coe MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                              (let v9 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                               let v10
                                     = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                         (coe v9) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
                                 (MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v10)) v1))))
                     (let v3
                            = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                      let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
                      let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
                      let v6
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v1)
                           v2)
                        (coe
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
                                                MAlonzo.Code.Algebra.Bundles.d_isRing_3468
                                                (coe v0))))))))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v1)
                              v2)
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                           (let v7 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                            let v8
                                  = coe
                                      MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v7) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
                              (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                 (coe v8))
                              v2 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v1))))
                  (coe
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
                                          MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_assoc_446
                        (MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_isGroup_988
                                 (coe
                                    MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                    (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))))
               (let v3
                      = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                          (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
                let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
                let v6
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                  (coe MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                  (coe
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
                                          MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                     (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                     (let v7 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                      let v8
                            = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                (coe v7) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
                        (MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v8))
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))))
            (coe
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
                                 (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
                  (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)
               (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                let v4
                      = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                          (coe v3) in
                let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_identity'691'_642
                  (MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1) v2)))))
-- Algebra.Properties.Ring.-‿distribʳ-*
d_'45''8255'distrib'691''45''42'_378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'distrib'691''45''42'_378 ~v0 ~v1 v2 v3 v4
  = du_'45''8255'distrib'691''45''42'_378 v2 v3 v4
du_'45''8255'distrib'691''45''42'_378 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'45''8255'distrib'691''45''42'_378 v0 v1 v2
  = coe
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
                        (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
         (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))
      (coe
         MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
         (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
             let v4
                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                       (coe v3) in
             let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
             let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
             let v7
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
               (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
               (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                  (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                let v4
                      = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                          (coe v3) in
                let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                let v7
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                  (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                     (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                   let v4
                         = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                             (coe v3) in
                   let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                   let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                   let v7
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                        (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                           (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                      let v4
                            = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                (coe v3) in
                      let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                      let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                      let v7
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                              (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0 v2
                              (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                         let v4
                               = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                   (coe v3) in
                         let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                         let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                         let v7
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0 v2
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                              (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                           (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                            let v4
                                  = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                      (coe v3) in
                            let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                            let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                            let v7
                                  = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_setoid_164
                              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                                 (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                              (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                              (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                               let v4
                                     = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                         (coe v3) in
                               let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                               let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5) in
                               let v7
                                     = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_setoid_164
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                    (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                                 (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                       (let v3
                                              = MAlonzo.Code.Algebra.Bundles.d_isRing_3468
                                                  (coe v0) in
                                        let v4
                                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                                  (coe v3) in
                                        let v5
                                              = MAlonzo.Code.Algebra.Structures.d_isGroup_988
                                                  (coe v4) in
                                        let v6
                                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                                                  (coe v5) in
                                        let v7
                                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                  (coe v6) in
                                        coe
                                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                                          (coe
                                             MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                             (coe v7)))))
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                    (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))
                              (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                               let v4
                                     = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                         (coe v3) in
                               let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_identity'691'_642
                                 (MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                    (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))))
                           (let v3
                                  = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                      (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                            let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
                            let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
                            let v6
                                  = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                                 (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
                              (coe MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                              (coe
                                 MAlonzo.Code.Algebra.Structures.du_zero'691'_2514
                                 (MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) v1)))
                        (let v3
                               = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                   (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                         let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
                         let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
                         let v6
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                           (coe
                              MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1)
                              (\ v7 v8 -> v7)
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0 v2
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))
                              (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
                           (coe
                              MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                              (\ v7 v8 -> v8)
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1)
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0 v2
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))
                              (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0)))
                           (let v7
                                  = coe
                                      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500
                                      (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                            let v8
                                  = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8))
                              (coe v1)
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0 v2
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))
                              (coe MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                              (let v9 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                               let v10
                                     = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                         (coe v9) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
                                 (MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v10)) v2))))
                     (let v3
                            = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                      let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
                      let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
                      let v6
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                              (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0 v2
                              (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))
                        (coe
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
                                                MAlonzo.Code.Algebra.Bundles.d_isRing_3468
                                                (coe v0))))))))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0 v2
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                              (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                                 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))
                           (let v7 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                            let v8
                                  = coe
                                      MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v7) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
                              (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                 (coe v8))
                              v1 v2 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))))
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_assoc_446
                     (MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isGroup_988
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                 (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                     (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                        (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))))
               (let v3
                      = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                          (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0)) in
                let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
                let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
                let v6
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))
                  (coe MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                     (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                  (coe
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
                                          MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                        (coe
                           MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                           (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2))
                     (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                     (let v7 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                      let v8
                            = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                (coe v7) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
                        (MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v8))
                        (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)))))
            (coe
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
                                 (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
                  (MAlonzo.Code.Algebra.Bundles.d_0'35'_3464 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                  (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))
               (let v3 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                let v4
                      = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                          (coe v3) in
                let v5 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_identity'737'_640
                  (MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v5))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v2))))))
-- Algebra.Properties.Ring.-1*x≈-x
d_'45'1'42'x'8776''45'x_386 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 -> AgdaAny -> AgdaAny
d_'45'1'42'x'8776''45'x_386 ~v0 ~v1 v2 v3
  = du_'45'1'42'x'8776''45'x_386 v2 v3
du_'45'1'42'x'8776''45'x_386 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 -> AgdaAny -> AgdaAny
du_'45'1'42'x'8776''45'x_386 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v2 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
          let v3
                = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                    (coe v2) in
          let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
          let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
          let v6
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
            (coe
               MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
               (MAlonzo.Code.Algebra.Bundles.d_1'35'_3466 (coe v0)))
            v1)
         (coe
            MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
               (MAlonzo.Code.Algebra.Bundles.d_1'35'_3466 (coe v0)) v1))
         (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v2 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
             let v3
                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                       (coe v2) in
             let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
             let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
             let v6
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                  (MAlonzo.Code.Algebra.Bundles.d_1'35'_3466 (coe v0)) v1))
            (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1)
            (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v2 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                      let v3
                            = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                (coe v2) in
                      let v4 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3) in
                      let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4) in
                      let v6
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))))
               (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v1))
            (coe
               MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
               (MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                     (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                  (MAlonzo.Code.Algebra.Bundles.d_1'35'_3466 (coe v0)) v1)
               v1
               (let v2 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_identity'737'_640
                  (coe
                     MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_2500 (coe v2))
                  v1)))
         (coe
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
                              (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
            (coe
               MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                  (MAlonzo.Code.Algebra.Bundles.d_1'35'_3466 (coe v0)) v1))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                  (MAlonzo.Code.Algebra.Bundles.d_1'35'_3466 (coe v0)))
               v1)
            (coe
               du_'45''8255'distrib'737''45''42'_368 (coe v0)
               (coe MAlonzo.Code.Algebra.Bundles.d_1'35'_3466 (coe v0))
               (coe v1))))
-- Algebra.Properties.Ring.x+x≈x⇒x≈0
d_x'43'x'8776'x'8658'x'8776'0_392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_x'43'x'8776'x'8658'x'8776'0_392 ~v0 ~v1 v2 v3 v4
  = du_x'43'x'8776'x'8658'x'8776'0_392 v2 v3 v4
du_x'43'x'8776'x'8658'x'8776'0_392 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_x'43'x'8776'x'8658'x'8776'0_392 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Properties.Group.du_identity'737''45'unique_326
      (coe
         MAlonzo.Code.Algebra.Bundles.du_group_1474
         (coe
            MAlonzo.Code.Algebra.Bundles.du_'43''45'abelianGroup_3578
            (coe v0)))
      (coe v1) (coe v1) (coe v2)
-- Algebra.Properties.Ring.x[y-z]≈xy-xz
d_x'91'y'45'z'93''8776'xy'45'xz_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'91'y'45'z'93''8776'xy'45'xz_404 ~v0 ~v1 v2 v3 v4 v5
  = du_x'91'y'45'z'93''8776'xy'45'xz_404 v2 v3 v4 v5
du_x'91'y'45'z'93''8776'xy'45'xz_404 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'91'y'45'z'93''8776'xy'45'xz_404 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                    (coe v4) in
          let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
          let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
          let v8
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
            (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
             let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
             coe
               MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
               (coe v2) (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
               (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3)))
         (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
          let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
          coe
            MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
            (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
            (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                       (coe v4) in
             let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
             let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
             let v8
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                  (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3)))
            (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
             let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
             coe
               MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v3))
            (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
             let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
             coe
               MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                      let v5
                            = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                (coe v4) in
                      let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
                      let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
                      let v8
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))))
               (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
                let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
                  (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v3)))
            (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                       (coe v4) in
             let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
             let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
             let v8
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
             coe
               MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8))
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v2)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                  (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v3))
               (coe
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
                                    (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1 v3))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v1
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3))
                  (coe
                     du_'45''8255'distrib'691''45''42'_378 (coe v0) (coe v1)
                     (coe v3)))))
         (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
          let v5
                = coe
                    MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
            (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v5))
            v1 v2 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3)))
-- Algebra.Properties.Ring.[y-z]x≈yx-zx
d_'91'y'45'z'93'x'8776'yx'45'zx_418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'91'y'45'z'93'x'8776'yx'45'zx_418 ~v0 ~v1 v2 v3 v4 v5
  = du_'91'y'45'z'93'x'8776'yx'45'zx_418 v2 v3 v4 v5
du_'91'y'45'z'93'x'8776'yx'45'zx_418 ::
  MAlonzo.Code.Algebra.Bundles.T_Ring_3432 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'91'y'45'z'93'x'8776'yx'45'zx_418 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                    (coe v4) in
          let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
          let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
          let v8
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
            (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
             let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
             coe
               MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
               (coe v2) (coe v3))
            v1)
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
            (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v2 v1)
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
               (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3) v1))
         (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
          let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
          coe
            MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
            (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v2 v1)
            (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v3 v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                       (coe v4) in
             let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
             let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
             let v8
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'43'__3458 v0
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v2 v1)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3) v1))
            (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
             let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
             coe
               MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v2 v1)
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v3 v1))
            (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
             let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
             coe
               MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v2 v1)
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v3 v1))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
                      let v5
                            = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                                (coe v4) in
                      let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
                      let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
                      let v8
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))))
               (let v4 = MAlonzo.Code.Algebra.Bundles.d__'43'__3458 (coe v0) in
                let v5 = MAlonzo.Code.Algebra.Bundles.d_'45'__3462 (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v4) (coe v5)
                  (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v2 v1)
                  (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v3 v1)))
            (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
                       (coe v4) in
             let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
             let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
             let v8
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
             coe
               MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8))
               (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v2 v1)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3) v1)
               (coe
                  MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                  (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v3 v1))
               (coe
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
                                    (coe MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0))))))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0 v3 v1))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'42'__3460 v0
                     (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3) v1)
                  (coe
                     du_'45''8255'distrib'737''45''42'_368 (coe v0) (coe v3)
                     (coe v1)))))
         (let v4 = MAlonzo.Code.Algebra.Bundles.d_isRing_3468 (coe v0) in
          let v5
                = coe
                    MAlonzo.Code.Algebra.Structures.du_isSemiring_2518 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
            (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
               (coe v5))
            v1 v2 (coe MAlonzo.Code.Algebra.Bundles.d_'45'__3462 v0 v3)))
