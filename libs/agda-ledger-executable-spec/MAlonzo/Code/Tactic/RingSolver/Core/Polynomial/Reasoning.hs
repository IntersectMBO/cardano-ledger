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

module MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Reasoning where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing

-- Tactic.RingSolver.Core.Polynomial.Reasoning._._IsRelatedTo_
d__IsRelatedTo__164 a0 a1 a2 a3 a4 = ()
-- Tactic.RingSolver.Core.Polynomial.Reasoning._._∎
d__'8718'_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d__'8718'_166 ~v0 ~v1 v2 = du__'8718'_166 v2
du__'8718'_166 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du__'8718'_166 v0
  = let v1
          = let v1
                  = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                      (coe v0) in
            let v2
                  = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
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
              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)) in
    coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1)))
-- Tactic.RingSolver.Core.Polynomial.Reasoning._._≡⟨⟩_
d__'8801''10216''10217'__168 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d__'8801''10216''10217'__168 v0 = coe v0
-- Tactic.RingSolver.Core.Polynomial.Reasoning._.begin_
d_begin__170 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  AgdaAny
d_begin__170
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
-- Tactic.RingSolver.Core.Polynomial.Reasoning._.step-≈
d_step'45''8776'_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45''8776'_174 ~v0 ~v1 v2 = du_step'45''8776'_174 v2
du_step'45''8776'_174 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45''8776'_174 v0
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
      (let v1
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                 (coe v0) in
       let v2
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
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
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
-- Tactic.RingSolver.Core.Polynomial.Reasoning._.step-≈˘
d_step'45''8776''728'_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45''8776''728'_176 ~v0 ~v1 v2
  = du_step'45''8776''728'_176 v2
du_step'45''8776''728'_176 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45''8776''728'_176 v0
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
      (let v1
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                 (coe v0) in
       let v2
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
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
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
-- Tactic.RingSolver.Core.Polynomial.Reasoning._.step-≡
d_step'45''8801'_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45''8801'_178 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7
  = du_step'45''8801'_178 v6
du_step'45''8801'_178 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45''8801'_178 v0 = coe v0
-- Tactic.RingSolver.Core.Polynomial.Reasoning._.step-≡˘
d_step'45''8801''728'_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
d_step'45''8801''728'_180 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7
  = du_step'45''8801''728'_180 v6
du_step'45''8801''728'_180 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.T__IsRelatedTo__26
du_step'45''8801''728'_180 v0 = coe v0
-- Tactic.RingSolver.Core.Polynomial.Reasoning.≪+_
d_'8810''43'__192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8810''43'__192 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_'8810''43'__192 v2 v3 v4 v5 v6
du_'8810''43'__192 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8810''43'__192 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (MAlonzo.Code.Algebra.Structures.d_isMagma_444
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
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                              (coe v0)))))))))
      v1 v2 v3 v3 v4
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
         (coe v0) (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Reasoning.+≫_
d_'43''8811'__202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'43''8811'__202 ~v0 ~v1 v2 v3 v4 v5
  = du_'43''8811'__202 v2 v3 v4 v5
du_'43''8811'__202 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'43''8811'__202 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (MAlonzo.Code.Algebra.Structures.d_isMagma_444
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
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                              (coe v0)))))))))
      v1 v1 v2 v3
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
         (coe v0) (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Reasoning.≪*_
d_'8810''42'__210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8810''42'__210 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_'8810''42'__210 v2 v3 v4 v5 v6
du_'8810''42'__210 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8810''42'__210 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                  (coe v0)))))
      v1 v2 v3 v3 v4
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
         (coe v0) (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Reasoning.*≫_
d_'42''8811'__220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''8811'__220 ~v0 ~v1 v2 v3 v4 v5
  = du_'42''8811'__220 v2 v3 v4 v5
du_'42''8811'__220 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'42''8811'__220 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                  (coe v0)))))
      v1 v1 v2 v3
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
         (coe v0) (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Reasoning._⊙_
d__'8857'__228 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d__'8857'__228 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
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
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                 (coe v0))))))))))
      v1 v2 v3
