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

module MAlonzo.Code.Algebra.Lattice.Properties.Lattice where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Lattice.Bundles
import qualified MAlonzo.Code.Algebra.Lattice.Properties.Semilattice
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Construct.NaturalOrder.Left
import qualified MAlonzo.Code.Relation.Binary.Lattice.Bundles
import qualified MAlonzo.Code.Relation.Binary.Lattice.Structures
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Lattice.Properties.Lattice._.Idempotent
d_Idempotent_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Idempotent_110 = erased
-- Algebra.Lattice.Properties.Lattice._.IsBand
d_IsBand_206 a0 a1 a2 a3 = ()
-- Algebra.Lattice.Properties.Lattice._.IsMagma
d_IsMagma_242 a0 a1 a2 a3 = ()
-- Algebra.Lattice.Properties.Lattice._.IsSemigroup
d_IsSemigroup_270 a0 a1 a2 a3 = ()
-- Algebra.Lattice.Properties.Lattice._.IsLattice
d_IsLattice_2516 a0 a1 a2 a3 a4 = ()
-- Algebra.Lattice.Properties.Lattice._.IsSemilattice
d_IsSemilattice_2520 a0 a1 a2 a3 = ()
-- Algebra.Lattice.Properties.Lattice.∧-idem
d_'8743''45'idem_2948 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny
d_'8743''45'idem_2948 ~v0 ~v1 v2 v3 = du_'8743''45'idem_2948 v2 v3
du_'8743''45'idem_2948 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny
du_'8743''45'idem_2948 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v1)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v1)))
         (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v1)))
            v1 v1
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))))
               (coe v1))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)) v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v1)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))
            (coe v1)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v1))
            (coe v1)
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)) v1
               v1)))
-- Algebra.Lattice.Properties.Lattice.∧-isMagma
d_'8743''45'isMagma_2952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8743''45'isMagma_2952 ~v0 ~v1 v2 = du_'8743''45'isMagma_2952 v2
du_'8743''45'isMagma_2952 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8743''45'isMagma_2952 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Properties.Lattice.∧-isSemigroup
d_'8743''45'isSemigroup_2954 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8743''45'isSemigroup_2954 ~v0 ~v1 v2
  = du_'8743''45'isSemigroup_2954 v2
du_'8743''45'isSemigroup_2954 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8743''45'isSemigroup_2954 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'8743''45'isMagma_2952 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Properties.Lattice.∧-isBand
d_'8743''45'isBand_2956 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8743''45'isBand_2956 ~v0 ~v1 v2 = du_'8743''45'isBand_2956 v2
du_'8743''45'isBand_2956 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_'8743''45'isBand_2956 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsBand'46'constructor_10089
      (coe du_'8743''45'isSemigroup_2954 (coe v0))
      (coe du_'8743''45'idem_2948 (coe v0))
-- Algebra.Lattice.Properties.Lattice.∧-isSemilattice
d_'8743''45'isSemilattice_2958 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8743''45'isSemilattice_2958 ~v0 ~v1 v2
  = du_'8743''45'isSemilattice_2958 v2
du_'8743''45'isSemilattice_2958 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_'8743''45'isSemilattice_2958 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsSemilattice'46'constructor_31019
      (coe du_'8743''45'isBand_2956 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Properties.Lattice.∧-semilattice
d_'8743''45'semilattice_2960 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8743''45'semilattice_2960 ~v0 ~v1 v2
  = du_'8743''45'semilattice_2960 v2
du_'8743''45'semilattice_2960 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
du_'8743''45'semilattice_2960 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_Semilattice'46'constructor_187
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 (coe v0))
      (coe du_'8743''45'isSemilattice_2958 (coe v0))
-- Algebra.Lattice.Properties.Lattice._.∧-isOrderTheoreticJoinSemilattice
d_'8743''45'isOrderTheoreticJoinSemilattice_2964 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_'8743''45'isOrderTheoreticJoinSemilattice_2964 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticJoinSemilattice_2964 v2
du_'8743''45'isOrderTheoreticJoinSemilattice_2964 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_'8743''45'isOrderTheoreticJoinSemilattice_2964 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticJoinSemilattice_162
      (coe du_'8743''45'semilattice_2960 (coe v0))
-- Algebra.Lattice.Properties.Lattice._.∧-isOrderTheoreticMeetSemilattice
d_'8743''45'isOrderTheoreticMeetSemilattice_2966 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_'8743''45'isOrderTheoreticMeetSemilattice_2966 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticMeetSemilattice_2966 v2
du_'8743''45'isOrderTheoreticMeetSemilattice_2966 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_'8743''45'isOrderTheoreticMeetSemilattice_2966 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticMeetSemilattice_160
      (coe du_'8743''45'semilattice_2960 (coe v0))
-- Algebra.Lattice.Properties.Lattice._.∧-orderTheoreticJoinSemilattice
d_'8743''45'orderTheoreticJoinSemilattice_2968 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
d_'8743''45'orderTheoreticJoinSemilattice_2968 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticJoinSemilattice_2968 v2
du_'8743''45'orderTheoreticJoinSemilattice_2968 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
du_'8743''45'orderTheoreticJoinSemilattice_2968 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticJoinSemilattice_166
      (coe du_'8743''45'semilattice_2960 (coe v0))
-- Algebra.Lattice.Properties.Lattice._.∧-orderTheoreticMeetSemilattice
d_'8743''45'orderTheoreticMeetSemilattice_2970 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
d_'8743''45'orderTheoreticMeetSemilattice_2970 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticMeetSemilattice_2970 v2
du_'8743''45'orderTheoreticMeetSemilattice_2970 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
du_'8743''45'orderTheoreticMeetSemilattice_2970 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticMeetSemilattice_164
      (coe du_'8743''45'semilattice_2960 (coe v0))
-- Algebra.Lattice.Properties.Lattice.∨-idem
d_'8744''45'idem_2972 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny
d_'8744''45'idem_2972 ~v0 ~v1 v2 v3 = du_'8744''45'idem_2972 v2 v3
du_'8744''45'idem_2972 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny
du_'8744''45'idem_2972 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1 v1)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v1))
         (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v1))
            v1 v1
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))))
               (coe v1))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)) v1
               v1))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))
            (coe v1)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v1)
            (coe v1) (coe du_'8743''45'idem_2948 (coe v0) (coe v1))))
-- Algebra.Lattice.Properties.Lattice.∨-isMagma
d_'8744''45'isMagma_2976 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8744''45'isMagma_2976 ~v0 ~v1 v2 = du_'8744''45'isMagma_2976 v2
du_'8744''45'isMagma_2976 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8744''45'isMagma_2976 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Properties.Lattice.∨-isSemigroup
d_'8744''45'isSemigroup_2978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8744''45'isSemigroup_2978 ~v0 ~v1 v2
  = du_'8744''45'isSemigroup_2978 v2
du_'8744''45'isSemigroup_2978 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8744''45'isSemigroup_2978 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'8744''45'isMagma_2976 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Properties.Lattice.∨-isBand
d_'8744''45'isBand_2980 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8744''45'isBand_2980 ~v0 ~v1 v2 = du_'8744''45'isBand_2980 v2
du_'8744''45'isBand_2980 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_'8744''45'isBand_2980 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsBand'46'constructor_10089
      (coe du_'8744''45'isSemigroup_2978 (coe v0))
      (coe du_'8744''45'idem_2972 (coe v0))
-- Algebra.Lattice.Properties.Lattice.∨-isSemilattice
d_'8744''45'isSemilattice_2982 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8744''45'isSemilattice_2982 ~v0 ~v1 v2
  = du_'8744''45'isSemilattice_2982 v2
du_'8744''45'isSemilattice_2982 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_'8744''45'isSemilattice_2982 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsSemilattice'46'constructor_31019
      (coe du_'8744''45'isBand_2980 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Properties.Lattice.∨-semilattice
d_'8744''45'semilattice_2984 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8744''45'semilattice_2984 ~v0 ~v1 v2
  = du_'8744''45'semilattice_2984 v2
du_'8744''45'semilattice_2984 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
du_'8744''45'semilattice_2984 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_Semilattice'46'constructor_187
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 (coe v0))
      (coe du_'8744''45'isSemilattice_2982 (coe v0))
-- Algebra.Lattice.Properties.Lattice._.∧-isOrderTheoreticJoinSemilattice
d_'8743''45'isOrderTheoreticJoinSemilattice_2988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_'8743''45'isOrderTheoreticJoinSemilattice_2988 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticJoinSemilattice_2988 v2
du_'8743''45'isOrderTheoreticJoinSemilattice_2988 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_'8743''45'isOrderTheoreticJoinSemilattice_2988 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticJoinSemilattice_162
      (coe du_'8744''45'semilattice_2984 (coe v0))
-- Algebra.Lattice.Properties.Lattice._.∧-isOrderTheoreticMeetSemilattice
d_'8743''45'isOrderTheoreticMeetSemilattice_2990 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_'8743''45'isOrderTheoreticMeetSemilattice_2990 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticMeetSemilattice_2990 v2
du_'8743''45'isOrderTheoreticMeetSemilattice_2990 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_'8743''45'isOrderTheoreticMeetSemilattice_2990 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticMeetSemilattice_160
      (coe du_'8744''45'semilattice_2984 (coe v0))
-- Algebra.Lattice.Properties.Lattice._.∧-orderTheoreticJoinSemilattice
d_'8743''45'orderTheoreticJoinSemilattice_2992 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
d_'8743''45'orderTheoreticJoinSemilattice_2992 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticJoinSemilattice_2992 v2
du_'8743''45'orderTheoreticJoinSemilattice_2992 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
du_'8743''45'orderTheoreticJoinSemilattice_2992 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticJoinSemilattice_166
      (coe du_'8744''45'semilattice_2984 (coe v0))
-- Algebra.Lattice.Properties.Lattice._.∧-orderTheoreticMeetSemilattice
d_'8743''45'orderTheoreticMeetSemilattice_2994 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
d_'8743''45'orderTheoreticMeetSemilattice_2994 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticMeetSemilattice_2994 v2
du_'8743''45'orderTheoreticMeetSemilattice_2994 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
du_'8743''45'orderTheoreticMeetSemilattice_2994 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticMeetSemilattice_164
      (coe du_'8744''45'semilattice_2984 (coe v0))
-- Algebra.Lattice.Properties.Lattice.∧-∨-isLattice
d_'8743''45''8744''45'isLattice_2996 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8743''45''8744''45'isLattice_2996 ~v0 ~v1 v2
  = du_'8743''45''8744''45'isLattice_2996 v2
du_'8743''45''8744''45'isLattice_2996 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
du_'8743''45''8744''45'isLattice_2996 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsLattice'46'constructor_34033
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe
         MAlonzo.Code.Data.Product.Base.du_swap_346
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_absorptive_2780
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))))
-- Algebra.Lattice.Properties.Lattice.∧-∨-lattice
d_'8743''45''8744''45'lattice_2998 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8743''45''8744''45'lattice_2998 ~v0 ~v1 v2
  = du_'8743''45''8744''45'lattice_2998 v2
du_'8743''45''8744''45'lattice_2998 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
du_'8743''45''8744''45'lattice_2998 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_Lattice'46'constructor_7911
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 (coe v0))
      (coe du_'8743''45''8744''45'isLattice_2996 (coe v0))
-- Algebra.Lattice.Properties.Lattice._.poset
d_poset_3002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_3002 ~v0 ~v1 v2 = du_poset_3002 v2
du_poset_3002 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_3002 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_poset_146
      (coe du_'8743''45'semilattice_2960 (coe v0))
-- Algebra.Lattice.Properties.Lattice._._≤_
d__'8804'__3006 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny -> ()
d__'8804'__3006 = erased
-- Algebra.Lattice.Properties.Lattice.∨-∧-isOrderTheoreticLattice
d_'8744''45''8743''45'isOrderTheoreticLattice_3010 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
d_'8744''45''8743''45'isOrderTheoreticLattice_3010 ~v0 ~v1 v2
  = du_'8744''45''8743''45'isOrderTheoreticLattice_3010 v2
du_'8744''45''8743''45'isOrderTheoreticLattice_3010 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
du_'8744''45''8743''45'isOrderTheoreticLattice_3010 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.C_IsLattice'46'constructor_14441
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
         (coe
            MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_poset_146
            (coe du_'8743''45'semilattice_2960 (coe v0))))
      (coe du_supremum_3054 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Construct.NaturalOrder.Left.du_infimum_3356
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 (coe v0))
         (coe du_'8743''45'isSemilattice_2958 (coe v0)))
-- Algebra.Lattice.Properties.Lattice._._._≤_
d__'8804'__3022 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny -> ()
d__'8804'__3022 = erased
-- Algebra.Lattice.Properties.Lattice._.sound
d_sound_3034 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sound_3034 ~v0 ~v1 v2 v3 v4 v5 = du_sound_3034 v2 v3 v4 v5
du_sound_3034 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sound_3034 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v2)
      v1
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v2)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2 v1))
            v1
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1 v2))
               v1
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1 v2))
                  v1 v1
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))))
                     (coe v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)) v1
                     v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))
                  (coe v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1 v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)) v2
                     v1)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))
               (coe v1) (coe v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2 v1)
               (coe v3))))
-- Algebra.Lattice.Properties.Lattice._.complete
d_complete_3046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_complete_3046 ~v0 ~v1 v2 v3 v4 v5 = du_complete_3046 v2 v3 v4 v5
du_complete_3046 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_complete_3046 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2 v1)
      v2
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2 v1)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v2))
            v2
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v2 v1))
               v2
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v2 v1))
                  v2 v2
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))))
                     (coe v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)) v2
                     v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))
                  (coe v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v2 v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0)) v1
                     v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_isLattice_522 (coe v0))
               (coe v2) (coe v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 v0 v1 v2)
               (coe v3))))
-- Algebra.Lattice.Properties.Lattice._.supremum
d_supremum_3054 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_supremum_3054 ~v0 ~v1 v2 v3 v4 = du_supremum_3054 v2 v3 v4
du_supremum_3054 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_supremum_3054 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_sound_3034 (coe v0) (coe v1)
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1 v2)
         (coe
            MAlonzo.Code.Relation.Binary.Lattice.Structures.du_x'8804'x'8744'y_38
            (coe
               MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticJoinSemilattice_162
               (coe du_'8744''45'semilattice_2984 (coe v0)))
            (coe v1) (coe v2)))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            du_sound_3034 (coe v0) (coe v2)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1 v2)
            (coe
               MAlonzo.Code.Relation.Binary.Lattice.Structures.du_y'8804'x'8744'y_50
               (coe
                  MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticJoinSemilattice_162
                  (coe du_'8744''45'semilattice_2984 (coe v0)))
               (coe v1) (coe v2)))
         (coe
            (\ v3 v4 v5 ->
               coe
                 du_sound_3034 (coe v0)
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 v0 v1 v2)
                 (coe v3)
                 (coe
                    MAlonzo.Code.Relation.Binary.Lattice.Structures.du_'8744''45'least_64
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticJoinSemilattice_162
                       (coe du_'8744''45'semilattice_2984 (coe v0)))
                    v1 v2 v3 (coe du_complete_3046 (coe v0) (coe v1) (coe v3) (coe v4))
                    (coe du_complete_3046 (coe v0) (coe v2) (coe v3) (coe v5))))))
-- Algebra.Lattice.Properties.Lattice.∨-∧-orderTheoreticLattice
d_'8744''45''8743''45'orderTheoreticLattice_3066 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_Lattice_362
d_'8744''45''8743''45'orderTheoreticLattice_3066 ~v0 ~v1 v2
  = du_'8744''45''8743''45'orderTheoreticLattice_3066 v2
du_'8744''45''8743''45'orderTheoreticLattice_3066 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_Lattice_362
du_'8744''45''8743''45'orderTheoreticLattice_3066 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Bundles.C_Lattice'46'constructor_8383
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__518 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__520 (coe v0))
      (coe du_'8744''45''8743''45'isOrderTheoreticLattice_3010 (coe v0))
