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

module MAlonzo.Code.Algebra.Lattice.Properties.BooleanAlgebra where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Consequences.Setoid
import qualified MAlonzo.Code.Algebra.Lattice.Bundles
import qualified MAlonzo.Code.Algebra.Lattice.Properties.DistributiveLattice
import qualified MAlonzo.Code.Algebra.Lattice.Properties.Lattice
import qualified MAlonzo.Code.Algebra.Lattice.Properties.Semilattice
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Lattice.Bundles
import qualified MAlonzo.Code.Relation.Binary.Lattice.Structures
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Lattice.Properties.BooleanAlgebra._.IsAbelianGroup
d_IsAbelianGroup_116 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsCommutativeMonoid
d_IsCommutativeMonoid_126 a0 a1 a2 a3 a4 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsCommutativeRing
d_IsCommutativeRing_128 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsCommutativeSemiring
d_IsCommutativeSemiring_132 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsGroup
d_IsGroup_138 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsMagma
d_IsMagma_156 a0 a1 a2 a3 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsMonoid
d_IsMonoid_162 a0 a1 a2 a3 a4 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsRing
d_IsRing_178 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsSemigroup
d_IsSemigroup_184 a0 a1 a2 a3 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsSemiring
d_IsSemiring_188 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._._DistributesOver_
d__DistributesOver__2420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver__2420 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._._DistributesOverʳ_
d__DistributesOver'691'__2422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'691'__2422 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._._DistributesOverˡ_
d__DistributesOver'737'__2424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'737'__2424 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.Associative
d_Associative_2438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Associative_2438 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.Commutative
d_Commutative_2442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Commutative_2442 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.Congruent₂
d_Congruent'8322'_2446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Congruent'8322'_2446 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.Identity
d_Identity_2458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Identity_2458 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.Inverse
d_Inverse_2462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Inverse_2462 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.Involutive
d_Involutive_2466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny) -> ()
d_Involutive_2466 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.LeftIdentity
d_LeftIdentity_2484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftIdentity_2484 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.LeftInverse
d_LeftInverse_2486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftInverse_2486 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.LeftZero
d_LeftZero_2492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftZero_2492 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.RightIdentity
d_RightIdentity_2514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightIdentity_2514 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.RightInverse
d_RightInverse_2516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightInverse_2516 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.RightZero
d_RightZero_2522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightZero_2522 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.Zero
d_Zero_2540 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Zero_2540 = erased
-- Algebra.Lattice.Properties.BooleanAlgebra._.IsBooleanAlgebra
d_IsBooleanAlgebra_2606 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Algebra.Lattice.Properties.BooleanAlgebra._.poset
d_poset_3052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_3052 ~v0 ~v1 v2 = du_poset_3052 v2
du_poset_3052 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_3052 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_poset_146
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-idem
d_'8743''45'idem_3054 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8743''45'idem_3054 ~v0 ~v1 v2 = du_'8743''45'idem_3054 v2
du_'8743''45'idem_3054 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8743''45'idem_3054 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'idem_2948
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-isBand
d_'8743''45'isBand_3056 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8743''45'isBand_3056 ~v0 ~v1 v2 = du_'8743''45'isBand_3056 v2
du_'8743''45'isBand_3056 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_'8743''45'isBand_3056 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isBand_2956
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-isMagma
d_'8743''45'isMagma_3058 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8743''45'isMagma_3058 ~v0 ~v1 v2 = du_'8743''45'isMagma_3058 v2
du_'8743''45'isMagma_3058 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8743''45'isMagma_3058 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isMagma_2952
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-isOrderTheoreticJoinSemilattice
d_'8743''45'isOrderTheoreticJoinSemilattice_3060 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_'8743''45'isOrderTheoreticJoinSemilattice_3060 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticJoinSemilattice_3060 v2
du_'8743''45'isOrderTheoreticJoinSemilattice_3060 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_'8743''45'isOrderTheoreticJoinSemilattice_3060 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticJoinSemilattice_162
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-isOrderTheoreticMeetSemilattice
d_'8743''45'isOrderTheoreticMeetSemilattice_3062 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_'8743''45'isOrderTheoreticMeetSemilattice_3062 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticMeetSemilattice_3062 v2
du_'8743''45'isOrderTheoreticMeetSemilattice_3062 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_'8743''45'isOrderTheoreticMeetSemilattice_3062 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticMeetSemilattice_160
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-isSemigroup
d_'8743''45'isSemigroup_3064 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8743''45'isSemigroup_3064 ~v0 ~v1 v2
  = du_'8743''45'isSemigroup_3064 v2
du_'8743''45'isSemigroup_3064 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8743''45'isSemigroup_3064 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isSemigroup_2954
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-isSemilattice
d_'8743''45'isSemilattice_3066 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8743''45'isSemilattice_3066 ~v0 ~v1 v2
  = du_'8743''45'isSemilattice_3066 v2
du_'8743''45'isSemilattice_3066 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_'8743''45'isSemilattice_3066 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isSemilattice_2958
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-orderTheoreticJoinSemilattice
d_'8743''45'orderTheoreticJoinSemilattice_3068 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
d_'8743''45'orderTheoreticJoinSemilattice_3068 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticJoinSemilattice_3068 v2
du_'8743''45'orderTheoreticJoinSemilattice_3068 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
du_'8743''45'orderTheoreticJoinSemilattice_3068 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticJoinSemilattice_166
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-orderTheoreticMeetSemilattice
d_'8743''45'orderTheoreticMeetSemilattice_3070 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
d_'8743''45'orderTheoreticMeetSemilattice_3070 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticMeetSemilattice_3070 v2
du_'8743''45'orderTheoreticMeetSemilattice_3070 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
du_'8743''45'orderTheoreticMeetSemilattice_3070 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticMeetSemilattice_164
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-semilattice
d_'8743''45'semilattice_3072 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8743''45'semilattice_3072 ~v0 ~v1 v2
  = du_'8743''45'semilattice_3072 v2
du_'8743''45'semilattice_3072 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
du_'8743''45'semilattice_3072 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-∨-distributiveLattice
d_'8743''45''8744''45'distributiveLattice_3074 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8743''45''8744''45'distributiveLattice_3074 ~v0 ~v1 v2
  = du_'8743''45''8744''45'distributiveLattice_3074 v2
du_'8743''45''8744''45'distributiveLattice_3074 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
du_'8743''45''8744''45'distributiveLattice_3074 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.DistributiveLattice.du_'8743''45''8744''45'distributiveLattice_716
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
         (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-∨-isDistributiveLattice
d_'8743''45''8744''45'isDistributiveLattice_3076 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8743''45''8744''45'isDistributiveLattice_3076 ~v0 ~v1 v2
  = du_'8743''45''8744''45'isDistributiveLattice_3076 v2
du_'8743''45''8744''45'isDistributiveLattice_3076 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
du_'8743''45''8744''45'isDistributiveLattice_3076 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.DistributiveLattice.du_'8743''45''8744''45'isDistributiveLattice_714
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
         (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-∨-isLattice
d_'8743''45''8744''45'isLattice_3078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8743''45''8744''45'isLattice_3078 ~v0 ~v1 v2
  = du_'8743''45''8744''45'isLattice_3078 v2
du_'8743''45''8744''45'isLattice_3078 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
du_'8743''45''8744''45'isLattice_3078 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45''8744''45'isLattice_2996
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-∨-lattice
d_'8743''45''8744''45'lattice_3080 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8743''45''8744''45'lattice_3080 ~v0 ~v1 v2
  = du_'8743''45''8744''45'lattice_3080 v2
du_'8743''45''8744''45'lattice_3080 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
du_'8743''45''8744''45'lattice_3080 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45''8744''45'lattice_2998
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∨-idem
d_'8744''45'idem_3082 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8744''45'idem_3082 ~v0 ~v1 v2 = du_'8744''45'idem_3082 v2
du_'8744''45'idem_3082 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8744''45'idem_3082 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'idem_2972
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∨-isBand
d_'8744''45'isBand_3084 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8744''45'isBand_3084 ~v0 ~v1 v2 = du_'8744''45'isBand_3084 v2
du_'8744''45'isBand_3084 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_'8744''45'isBand_3084 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isBand_2980
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∨-isMagma
d_'8744''45'isMagma_3086 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8744''45'isMagma_3086 ~v0 ~v1 v2 = du_'8744''45'isMagma_3086 v2
du_'8744''45'isMagma_3086 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8744''45'isMagma_3086 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isMagma_2976
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-isOrderTheoreticJoinSemilattice
d_'8743''45'isOrderTheoreticJoinSemilattice_3088 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_'8743''45'isOrderTheoreticJoinSemilattice_3088 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticJoinSemilattice_3088 v2
du_'8743''45'isOrderTheoreticJoinSemilattice_3088 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_'8743''45'isOrderTheoreticJoinSemilattice_3088 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticJoinSemilattice_162
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-isOrderTheoreticMeetSemilattice
d_'8743''45'isOrderTheoreticMeetSemilattice_3090 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_'8743''45'isOrderTheoreticMeetSemilattice_3090 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticMeetSemilattice_3090 v2
du_'8743''45'isOrderTheoreticMeetSemilattice_3090 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_'8743''45'isOrderTheoreticMeetSemilattice_3090 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticMeetSemilattice_160
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∨-isSemigroup
d_'8744''45'isSemigroup_3092 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8744''45'isSemigroup_3092 ~v0 ~v1 v2
  = du_'8744''45'isSemigroup_3092 v2
du_'8744''45'isSemigroup_3092 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8744''45'isSemigroup_3092 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isSemigroup_2978
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∨-isSemilattice
d_'8744''45'isSemilattice_3094 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8744''45'isSemilattice_3094 ~v0 ~v1 v2
  = du_'8744''45'isSemilattice_3094 v2
du_'8744''45'isSemilattice_3094 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_'8744''45'isSemilattice_3094 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isSemilattice_2982
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-orderTheoreticJoinSemilattice
d_'8743''45'orderTheoreticJoinSemilattice_3096 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
d_'8743''45'orderTheoreticJoinSemilattice_3096 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticJoinSemilattice_3096 v2
du_'8743''45'orderTheoreticJoinSemilattice_3096 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
du_'8743''45'orderTheoreticJoinSemilattice_3096 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticJoinSemilattice_166
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∧-orderTheoreticMeetSemilattice
d_'8743''45'orderTheoreticMeetSemilattice_3098 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
d_'8743''45'orderTheoreticMeetSemilattice_3098 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticMeetSemilattice_3098 v2
du_'8743''45'orderTheoreticMeetSemilattice_3098 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
du_'8743''45'orderTheoreticMeetSemilattice_3098 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticMeetSemilattice_164
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∨-semilattice
d_'8744''45'semilattice_3100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8744''45'semilattice_3100 ~v0 ~v1 v2
  = du_'8744''45'semilattice_3100 v2
du_'8744''45'semilattice_3100 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
du_'8744''45'semilattice_3100 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∨-∧-isOrderTheoreticLattice
d_'8744''45''8743''45'isOrderTheoreticLattice_3102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
d_'8744''45''8743''45'isOrderTheoreticLattice_3102 ~v0 ~v1 v2
  = du_'8744''45''8743''45'isOrderTheoreticLattice_3102 v2
du_'8744''45''8743''45'isOrderTheoreticLattice_3102 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
du_'8744''45''8743''45'isOrderTheoreticLattice_3102 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45''8743''45'isOrderTheoreticLattice_3010
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra._.∨-∧-orderTheoreticLattice
d_'8744''45''8743''45'orderTheoreticLattice_3104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_Lattice_362
d_'8744''45''8743''45'orderTheoreticLattice_3104 ~v0 ~v1 v2
  = du_'8744''45''8743''45'orderTheoreticLattice_3104 v2
du_'8744''45''8743''45'orderTheoreticLattice_3104 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_Lattice_362
du_'8744''45''8743''45'orderTheoreticLattice_3104 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45''8743''45'orderTheoreticLattice_3066
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-∨-isBooleanAlgebra
d_'8743''45''8744''45'isBooleanAlgebra_3106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894
d_'8743''45''8744''45'isBooleanAlgebra_3106 ~v0 ~v1 v2
  = du_'8743''45''8744''45'isBooleanAlgebra_3106 v2
du_'8743''45''8744''45'isBooleanAlgebra_3106 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894
du_'8743''45''8744''45'isBooleanAlgebra_3106 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsBooleanAlgebra'46'constructor_41193
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.DistributiveLattice.du_'8743''45''8744''45'isDistributiveLattice_714
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
            (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'complement_2918
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
            (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'complement_2916
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
            (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
            (coe v0)))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-∨-booleanAlgebra
d_'8743''45''8744''45'booleanAlgebra_3108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680
d_'8743''45''8744''45'booleanAlgebra_3108 ~v0 ~v1 v2
  = du_'8743''45''8744''45'booleanAlgebra_3108 v2
du_'8743''45''8744''45'booleanAlgebra_3108 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680
du_'8743''45''8744''45'booleanAlgebra_3108 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_BooleanAlgebra'46'constructor_11433
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
      (coe du_'8743''45''8744''45'isBooleanAlgebra_3106 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-identityʳ
d_'8743''45'identity'691'_3110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8743''45'identity'691'_3110 ~v0 ~v1 v2 v3
  = du_'8743''45'identity'691'_3110 v2 v3
du_'8743''45'identity'691'_3110 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8743''45'identity'691'_3110 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v2
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
         v1
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v2
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
            v1 v1
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v2
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))))
               (coe v1))
            (let v2
                   = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0) in
             let v3
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe v2) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
               v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
         (let v2
                = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                    (coe v0) in
          let v3
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                    (coe v2) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
            (coe v1)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_sym_36
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0)))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'691'_2978
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0))
                  v1))))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-identityˡ
d_'8743''45'identity'737'_3114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8743''45'identity'737'_3114 ~v0 ~v1 v2
  = du_'8743''45'identity'737'_3114 v2
du_'8743''45'identity'737'_3114 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8743''45'identity'737'_3114 v0
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'id'691''8658'id'737'_232
      (let v1
             = coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                 (coe v0) in
       coe
         MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1)))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
      (coe du_'8743''45'identity'691'_3110 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-identity
d_'8743''45'identity_3116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'identity_3116 ~v0 ~v1 v2
  = du_'8743''45'identity_3116 v2
du_'8743''45'identity_3116 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8743''45'identity_3116 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_'8743''45'identity'737'_3114 (coe v0))
      (coe du_'8743''45'identity'691'_3110 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-identityʳ
d_'8744''45'identity'691'_3118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8744''45'identity'691'_3118 ~v0 ~v1 v2 v3
  = du_'8744''45'identity'691'_3118 v2 v3
du_'8744''45'identity'691'_3118 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8744''45'identity'691'_3118 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v2
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
         v1
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v2
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
            v1 v1
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v2
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))))
               (coe v1))
            (let v2
                   = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0) in
             let v3
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe v2) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
               v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
         (let v2
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0)) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v2))
            (coe v1)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_sym_36
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0)))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'691'_2982
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0))
                  v1))))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-identityˡ
d_'8744''45'identity'737'_3122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8744''45'identity'737'_3122 ~v0 ~v1 v2
  = du_'8744''45'identity'737'_3122 v2
du_'8744''45'identity'737'_3122 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8744''45'identity'737'_3122 v0
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'id'691''8658'id'737'_232
      (let v1
             = coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                 (coe v0) in
       coe
         MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1)))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
      (coe du_'8744''45'identity'691'_3118 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-identity
d_'8744''45'identity_3124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'identity_3124 ~v0 ~v1 v2
  = du_'8744''45'identity_3124 v2
du_'8744''45'identity_3124 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8744''45'identity_3124 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_'8744''45'identity'737'_3122 (coe v0))
      (coe du_'8744''45'identity'691'_3118 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-zeroʳ
d_'8743''45'zero'691'_3126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8743''45'zero'691'_3126 ~v0 ~v1 v2 v3
  = du_'8743''45'zero'691'_3126 v2 v3
du_'8743''45'zero'691'_3126 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8743''45'zero'691'_3126 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v2
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v2
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v2
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v2
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                           (let v2
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'691'_2982
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))
                     v1))
               (let v2
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                let v3
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                          (coe v2) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v1)
                  (coe v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'idem_2948
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                           (coe v0)))
                     (coe v1))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))))
               v1 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
         (let v2
                = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                    (coe v0) in
          let v3
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                    (coe v2) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
            (coe v1)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'691'_2982
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0))
               v1)))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-zeroˡ
d_'8743''45'zero'737'_3130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8743''45'zero'737'_3130 ~v0 ~v1 v2
  = du_'8743''45'zero'737'_3130 v2
du_'8743''45'zero'737'_3130 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8743''45'zero'737'_3130 v0
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'ze'691''8658'ze'737'_252
      (let v1
             = coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                 (coe v0) in
       coe
         MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1)))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
      (coe du_'8743''45'zero'691'_3126 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-zero
d_'8743''45'zero_3132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'zero_3132 ~v0 ~v1 v2 = du_'8743''45'zero_3132 v2
du_'8743''45'zero_3132 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8743''45'zero_3132 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_'8743''45'zero'737'_3130 (coe v0))
      (coe du_'8743''45'zero'691'_3126 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-zeroʳ
d_'8744''45'zero'691'_3136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8744''45'zero'691'_3136 ~v0 ~v1 v2 v3
  = du_'8744''45'zero'691'_3136 v2 v3
du_'8744''45'zero'691'_3136 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8744''45'zero'691'_3136 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v2
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v2
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v2
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v2
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                           (let v2
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v2)))))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'691'_2978
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))
                     v1))
               (let v2
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                let v3
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                          (coe v2) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v1)
                  (coe v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'idem_2972
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                           (coe v0)))
                     (coe v1))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))))
               v1 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
         (let v2
                = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                    (coe v0) in
          let v3
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                    (coe v2) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
            (coe v1)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'691'_2978
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0))
               v1)))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-zeroˡ
d_'8744''45'zero'737'_3140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8744''45'zero'737'_3140 ~v0 ~v1 v2
  = du_'8744''45'zero'737'_3140 v2
du_'8744''45'zero'737'_3140 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8744''45'zero'737'_3140 v0
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'ze'691''8658'ze'737'_252
      (let v1
             = coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                 (coe v0) in
       coe
         MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v1)))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
      (coe du_'8744''45'zero'691'_3136 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-zero
d_'8744''45'zero_3142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'zero_3142 ~v0 ~v1 v2 = du_'8744''45'zero_3142 v2
du_'8744''45'zero_3142 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8744''45'zero_3142 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_'8744''45'zero'737'_3140 (coe v0))
      (coe du_'8744''45'zero'691'_3136 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-⊥-isMonoid
d_'8744''45''8869''45'isMonoid_3144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8744''45''8869''45'isMonoid_3144 ~v0 ~v1 v2
  = du_'8744''45''8869''45'isMonoid_3144 v2
du_'8744''45''8869''45'isMonoid_3144 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'8744''45''8869''45'isMonoid_3144 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isSemigroup_2978
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
               (coe v0))))
      (coe du_'8744''45'identity_3124 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-⊤-isMonoid
d_'8743''45''8868''45'isMonoid_3146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8743''45''8868''45'isMonoid_3146 ~v0 ~v1 v2
  = du_'8743''45''8868''45'isMonoid_3146 v2
du_'8743''45''8868''45'isMonoid_3146 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'8743''45''8868''45'isMonoid_3146 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isSemigroup_2954
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
               (coe v0))))
      (coe du_'8743''45'identity_3116 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-⊥-isCommutativeMonoid
d_'8744''45''8869''45'isCommutativeMonoid_3148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'8744''45''8869''45'isCommutativeMonoid_3148 ~v0 ~v1 v2
  = du_'8744''45''8869''45'isCommutativeMonoid_3148 v2
du_'8744''45''8869''45'isCommutativeMonoid_3148 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'8744''45''8869''45'isCommutativeMonoid_3148 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe du_'8744''45''8869''45'isMonoid_3144 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-⊤-isCommutativeMonoid
d_'8743''45''8868''45'isCommutativeMonoid_3150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'8743''45''8868''45'isCommutativeMonoid_3150 ~v0 ~v1 v2
  = du_'8743''45''8868''45'isCommutativeMonoid_3150 v2
du_'8743''45''8868''45'isCommutativeMonoid_3150 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'8743''45''8868''45'isCommutativeMonoid_3150 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe du_'8743''45''8868''45'isMonoid_3146 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-∧-isSemiring
d_'8744''45''8743''45'isSemiring_3152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_'8744''45''8743''45'isSemiring_3152 ~v0 ~v1 v2
  = du_'8744''45''8743''45'isSemiring_3152 v2
du_'8744''45''8743''45'isSemiring_3152 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
du_'8744''45''8743''45'isSemiring_3152 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiring'46'constructor_42303
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
         (coe du_'8744''45''8869''45'isCommutativeMonoid_3148 (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0)))))
         (coe du_'8743''45'identity_3116 (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'distrib'45''8744'_2834
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe du_'8743''45'zero_3132 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-∨-isSemiring
d_'8743''45''8744''45'isSemiring_3154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_'8743''45''8744''45'isSemiring_3154 ~v0 ~v1 v2
  = du_'8743''45''8744''45'isSemiring_3154 v2
du_'8743''45''8744''45'isSemiring_3154 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
du_'8743''45''8744''45'isSemiring_3154 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiring'46'constructor_42303
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
         (coe du_'8743''45''8868''45'isCommutativeMonoid_3150 (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0)))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0)))))
         (coe du_'8744''45'identity_3124 (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'distrib'45''8743'_2832
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe du_'8744''45'zero_3142 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-∧-isCommutativeSemiring
d_'8744''45''8743''45'isCommutativeSemiring_3156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_'8744''45''8743''45'isCommutativeSemiring_3156 ~v0 ~v1 v2
  = du_'8744''45''8743''45'isCommutativeSemiring_3156 v2
du_'8744''45''8743''45'isCommutativeSemiring_3156 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_'8744''45''8743''45'isCommutativeSemiring_3156 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiring'46'constructor_46125
      (coe du_'8744''45''8743''45'isSemiring_3152 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-∨-isCommutativeSemiring
d_'8743''45''8744''45'isCommutativeSemiring_3158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_'8743''45''8744''45'isCommutativeSemiring_3158 ~v0 ~v1 v2
  = du_'8743''45''8744''45'isCommutativeSemiring_3158 v2
du_'8743''45''8744''45'isCommutativeSemiring_3158 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_'8743''45''8744''45'isCommutativeSemiring_3158 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiring'46'constructor_46125
      (coe du_'8743''45''8744''45'isSemiring_3154 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.∨-∧-commutativeSemiring
d_'8744''45''8743''45'commutativeSemiring_3160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_'8744''45''8743''45'commutativeSemiring_3160 ~v0 ~v1 v2
  = du_'8744''45''8743''45'commutativeSemiring_3160 v2
du_'8744''45''8743''45'commutativeSemiring_3160 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_'8744''45''8743''45'commutativeSemiring_3160 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
      (coe du_'8744''45''8743''45'isCommutativeSemiring_3156 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.∧-∨-commutativeSemiring
d_'8743''45''8744''45'commutativeSemiring_3162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_'8743''45''8744''45'commutativeSemiring_3162 ~v0 ~v1 v2
  = du_'8743''45''8744''45'commutativeSemiring_3162 v2
du_'8743''45''8744''45'commutativeSemiring_3162 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_'8743''45''8744''45'commutativeSemiring_3162 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
      (coe du_'8743''45''8744''45'isCommutativeSemiring_3158 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.lemma
d_lemma_3168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lemma_3168 ~v0 ~v1 v2 v3 v4 v5 v6 = du_lemma_3168 v2 v3 v4 v5 v6
du_lemma_3168 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lemma_3168 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v5
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
         (coe v2)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v5
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
            (coe v2)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v5
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
               v2
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v5
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                  v2
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (let v5
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                     (coe v2)
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                        (let v5
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                           v2)
                        (coe v2)
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                           (let v5
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                              v2)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)) v2)
                           v2
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                              (let v5
                                     = coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                         (coe v0) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)) v2)
                              v2 v2
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                       (let v5
                                              = coe
                                                  MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                                  (coe v0) in
                                        coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                             (coe v5)))))
                                 (coe v2))
                              (coe du_'8743''45'identity'737'_3114 v0 v2))
                           (let v5
                                  = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                      (coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                         (coe v0)) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
                              (coe v2)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'691'_2978
                                 (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                    (coe v0))
                                 v1)))
                        (let v5
                               = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'691''45''8744'_2882
                           (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe v5))
                           v2 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
                     (let v5
                            = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                (coe v0) in
                      let v6
                            = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                (coe v5) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v6))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                        (coe v3)))
                  (let v5
                         = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                             (coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                (coe v0)) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'737'_2980
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))
                        v1)))
               (let v5
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'737''45''8744'_2880
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe v5))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1
                  v2))
            (let v5
                   = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0) in
             let v6
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe v5) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v6))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
               (coe v4)))
         (coe
            du_'8743''45'identity'691'_3110 (coe v0)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
-- Algebra.Lattice.Properties.BooleanAlgebra.⊥≉⊤
d_'8869''8777''8868'_3178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny
d_'8869''8777''8868'_3178 ~v0 ~v1 v2
  = du_'8869''8777''8868'_3178 v2
du_'8869''8777''8868'_3178 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny
du_'8869''8777''8868'_3178 v0
  = coe
      du_lemma_3168 (coe v0)
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
      (coe
         du_'8743''45'identity'691'_3110 (coe v0)
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
      (coe
         du_'8744''45'zero'691'_3136 (coe v0)
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
-- Algebra.Lattice.Properties.BooleanAlgebra.⊤≉⊥
d_'8868''8777''8869'_3180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny
d_'8868''8777''8869'_3180 ~v0 ~v1 v2
  = du_'8868''8777''8869'_3180 v2
du_'8868''8777''8869'_3180 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny
du_'8868''8777''8869'_3180 v0
  = coe
      du_lemma_3168 (coe v0)
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
      (coe
         du_'8743''45'zero'691'_3126 (coe v0)
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
      (coe
         du_'8744''45'identity'691'_3118 (coe v0)
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
-- Algebra.Lattice.Properties.BooleanAlgebra.¬-involutive
d_'172''45'involutive_3182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'172''45'involutive_3182 ~v0 ~v1 v2 v3
  = du_'172''45'involutive_3182 v2 v3
du_'172''45'involutive_3182 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'172''45'involutive_3182 v0 v1
  = coe
      du_lemma_3168 (coe v0)
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
      (coe v1)
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'737'_2980
         (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
            (coe v0))
         v1)
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'737'_2976
         (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
            (coe v0))
         v1)
-- Algebra.Lattice.Properties.BooleanAlgebra.deMorgan₁
d_deMorgan'8321'_3190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_deMorgan'8321'_3190 ~v0 ~v1 v2 v3 v4
  = du_deMorgan'8321'_3190 v2 v3 v4
du_deMorgan'8321'_3190 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_deMorgan'8321'_3190 v0 v1 v2
  = coe
      du_lemma_3168 (coe v0)
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
      (coe du_lem'8321'_3200 (coe v0) (coe v1) (coe v2))
      (coe du_lem'8322'_3204 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.lem₁
d_lem'8321'_3200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_lem'8321'_3200 ~v0 ~v1 v2 v3 v4 = du_lem'8321'_3200 v2 v3 v4
du_lem'8321'_3200 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_lem'8321'_3200 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v3
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
         (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v3
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v3
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v3
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))))
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v3
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        (let v3
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe
                                 MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                 (let v3
                                        = coe
                                            MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                            (coe v0) in
                                  coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                       (coe v3)))))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                        (coe
                           du_'8744''45'identity'691'_3118 (coe v0)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))))
                     (coe
                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                        (coe du_'8743''45'zero'691'_3126 (coe v0) (coe v2))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
                           (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                    (coe v0))))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                              (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                              (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                        (coe du_'8743''45'zero'691'_3126 (coe v0) (coe v1))))
                  (coe
                     MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                     (let v3
                            = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                (coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                   (coe v0)) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
                        (coe v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'691'_2982
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))
                           v1))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2)
                           (\ v3 v4 -> v3)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v3 v4 -> v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1)
                           (\ v3 v4 -> v3)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v3 v4 -> v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))))
                     (let v3
                            = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                (coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                   (coe v0)) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
                        (coe v1)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'691'_2982
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))
                           v2))))
               (coe
                  MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     v2 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     v1 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
            (let v3
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe
                  MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                  (\ v4 ->
                     coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (\ v4 v5 -> v4)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v1))
               (coe
                  MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                  (\ v4 v5 -> v5)
                  (\ v4 ->
                     coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v1))
               (let v4
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                          (coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                             (coe v0)) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v4))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     v1 v2))))
         (let v3
                = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'737''45''8744'_2880
            (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe v3))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
-- Algebra.Lattice.Properties.BooleanAlgebra._.lem₃
d_lem'8323'_3202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_lem'8323'_3202 ~v0 ~v1 v2 v3 v4 = du_lem'8323'_3202 v2 v3 v4
du_lem'8323'_3202 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_lem'8323'_3202 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v3
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v3
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v3
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v3
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                           (let v3
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     v2 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
               (coe
                  du_'8743''45'identity'737'_3114 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))))
            (let v3
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'691'_2978
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0))
                  v1)))
         (let v3
                = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
            (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe v3))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1
            v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._.lem₂
d_lem'8322'_3204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_lem'8322'_3204 ~v0 ~v1 v2 v3 v4 = du_lem'8322'_3204 v2 v3 v4
du_lem'8322'_3204 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_lem'8322'_3204 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v3
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v3
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v3
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v3
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v3
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe
                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                              (let v3
                                     = coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                         (coe v0) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
                     (coe
                        du_'8744''45'zero'691'_3136 (coe v0)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
                  (let v3
                         = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                             (coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                (coe v0)) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'691'_2978
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))
                        v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
            (let v3
                   = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0) in
             let v4
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe v3) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v4))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
               (coe du_lem'8323'_3202 (coe v0) (coe v1) (coe v2))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
            (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0))))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
-- Algebra.Lattice.Properties.BooleanAlgebra.deMorgan₂
d_deMorgan'8322'_3210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_deMorgan'8322'_3210 ~v0 ~v1 v2 v3 v4
  = du_deMorgan'8322'_3210 v2 v3 v4
du_deMorgan'8322'_3210 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_deMorgan'8322'_3210 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v3
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v3
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v3
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v3
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (coe
                  du_'172''45'involutive_3182 (coe v0)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (coe
                  du_deMorgan'8321'_3190 (coe v0)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
               (coe v0))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
            (coe
               MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
               (coe du_'172''45'involutive_3182 (coe v0) (coe v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  v1
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  v2)
               (coe du_'172''45'involutive_3182 (coe v0) (coe v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._⊕_
d__'8853'__3226 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8853'__3226 ~v0 v1 ~v2 = du__'8853'__3226 v1
du__'8853'__3226 ::
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8853'__3226 v0 = coe v0
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.helper
d_helper_3236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_helper_3236 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7 v8 v9 v10
  = du_helper_3236 v2 v5 v6 v7 v8 v9 v10
du_helper_3236 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_helper_3236 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Function.Base.du__'10216'_'10217'__240 (coe v5)
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
         (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0))))
         v1 v2
         (coe
            MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
            (\ v7 v8 -> v7) v3 v4)
         (coe
            MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
            (\ v7 v8 -> v8)
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0)) v3
            v4))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
         (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
            (coe v0))
         v3 v4 v6)
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-cong
d_'8853''45'cong_3242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8853''45'cong_3242 ~v0 ~v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = du_'8853''45'cong_3242 v2 v3 v4 v5 v6 v7 v8 v9 v10
du_'8853''45'cong_3242 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8853''45'cong_3242 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v9
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v9)))
         (coe v1 v3 v5)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v5)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5)))
         (coe v1 v4 v6)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v9
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v9)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v5)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v6)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v6)))
            (coe v1 v4 v6)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (let v9
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v9)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v6)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v6)))
               (coe v1 v4 v6) (coe v1 v4 v6)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v9
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v9)))))
                  (coe v1 v4 v6))
               (coe v2 v4 v6))
            (coe
               du_helper_3236 (coe v0)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v5)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v6)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v6)
               (coe
                  MAlonzo.Code.Function.Base.du__'10216'_'10217'__240 (coe v7)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     v3 v4 v5 v6)
                  (coe v8))
               (coe
                  MAlonzo.Code.Function.Base.du__'10216'_'10217'__240 (coe v7)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     v3 v4 v5 v6)
                  (coe v8))))
         (coe v2 v3 v5))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-comm
d_'8853''45'comm_3256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8853''45'comm_3256 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_'8853''45'comm_3256 v2 v3 v4 v5 v6
du_'8853''45'comm_3256 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8853''45'comm_3256 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v5
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
         (coe v1 v3 v4)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
         (coe v1 v4 v3)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v5
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)))
            (coe v1 v4 v3)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (let v5
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)))
               (coe v1 v4 v3) (coe v1 v4 v3)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v5
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))))
                  (coe v1 v4 v3))
               (coe v2 v4 v3))
            (coe
               du_helper_3236 (coe v0)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v3)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  v3 v4)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  v3 v4)))
         (coe v2 v3 v4))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.¬-distribˡ-⊕
d_'172''45'distrib'737''45''8853'_3266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'172''45'distrib'737''45''8853'_3266 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_'172''45'distrib'737''45''8853'_3266 v2 v3 v4 v5 v6
du_'172''45'distrib'737''45''8853'_3266 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'172''45'distrib'737''45''8853'_3266 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v5
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe v1 v3 v4))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4))))
         (coe
            v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
            v4)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v5
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))))
            (coe
               v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
               v4)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v5
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)))))
               (coe
                  v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                  v4)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v5
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                  (coe
                     v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                     v4)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v5
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                     (coe
                        v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                        v4)
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        (let v5
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                        (coe
                           v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                           v4)
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                           (let v5
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3) v4)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    v4)))
                           (coe
                              v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                              v4)
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                              (let v5
                                     = coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                         (coe v0) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    v4)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       v4)))
                              (coe
                                 v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 v4)
                              (coe
                                 v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 v4)
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                       (let v5
                                              = coe
                                                  MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                                  (coe v0) in
                                        coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                             (coe v5)))))
                                 (coe
                                    v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    v4))
                              (coe
                                 v2 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 v4))
                           (coe
                              du_helper_3236 (coe v0)
                              (coe
                                 MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                                 (\ v5 v6 -> v5)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                 v4)
                              (coe
                                 MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                 (\ v5 v6 -> v6)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                 v4)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3) v4)
                              (let v5
                                     = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                         (coe
                                            MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                            (coe v0)) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                    (coe v5))
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                 (coe v4) (coe du_'172''45'involutive_3182 (coe v0) (coe v4)))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
                                 (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                          (coe v0))))
                                 v4 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                        (let v5
                               = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                   (coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                      (coe v0)) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)))
                           (coe
                              du_deMorgan'8321'_3190 (coe v0) (coe v3)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))))
                     (coe
                        du_deMorgan'8322'_3210 (coe v0)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                     (coe
                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                        (coe du_lem_3280 (coe v0) (coe v3) (coe v4))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
                           (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                    (coe v0))))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                        (coe du_lem_3280 (coe v0) (coe v4) (coe v3)))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0))
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4))))
                     (\ v5 v6 -> v5)
                     (coe
                        MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4)
                        (\ v5 v6 -> v5)
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (\ v5 v6 -> v5)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v5 v6 -> v6)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)))
                     (coe
                        MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                        (\ v5 v6 -> v6)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4)
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (\ v5 v6 -> v5)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v5 v6 -> v6)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))))
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v5 v6 -> v6)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4))))
                     (coe
                        MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4)
                        (\ v5 v6 -> v5)
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (\ v5 v6 -> v5)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v5 v6 -> v6)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)))
                     (coe
                        MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                        (\ v5 v6 -> v6)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4)
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (\ v5 v6 -> v5)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v5 v6 -> v6)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))))
                  (let v5
                         = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                             (coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                (coe v0)) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
                     (coe
                        MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4)
                        (\ v6 v7 -> v6)
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (\ v6 v7 -> v6)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v6 v7 -> v7)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)))
                     (coe
                        MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                        (\ v6 v7 -> v7)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4)
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (\ v6 v7 -> v6)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v6 v7 -> v7)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)))
                     (let v6
                            = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                (coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                   (coe v0)) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v6))
                        (coe v4)
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (\ v7 v8 -> v7)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v7 v8 -> v8)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 (coe v0))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v3)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
                              (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                       (coe v0))))
                              v3 v4))))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4))))
               (let v5
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'691''45''8744'_2882
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe v5))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4))
                  v3 v4)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
               (coe v0))
            (coe v1 v3 v4)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)))
            (coe v2 v3 v4)))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem
d_lem_3280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem_3280 ~v0 ~v1 v2 ~v3 ~v4 ~v5 ~v6 v7 v8 = du_lem_3280 v2 v7 v8
du_lem_3280 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_lem_3280 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v3
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v3
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v3
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v3
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                           (let v3
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                  (coe
                     du_'8744''45'identity'737'_3122 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
               (let v3
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                          (coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                             (coe v0)) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'691'_2982
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))
                     v1)))
            (let v3
                   = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'737''45''8744'_2880
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe v3))
               v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
         (let v3
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0)) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
            (coe v1)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
            (coe du_deMorgan'8321'_3190 (coe v0) (coe v1) (coe v2))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.¬-distribʳ-⊕
d_'172''45'distrib'691''45''8853'_3290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'172''45'distrib'691''45''8853'_3290 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_'172''45'distrib'691''45''8853'_3290 v2 v3 v4 v5 v6
du_'172''45'distrib'691''45''8853'_3290 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'172''45'distrib'691''45''8853'_3290 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v5
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe v1 v3 v4))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe v1 v4 v3))
         (coe
            v1 v3
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v5
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe v1 v4 v3))
            (coe
               v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
               v3)
            (coe
               v1 v3
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v5
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
               (coe
                  v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                  v3)
               (coe
                  v1 v3
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
               (coe
                  v1 v3
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v5
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))))
                  (coe
                     v1 v3
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)))
               (coe
                  du_'8853''45'comm_3256 (coe v0) (coe v1) (coe v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                  (coe v3)))
            (coe
               du_'172''45'distrib'737''45''8853'_3266 (coe v0) (coe v1) (coe v2)
               (coe v4) (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
               (coe v0))
            (coe v1 v3 v4) (coe v1 v4 v3)
            (coe
               du_'8853''45'comm_3256 (coe v0) (coe v1) (coe v2) (coe v3)
               (coe v4))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-annihilates-¬
d_'8853''45'annihilates'45''172'_3300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8853''45'annihilates'45''172'_3300 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_'8853''45'annihilates'45''172'_3300 v2 v3 v4 v5 v6
du_'8853''45'annihilates'45''172'_3300 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8853''45'annihilates'45''172'_3300 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v5
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
         (coe v1 v3 v4)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe v1 v3 v4)))
         (coe
            v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v5
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe v1 v3 v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                  v4))
            (coe
               v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v5
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                     v4))
               (coe
                  v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
               (coe
                  v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v5
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))))
                  (coe
                     v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)))
               (coe
                  du_'172''45'distrib'691''45''8853'_3290 (coe v0) (coe v1) (coe v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                  (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe v1 v3 v4))
               (coe
                  v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                  v4)
               (coe
                  du_'172''45'distrib'737''45''8853'_3266 (coe v0) (coe v1) (coe v2)
                  (coe v3) (coe v4))))
         (coe du_'172''45'involutive_3182 (coe v0) (coe v1 v3 v4)))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-identityˡ
d_'8853''45'identity'737'_3306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8853''45'identity'737'_3306 ~v0 ~v1 v2 v3 v4 v5
  = du_'8853''45'identity'737'_3306 v2 v3 v4 v5
du_'8853''45'identity'737'_3306 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8853''45'identity'737'_3306 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe
            v1 (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)) v3)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)) v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)) v3)))
         v3
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)) v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)) v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))))
            v3
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
               v3
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v4
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
                  v3 v3
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                           (let v4
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
                     (coe v3))
                  (coe du_'8743''45'identity'691'_3110 (coe v0) (coe v3)))
               (let v4
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                          (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
                  (coe v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                  (coe du_'8869''8777''8868'_3178 (coe v0))))
            (coe
               du_helper_3236 (coe v0)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)) v3)
               (coe v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)) v3)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
               (coe du_'8744''45'identity'737'_3122 v0 v3)
               (coe du_'8743''45'zero'737'_3130 v0 v3)))
         (coe
            v2 (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
            v3))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-identityʳ
d_'8853''45'identity'691'_3310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8853''45'identity'691'_3310 ~v0 ~v1 v2 v3 v4 v5
  = du_'8853''45'identity'691'_3310 v2 v3 v4 v5
du_'8853''45'identity'691'_3310 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8853''45'identity'691'_3310 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
      (coe
         du_'8853''45'comm_3256 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
         (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0)))))
         (coe
            v1 v3 (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
         (coe
            v1 (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)) v3)
         v3)
      (coe
         du_'8853''45'identity'737'_3306 (coe v0) (coe v1) (coe v2)
         (coe v3))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-identity
d_'8853''45'identity_3312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8853''45'identity_3312 ~v0 ~v1 v2 v3 v4
  = du_'8853''45'identity_3312 v2 v3 v4
du_'8853''45'identity_3312 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8853''45'identity_3312 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_'8853''45'identity'737'_3306 (coe v0) (coe v1) (coe v2))
      (coe du_'8853''45'identity'691'_3310 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-inverseˡ
d_'8853''45'inverse'737'_3314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8853''45'inverse'737'_3314 ~v0 ~v1 v2 v3 v4 v5
  = du_'8853''45'inverse'737'_3314 v2 v3 v4 v5
du_'8853''45'inverse'737'_3314 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8853''45'inverse'737'_3314 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe v1 v3 v3)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v3)))
         (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'691'_2982
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0))
                  v3))
            (coe
               du_helper_3236 (coe v0)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v3)
               (coe v3)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v3)
               (coe v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'idem_2972
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                        (coe v0)))
                  (coe v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'idem_2948
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                        (coe v0)))
                  (coe v3))))
         (coe v2 v3 v3))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-inverseʳ
d_'8853''45'inverse'691'_3318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8853''45'inverse'691'_3318 ~v0 ~v1 v2 v3 v4 v5
  = du_'8853''45'inverse'691'_3318 v2 v3 v4 v5
du_'8853''45'inverse'691'_3318 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8853''45'inverse'691'_3318 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
      (coe
         du_'8853''45'comm_3256 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
         (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0)))))
         (coe v1 v3 v3) (coe v1 v3 v3)
         (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
      (coe
         du_'8853''45'inverse'737'_3314 (coe v0) (coe v1) (coe v2) (coe v3))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-inverse
d_'8853''45'inverse_3320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8853''45'inverse_3320 ~v0 ~v1 v2 v3 v4
  = du_'8853''45'inverse_3320 v2 v3 v4
du_'8853''45'inverse_3320 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8853''45'inverse_3320 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_'8853''45'inverse'737'_3314 (coe v0) (coe v1) (coe v2))
      (coe du_'8853''45'inverse'691'_3318 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.∧-distribˡ-⊕
d_'8743''45'distrib'737''45''8853'_3322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8853'_3322 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_'8743''45'distrib'737''45''8853'_3322 v2 v3 v4 v5 v6 v7
du_'8743''45'distrib'737''45''8853'_3322 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'737''45''8853'_3322 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v6
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
            (coe v1 v4 v5))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))
         (coe
            v1
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v6
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5)))
            (coe
               v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v6
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))
               (coe
                  v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                  (let v6
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                  (coe
                     v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v6
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                     (coe
                        v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                        (let v6
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                        (coe
                           v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                           (let v6
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                       v5))))
                           (coe
                              v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5))
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                              (let v6
                                     = coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                         (coe v0) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4
                                       v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                          v5))))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4
                                       v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                          v5))))
                              (coe
                                 v1
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v5))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                 (let v6
                                        = coe
                                            MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                            (coe v0) in
                                  coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                       (coe v6)))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4
                                          v5))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v4 v5))))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4
                                          v5))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v5))))
                                 (coe
                                    v1
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                       v5))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                    (let v6
                                           = coe
                                               MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                               (coe v0) in
                                     coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                          (coe v6)))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             v4 v5))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v4)
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v5))))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v5))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v4)
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v5))))
                                    (coe
                                       v1
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                          v4)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                          v5))
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                                       (let v6
                                              = coe
                                                  MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                                  (coe v0) in
                                        coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                             (coe v6)))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v4)
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v5))
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0
                                                (coe
                                                   MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                   v0 v3 v4)
                                                (coe
                                                   MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                   v0 v3 v5))))
                                       (coe
                                          v1
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v5))
                                       (coe
                                          v1
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v5))
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                          (coe
                                             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                             (coe
                                                MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                (let v6
                                                       = coe
                                                           MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                                           (coe v0) in
                                                 coe
                                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                                   (coe
                                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                                      (coe v6)))))
                                          (coe
                                             v1
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v4)
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v5)))
                                       (coe
                                          v2
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v5)))
                                    (let v6
                                           = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                               (coe
                                                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                                  (coe v0)) in
                                     coe
                                       MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                          (coe v6))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v4)
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v5)))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             v4 v5))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v5))
                                       (let v7
                                              = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                                  (coe v0) in
                                        coe
                                          MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'737''45''8744'_2880
                                          (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                             (coe v7))
                                          v3 v4 v5)))
                                 (coe
                                    du_helper_3236 (coe v0)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4
                                          v5))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4
                                          v5))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                          v5))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                          v4)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                          v5))
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                       (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                                (coe
                                                   MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                                   (coe v0)))))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             v4 v5)))
                                    (coe du_lem'8321'_3336 (coe v0) (coe v3) (coe v4) (coe v5))))
                              (let v6
                                     = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                         (coe v0) in
                               let v7
                                     = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                         (coe v6) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                    (coe v7))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4
                                       v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                          v5)))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                          v5)))
                                 (coe
                                    du_deMorgan'8321'_3190 (coe v0) (coe v3)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                       v5))))
                           (let v6
                                  = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                      (coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                         (coe v0)) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v6))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                              (coe
                                 MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                                 (\ v7 v8 -> v7)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                       v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))
                              (coe
                                 MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                 (\ v7 v8 -> v8)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                       v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))
                              (let v7
                                     = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                         (coe v0) in
                               let v8
                                     = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                         (coe v7) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                    (coe v8))
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                       v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                                 (coe du_deMorgan'8321'_3190 (coe v0) (coe v4) (coe v5)))))
                        (let v6
                               = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'737''45''8744'_2880
                           (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe v6))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                     (let v6
                            = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                (coe v0) in
                      let v7
                            = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                (coe v6) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v7))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                        (coe du_lem'8323'_3338 (coe v0) (coe v3) (coe v4) (coe v5))))
                  (coe
                     du_'8744''45'identity'737'_3122 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))))
               (let v6
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                          (coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                             (coe v0)) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v6))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                  (coe du_deMorgan'8321'_3190 (coe v0) (coe v4) (coe v5))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))))
               v3
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))
         (let v6
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0)) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v6))
            (coe v3) (coe v1 v4 v5)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5)))
            (coe v2 v4 v5)))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₂
d_lem'8322'_3334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8322'_3334 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7
  = du_lem'8322'_3334 v2 v5 v6 v7
du_lem'8322'_3334 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lem'8322'_3334 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
            v3)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v1)
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v1)
                  v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  v2 v1 v3))
            (let v4
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v4))
               (coe v3)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v1)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  v1 v2)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
            (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0))))
            v1 v2 v3))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₁
d_lem'8321'_3336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8321'_3336 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7
  = du_lem'8321'_3336 v2 v5 v6 v7
du_lem'8321'_3336 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lem'8321'_3336 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v1)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                  (let v4
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                           (let v4
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     v1 v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3)))
               (let v4
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                          (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
                  (coe v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v3))
                  (coe du_lem'8322'_3334 (coe v0) (coe v1) (coe v2) (coe v3))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))))
               v1 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3)))
         (let v4
                = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                    (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                    (coe v4) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v1)
            (coe v1)
            (coe
               MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'idem_2948
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                     (coe v0)))
               (coe v1))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₃
d_lem'8323'_3338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8323'_3338 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7
  = du_lem'8323'_3338 v2 v5 v6 v7
du_lem'8323'_3338 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lem'8323'_3338 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
            (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                     v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v4
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                        v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                           (let v4
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
                  (coe
                     MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                        v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                           v1)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                    (coe v0)))))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                  v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)))
            (let v4
                   = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe v4) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'691'_2982
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0))
                  v1)))
         (coe
            du_'8743''45'zero'691'_3126 (coe v0)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.∧-distribʳ-⊕
d_'8743''45'distrib'691''45''8853'_3340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'691''45''8853'_3340 ~v0 ~v1 v2 v3 v4
  = du_'8743''45'distrib'691''45''8853'_3340 v2 v3 v4
du_'8743''45'distrib'691''45''8853'_3340 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'691''45''8853'_3340 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'distr'737''8658'distr'691'_390
      (let v3
             = coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                 (coe v0) in
       coe
         MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 (coe v0))
      (coe v1) (coe du_'8853''45'cong_3242 (coe v0) (coe v1) (coe v2))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe
         du_'8743''45'distrib'737''45''8853'_3322 (coe v0) (coe v1)
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.∧-distrib-⊕
d_'8743''45'distrib'45''8853'_3342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8853'_3342 ~v0 ~v1 v2 v3 v4
  = du_'8743''45'distrib'45''8853'_3342 v2 v3 v4
du_'8743''45'distrib'45''8853'_3342 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8743''45'distrib'45''8853'_3342 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_'8743''45'distrib'737''45''8853'_3322 (coe v0) (coe v1)
         (coe v2))
      (coe
         du_'8743''45'distrib'691''45''8853'_3340 (coe v0) (coe v1)
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.lemma₂
d_lemma'8322'_3352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lemma'8322'_3352 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7 v8
  = du_lemma'8322'_3352 v2 v5 v6 v7 v8
du_lemma'8322'_3352 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lemma'8322'_3352 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v5
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3 v4))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
               v4))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v3)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v4)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v4)))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v5
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                  v4))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v3)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v4)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v3)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v4)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v4)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v5
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v5)))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v3)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v4)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v4))))
            (coe
               MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
               (let v5
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe v5))
                  v3 v1 v2)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                     v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v3)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
                     v4)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v4)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v4)))
               (let v5
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe v5))
                  v4 v1 v2)))
         (let v5
                = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'737''45''8743'_2876
            (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe v5))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)
            v3 v4))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-assoc
d_'8853''45'assoc_3362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8853''45'assoc_3362 ~v0 ~v1 v2 v3 v4 v5 v6 v7
  = du_'8853''45'assoc_3362 v2 v3 v4 v5 v6 v7
du_'8853''45'assoc_3362 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8853''45'assoc_3362 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe v1 v3 (coe v1 v4 v5)) (coe v1 (coe v1 v3 v4) v5)
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v6
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
            (coe v1 v3 (coe v1 v4 v5))
            (coe
               v1 v3
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))
            (coe v1 (coe v1 v3 v4) v5)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v6
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
               (coe
                  v1 v3
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))))
               (coe v1 (coe v1 v3 v4) v5)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v6
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                    v5))))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                           v5)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                           v5)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3) v4)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                  (coe v1 (coe v1 v3 v4) v5)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v6
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                              v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                              v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3) v4)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                           v5)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                 v5)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    v4)
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))))
                     (coe v1 (coe v1 v3 v4) v5)
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        (let v6
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                              v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                    v5)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       v4)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                              v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                 v5)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       v4)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))))
                        (coe v1 (coe v1 v3 v4) v5)
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                           (let v6
                                  = coe
                                      MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                                 v5)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                    v5)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             v4))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             v3)
                                          v4)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                          v5)))))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                                    v5)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                    v5))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       v4)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                           (coe v1 (coe v1 v3 v4) v5)
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                              (let v6
                                     = coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                         (coe v0) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v6)))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                          v4)
                                       v5)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             v3)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             v4))
                                       v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             v4))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             v3)
                                          v4)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                          v5))))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                          v4)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v4)))
                                    v5)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v4)))
                                       v5)))
                              (coe v1 (coe v1 v3 v4) v5)
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                                 (let v6
                                        = coe
                                            MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                            (coe v0) in
                                  coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                       (coe v6)))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v4)))
                                       v5)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706
                                                v0 v3 v4)
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                                (coe
                                                   MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                   v0 v3 v4)))
                                          v5)))
                                 (coe
                                    v1
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                          v4)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v4)))
                                    v5)
                                 (coe v1 (coe v1 v3 v4) v5)
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                                    (let v6
                                           = coe
                                               MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                               (coe v0) in
                                     coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                          (coe v6)))
                                    (coe
                                       v1
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v4)))
                                       v5)
                                    (coe v1 (coe v1 v3 v4) v5) (coe v1 (coe v1 v3 v4) v5)
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                          (coe
                                             MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                             (let v6
                                                    = coe
                                                        MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                                        (coe v0) in
                                              coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                                (coe
                                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662
                                                   (coe v6)))))
                                       (coe v1 (coe v1 v3 v4) v5))
                                    (coe
                                       MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                       (coe v2 v3 v4)
                                       (coe
                                          du_'8853''45'cong_3242 (coe v0) (coe v1) (coe v2)
                                          (coe v1 v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706
                                                v0 v3 v4)
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                                (coe
                                                   MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                   v0 v3 v4)))
                                          (coe v5) (coe v5))
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                          (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                                (coe
                                                   MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                                   (coe
                                                      MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                                      (coe v0)))))
                                          v5)))
                                 (coe
                                    v2
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                          v4)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             v3 v4)))
                                    v5))
                              (coe
                                 MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                 (coe du_lem'8321'_3374 (coe v0) (coe v3) (coe v4) (coe v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                                    (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                             (coe v0))))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             v3 v4)
                                          v5)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                                v3)
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                                v4))
                                          v5))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             v3 v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                v0 v3 v4)))
                                       v5)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             v3
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                                v4))
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             v5))
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                                v3)
                                             v4)
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                             v5)))
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                          (coe
                                             MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706
                                                v0 v3 v4)
                                             (coe
                                                MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                                (coe
                                                   MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708
                                                   v0 v3 v4)))
                                          v5)))
                                 (coe du_lem'8322'_3378 (coe v0) (coe v3) (coe v4) (coe v5))))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
                              (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                       (coe v0))))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                                 v5)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                 v5)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       v4)
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))))
                        (let v6
                               = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                   (coe v0) in
                         let v7
                               = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                   (coe v6) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v7))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                              v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                    v5)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       v4)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                 v5)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                       (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                       v4)
                                    (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                           (coe du_lem'8325'_3386 (coe v0) (coe v3) (coe v4) (coe v5))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                           v5)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                              v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3) v4)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))))
                  (coe
                     MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                     (coe du_lem'8323'_3380 (coe v0) (coe v3) (coe v4) (coe v5))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3 v4)
                              v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v3
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v3
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                    (coe
                                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4
                                       v5)))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v4))
                              v5)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3) v4)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v5))))
                     (coe du_lem'8324'_3384 (coe v0) (coe v3) (coe v4) (coe v5))))
               (coe
                  v2 v3
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5)))))
            (coe
               MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0)))))
                  v3)
               (coe
                  du_'8853''45'cong_3242 (coe v0) (coe v1) (coe v2) (coe v3) (coe v3)
                  (coe v1 v4 v5)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v4 v5)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v4 v5))))
               (coe v2 v4 v5))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₁
d_lem'8321'_3374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8321'_3374 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7
  = du_lem'8321'_3374 v2 v5 v6 v7
du_lem'8321'_3374 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lem'8321'_3374 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
            v3)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
            v3)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
               v3)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                     (let v4
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
                  v3))
            (let v4
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v4))
               (coe v3)
               (coe
                  MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
                  (\ v5 v6 -> v5)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (coe
                  MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                  (\ v5 v6 -> v6)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (let v5
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                let v6
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                          (coe v5) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v6))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe du_deMorgan'8321'_3190 (coe v0) (coe v1) (coe v2)))))
         (let v4
                = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
            (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe v4))
            v3
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₂′
d_lem'8322''8242'_3376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8322''8242'_3376 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 ~v7
  = du_lem'8322''8242'_3376 v2 v5 v6
du_lem'8322''8242'_3376 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_lem'8322''8242'_3376 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v3
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
               (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v3
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (let v3
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                  (let v3
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (let v3
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe
                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                              (let v3
                                     = coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                         (coe v0) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
                                    v2)))))
                     (coe
                        du_deMorgan'8321'_3190 (coe v0)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
                  (coe
                     MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                     (coe du_deMorgan'8322'_3210 (coe v0) (coe v1) (coe v2))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
                     (coe
                        du_'172''45'involutive_3182 (coe v0)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
               (coe
                  du_lemma'8322'_3352 (coe v0)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                  (coe v1) (coe v2)))
            (coe
               MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
               (coe
                  MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'737'_2976
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))
                     v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                  (coe
                     MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                     (\ v3 v4 -> v3)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
                  (coe
                     MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                     (\ v3 v4 -> v4)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))))
               (let v3
                      = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                          (coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                             (coe v0)) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'737'_2976
                     (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))
                     v2))))
         (coe
            MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
            (coe
               du_'8743''45'identity'737'_3114 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                        (coe v0))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
            (coe
               du_'8743''45'identity'691'_3110 (coe v0)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  v2))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₂
d_lem'8322'_3378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8322'_3378 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7
  = du_lem'8322'_3378 v2 v5 v6 v7
du_lem'8322'_3378 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lem'8322'_3378 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
               v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
                  v3))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
                     v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
                     v3))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
                        v3)))
               (coe
                  du_deMorgan'8321'_3190 (coe v0)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
                  (coe v3)))
            (let v4
                   = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe v4) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
               (coe du_lem'8322''8242'_3376 (coe v0) (coe v1) (coe v2))))
         (let v4
                = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
            (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe v4))
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₃
d_lem'8323'_3380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8323'_3380 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7
  = du_lem'8323'_3380 v2 v5 v6 v7
du_lem'8323'_3380 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lem'8323'_3380 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                  v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                     v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                     v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                        v3)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
               (coe
                  MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     v1 v2 v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                        v3)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
                     (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))))
                     v1 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
            (let v4
                   = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'737''45''8743'_2876
               (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe v4))
               v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
         (let v4
                = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0)) in
          coe
            MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v4))
            (coe v1)
            (coe
               MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
               (\ v5 v6 -> v5)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
            (coe
               MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
               (\ v5 v6 -> v6)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
            (let v5
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
               (coe du_deMorgan'8321'_3190 (coe v0) (coe v2) (coe v3)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₄′
d_lem'8324''8242'_3382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8324''8242'_3382 ~v0 ~v1 v2 ~v3 ~v4 ~v5 v6 v7
  = du_lem'8324''8242'_3382 v2 v6 v7
du_lem'8324''8242'_3382 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_lem'8324''8242'_3382 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v3
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v3
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v3
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v3
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                        (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v3
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe
                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                              (let v3
                                     = coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                         (coe v0) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v3)))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)))
                     (coe
                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                        (coe
                           du_'8743''45'identity'737'_3114 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                           (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                    (coe v0))))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                              (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                        (coe
                           du_'8743''45'identity'691'_3110 (coe v0)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                              v2))))
                  (coe
                     MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                     (coe
                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'737'_2976
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))
                           v1)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                           (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                    (coe v0))))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
                           (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                    (coe v0))))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v1)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v1))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)))
                        (coe
                           MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                           (\ v3 v4 -> v3)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0)))
                        (coe
                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                           (\ v3 v4 -> v4)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))))
                     (let v3
                            = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                                (coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                   (coe v0)) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v3))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v2)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'737'_2976
                           (MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                              (coe v0))
                           v2))))
               (coe
                  du_lemma'8322'_3352 (coe v0)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2)
                  (coe v1) (coe v2)))
            (coe
               MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
               (coe du_deMorgan'8322'_3210 (coe v0) (coe v1) (coe v2))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
               (coe
                  du_'172''45'involutive_3182 (coe v0)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
         (coe
            du_deMorgan'8321'_3190 (coe v0)
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₄
d_lem'8324'_3384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8324'_3384 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7
  = du_lem'8324'_3384 v2 v5 v6 v7
du_lem'8324'_3384 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lem'8324'_3384 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3)))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3)))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3)))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                  (let v4
                         = coe
                             MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                             (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        v3)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v4
                            = coe
                                MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                (coe v0) in
                      coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           v3))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           v3)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           v3)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe
                              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                              (let v4
                                     = coe
                                         MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                         (coe v0) in
                               coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                              v3)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           v3)))
                  (coe
                     MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           v3)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v3)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
                        (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                                 (coe v0))))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v3)))
               (let v4
                      = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'737''45''8743'_2876
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                     (coe v4))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v3)))
            (let v4
                   = MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                       (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe v4) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v5))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2) v3))
               (coe du_lem'8324''8242'_3382 (coe v0) (coe v2) (coe v3))))
         (coe
            du_deMorgan'8321'_3190 (coe v0) (coe v1)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v2 v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v2 v3)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing._.lem₅
d_lem'8325'_3386 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_lem'8325'_3386 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 v7
  = du_lem'8325'_3386 v2 v5 v6 v7
du_lem'8325'_3386 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_lem'8325'_3386 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                    (coe v0) in
          coe
            MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
            (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  v3))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                       (coe v0) in
             coe
               MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                  v3)
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                          (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        v3)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4
                               = coe
                                   MAlonzo.Code.Algebra.Lattice.Bundles.du_distributiveLattice_788
                                   (coe v0) in
                         coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.du_setoid_572
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v4)))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                        v3)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                           (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
            (let v4
                   = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)) in
             coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     v3)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
                  (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                           (coe v0))))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                        (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
                     v3))))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
            (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                     (coe v0))))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1)
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v2))
               v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0
                  (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v1) v2)
               (coe MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0 v3))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-isMagma
d_'8853''45'isMagma_3388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8853''45'isMagma_3388 ~v0 ~v1 v2 v3 v4
  = du_'8853''45'isMagma_3388 v2 v3 v4
du_'8853''45'isMagma_3388 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8853''45'isMagma_3388 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe du_'8853''45'cong_3242 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-isSemigroup
d_'8853''45'isSemigroup_3390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8853''45'isSemigroup_3390 ~v0 ~v1 v2 v3 v4
  = du_'8853''45'isSemigroup_3390 v2 v3 v4
du_'8853''45'isSemigroup_3390 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8853''45'isSemigroup_3390 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'8853''45'isMagma_3388 (coe v0) (coe v1) (coe v2))
      (coe du_'8853''45'assoc_3362 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-⊥-isMonoid
d_'8853''45''8869''45'isMonoid_3392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8853''45''8869''45'isMonoid_3392 ~v0 ~v1 v2 v3 v4
  = du_'8853''45''8869''45'isMonoid_3392 v2 v3 v4
du_'8853''45''8869''45'isMonoid_3392 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'8853''45''8869''45'isMonoid_3392 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'8853''45'isSemigroup_3390 (coe v0) (coe v1) (coe v2))
      (coe du_'8853''45'identity_3312 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-⊥-isGroup
d_'8853''45''8869''45'isGroup_3394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_'8853''45''8869''45'isGroup_3394 ~v0 ~v1 v2 v3 v4
  = du_'8853''45''8869''45'isGroup_3394 v2 v3 v4
du_'8853''45''8869''45'isGroup_3394 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
du_'8853''45''8869''45'isGroup_3394 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsGroup'46'constructor_22905
      (coe
         du_'8853''45''8869''45'isMonoid_3392 (coe v0) (coe v1) (coe v2))
      (coe du_'8853''45'inverse_3320 (coe v0) (coe v1) (coe v2))
      (coe (\ v3 v4 v5 -> v5))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-⊥-isAbelianGroup
d_'8853''45''8869''45'isAbelianGroup_3396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'8853''45''8869''45'isAbelianGroup_3396 ~v0 ~v1 v2 v3 v4
  = du_'8853''45''8869''45'isAbelianGroup_3396 v2 v3 v4
du_'8853''45''8869''45'isAbelianGroup_3396 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
du_'8853''45''8869''45'isAbelianGroup_3396 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsAbelianGroup'46'constructor_27897
      (coe
         du_'8853''45''8869''45'isGroup_3394 (coe v0) (coe v1) (coe v2))
      (coe du_'8853''45'comm_3256 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-∧-isRing
d_'8853''45''8743''45'isRing_3398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_'8853''45''8743''45'isRing_3398 ~v0 ~v1 v2 v3 v4
  = du_'8853''45''8743''45'isRing_3398 v2 v3 v4
du_'8853''45''8743''45'isRing_3398 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
du_'8853''45''8743''45'isRing_3398 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsRing'46'constructor_80853
      (coe
         du_'8853''45''8869''45'isAbelianGroup_3396 (coe v0) (coe v1)
         (coe v2))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
      (coe du_'8743''45'identity_3116 (coe v0))
      (coe
         du_'8743''45'distrib'45''8853'_3342 (coe v0) (coe v1) (coe v2))
      (coe du_'8743''45'zero_3132 (coe v0))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-∧-isCommutativeRing
d_'8853''45''8743''45'isCommutativeRing_3400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
d_'8853''45''8743''45'isCommutativeRing_3400 ~v0 ~v1 v2 v3 v4
  = du_'8853''45''8743''45'isCommutativeRing_3400 v2 v3 v4
du_'8853''45''8743''45'isCommutativeRing_3400 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
du_'8853''45''8743''45'isCommutativeRing_3400 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeRing'46'constructor_87819
      (coe du_'8853''45''8743''45'isRing_3398 (coe v0) (coe v1) (coe v2))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                  (coe v0)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.XorRing.⊕-∧-commutativeRing
d_'8853''45''8743''45'commutativeRing_3402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634
d_'8853''45''8743''45'commutativeRing_3402 ~v0 ~v1 v2 v3 v4
  = du_'8853''45''8743''45'commutativeRing_3402 v2 v3 v4
du_'8853''45''8743''45'commutativeRing_3402 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634
du_'8853''45''8743''45'commutativeRing_3402 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeRing'46'constructor_64147
      v1 (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 (coe v0))
      (\ v3 -> v3)
      (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8869'_714 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d_'8868'_712 (coe v0))
      (coe
         du_'8853''45''8743''45'isCommutativeRing_3400 (coe v0) (coe v1)
         (coe v2))
-- Algebra.Lattice.Properties.BooleanAlgebra._⊕_
d__'8853'__3404 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8853'__3404 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.¬-distribʳ-⊕
d_'172''45'distrib'691''45''8853'_3416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'172''45'distrib'691''45''8853'_3416 ~v0 ~v1 v2
  = du_'172''45'distrib'691''45''8853'_3416 v2
du_'172''45'distrib'691''45''8853'_3416 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'172''45'distrib'691''45''8853'_3416 v0
  = coe
      du_'172''45'distrib'691''45''8853'_3290 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.¬-distribˡ-⊕
d_'172''45'distrib'737''45''8853'_3418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'172''45'distrib'737''45''8853'_3418 ~v0 ~v1 v2
  = du_'172''45'distrib'737''45''8853'_3418 v2
du_'172''45'distrib'737''45''8853'_3418 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'172''45'distrib'737''45''8853'_3418 v0
  = coe
      du_'172''45'distrib'737''45''8853'_3266 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.∧-distrib-⊕
d_'8743''45'distrib'45''8853'_3420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8853'_3420 ~v0 ~v1 v2
  = du_'8743''45'distrib'45''8853'_3420 v2
du_'8743''45'distrib'45''8853'_3420 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8743''45'distrib'45''8853'_3420 v0
  = coe
      du_'8743''45'distrib'45''8853'_3342 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.∧-distribʳ-⊕
d_'8743''45'distrib'691''45''8853'_3422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'691''45''8853'_3422 ~v0 ~v1 v2
  = du_'8743''45'distrib'691''45''8853'_3422 v2
du_'8743''45'distrib'691''45''8853'_3422 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'691''45''8853'_3422 v0
  = coe
      du_'8743''45'distrib'691''45''8853'_3340 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.∧-distribˡ-⊕
d_'8743''45'distrib'737''45''8853'_3424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8853'_3424 ~v0 ~v1 v2
  = du_'8743''45'distrib'737''45''8853'_3424 v2
du_'8743''45'distrib'737''45''8853'_3424 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'737''45''8853'_3424 v0
  = coe
      du_'8743''45'distrib'737''45''8853'_3322 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-annihilates-¬
d_'8853''45'annihilates'45''172'_3426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8853''45'annihilates'45''172'_3426 ~v0 ~v1 v2
  = du_'8853''45'annihilates'45''172'_3426 v2
du_'8853''45'annihilates'45''172'_3426 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8853''45'annihilates'45''172'_3426 v0
  = coe
      du_'8853''45'annihilates'45''172'_3300 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-assoc
d_'8853''45'assoc_3428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8853''45'assoc_3428 ~v0 ~v1 v2 = du_'8853''45'assoc_3428 v2
du_'8853''45'assoc_3428 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8853''45'assoc_3428 v0
  = coe
      du_'8853''45'assoc_3362 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-comm
d_'8853''45'comm_3430 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8853''45'comm_3430 ~v0 ~v1 v2 = du_'8853''45'comm_3430 v2
du_'8853''45'comm_3430 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8853''45'comm_3430 v0
  = coe
      du_'8853''45'comm_3256 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-cong
d_'8853''45'cong_3432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8853''45'cong_3432 ~v0 ~v1 v2 = du_'8853''45'cong_3432 v2
du_'8853''45'cong_3432 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8853''45'cong_3432 v0
  = coe
      du_'8853''45'cong_3242 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-identity
d_'8853''45'identity_3434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8853''45'identity_3434 ~v0 ~v1 v2
  = du_'8853''45'identity_3434 v2
du_'8853''45'identity_3434 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8853''45'identity_3434 v0
  = coe
      du_'8853''45'identity_3312 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-identityʳ
d_'8853''45'identity'691'_3436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8853''45'identity'691'_3436 ~v0 ~v1 v2
  = du_'8853''45'identity'691'_3436 v2
du_'8853''45'identity'691'_3436 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8853''45'identity'691'_3436 v0
  = coe
      du_'8853''45'identity'691'_3310 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-identityˡ
d_'8853''45'identity'737'_3438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8853''45'identity'737'_3438 ~v0 ~v1 v2
  = du_'8853''45'identity'737'_3438 v2
du_'8853''45'identity'737'_3438 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8853''45'identity'737'_3438 v0
  = coe
      du_'8853''45'identity'737'_3306 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-inverse
d_'8853''45'inverse_3440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8853''45'inverse_3440 ~v0 ~v1 v2 = du_'8853''45'inverse_3440 v2
du_'8853''45'inverse_3440 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8853''45'inverse_3440 v0
  = coe
      du_'8853''45'inverse_3320 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-inverseʳ
d_'8853''45'inverse'691'_3442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8853''45'inverse'691'_3442 ~v0 ~v1 v2
  = du_'8853''45'inverse'691'_3442 v2
du_'8853''45'inverse'691'_3442 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8853''45'inverse'691'_3442 v0
  = coe
      du_'8853''45'inverse'691'_3318 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-inverseˡ
d_'8853''45'inverse'737'_3444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
d_'8853''45'inverse'737'_3444 ~v0 ~v1 v2
  = du_'8853''45'inverse'737'_3444 v2
du_'8853''45'inverse'737'_3444 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny
du_'8853''45'inverse'737'_3444 v0
  = coe
      du_'8853''45'inverse'737'_3314 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-isMagma
d_'8853''45'isMagma_3446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8853''45'isMagma_3446 ~v0 ~v1 v2 = du_'8853''45'isMagma_3446 v2
du_'8853''45'isMagma_3446 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8853''45'isMagma_3446 v0
  = coe
      du_'8853''45'isMagma_3388 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-isSemigroup
d_'8853''45'isSemigroup_3448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8853''45'isSemigroup_3448 ~v0 ~v1 v2
  = du_'8853''45'isSemigroup_3448 v2
du_'8853''45'isSemigroup_3448 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8853''45'isSemigroup_3448 v0
  = coe
      du_'8853''45'isSemigroup_3390 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-∧-commutativeRing
d_'8853''45''8743''45'commutativeRing_3450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634
d_'8853''45''8743''45'commutativeRing_3450 ~v0 ~v1 v2
  = du_'8853''45''8743''45'commutativeRing_3450 v2
du_'8853''45''8743''45'commutativeRing_3450 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeRing_3634
du_'8853''45''8743''45'commutativeRing_3450 v0
  = coe
      du_'8853''45''8743''45'commutativeRing_3402 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-∧-isCommutativeRing
d_'8853''45''8743''45'isCommutativeRing_3452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
d_'8853''45''8743''45'isCommutativeRing_3452 ~v0 ~v1 v2
  = du_'8853''45''8743''45'isCommutativeRing_3452 v2
du_'8853''45''8743''45'isCommutativeRing_3452 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
du_'8853''45''8743''45'isCommutativeRing_3452 v0
  = coe
      du_'8853''45''8743''45'isCommutativeRing_3400 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-∧-isRing
d_'8853''45''8743''45'isRing_3454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_'8853''45''8743''45'isRing_3454 ~v0 ~v1 v2
  = du_'8853''45''8743''45'isRing_3454 v2
du_'8853''45''8743''45'isRing_3454 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
du_'8853''45''8743''45'isRing_3454 v0
  = coe
      du_'8853''45''8743''45'isRing_3398 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-⊥-isAbelianGroup
d_'8853''45''8869''45'isAbelianGroup_3456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'8853''45''8869''45'isAbelianGroup_3456 ~v0 ~v1 v2
  = du_'8853''45''8869''45'isAbelianGroup_3456 v2
du_'8853''45''8869''45'isAbelianGroup_3456 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
du_'8853''45''8869''45'isAbelianGroup_3456 v0
  = coe
      du_'8853''45''8869''45'isAbelianGroup_3396 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-⊥-isGroup
d_'8853''45''8869''45'isGroup_3458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_'8853''45''8869''45'isGroup_3458 ~v0 ~v1 v2
  = du_'8853''45''8869''45'isGroup_3458 v2
du_'8853''45''8869''45'isGroup_3458 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
du_'8853''45''8869''45'isGroup_3458 v0
  = coe
      du_'8853''45''8869''45'isGroup_3394 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
-- Algebra.Lattice.Properties.BooleanAlgebra.DefaultXorRing.⊕-⊥-isMonoid
d_'8853''45''8869''45'isMonoid_3460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8853''45''8869''45'isMonoid_3460 ~v0 ~v1 v2
  = du_'8853''45''8869''45'isMonoid_3460 v2
du_'8853''45''8869''45'isMonoid_3460 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'8853''45''8869''45'isMonoid_3460 v0
  = coe
      du_'8853''45''8869''45'isMonoid_3392 (coe v0)
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
              (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2))))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
                       (coe
                          MAlonzo.Code.Algebra.Lattice.Bundles.d_isBooleanAlgebra_716
                          (coe v0)))))
              (coe
                 MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0
                 (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__706 v0 v1 v2)
                 (coe
                    MAlonzo.Code.Algebra.Lattice.Bundles.d_'172'__710 v0
                    (coe
                       MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__708 v0 v1 v2)))))
