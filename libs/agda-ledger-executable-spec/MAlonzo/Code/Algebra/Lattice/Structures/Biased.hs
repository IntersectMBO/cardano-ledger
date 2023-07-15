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

module MAlonzo.Code.Algebra.Lattice.Structures.Biased where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Consequences.Setoid
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Lattice.Structures.Biased._._DistributesOverʳ_
d__DistributesOver'691'__22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'691'__22 = erased
-- Algebra.Lattice.Structures.Biased._.Absorptive
d_Absorptive_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Absorptive_28 = erased
-- Algebra.Lattice.Structures.Biased._.Congruent₁
d_Congruent'8321'_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> ()
d_Congruent'8321'_44 = erased
-- Algebra.Lattice.Structures.Biased._.RightInverse
d_RightInverse_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightInverse_116 = erased
-- Algebra.Lattice.Structures.Biased._.IsBooleanAlgebra
d_IsBooleanAlgebra_144 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice
d_IsDistributiveLattice_152 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Lattice.Structures.Biased._.IsJoinSemilattice
d_IsJoinSemilattice_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_IsJoinSemilattice_154 = erased
-- Algebra.Lattice.Structures.Biased._.IsLattice
d_IsLattice_156 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Lattice.Structures.Biased._.IsMeetSemilattice
d_IsMeetSemilattice_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_IsMeetSemilattice_158 = erased
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.isPartialEquivalence
d_isPartialEquivalence_376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_376 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_376 v6
du_isPartialEquivalence_376 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_376 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
              (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe v1))
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.reflexive
d_reflexive_380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_380 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_380 v6
du_reflexive_380 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_380 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
              (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
           (coe v1))
        v2
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_386 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_386 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'absorbs'45''8744'_386 v6
du_'8743''45'absorbs'45''8744'_386 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_386 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∧-congʳ
d_'8743''45'cong'691'_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_394 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'cong'691'_394 v6
du_'8743''45'cong'691'_394 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_394 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∧-congˡ
d_'8743''45'cong'737'_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_396 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'cong'737'_396 v6
du_'8743''45'cong'737'_396 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_396 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∧-distribʳ-∨
d_'8743''45'distrib'691''45''8744'_400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'691''45''8744'_400 ~v0 ~v1 ~v2 ~v3
  = du_'8743''45'distrib'691''45''8744'_400
du_'8743''45'distrib'691''45''8744'_400 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'691''45''8744'_400 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'691''45''8744'_2882
      v2
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8744'_402 ~v0 ~v1 ~v2 ~v3
  = du_'8743''45'distrib'737''45''8744'_402
du_'8743''45'distrib'737''45''8744'_402 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'737''45''8744'_402 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'737''45''8744'_2880
      v2
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_404 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'absorbs'45''8743'_404 v6
du_'8744''45'absorbs'45''8743'_404 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_404 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∨-congʳ
d_'8744''45'cong'691'_412 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_412 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'cong'691'_412 v6
du_'8744''45'cong'691'_412 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_412 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∨-congˡ
d_'8744''45'cong'737'_414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_414 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'cong'737'_414 v6
du_'8744''45'cong'737'_414 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_414 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∨-distribʳ-∧
d_'8744''45'distrib'691''45''8743'_418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'691''45''8743'_418 ~v0 ~v1 ~v2 ~v3
  = du_'8744''45'distrib'691''45''8743'_418
du_'8744''45'distrib'691''45''8743'_418 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'691''45''8743'_418 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
      v2
-- Algebra.Lattice.Structures.Biased._.IsDistributiveLattice.∨-distribˡ-∧
d_'8744''45'distrib'737''45''8743'_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'737''45''8743'_420 ~v0 ~v1 ~v2 ~v3
  = du_'8744''45'distrib'737''45''8743'_420
du_'8744''45'distrib'737''45''8743'_420 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'737''45''8743'_420 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'737''45''8743'_2876
      v2
-- Algebra.Lattice.Structures.Biased._.IsJoinSemilattice.isPartialEquivalence
d_isPartialEquivalence_436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_436 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_436 v5
du_isPartialEquivalence_436 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_436 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Lattice.Structures.Biased._.IsJoinSemilattice.reflexive
d_reflexive_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_442 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_442 v5
du_reflexive_442 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_442 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Lattice.Structures.Biased._.IsJoinSemilattice.setoid
d_setoid_444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_444 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_444 v5
du_setoid_444 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_444 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.Biased._.IsJoinSemilattice.∙-congʳ
d_'8729''45'cong'691'_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_452 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_452 v5
du_'8729''45'cong'691'_452 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_452 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.Biased._.IsJoinSemilattice.∙-congˡ
d_'8729''45'cong'737'_454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_454 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_454 v5
du_'8729''45'cong'737'_454 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_454 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.Biased._.IsLattice.isPartialEquivalence
d_isPartialEquivalence_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_462 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_462 v6
du_isPartialEquivalence_462 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_462 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe v0))
-- Algebra.Lattice.Structures.Biased._.IsLattice.reflexive
d_reflexive_466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_466 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_466 v6
du_reflexive_466 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_466 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe v0))
      v1
-- Algebra.Lattice.Structures.Biased._.IsLattice.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_472 ~v0 ~v1 ~v2 ~v3
  = du_'8743''45'absorbs'45''8744'_472
du_'8743''45'absorbs'45''8744'_472 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_472 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
      v2
-- Algebra.Lattice.Structures.Biased._.IsLattice.∧-congʳ
d_'8743''45'cong'691'_480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_480 ~v0 ~v1 ~v2 ~v3
  = du_'8743''45'cong'691'_480
du_'8743''45'cong'691'_480 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_480 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
      v2 v3 v4 v5 v6
-- Algebra.Lattice.Structures.Biased._.IsLattice.∧-congˡ
d_'8743''45'cong'737'_482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_482 ~v0 ~v1 ~v2 ~v3
  = du_'8743''45'cong'737'_482
du_'8743''45'cong'737'_482 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_482 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
      v2 v3 v4 v5 v6
-- Algebra.Lattice.Structures.Biased._.IsLattice.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_484 ~v0 ~v1 ~v2 ~v3
  = du_'8744''45'absorbs'45''8743'_484
du_'8744''45'absorbs'45''8743'_484 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_484 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
      v2
-- Algebra.Lattice.Structures.Biased._.IsLattice.∨-congʳ
d_'8744''45'cong'691'_492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_492 ~v0 ~v1 ~v2 ~v3
  = du_'8744''45'cong'691'_492
du_'8744''45'cong'691'_492 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_492 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
      v2 v3 v4 v5 v6
-- Algebra.Lattice.Structures.Biased._.IsLattice.∨-congˡ
d_'8744''45'cong'737'_494 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_494 ~v0 ~v1 ~v2 ~v3
  = du_'8744''45'cong'737'_494
du_'8744''45'cong'737'_494 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_494 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
      v2 v3 v4 v5 v6
-- Algebra.Lattice.Structures.Biased._.IsMeetSemilattice.isPartialEquivalence
d_isPartialEquivalence_510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_510 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_510 v5
du_isPartialEquivalence_510 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_510 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Lattice.Structures.Biased._.IsMeetSemilattice.reflexive
d_reflexive_516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_516 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_516 v5
du_reflexive_516 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_516 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Lattice.Structures.Biased._.IsMeetSemilattice.setoid
d_setoid_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_518 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_518 v5
du_setoid_518 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_518 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.Biased._.IsMeetSemilattice.∙-congʳ
d_'8729''45'cong'691'_526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_526 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_526 v5
du_'8729''45'cong'691'_526 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_526 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.Biased._.IsMeetSemilattice.∙-congˡ
d_'8729''45'cong'737'_528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_528 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_528 v5
du_'8729''45'cong'737'_528 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_528 v0
  = let v1
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.Biased.IsLattice₂
d_IsLattice'8322'_578 a0 a1 a2 a3 a4 a5 = ()
data T_IsLattice'8322'_578
  = C_IsLattice'8322''46'constructor_3983 MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
                                          MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
                                          MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Lattice.Structures.Biased.IsLattice₂.isJoinSemilattice
d_isJoinSemilattice_590 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isJoinSemilattice_590 v0
  = case coe v0 of
      C_IsLattice'8322''46'constructor_3983 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.Biased.IsLattice₂.isMeetSemilattice
d_isMeetSemilattice_592 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isMeetSemilattice_592 v0
  = case coe v0 of
      C_IsLattice'8322''46'constructor_3983 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.Biased.IsLattice₂.absorptive
d_absorptive_594 ::
  T_IsLattice'8322'_578 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_594 v0
  = case coe v0 of
      C_IsLattice'8322''46'constructor_3983 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.assoc
d_assoc_598 ::
  T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_598 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isMeetSemilattice_592 (coe v0))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.comm
d_comm_600 ::
  T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_600 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454
      (coe d_isMeetSemilattice_592 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.idem
d_idem_602 :: T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny
d_idem_602 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_482
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isMeetSemilattice_592 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.isBand
d_isBand_604 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_604 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
      (coe d_isMeetSemilattice_592 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.isEquivalence
d_isEquivalence_606 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_606 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isMeetSemilattice_592 (coe v0)))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.isMagma
d_isMagma_608 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_608 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isMeetSemilattice_592 (coe v0))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.isPartialEquivalence
d_isPartialEquivalence_610 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_610 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_610 v6
du_isPartialEquivalence_610 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_610 v0
  = let v1 = d_isMeetSemilattice_592 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.isSemigroup
d_isSemigroup_612 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_612 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isMeetSemilattice_592 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.refl
d_refl_614 :: T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny
d_refl_614 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isMeetSemilattice_592 (coe v0))))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.reflexive
d_reflexive_616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_616 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_616 v6
du_reflexive_616 ::
  T_IsLattice'8322'_578 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_616 v0
  = let v1 = d_isMeetSemilattice_592 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.setoid
d_setoid_618 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_618 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_618 v6
du_setoid_618 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_618 v0
  = let v1 = d_isMeetSemilattice_592 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.sym
d_sym_620 ::
  T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_620 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isMeetSemilattice_592 (coe v0))))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.trans
d_trans_622 ::
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_622 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isMeetSemilattice_592 (coe v0))))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.∙-cong
d_'8729''45'cong_624 ::
  T_IsLattice'8322'_578 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_624 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isMeetSemilattice_592 (coe v0)))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.∙-congʳ
d_'8729''45'cong'691'_626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_626 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_626 v6
du_'8729''45'cong'691'_626 ::
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_626 v0
  = let v1 = d_isMeetSemilattice_592 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.ML.∙-congˡ
d_'8729''45'cong'737'_628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_628 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_628 v6
du_'8729''45'cong'737'_628 ::
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_628 v0
  = let v1 = d_isMeetSemilattice_592 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.assoc
d_assoc_632 ::
  T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_632 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isJoinSemilattice_590 (coe v0))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.comm
d_comm_634 ::
  T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_634 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454
      (coe d_isJoinSemilattice_590 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.idem
d_idem_636 :: T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny
d_idem_636 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_482
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isJoinSemilattice_590 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.isBand
d_isBand_638 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_638 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
      (coe d_isJoinSemilattice_590 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.isEquivalence
d_isEquivalence_640 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_640 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isJoinSemilattice_590 (coe v0)))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.isMagma
d_isMagma_642 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isJoinSemilattice_590 (coe v0))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.isPartialEquivalence
d_isPartialEquivalence_644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_644 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_644 v6
du_isPartialEquivalence_644 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_644 v0
  = let v1 = d_isJoinSemilattice_590 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.isSemigroup
d_isSemigroup_646 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_646 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isJoinSemilattice_590 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.refl
d_refl_648 :: T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny
d_refl_648 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isJoinSemilattice_590 (coe v0))))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.reflexive
d_reflexive_650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_650 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_650 v6
du_reflexive_650 ::
  T_IsLattice'8322'_578 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_650 v0
  = let v1 = d_isJoinSemilattice_590 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.setoid
d_setoid_652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_652 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_652 v6
du_setoid_652 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_652 v0
  = let v1 = d_isJoinSemilattice_590 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.sym
d_sym_654 ::
  T_IsLattice'8322'_578 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_654 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isJoinSemilattice_590 (coe v0))))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.trans
d_trans_656 ::
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_656 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isJoinSemilattice_590 (coe v0))))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.∙-cong
d_'8729''45'cong_658 ::
  T_IsLattice'8322'_578 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_658 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isJoinSemilattice_590 (coe v0)))))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.∙-congʳ
d_'8729''45'cong'691'_660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_660 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_660 v6
du_'8729''45'cong'691'_660 ::
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_660 v0
  = let v1 = d_isJoinSemilattice_590 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.JL.∙-congˡ
d_'8729''45'cong'737'_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_662 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_662 v6
du_'8729''45'cong'737'_662 ::
  T_IsLattice'8322'_578 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_662 v0
  = let v1 = d_isJoinSemilattice_590 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.Biased.IsLattice₂.isLattice₂
d_isLattice'8322'_664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_isLattice'8322'_664 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isLattice'8322'_664 v6
du_isLattice'8322'_664 ::
  T_IsLattice'8322'_578 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
du_isLattice'8322'_664 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsLattice'46'constructor_34033
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isMeetSemilattice_592 (coe v0))))))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454
         (coe d_isJoinSemilattice_590 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isJoinSemilattice_590 (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isJoinSemilattice_590 (coe v0))))))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454
         (coe d_isMeetSemilattice_592 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isMeetSemilattice_592 (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isMeetSemilattice_592 (coe v0))))))
      (coe d_absorptive_594 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ
d_IsDistributiveLattice'691''690''7504'_670 a0 a1 a2 a3 a4 a5 = ()
data T_IsDistributiveLattice'691''690''7504'_670
  = C_IsDistributiveLattice'691''690''7504''46'constructor_7017 MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
                                                                (AgdaAny ->
                                                                 AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ.isLattice
d_isLattice_680 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_isLattice_680 v0
  = case coe v0 of
      C_IsDistributiveLattice'691''690''7504''46'constructor_7017 v1 v2
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ.∨-distribʳ-∧
d_'8744''45'distrib'691''45''8743'_682 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'691''45''8743'_682 v0
  = case coe v0 of
      C_IsDistributiveLattice'691''690''7504''46'constructor_7017 v1 v2
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.absorptive
d_absorptive_686 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_686 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_absorptive_2780
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.isEquivalence
d_isEquivalence_688 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_688 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.isPartialEquivalence
d_isPartialEquivalence_690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_690 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_690 v6
du_isPartialEquivalence_690 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_690 v0
  = let v1 = d_isLattice_680 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe v1))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.refl
d_refl_692 ::
  T_IsDistributiveLattice'691''690''7504'_670 -> AgdaAny -> AgdaAny
d_refl_692 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe d_isLattice_680 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.reflexive
d_reflexive_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_694 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_694 v6
du_reflexive_694 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_694 v0
  = let v1 = d_isLattice_680 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
           (coe v1))
        v2
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.sym
d_sym_696 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_696 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe d_isLattice_680 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.trans
d_trans_698 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_698 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe d_isLattice_680 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_700 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'absorbs'45''8744'_700 v6
du_'8743''45'absorbs'45''8744'_700 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_700 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∧-assoc
d_'8743''45'assoc_702 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'assoc_702 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∧-comm
d_'8743''45'comm_704 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'comm_704 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∧-cong
d_'8743''45'cong_706 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong_706 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∧-congʳ
d_'8743''45'cong'691'_708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_708 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'cong'691'_708 v6
du_'8743''45'cong'691'_708 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_708 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∧-congˡ
d_'8743''45'cong'737'_710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_710 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'cong'737'_710 v6
du_'8743''45'cong'737'_710 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_710 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_712 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'absorbs'45''8743'_712 v6
du_'8744''45'absorbs'45''8743'_712 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_712 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∨-assoc
d_'8744''45'assoc_714 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'assoc_714 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∨-comm
d_'8744''45'comm_716 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'comm_716 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∨-cong
d_'8744''45'cong_718 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong_718 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∨-congʳ
d_'8744''45'cong'691'_720 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_720 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'cong'691'_720 v6
du_'8744''45'cong'691'_720 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_720 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ._.∨-congˡ
d_'8744''45'cong'737'_722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_722 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'cong'737'_722 v6
du_'8744''45'cong'737'_722 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_722 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
      (coe d_isLattice_680 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ.setoid
d_setoid_724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_724 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_724 v6
du_setoid_724 ::
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_724 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe d_isLattice_680 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ.∨-distrib-∧
d_'8744''45'distrib'45''8743'_726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'distrib'45''8743'_726 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_'8744''45'distrib'45''8743'_726 v4 v5 v6
du_'8744''45'distrib'45''8743'_726 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8744''45'distrib'45''8743'_726 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'distr'691''8658'distr_414
      (coe du_setoid_724 (coe v2)) (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
         (coe d_isLattice_680 (coe v2)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
         (coe d_isLattice_680 (coe v2)))
      (coe d_'8744''45'distrib'691''45''8743'_682 (coe v2))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8744'_728 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_'8743''45'distrib'737''45''8744'_728 v4 v5 v6
du_'8743''45'distrib'737''45''8744'_728 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'737''45''8744'_728 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_distrib'43'absorbs'8658'distrib'737'_448
      (coe du_setoid_724 (coe v2)) (coe v1) (coe v0)
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
         (coe d_isLattice_680 (coe v2)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
         (coe d_isLattice_680 (coe v2)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
         (coe d_isLattice_680 (coe v2)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
         (coe d_isLattice_680 (coe v2)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
         (coe d_isLattice_680 (coe v2)))
      (coe du_'8744''45'distrib'45''8743'_726 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ.∧-distrib-∨
d_'8743''45'distrib'45''8744'_730 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8744'_730 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_'8743''45'distrib'45''8744'_730 v4 v5 v6
du_'8743''45'distrib'45''8744'_730 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8743''45'distrib'45''8744'_730 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'distr'737''8658'distr_410
      (coe du_setoid_724 (coe v2)) (coe v1) (coe v0)
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
         (coe d_isLattice_680 (coe v2)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
         (coe d_isLattice_680 (coe v2)))
      (coe
         du_'8743''45'distrib'737''45''8744'_728 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Structures.Biased.IsDistributiveLatticeʳʲᵐ.isDistributiveLatticeʳʲᵐ
d_isDistributiveLattice'691''690''7504'_732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_isDistributiveLattice'691''690''7504'_732 ~v0 ~v1 ~v2 ~v3 v4 v5
                                            v6
  = du_isDistributiveLattice'691''690''7504'_732 v4 v5 v6
du_isDistributiveLattice'691''690''7504'_732 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice'691''690''7504'_670 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
du_isDistributiveLattice'691''690''7504'_732 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsDistributiveLattice'46'constructor_38127
      (coe d_isLattice_680 (coe v2))
      (coe du_'8744''45'distrib'45''8743'_726 (coe v0) (coe v1) (coe v2))
      (coe du_'8743''45'distrib'45''8744'_730 (coe v0) (coe v1) (coe v2))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ
d_IsBooleanAlgebra'691'_744 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsBooleanAlgebra'691'_744
  = C_IsBooleanAlgebra'691''46'constructor_11035 MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
                                                 (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                                 (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ.isDistributiveLattice
d_isDistributiveLattice_764 ::
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_isDistributiveLattice_764 v0
  = case coe v0 of
      C_IsBooleanAlgebra'691''46'constructor_11035 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ.∨-complementʳ
d_'8744''45'complement'691'_766 ::
  T_IsBooleanAlgebra'691'_744 -> AgdaAny -> AgdaAny
d_'8744''45'complement'691'_766 v0
  = case coe v0 of
      C_IsBooleanAlgebra'691''46'constructor_11035 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ.∧-complementʳ
d_'8743''45'complement'691'_768 ::
  T_IsBooleanAlgebra'691'_744 -> AgdaAny -> AgdaAny
d_'8743''45'complement'691'_768 v0
  = case coe v0 of
      C_IsBooleanAlgebra'691''46'constructor_11035 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ.¬-cong
d_'172''45'cong_770 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'172''45'cong_770 v0
  = case coe v0 of
      C_IsBooleanAlgebra'691''46'constructor_11035 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.absorptive
d_absorptive_774 ::
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_774 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_absorptive_2780
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_764 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.isEquivalence
d_isEquivalence_776 ::
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_776 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_764 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.isLattice
d_isLattice_778 ::
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_isLattice_778 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
      (coe d_isDistributiveLattice_764 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.isPartialEquivalence
d_isPartialEquivalence_780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_780 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_780 v9
du_isPartialEquivalence_780 ::
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_780 v0
  = let v1 = d_isDistributiveLattice_764 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe v2))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.refl
d_refl_782 :: T_IsBooleanAlgebra'691'_744 -> AgdaAny -> AgdaAny
d_refl_782 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe d_isDistributiveLattice_764 (coe v0))))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.reflexive
d_reflexive_784 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_784 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_784 v9
du_reflexive_784 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_784 v0
  = let v1 = d_isDistributiveLattice_764 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
              (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
           (coe v2))
        v3
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.sym
d_sym_786 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_786 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe d_isDistributiveLattice_764 (coe v0))))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.trans
d_trans_788 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_788 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe d_isDistributiveLattice_764 (coe v0))))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_790 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                  ~v8 v9
  = du_'8743''45'absorbs'45''8744'_790 v9
du_'8743''45'absorbs'45''8744'_790 ::
  T_IsBooleanAlgebra'691'_744 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_790 v0
  = let v1 = d_isDistributiveLattice_764 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∧-assoc
d_'8743''45'assoc_792 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'assoc_792 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_764 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∧-comm
d_'8743''45'comm_794 ::
  T_IsBooleanAlgebra'691'_744 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'comm_794 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_764 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∧-cong
d_'8743''45'cong_796 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong_796 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_764 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∧-congʳ
d_'8743''45'cong'691'_798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_798 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8743''45'cong'691'_798 v9
du_'8743''45'cong'691'_798 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_798 v0
  = let v1 = d_isDistributiveLattice_764 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∧-congˡ
d_'8743''45'cong'737'_800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_800 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8743''45'cong'737'_800 v9
du_'8743''45'cong'737'_800 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_800 v0
  = let v1 = d_isDistributiveLattice_764 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∧-distrib-∨
d_'8743''45'distrib'45''8744'_802 ::
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8744'_802 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'distrib'45''8744'_2834
      (coe d_isDistributiveLattice_764 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∧-distribʳ-∨
d_'8743''45'distrib'691''45''8744'_804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'691''45''8744'_804 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       ~v7 ~v8 v9
  = du_'8743''45'distrib'691''45''8744'_804 v9
du_'8743''45'distrib'691''45''8744'_804 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'691''45''8744'_804 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'691''45''8744'_2882
      (coe d_isDistributiveLattice_764 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8744'_806 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       ~v7 ~v8 v9
  = du_'8743''45'distrib'737''45''8744'_806 v9
du_'8743''45'distrib'737''45''8744'_806 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'737''45''8744'_806 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'737''45''8744'_2880
      (coe d_isDistributiveLattice_764 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_808 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                  ~v8 v9
  = du_'8744''45'absorbs'45''8743'_808 v9
du_'8744''45'absorbs'45''8743'_808 ::
  T_IsBooleanAlgebra'691'_744 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_808 v0
  = let v1 = d_isDistributiveLattice_764 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∨-assoc
d_'8744''45'assoc_810 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'assoc_810 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_764 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∨-comm
d_'8744''45'comm_812 ::
  T_IsBooleanAlgebra'691'_744 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'comm_812 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_764 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∨-cong
d_'8744''45'cong_814 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong_814 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_764 (coe v0)))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∨-congʳ
d_'8744''45'cong'691'_816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_816 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8744''45'cong'691'_816 v9
du_'8744''45'cong'691'_816 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_816 v0
  = let v1 = d_isDistributiveLattice_764 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∨-congˡ
d_'8744''45'cong'737'_818 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_818 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8744''45'cong'737'_818 v9
du_'8744''45'cong'737'_818 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_818 v0
  = let v1 = d_isDistributiveLattice_764 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∨-distrib-∧
d_'8744''45'distrib'45''8743'_820 ::
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'distrib'45''8743'_820 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'distrib'45''8743'_2832
      (coe d_isDistributiveLattice_764 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∨-distribʳ-∧
d_'8744''45'distrib'691''45''8743'_822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'691''45''8743'_822 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       ~v7 ~v8 v9
  = du_'8744''45'distrib'691''45''8743'_822 v9
du_'8744''45'distrib'691''45''8743'_822 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'691''45''8743'_822 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
      (coe d_isDistributiveLattice_764 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ._.∨-distribˡ-∧
d_'8744''45'distrib'737''45''8743'_824 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'737''45''8743'_824 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       ~v7 ~v8 v9
  = du_'8744''45'distrib'737''45''8743'_824 v9
du_'8744''45'distrib'737''45''8743'_824 ::
  T_IsBooleanAlgebra'691'_744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'737''45''8743'_824 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'737''45''8743'_2876
      (coe d_isDistributiveLattice_764 (coe v0))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ.setoid
d_setoid_826 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_826 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_826 v9
du_setoid_826 ::
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_826 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe d_isDistributiveLattice_764 (coe v0))))
-- Algebra.Lattice.Structures.Biased.IsBooleanAlgebraʳ.isBooleanAlgebraʳ
d_isBooleanAlgebra'691'_828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894
d_isBooleanAlgebra'691'_828 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
  = du_isBooleanAlgebra'691'_828 v4 v5 v6 v7 v8 v9
du_isBooleanAlgebra'691'_828 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra'691'_744 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894
du_isBooleanAlgebra'691'_828 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsBooleanAlgebra'46'constructor_41193
      (coe d_isDistributiveLattice_764 (coe v5))
      (coe
         MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'inv'691''8658'inv_322
         (coe du_setoid_826 (coe v5)) (coe v0) (coe v2) (coe v3)
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe d_isDistributiveLattice_764 (coe v5))))
         (coe d_'8744''45'complement'691'_766 (coe v5)))
      (coe
         MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'inv'691''8658'inv_322
         (coe du_setoid_826 (coe v5)) (coe v1) (coe v2) (coe v4)
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
               (coe d_isDistributiveLattice_764 (coe v5))))
         (coe d_'8743''45'complement'691'_768 (coe v5)))
      (coe d_'172''45'cong_770 (coe v5))
