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

module MAlonzo.Code.Algebra.Lattice.Structures where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Lattice.Structures._._Absorbs_
d__Absorbs__16 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__Absorbs__16 = erased
-- Algebra.Lattice.Structures._._DistributesOver_
d__DistributesOver__18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver__18 = erased
-- Algebra.Lattice.Structures._._DistributesOverʳ_
d__DistributesOver'691'__20 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'691'__20 = erased
-- Algebra.Lattice.Structures._._DistributesOverˡ_
d__DistributesOver'737'__22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'737'__22 = erased
-- Algebra.Lattice.Structures._.Absorptive
d_Absorptive_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Absorptive_26 = erased
-- Algebra.Lattice.Structures._.Associative
d_Associative_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Associative_36 = erased
-- Algebra.Lattice.Structures._.Commutative
d_Commutative_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Commutative_40 = erased
-- Algebra.Lattice.Structures._.Congruent₁
d_Congruent'8321'_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> ()
d_Congruent'8321'_42 = erased
-- Algebra.Lattice.Structures._.Congruent₂
d_Congruent'8322'_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Congruent'8322'_44 = erased
-- Algebra.Lattice.Structures._.Inverse
d_Inverse_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Inverse_60 = erased
-- Algebra.Lattice.Structures._.LeftCongruent
d_LeftCongruent_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftCongruent_72 = erased
-- Algebra.Lattice.Structures._.LeftInverse
d_LeftInverse_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftInverse_84 = erased
-- Algebra.Lattice.Structures._.RightCongruent
d_RightCongruent_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightCongruent_102 = erased
-- Algebra.Lattice.Structures._.RightInverse
d_RightInverse_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightInverse_114 = erased
-- Algebra.Lattice.Structures._.IsBand
d_IsBand_146 a0 a1 a2 a3 a4 = ()
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid
d_IsIdempotentCommutativeMonoid_166 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Lattice.Structures._.IsBand.isPartialEquivalence
d_isPartialEquivalence_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_328 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_328 v5
du_isPartialEquivalence_328 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_328 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Lattice.Structures._.IsBand.reflexive
d_reflexive_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_334 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_334 v5
du_reflexive_334 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_334 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Lattice.Structures._.IsBand.setoid
d_setoid_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_336 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_336 v5
du_setoid_336 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_336 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
-- Algebra.Lattice.Structures._.IsBand.∙-congʳ
d_'8729''45'cong'691'_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_344 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_344 v5
du_'8729''45'cong'691'_344 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_344 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
-- Algebra.Lattice.Structures._.IsBand.∙-congˡ
d_'8729''45'cong'737'_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_346 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_346 v5
du_'8729''45'cong'737'_346 ::
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_346 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.identityʳ
d_identity'691'_904 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_identity'691'_904 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'691'_904 v6
du_identity'691'_904 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_identity'691'_904 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.identityˡ
d_identity'737'_906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_identity'737'_906 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'737'_906 v6
du_identity'737'_906 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_identity'737'_906 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.isBand
d_isBand_908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_908 ~v0 ~v1 ~v2 ~v3 = du_isBand_908
du_isBand_908 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_908 v0 v1 v2
  = coe MAlonzo.Code.Algebra.Structures.du_isBand_768 v2
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.isCommutativeMagma
d_isCommutativeMagma_910 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_910 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isCommutativeMagma_910 v6
du_isCommutativeMagma_910 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_910 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v1))
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.isCommutativeSemigroup
d_isCommutativeSemigroup_914 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_914 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isCommutativeSemigroup_914 v6
du_isCommutativeSemigroup_914 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_914 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.isPartialEquivalence
d_isPartialEquivalence_922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_922 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_922 v6
du_isPartialEquivalence_922 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_922 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.isUnitalMagma
d_isUnitalMagma_926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_926 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isUnitalMagma_926 v6
du_isUnitalMagma_926 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_926 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.reflexive
d_reflexive_930 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_930 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_930 v6
du_reflexive_930 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_930 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.setoid
d_setoid_932 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_932 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_932 v6
du_setoid_932 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_932 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.∙-congʳ
d_'8729''45'cong'691'_940 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_940 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_940 v6
du_'8729''45'cong'691'_940 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_940 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures._.IsIdempotentCommutativeMonoid.∙-congˡ
d_'8729''45'cong'737'_942 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_942 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_942 v6
du_'8729''45'cong'737'_942 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_942 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsSemilattice
d_IsSemilattice_2444 a0 a1 a2 a3 a4 = ()
data T_IsSemilattice_2444
  = C_IsSemilattice'46'constructor_31019 MAlonzo.Code.Algebra.Structures.T_IsBand_472
                                         (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Lattice.Structures.IsSemilattice.isBand
d_isBand_2452 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2452 v0
  = case coe v0 of
      C_IsSemilattice'46'constructor_31019 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsSemilattice.comm
d_comm_2454 ::
  T_IsSemilattice_2444 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2454 v0
  = case coe v0 of
      C_IsSemilattice'46'constructor_31019 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsSemilattice._.assoc
d_assoc_2458 ::
  T_IsSemilattice_2444 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2458 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe d_isBand_2452 (coe v0)))
-- Algebra.Lattice.Structures.IsSemilattice._.idem
d_idem_2460 :: T_IsSemilattice_2444 -> AgdaAny -> AgdaAny
d_idem_2460 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_482
      (coe d_isBand_2452 (coe v0))
-- Algebra.Lattice.Structures.IsSemilattice._.isEquivalence
d_isEquivalence_2462 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2462 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe d_isBand_2452 (coe v0))))
-- Algebra.Lattice.Structures.IsSemilattice._.isMagma
d_isMagma_2464 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2464 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe d_isBand_2452 (coe v0)))
-- Algebra.Lattice.Structures.IsSemilattice._.isPartialEquivalence
d_isPartialEquivalence_2466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2466 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_2466 v5
du_isPartialEquivalence_2466 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2466 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Lattice.Structures.IsSemilattice._.isSemigroup
d_isSemigroup_2468 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2468 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe d_isBand_2452 (coe v0))
-- Algebra.Lattice.Structures.IsSemilattice._.refl
d_refl_2470 :: T_IsSemilattice_2444 -> AgdaAny -> AgdaAny
d_refl_2470 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_2452 (coe v0)))))
-- Algebra.Lattice.Structures.IsSemilattice._.reflexive
d_reflexive_2472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2472 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_2472 v5
du_reflexive_2472 ::
  T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2472 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Lattice.Structures.IsSemilattice._.setoid
d_setoid_2474 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2474 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_2474 v5
du_setoid_2474 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2474 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.IsSemilattice._.sym
d_sym_2476 ::
  T_IsSemilattice_2444 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2476 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_2452 (coe v0)))))
-- Algebra.Lattice.Structures.IsSemilattice._.trans
d_trans_2478 ::
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2478 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_2452 (coe v0)))))
-- Algebra.Lattice.Structures.IsSemilattice._.∙-cong
d_'8729''45'cong_2480 ::
  T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2480 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe d_isBand_2452 (coe v0))))
-- Algebra.Lattice.Structures.IsSemilattice._.∙-congʳ
d_'8729''45'cong'691'_2482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2482 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_2482 v5
du_'8729''45'cong'691'_2482 ::
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2482 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.IsSemilattice._.∙-congˡ
d_'8729''45'cong'737'_2484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2484 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_2484 v5
du_'8729''45'cong'737'_2484 ::
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2484 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.IsMeetSemilattice
d_IsMeetSemilattice_2486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_IsMeetSemilattice_2486 = erased
-- Algebra.Lattice.Structures.IsMeetSemilattice._.assoc
d_assoc_2496 ::
  T_IsSemilattice_2444 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2496 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe d_isBand_2452 (coe v0)))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.comm
d_comm_2498 ::
  T_IsSemilattice_2444 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2498 v0 = coe d_comm_2454 (coe v0)
-- Algebra.Lattice.Structures.IsMeetSemilattice._.idem
d_idem_2500 :: T_IsSemilattice_2444 -> AgdaAny -> AgdaAny
d_idem_2500 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_482
      (coe d_isBand_2452 (coe v0))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.isBand
d_isBand_2502 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2502 v0 = coe d_isBand_2452 (coe v0)
-- Algebra.Lattice.Structures.IsMeetSemilattice._.isEquivalence
d_isEquivalence_2504 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2504 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe d_isBand_2452 (coe v0))))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.isMagma
d_isMagma_2506 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2506 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe d_isBand_2452 (coe v0)))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.isPartialEquivalence
d_isPartialEquivalence_2508 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2508 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_2508 v5
du_isPartialEquivalence_2508 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2508 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.isSemigroup
d_isSemigroup_2510 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2510 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe d_isBand_2452 (coe v0))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.refl
d_refl_2512 :: T_IsSemilattice_2444 -> AgdaAny -> AgdaAny
d_refl_2512 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_2452 (coe v0)))))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.reflexive
d_reflexive_2514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2514 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_2514 v5
du_reflexive_2514 ::
  T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2514 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Lattice.Structures.IsMeetSemilattice._.setoid
d_setoid_2516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2516 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_2516 v5
du_setoid_2516 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2516 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.sym
d_sym_2518 ::
  T_IsSemilattice_2444 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2518 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_2452 (coe v0)))))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.trans
d_trans_2520 ::
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2520 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_2452 (coe v0)))))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.∙-cong
d_'8729''45'cong_2522 ::
  T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2522 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe d_isBand_2452 (coe v0))))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.∙-congʳ
d_'8729''45'cong'691'_2524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2524 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_2524 v5
du_'8729''45'cong'691'_2524 ::
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2524 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.IsMeetSemilattice._.∙-congˡ
d_'8729''45'cong'737'_2526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2526 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_2526 v5
du_'8729''45'cong'737'_2526 ::
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2526 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.IsJoinSemilattice
d_IsJoinSemilattice_2528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_IsJoinSemilattice_2528 = erased
-- Algebra.Lattice.Structures.IsJoinSemilattice._.assoc
d_assoc_2538 ::
  T_IsSemilattice_2444 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2538 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe d_isBand_2452 (coe v0)))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.comm
d_comm_2540 ::
  T_IsSemilattice_2444 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2540 v0 = coe d_comm_2454 (coe v0)
-- Algebra.Lattice.Structures.IsJoinSemilattice._.idem
d_idem_2542 :: T_IsSemilattice_2444 -> AgdaAny -> AgdaAny
d_idem_2542 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_482
      (coe d_isBand_2452 (coe v0))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.isBand
d_isBand_2544 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2544 v0 = coe d_isBand_2452 (coe v0)
-- Algebra.Lattice.Structures.IsJoinSemilattice._.isEquivalence
d_isEquivalence_2546 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2546 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe d_isBand_2452 (coe v0))))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.isMagma
d_isMagma_2548 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2548 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe d_isBand_2452 (coe v0)))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.isPartialEquivalence
d_isPartialEquivalence_2550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2550 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_2550 v5
du_isPartialEquivalence_2550 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2550 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.isSemigroup
d_isSemigroup_2552 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2552 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe d_isBand_2452 (coe v0))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.refl
d_refl_2554 :: T_IsSemilattice_2444 -> AgdaAny -> AgdaAny
d_refl_2554 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_2452 (coe v0)))))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.reflexive
d_reflexive_2556 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2556 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_2556 v5
du_reflexive_2556 ::
  T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2556 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Lattice.Structures.IsJoinSemilattice._.setoid
d_setoid_2558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2558 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_2558 v5
du_setoid_2558 ::
  T_IsSemilattice_2444 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2558 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.sym
d_sym_2560 ::
  T_IsSemilattice_2444 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2560 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_2452 (coe v0)))))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.trans
d_trans_2562 ::
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2562 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe d_isBand_2452 (coe v0)))))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.∙-cong
d_'8729''45'cong_2564 ::
  T_IsSemilattice_2444 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2564 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe d_isBand_2452 (coe v0))))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.∙-congʳ
d_'8729''45'cong'691'_2566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2566 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_2566 v5
du_'8729''45'cong'691'_2566 ::
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2566 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.IsJoinSemilattice._.∙-congˡ
d_'8729''45'cong'737'_2568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2568 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_2568 v5
du_'8729''45'cong'737'_2568 ::
  T_IsSemilattice_2444 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2568 v0
  = let v1 = d_isBand_2452 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Lattice.Structures.IsBoundedSemilattice
d_IsBoundedSemilattice_2570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> ()
d_IsBoundedSemilattice_2570 = erased
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.assoc
d_assoc_2582 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2582 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.comm
d_comm_2584 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_2584 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.idem
d_idem_2586 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_idem_2586 v0
  = coe MAlonzo.Code.Algebra.Structures.d_idem_722 (coe v0)
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.identity
d_identity_2588 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2588 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.identityʳ
d_identity'691'_2590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_identity'691'_2590 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'691'_2590 v6
du_identity'691'_2590 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_identity'691'_2590 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.identityˡ
d_identity'737'_2592 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_identity'737'_2592 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'737'_2592 v6
du_identity'737'_2592 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_identity'737'_2592 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isBand
d_isBand_2594 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2594 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_isBand_2594 v6
du_isBand_2594 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_2594 v0
  = coe MAlonzo.Code.Algebra.Structures.du_isBand_768 (coe v0)
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isCommutativeMagma
d_isCommutativeMagma_2596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_2596 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isCommutativeMagma_2596 v6
du_isCommutativeMagma_2596 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_2596 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v1))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isCommutativeMonoid
d_isCommutativeMonoid_2598 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_2598 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0)
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isCommutativeSemigroup
d_isCommutativeSemigroup_2600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2600 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isCommutativeSemigroup_2600 v6
du_isCommutativeSemigroup_2600 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2600 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isEquivalence
d_isEquivalence_2602 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2602 v0
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
                  (coe v0)))))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isMagma
d_isMagma_2604 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2604 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isMonoid
d_isMonoid_2606 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_2606 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isPartialEquivalence
d_isPartialEquivalence_2608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2608 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_2608 v6
du_isPartialEquivalence_2608 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2608 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isSemigroup
d_isSemigroup_2610 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2610 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.isUnitalMagma
d_isUnitalMagma_2612 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_2612 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isUnitalMagma_2612 v6
du_isUnitalMagma_2612 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_2612 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.refl
d_refl_2614 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_refl_2614 v0
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
                     (coe v0))))))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.reflexive
d_reflexive_2616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2616 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_2616 v6
du_reflexive_2616 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2616 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.setoid
d_setoid_2618 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2618 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_2618 v6
du_setoid_2618 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2618 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.sym
d_sym_2620 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2620 v0
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
                     (coe v0))))))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.trans
d_trans_2622 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2622 v0
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
                     (coe v0))))))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.∙-cong
d_'8729''45'cong_2624 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2624 v0
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
                  (coe v0)))))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.∙-congʳ
d_'8729''45'cong'691'_2626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2626 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_2626 v6
du_'8729''45'cong'691'_2626 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2626 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsBoundedSemilattice._.∙-congˡ
d_'8729''45'cong'737'_2628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2628 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_2628 v6
du_'8729''45'cong'737'_2628 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2628 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsBoundedSemilattice.isSemilattice
d_isSemilattice_2630 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  T_IsSemilattice_2444
d_isSemilattice_2630 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isSemilattice_2630 v6
du_isSemilattice_2630 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  T_IsSemilattice_2444
du_isSemilattice_2630 v0
  = coe
      C_IsSemilattice'46'constructor_31019
      (coe MAlonzo.Code.Algebra.Structures.du_isBand_768 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.d_comm_662
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice
d_IsBoundedMeetSemilattice_2632 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> ()
d_IsBoundedMeetSemilattice_2632 = erased
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.identity
d_identity_2644 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2644 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.identityʳ
d_identity'691'_2646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_identity'691'_2646 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'691'_2646 v6
du_identity'691'_2646 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_identity'691'_2646 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.identityˡ
d_identity'737'_2648 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_identity'737'_2648 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'737'_2648 v6
du_identity'737'_2648 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_identity'737'_2648 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.isSemilattice
d_isSemilattice_2650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  T_IsSemilattice_2444
d_isSemilattice_2650 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isSemilattice_2650 v6
du_isSemilattice_2650 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  T_IsSemilattice_2444
du_isSemilattice_2650 v0 = coe du_isSemilattice_2630 (coe v0)
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.assoc
d_assoc_2654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2654 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_assoc_2654 v6
du_assoc_2654 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_2654 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.comm
d_comm_2656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_2656 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_comm_2656 v6
du_comm_2656 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_comm_2656 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.idem
d_idem_2658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_idem_2658 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_idem_2658 v6
du_idem_2658 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_idem_2658 v0
  = coe MAlonzo.Code.Algebra.Structures.d_idem_722 (coe v0)
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.isBand
d_isBand_2660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2660 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_isBand_2660 v6
du_isBand_2660 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_2660 v0
  = coe MAlonzo.Code.Algebra.Structures.du_isBand_768 (coe v0)
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.isEquivalence
d_isEquivalence_2662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2662 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isEquivalence_2662 v6
du_isEquivalence_2662 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_2662 v0
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
                  (coe v0)))))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.isMagma
d_isMagma_2664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2664 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_isMagma_2664 v6
du_isMagma_2664 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_2664 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.isPartialEquivalence
d_isPartialEquivalence_2666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2666 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_2666 v6
du_isPartialEquivalence_2666 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2666 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.isSemigroup
d_isSemigroup_2668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2668 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isSemigroup_2668 v6
du_isSemigroup_2668 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_2668 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.refl
d_refl_2670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_refl_2670 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_2670 v6
du_refl_2670 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_refl_2670 v0
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
                     (coe v0))))))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.reflexive
d_reflexive_2672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2672 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_2672 v6
du_reflexive_2672 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2672 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.setoid
d_setoid_2674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2674 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_2674 v6
du_setoid_2674 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2674 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.sym
d_sym_2676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2676 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_sym_2676 v6
du_sym_2676 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_2676 v0
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
                     (coe v0))))))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.trans
d_trans_2678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2678 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_trans_2678 v6
du_trans_2678 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_2678 v0
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
                     (coe v0))))))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.∙-cong
d_'8729''45'cong_2680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2680 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong_2680 v6
du_'8729''45'cong_2680 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong_2680 v0
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
                  (coe v0)))))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.∙-congʳ
d_'8729''45'cong'691'_2682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2682 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_2682 v6
du_'8729''45'cong'691'_2682 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2682 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsBoundedMeetSemilattice._.∙-congˡ
d_'8729''45'cong'737'_2684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2684 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_2684 v6
du_'8729''45'cong'737'_2684 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2684 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice
d_IsBoundedJoinSemilattice_2686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> ()
d_IsBoundedJoinSemilattice_2686 = erased
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.identity
d_identity_2698 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2698 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.identityʳ
d_identity'691'_2700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_identity'691'_2700 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'691'_2700 v6
du_identity'691'_2700 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_identity'691'_2700 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.identityˡ
d_identity'737'_2702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_identity'737'_2702 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'737'_2702 v6
du_identity'737'_2702 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_identity'737'_2702 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v1))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.isSemilattice
d_isSemilattice_2704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  T_IsSemilattice_2444
d_isSemilattice_2704 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isSemilattice_2704 v6
du_isSemilattice_2704 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  T_IsSemilattice_2444
du_isSemilattice_2704 v0 = coe du_isSemilattice_2630 (coe v0)
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.assoc
d_assoc_2708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2708 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_assoc_2708 v6
du_assoc_2708 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_2708 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.comm
d_comm_2710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_2710 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_comm_2710 v6
du_comm_2710 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_comm_2710 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v0))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.idem
d_idem_2712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_idem_2712 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_idem_2712 v6
du_idem_2712 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_idem_2712 v0
  = coe MAlonzo.Code.Algebra.Structures.d_idem_722 (coe v0)
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.isBand
d_isBand_2714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_2714 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_isBand_2714 v6
du_isBand_2714 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_2714 v0
  = coe MAlonzo.Code.Algebra.Structures.du_isBand_768 (coe v0)
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.isEquivalence
d_isEquivalence_2716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2716 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isEquivalence_2716 v6
du_isEquivalence_2716 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_2716 v0
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
                  (coe v0)))))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.isMagma
d_isMagma_2718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_2718 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_isMagma_2718 v6
du_isMagma_2718 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_2718 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v0))))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.isPartialEquivalence
d_isPartialEquivalence_2720 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2720 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_2720 v6
du_isPartialEquivalence_2720 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2720 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.isSemigroup
d_isSemigroup_2722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2722 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isSemigroup_2722 v6
du_isSemigroup_2722 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_2722 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v0)))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.refl
d_refl_2724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
d_refl_2724 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_2724 v6
du_refl_2724 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny
du_refl_2724 v0
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
                     (coe v0))))))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.reflexive
d_reflexive_2726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2726 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_2726 v6
du_reflexive_2726 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2726 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.setoid
d_setoid_2728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2728 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_2728 v6
du_setoid_2728 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2728 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.sym
d_sym_2730 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2730 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_sym_2730 v6
du_sym_2730 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_2730 v0
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
                     (coe v0))))))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.trans
d_trans_2732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2732 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_trans_2732 v6
du_trans_2732 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_2732 v0
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
                     (coe v0))))))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.∙-cong
d_'8729''45'cong_2734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2734 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong_2734 v6
du_'8729''45'cong_2734 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong_2734 v0
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
                  (coe v0)))))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.∙-congʳ
d_'8729''45'cong'691'_2736 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2736 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_2736 v6
du_'8729''45'cong'691'_2736 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2736 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsBoundedJoinSemilattice._.∙-congˡ
d_'8729''45'cong'737'_2738 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2738 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_2738 v6
du_'8729''45'cong'737'_2738 ::
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2738 v0
  = let v1 = coe du_isSemilattice_2630 (coe v0) in
    let v2 = d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Structures.IsLattice
d_IsLattice_2744 a0 a1 a2 a3 a4 a5 = ()
data T_IsLattice_2744
  = C_IsLattice'46'constructor_34033 MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
                                     (AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny ->
                                      AgdaAny ->
                                      AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny ->
                                      AgdaAny ->
                                      AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                     MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Lattice.Structures.IsLattice.isEquivalence
d_isEquivalence_2766 ::
  T_IsLattice_2744 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2766 v0
  = case coe v0 of
      C_IsLattice'46'constructor_34033 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsLattice.∨-comm
d_'8744''45'comm_2768 ::
  T_IsLattice_2744 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'comm_2768 v0
  = case coe v0 of
      C_IsLattice'46'constructor_34033 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsLattice.∨-assoc
d_'8744''45'assoc_2770 ::
  T_IsLattice_2744 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'assoc_2770 v0
  = case coe v0 of
      C_IsLattice'46'constructor_34033 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsLattice.∨-cong
d_'8744''45'cong_2772 ::
  T_IsLattice_2744 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong_2772 v0
  = case coe v0 of
      C_IsLattice'46'constructor_34033 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsLattice.∧-comm
d_'8743''45'comm_2774 ::
  T_IsLattice_2744 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'comm_2774 v0
  = case coe v0 of
      C_IsLattice'46'constructor_34033 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsLattice.∧-assoc
d_'8743''45'assoc_2776 ::
  T_IsLattice_2744 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'assoc_2776 v0
  = case coe v0 of
      C_IsLattice'46'constructor_34033 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsLattice.∧-cong
d_'8743''45'cong_2778 ::
  T_IsLattice_2744 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong_2778 v0
  = case coe v0 of
      C_IsLattice'46'constructor_34033 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsLattice.absorptive
d_absorptive_2780 ::
  T_IsLattice_2744 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_2780 v0
  = case coe v0 of
      C_IsLattice'46'constructor_34033 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsLattice._.isPartialEquivalence
d_isPartialEquivalence_2784 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_2744 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2784 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_2784 v6
du_isPartialEquivalence_2784 ::
  T_IsLattice_2744 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2784 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_2766 (coe v0))
-- Algebra.Lattice.Structures.IsLattice._.refl
d_refl_2786 :: T_IsLattice_2744 -> AgdaAny -> AgdaAny
d_refl_2786 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_2766 (coe v0))
-- Algebra.Lattice.Structures.IsLattice._.reflexive
d_reflexive_2788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_2744 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2788 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_2788 v6
du_reflexive_2788 ::
  T_IsLattice_2744 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2788 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
      (coe d_isEquivalence_2766 (coe v0)) v1
-- Algebra.Lattice.Structures.IsLattice._.sym
d_sym_2790 ::
  T_IsLattice_2744 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2790 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_2766 (coe v0))
-- Algebra.Lattice.Structures.IsLattice._.trans
d_trans_2792 ::
  T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2792 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_2766 (coe v0))
-- Algebra.Lattice.Structures.IsLattice.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_2794 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_2744 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_2794 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'absorbs'45''8743'_2794 v6
du_'8744''45'absorbs'45''8743'_2794 ::
  T_IsLattice_2744 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_2794 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_absorptive_2780 (coe v0))
-- Algebra.Lattice.Structures.IsLattice.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_2796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_2744 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_2796 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'absorbs'45''8744'_2796 v6
du_'8743''45'absorbs'45''8744'_2796 ::
  T_IsLattice_2744 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_2796 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_absorptive_2780 (coe v0))
-- Algebra.Lattice.Structures.IsLattice.∧-congˡ
d_'8743''45'cong'737'_2798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_2798 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10
  = du_'8743''45'cong'737'_2798 v6 v7 v8 v9 v10
du_'8743''45'cong'737'_2798 ::
  T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_2798 v0 v1 v2 v3 v4
  = coe
      d_'8743''45'cong_2778 v0 v1 v1 v2 v3
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_2766 (coe v0)) v1)
      v4
-- Algebra.Lattice.Structures.IsLattice.∧-congʳ
d_'8743''45'cong'691'_2802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_2802 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10
  = du_'8743''45'cong'691'_2802 v6 v7 v8 v9 v10
du_'8743''45'cong'691'_2802 ::
  T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_2802 v0 v1 v2 v3 v4
  = coe
      d_'8743''45'cong_2778 v0 v2 v3 v1 v1 v4
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_2766 (coe v0)) v1)
-- Algebra.Lattice.Structures.IsLattice.∨-congˡ
d_'8744''45'cong'737'_2806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_2806 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10
  = du_'8744''45'cong'737'_2806 v6 v7 v8 v9 v10
du_'8744''45'cong'737'_2806 ::
  T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_2806 v0 v1 v2 v3 v4
  = coe
      d_'8744''45'cong_2772 v0 v1 v1 v2 v3
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_2766 (coe v0)) v1)
      v4
-- Algebra.Lattice.Structures.IsLattice.∨-congʳ
d_'8744''45'cong'691'_2810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_2810 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10
  = du_'8744''45'cong'691'_2810 v6 v7 v8 v9 v10
du_'8744''45'cong'691'_2810 ::
  T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_2810 v0 v1 v2 v3 v4
  = coe
      d_'8744''45'cong_2772 v0 v2 v3 v1 v1 v4
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_2766 (coe v0)) v1)
-- Algebra.Lattice.Structures.IsDistributiveLattice
d_IsDistributiveLattice_2818 a0 a1 a2 a3 a4 a5 = ()
data T_IsDistributiveLattice_2818
  = C_IsDistributiveLattice'46'constructor_38127 T_IsLattice_2744
                                                 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                                 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Lattice.Structures.IsDistributiveLattice.isLattice
d_isLattice_2830 ::
  T_IsDistributiveLattice_2818 -> T_IsLattice_2744
d_isLattice_2830 v0
  = case coe v0 of
      C_IsDistributiveLattice'46'constructor_38127 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsDistributiveLattice.∨-distrib-∧
d_'8744''45'distrib'45''8743'_2832 ::
  T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'distrib'45''8743'_2832 v0
  = case coe v0 of
      C_IsDistributiveLattice'46'constructor_38127 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsDistributiveLattice.∧-distrib-∨
d_'8743''45'distrib'45''8744'_2834 ::
  T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8744'_2834 v0
  = case coe v0 of
      C_IsDistributiveLattice'46'constructor_38127 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsDistributiveLattice._.absorptive
d_absorptive_2838 ::
  T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_2838 v0
  = coe d_absorptive_2780 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.isEquivalence
d_isEquivalence_2840 ::
  T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2840 v0
  = coe d_isEquivalence_2766 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.isPartialEquivalence
d_isPartialEquivalence_2842 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2842 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_2842 v6
du_isPartialEquivalence_2842 ::
  T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2842 v0
  = let v1 = d_isLattice_2830 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_2766 (coe v1))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.refl
d_refl_2844 :: T_IsDistributiveLattice_2818 -> AgdaAny -> AgdaAny
d_refl_2844 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_2766 (coe d_isLattice_2830 (coe v0)))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.reflexive
d_reflexive_2846 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2846 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_2846 v6
du_reflexive_2846 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2846 v0
  = let v1 = d_isLattice_2830 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_2766 (coe v1)) v2
-- Algebra.Lattice.Structures.IsDistributiveLattice._.sym
d_sym_2848 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2848 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_2766 (coe d_isLattice_2830 (coe v0)))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.trans
d_trans_2850 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2850 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_2766 (coe d_isLattice_2830 (coe v0)))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_2852 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_2852 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'absorbs'45''8744'_2852 v6
du_'8743''45'absorbs'45''8744'_2852 ::
  T_IsDistributiveLattice_2818 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_2852 v0
  = coe
      du_'8743''45'absorbs'45''8744'_2796 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∧-assoc
d_'8743''45'assoc_2854 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'assoc_2854 v0
  = coe d_'8743''45'assoc_2776 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∧-comm
d_'8743''45'comm_2856 ::
  T_IsDistributiveLattice_2818 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'comm_2856 v0
  = coe d_'8743''45'comm_2774 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∧-cong
d_'8743''45'cong_2858 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong_2858 v0
  = coe d_'8743''45'cong_2778 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∧-congʳ
d_'8743''45'cong'691'_2860 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_2860 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'cong'691'_2860 v6
du_'8743''45'cong'691'_2860 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_2860 v0
  = coe du_'8743''45'cong'691'_2802 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∧-congˡ
d_'8743''45'cong'737'_2862 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_2862 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'cong'737'_2862 v6
du_'8743''45'cong'737'_2862 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_2862 v0
  = coe du_'8743''45'cong'737'_2798 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_2864 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_2864 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'absorbs'45''8743'_2864 v6
du_'8744''45'absorbs'45''8743'_2864 ::
  T_IsDistributiveLattice_2818 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_2864 v0
  = coe
      du_'8744''45'absorbs'45''8743'_2794 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∨-assoc
d_'8744''45'assoc_2866 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'assoc_2866 v0
  = coe d_'8744''45'assoc_2770 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∨-comm
d_'8744''45'comm_2868 ::
  T_IsDistributiveLattice_2818 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'comm_2868 v0
  = coe d_'8744''45'comm_2768 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∨-cong
d_'8744''45'cong_2870 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong_2870 v0
  = coe d_'8744''45'cong_2772 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∨-congʳ
d_'8744''45'cong'691'_2872 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_2872 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'cong'691'_2872 v6
du_'8744''45'cong'691'_2872 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_2872 v0
  = coe du_'8744''45'cong'691'_2810 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice._.∨-congˡ
d_'8744''45'cong'737'_2874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_2874 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'cong'737'_2874 v6
du_'8744''45'cong'737'_2874 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_2874 v0
  = coe du_'8744''45'cong'737'_2806 (coe d_isLattice_2830 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice.∨-distribˡ-∧
d_'8744''45'distrib'737''45''8743'_2876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'737''45''8743'_2876 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'distrib'737''45''8743'_2876 v6
du_'8744''45'distrib'737''45''8743'_2876 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'737''45''8743'_2876 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_'8744''45'distrib'45''8743'_2832 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice.∨-distribʳ-∧
d_'8744''45'distrib'691''45''8743'_2878 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'691''45''8743'_2878 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8744''45'distrib'691''45''8743'_2878 v6
du_'8744''45'distrib'691''45''8743'_2878 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'691''45''8743'_2878 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_'8744''45'distrib'45''8743'_2832 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_2880 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8744'_2880 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'distrib'737''45''8744'_2880 v6
du_'8743''45'distrib'737''45''8744'_2880 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'737''45''8744'_2880 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_'8743''45'distrib'45''8744'_2834 (coe v0))
-- Algebra.Lattice.Structures.IsDistributiveLattice.∧-distribʳ-∨
d_'8743''45'distrib'691''45''8744'_2882 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'691''45''8744'_2882 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8743''45'distrib'691''45''8744'_2882 v6
du_'8743''45'distrib'691''45''8744'_2882 ::
  T_IsDistributiveLattice_2818 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'691''45''8744'_2882 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_'8743''45'distrib'45''8744'_2834 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra
d_IsBooleanAlgebra_2894 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsBooleanAlgebra_2894
  = C_IsBooleanAlgebra'46'constructor_41193 T_IsDistributiveLattice_2818
                                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                            (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Lattice.Structures.IsBooleanAlgebra.isDistributiveLattice
d_isDistributiveLattice_2914 ::
  T_IsBooleanAlgebra_2894 -> T_IsDistributiveLattice_2818
d_isDistributiveLattice_2914 v0
  = case coe v0 of
      C_IsBooleanAlgebra'46'constructor_41193 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsBooleanAlgebra.∨-complement
d_'8744''45'complement_2916 ::
  T_IsBooleanAlgebra_2894 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'complement_2916 v0
  = case coe v0 of
      C_IsBooleanAlgebra'46'constructor_41193 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsBooleanAlgebra.∧-complement
d_'8743''45'complement_2918 ::
  T_IsBooleanAlgebra_2894 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'complement_2918 v0
  = case coe v0 of
      C_IsBooleanAlgebra'46'constructor_41193 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsBooleanAlgebra.¬-cong
d_'172''45'cong_2920 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'172''45'cong_2920 v0
  = case coe v0 of
      C_IsBooleanAlgebra'46'constructor_41193 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.absorptive
d_absorptive_2924 ::
  T_IsBooleanAlgebra_2894 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_2924 v0
  = coe
      d_absorptive_2780
      (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0)))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.isEquivalence
d_isEquivalence_2926 ::
  T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2926 v0
  = coe
      d_isEquivalence_2766
      (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0)))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.isLattice
d_isLattice_2928 :: T_IsBooleanAlgebra_2894 -> T_IsLattice_2744
d_isLattice_2928 v0
  = coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.isPartialEquivalence
d_isPartialEquivalence_2930 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2930 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_2930 v9
du_isPartialEquivalence_2930 ::
  T_IsBooleanAlgebra_2894 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2930 v0
  = let v1 = d_isDistributiveLattice_2914 (coe v0) in
    let v2 = d_isLattice_2830 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_2766 (coe v2))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.refl
d_refl_2932 :: T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny
d_refl_2932 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_2766
         (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0))))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.reflexive
d_reflexive_2934 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2934 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_2934 v9
du_reflexive_2934 ::
  T_IsBooleanAlgebra_2894 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2934 v0
  = let v1 = d_isDistributiveLattice_2914 (coe v0) in
    let v2 = d_isLattice_2830 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_2766 (coe v2)) v3
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.sym
d_sym_2936 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2936 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_2766
         (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0))))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.trans
d_trans_2938 ::
  T_IsBooleanAlgebra_2894 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2938 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_2766
         (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0))))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_2940 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_2940 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                   ~v8 v9
  = du_'8743''45'absorbs'45''8744'_2940 v9
du_'8743''45'absorbs'45''8744'_2940 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_2940 v0
  = let v1 = d_isDistributiveLattice_2914 (coe v0) in
    coe
      du_'8743''45'absorbs'45''8744'_2796 (coe d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∧-assoc
d_'8743''45'assoc_2942 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'assoc_2942 v0
  = coe
      d_'8743''45'assoc_2776
      (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0)))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∧-comm
d_'8743''45'comm_2944 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'comm_2944 v0
  = coe
      d_'8743''45'comm_2774
      (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0)))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∧-cong
d_'8743''45'cong_2946 ::
  T_IsBooleanAlgebra_2894 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong_2946 v0
  = coe
      d_'8743''45'cong_2778
      (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0)))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∧-congʳ
d_'8743''45'cong'691'_2948 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_2948 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8743''45'cong'691'_2948 v9
du_'8743''45'cong'691'_2948 ::
  T_IsBooleanAlgebra_2894 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_2948 v0
  = let v1 = d_isDistributiveLattice_2914 (coe v0) in
    coe du_'8743''45'cong'691'_2802 (coe d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∧-congˡ
d_'8743''45'cong'737'_2950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_2950 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8743''45'cong'737'_2950 v9
du_'8743''45'cong'737'_2950 ::
  T_IsBooleanAlgebra_2894 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_2950 v0
  = let v1 = d_isDistributiveLattice_2914 (coe v0) in
    coe du_'8743''45'cong'737'_2798 (coe d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∧-distrib-∨
d_'8743''45'distrib'45''8744'_2952 ::
  T_IsBooleanAlgebra_2894 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8744'_2952 v0
  = coe
      d_'8743''45'distrib'45''8744'_2834
      (coe d_isDistributiveLattice_2914 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∧-distribʳ-∨
d_'8743''45'distrib'691''45''8744'_2954 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'691''45''8744'_2954 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                        ~v7 ~v8 v9
  = du_'8743''45'distrib'691''45''8744'_2954 v9
du_'8743''45'distrib'691''45''8744'_2954 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'691''45''8744'_2954 v0
  = coe
      du_'8743''45'distrib'691''45''8744'_2882
      (coe d_isDistributiveLattice_2914 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_2956 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8744'_2956 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                        ~v7 ~v8 v9
  = du_'8743''45'distrib'737''45''8744'_2956 v9
du_'8743''45'distrib'737''45''8744'_2956 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'737''45''8744'_2956 v0
  = coe
      du_'8743''45'distrib'737''45''8744'_2880
      (coe d_isDistributiveLattice_2914 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_2958 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_2958 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                   ~v8 v9
  = du_'8744''45'absorbs'45''8743'_2958 v9
du_'8744''45'absorbs'45''8743'_2958 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_2958 v0
  = let v1 = d_isDistributiveLattice_2914 (coe v0) in
    coe
      du_'8744''45'absorbs'45''8743'_2794 (coe d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∨-assoc
d_'8744''45'assoc_2960 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'assoc_2960 v0
  = coe
      d_'8744''45'assoc_2770
      (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0)))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∨-comm
d_'8744''45'comm_2962 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'comm_2962 v0
  = coe
      d_'8744''45'comm_2768
      (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0)))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∨-cong
d_'8744''45'cong_2964 ::
  T_IsBooleanAlgebra_2894 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong_2964 v0
  = coe
      d_'8744''45'cong_2772
      (coe d_isLattice_2830 (coe d_isDistributiveLattice_2914 (coe v0)))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∨-congʳ
d_'8744''45'cong'691'_2966 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_2966 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8744''45'cong'691'_2966 v9
du_'8744''45'cong'691'_2966 ::
  T_IsBooleanAlgebra_2894 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_2966 v0
  = let v1 = d_isDistributiveLattice_2914 (coe v0) in
    coe du_'8744''45'cong'691'_2810 (coe d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∨-congˡ
d_'8744''45'cong'737'_2968 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_2968 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8744''45'cong'737'_2968 v9
du_'8744''45'cong'737'_2968 ::
  T_IsBooleanAlgebra_2894 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_2968 v0
  = let v1 = d_isDistributiveLattice_2914 (coe v0) in
    coe du_'8744''45'cong'737'_2806 (coe d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∨-distrib-∧
d_'8744''45'distrib'45''8743'_2970 ::
  T_IsBooleanAlgebra_2894 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'distrib'45''8743'_2970 v0
  = coe
      d_'8744''45'distrib'45''8743'_2832
      (coe d_isDistributiveLattice_2914 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∨-distribʳ-∧
d_'8744''45'distrib'691''45''8743'_2972 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'691''45''8743'_2972 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                        ~v7 ~v8 v9
  = du_'8744''45'distrib'691''45''8743'_2972 v9
du_'8744''45'distrib'691''45''8743'_2972 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'691''45''8743'_2972 v0
  = coe
      du_'8744''45'distrib'691''45''8743'_2878
      (coe d_isDistributiveLattice_2914 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra._.∨-distribˡ-∧
d_'8744''45'distrib'737''45''8743'_2974 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'737''45''8743'_2974 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                        ~v7 ~v8 v9
  = du_'8744''45'distrib'737''45''8743'_2974 v9
du_'8744''45'distrib'737''45''8743'_2974 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'737''45''8743'_2974 v0
  = coe
      du_'8744''45'distrib'737''45''8743'_2876
      (coe d_isDistributiveLattice_2914 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra.∨-complementˡ
d_'8744''45'complement'737'_2976 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny
d_'8744''45'complement'737'_2976 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                 ~v8 v9
  = du_'8744''45'complement'737'_2976 v9
du_'8744''45'complement'737'_2976 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny
du_'8744''45'complement'737'_2976 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_'8744''45'complement_2916 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra.∨-complementʳ
d_'8744''45'complement'691'_2978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny
d_'8744''45'complement'691'_2978 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                 ~v8 v9
  = du_'8744''45'complement'691'_2978 v9
du_'8744''45'complement'691'_2978 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny
du_'8744''45'complement'691'_2978 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_'8744''45'complement_2916 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra.∧-complementˡ
d_'8743''45'complement'737'_2980 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny
d_'8743''45'complement'737'_2980 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                 ~v8 v9
  = du_'8743''45'complement'737'_2980 v9
du_'8743''45'complement'737'_2980 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny
du_'8743''45'complement'737'_2980 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_'8743''45'complement_2918 (coe v0))
-- Algebra.Lattice.Structures.IsBooleanAlgebra.∧-complementʳ
d_'8743''45'complement'691'_2982 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny
d_'8743''45'complement'691'_2982 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                 ~v8 v9
  = du_'8743''45'complement'691'_2982 v9
du_'8743''45'complement'691'_2982 ::
  T_IsBooleanAlgebra_2894 -> AgdaAny -> AgdaAny
du_'8743''45'complement'691'_2982 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_'8743''45'complement_2918 (coe v0))
