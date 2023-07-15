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

module MAlonzo.Code.Algebra.Structures.Biased where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Consequences.Setoid
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Structures.Biased._._DistributesOver_
d__DistributesOver__18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver__18 = erased
-- Algebra.Structures.Biased._._DistributesOverʳ_
d__DistributesOver'691'__20 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'691'__20 = erased
-- Algebra.Structures.Biased._._DistributesOverˡ_
d__DistributesOver'737'__22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'737'__22 = erased
-- Algebra.Structures.Biased._.Commutative
d_Commutative_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Commutative_40 = erased
-- Algebra.Structures.Biased._.LeftIdentity
d_LeftIdentity_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftIdentity_82 = erased
-- Algebra.Structures.Biased._.LeftZero
d_LeftZero_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftZero_90 = erased
-- Algebra.Structures.Biased._.RightIdentity
d_RightIdentity_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightIdentity_112 = erased
-- Algebra.Structures.Biased._.RightZero
d_RightZero_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightZero_120 = erased
-- Algebra.Structures.Biased._.Zero
d_Zero_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Zero_138 = erased
-- Algebra.Structures.Biased._.IsAbelianGroup
d_IsAbelianGroup_142 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Structures.Biased._.IsCommutativeMonoid
d_IsCommutativeMonoid_152 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Structures.Biased._.IsCommutativeSemiring
d_IsCommutativeSemiring_158 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Algebra.Structures.Biased._.IsMonoid
d_IsMonoid_188 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Structures.Biased._.IsNearSemiring
d_IsNearSemiring_192 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Structures.Biased._.IsRing
d_IsRing_204 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
-- Algebra.Structures.Biased._.IsSemigroup
d_IsSemigroup_210 a0 a1 a2 a3 a4 = ()
-- Algebra.Structures.Biased._.IsSemiringWithoutAnnihilatingZero
d_IsSemiringWithoutAnnihilatingZero_216 a0 a1 a2 a3 a4 a5 a6 a7
  = ()
-- Algebra.Structures.Biased._.IsSemiringWithoutOne
d_IsSemiringWithoutOne_218 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Structures.Biased._.IsAbelianGroup._-_
d__'45'__224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'45'__224 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 ~v7 = du__'45'__224 v4 v6
du__'45'__224 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__224 v0 v1
  = coe
      MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v0) (coe v1)
-- Algebra.Structures.Biased._.IsAbelianGroup.identityʳ
d_identity'691'_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny
d_identity'691'_232 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'691'_232 v7
du_identity'691'_232 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny
du_identity'691'_232 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1))
-- Algebra.Structures.Biased._.IsAbelianGroup.identityˡ
d_identity'737'_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny
d_identity'737'_234 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'737'_234 v7
du_identity'737'_234 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny
du_identity'737'_234 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1))
-- Algebra.Structures.Biased._.IsAbelianGroup.inverseʳ
d_inverse'691'_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny
d_inverse'691'_238 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'691'_238 v7
du_inverse'691'_238 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny
du_inverse'691'_238 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Algebra.Structures.Biased._.IsAbelianGroup.inverseˡ
d_inverse'737'_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny
d_inverse'737'_240 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'737'_240 v7
du_inverse'737'_240 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny
du_inverse'737'_240 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Algebra.Structures.Biased._.IsAbelianGroup.isCommutativeMagma
d_isCommutativeMagma_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_242 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeMagma_242 v7
du_isCommutativeMagma_242 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_242 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
              (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v1))
-- Algebra.Structures.Biased._.IsAbelianGroup.isCommutativeMonoid
d_isCommutativeMonoid_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_244 ~v0 ~v1 ~v2 ~v3
  = du_isCommutativeMonoid_244
du_isCommutativeMonoid_244 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_244 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048 v3
-- Algebra.Structures.Biased._.IsAbelianGroup.isCommutativeSemigroup
d_isCommutativeSemigroup_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_246 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeSemigroup_246 v7
du_isCommutativeSemigroup_246 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_246 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
         (coe v0))
-- Algebra.Structures.Biased._.IsAbelianGroup.isInvertibleMagma
d_isInvertibleMagma_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_252 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isInvertibleMagma_252 v7
du_isInvertibleMagma_252 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
du_isInvertibleMagma_252 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleMagma_966
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Algebra.Structures.Biased._.IsAbelianGroup.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_254 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isInvertibleUnitalMagma_254 v7
du_isInvertibleUnitalMagma_254 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleUnitalMagma_968
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0))
-- Algebra.Structures.Biased._.IsAbelianGroup.isPartialEquivalence
d_isPartialEquivalence_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_260 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_260 v7
du_isPartialEquivalence_260 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_260 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Structures.Biased._.IsAbelianGroup.isUnitalMagma
d_isUnitalMagma_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_264 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isUnitalMagma_264 v7
du_isUnitalMagma_264 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_264 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1))
-- Algebra.Structures.Biased._.IsAbelianGroup.reflexive
d_reflexive_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_268 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_268 v7
du_reflexive_268 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_268 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Structures.Biased._.IsAbelianGroup.setoid
d_setoid_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_270 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_setoid_270 v7
du_setoid_270 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_270 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Structures.Biased._.IsAbelianGroup.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_276 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_unique'691''45''8315''185'_276 v4 v5 v6 v7
du_unique'691''45''8315''185'_276 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_276 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Structures.du_unique'691''45''8315''185'_964
      (coe v0) (coe v1) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3))
-- Algebra.Structures.Biased._.IsAbelianGroup.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_278 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_unique'737''45''8315''185'_278 v4 v5 v6 v7
du_unique'737''45''8315''185'_278 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_278 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Structures.du_unique'737''45''8315''185'_958
      (coe v0) (coe v1) (coe v2)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v3))
-- Algebra.Structures.Biased._.IsAbelianGroup.∙-congʳ
d_'8729''45'cong'691'_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_284 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_284 v7
du_'8729''45'cong'691'_284 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_284 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Structures.Biased._.IsAbelianGroup.∙-congˡ
d_'8729''45'cong'737'_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_286 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_286 v7
du_'8729''45'cong'737'_286 ::
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_286 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Structures.Biased._.IsMonoid.identityʳ
d_identity'691'_1464 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny -> AgdaAny
d_identity'691'_1464 ~v0 ~v1 ~v2 ~v3 = du_identity'691'_1464
du_identity'691'_1464 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny -> AgdaAny
du_identity'691'_1464 v0 v1 v2
  = coe MAlonzo.Code.Algebra.Structures.du_identity'691'_642 v2
-- Algebra.Structures.Biased._.IsMonoid.identityˡ
d_identity'737'_1466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny -> AgdaAny
d_identity'737'_1466 ~v0 ~v1 ~v2 ~v3 = du_identity'737'_1466
du_identity'737'_1466 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny -> AgdaAny
du_identity'737'_1466 v0 v1 v2
  = coe MAlonzo.Code.Algebra.Structures.du_identity'737'_640 v2
-- Algebra.Structures.Biased._.IsMonoid.isPartialEquivalence
d_isPartialEquivalence_1472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1472 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_1472 v6
du_isPartialEquivalence_1472 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1472 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
-- Algebra.Structures.Biased._.IsMonoid.isUnitalMagma
d_isUnitalMagma_1476 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_1476 ~v0 ~v1 ~v2 ~v3 = du_isUnitalMagma_1476
du_isUnitalMagma_1476 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_1476 v0 v1 v2
  = coe MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644 v2
-- Algebra.Structures.Biased._.IsMonoid.reflexive
d_reflexive_1480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1480 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_1480 v6
du_reflexive_1480 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1480 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v2))
        v3
-- Algebra.Structures.Biased._.IsMonoid.setoid
d_setoid_1482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1482 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_1482 v6
du_setoid_1482 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1482 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
-- Algebra.Structures.Biased._.IsMonoid.∙-congʳ
d_'8729''45'cong'691'_1490 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1490 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_1490 v6
du_'8729''45'cong'691'_1490 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1490 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
-- Algebra.Structures.Biased._.IsMonoid.∙-congˡ
d_'8729''45'cong'737'_1492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1492 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_1492 v6
du_'8729''45'cong'737'_1492 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1492 v0
  = let v1
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v1))
-- Algebra.Structures.Biased.IsCommutativeMonoidˡ
d_IsCommutativeMonoid'737'_2446 a0 a1 a2 a3 a4 a5 = ()
data T_IsCommutativeMonoid'737'_2446
  = C_IsCommutativeMonoid'737''46'constructor_31055 MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
                                                    (AgdaAny -> AgdaAny)
                                                    (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.Biased.IsCommutativeMonoidˡ.isSemigroup
d_isSemigroup_2458 ::
  T_IsCommutativeMonoid'737'_2446 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2458 v0
  = case coe v0 of
      C_IsCommutativeMonoid'737''46'constructor_31055 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeMonoidˡ.identityˡ
d_identity'737'_2460 ::
  T_IsCommutativeMonoid'737'_2446 -> AgdaAny -> AgdaAny
d_identity'737'_2460 v0
  = case coe v0 of
      C_IsCommutativeMonoid'737''46'constructor_31055 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeMonoidˡ.comm
d_comm_2462 ::
  T_IsCommutativeMonoid'737'_2446 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2462 v0
  = case coe v0 of
      C_IsCommutativeMonoid'737''46'constructor_31055 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeMonoidˡ.isCommutativeMonoid
d_isCommutativeMonoid_2464 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid'737'_2446 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_2464 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isCommutativeMonoid_2464 v4 v5 v6
du_isCommutativeMonoid_2464 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid'737'_2446 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_2464 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
         (coe d_isSemigroup_2458 (coe v2))
         (coe
            MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'id'737''8658'id_238
            (let v3 = d_isSemigroup_2458 (coe v2) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3)))
            (coe v0) (coe d_comm_2462 (coe v2)) (coe v1)
            (coe d_identity'737'_2460 (coe v2))))
      (coe d_comm_2462 (coe v2))
-- Algebra.Structures.Biased.IsCommutativeMonoidʳ
d_IsCommutativeMonoid'691'_2500 a0 a1 a2 a3 a4 a5 = ()
data T_IsCommutativeMonoid'691'_2500
  = C_IsCommutativeMonoid'691''46'constructor_32491 MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
                                                    (AgdaAny -> AgdaAny)
                                                    (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.Biased.IsCommutativeMonoidʳ.isSemigroup
d_isSemigroup_2512 ::
  T_IsCommutativeMonoid'691'_2500 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_2512 v0
  = case coe v0 of
      C_IsCommutativeMonoid'691''46'constructor_32491 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeMonoidʳ.identityʳ
d_identity'691'_2514 ::
  T_IsCommutativeMonoid'691'_2500 -> AgdaAny -> AgdaAny
d_identity'691'_2514 v0
  = case coe v0 of
      C_IsCommutativeMonoid'691''46'constructor_32491 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeMonoidʳ.comm
d_comm_2516 ::
  T_IsCommutativeMonoid'691'_2500 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2516 v0
  = case coe v0 of
      C_IsCommutativeMonoid'691''46'constructor_32491 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeMonoidʳ.isCommutativeMonoid
d_isCommutativeMonoid_2518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid'691'_2500 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_2518 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isCommutativeMonoid_2518 v4 v5 v6
du_isCommutativeMonoid_2518 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid'691'_2500 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_2518 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
         (coe d_isSemigroup_2512 (coe v2))
         (coe
            MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'id'691''8658'id_242
            (let v3 = d_isSemigroup_2512 (coe v2) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3)))
            (coe v0) (coe d_comm_2516 (coe v2)) (coe v1)
            (coe d_identity'691'_2514 (coe v2))))
      (coe d_comm_2516 (coe v2))
-- Algebra.Structures.Biased.IsSemiringWithoutOne*
d_IsSemiringWithoutOne'42'_2556 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsSemiringWithoutOne'42'_2556
  = C_IsSemiringWithoutOne'42''46'constructor_33965 MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
                                                    MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
                                                    MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                                    MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.Biased.IsSemiringWithoutOne*.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2572 ::
  T_IsSemiringWithoutOne'42'_2556 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2572 v0
  = case coe v0 of
      C_IsSemiringWithoutOne'42''46'constructor_33965 v1 v2 v3 v4
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsSemiringWithoutOne*.*-isSemigroup
d_'42''45'isSemigroup_2574 ::
  T_IsSemiringWithoutOne'42'_2556 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2574 v0
  = case coe v0 of
      C_IsSemiringWithoutOne'42''46'constructor_33965 v1 v2 v3 v4
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsSemiringWithoutOne*.distrib
d_distrib_2576 ::
  T_IsSemiringWithoutOne'42'_2556 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2576 v0
  = case coe v0 of
      C_IsSemiringWithoutOne'42''46'constructor_33965 v1 v2 v3 v4
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsSemiringWithoutOne*.zero
d_zero_2578 ::
  T_IsSemiringWithoutOne'42'_2556 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2578 v0
  = case coe v0 of
      C_IsSemiringWithoutOne'42''46'constructor_33965 v1 v2 v3 v4
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsSemiringWithoutOne*.isSemiringWithoutOne
d_isSemiringWithoutOne_2580 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsSemiringWithoutOne'42'_2556 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_2580 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isSemiringWithoutOne_2580 v7
du_isSemiringWithoutOne_2580 ::
  T_IsSemiringWithoutOne'42'_2556 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_2580 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutOne'46'constructor_33063
      (coe d_'43''45'isCommutativeMonoid_2572 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe d_'42''45'isSemigroup_2574 (coe v0))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (coe d_'42''45'isSemigroup_2574 (coe v0)))
      (coe d_distrib_2576 (coe v0)) (coe d_zero_2578 (coe v0))
-- Algebra.Structures.Biased.IsNearSemiring*
d_IsNearSemiring'42'_2618 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsNearSemiring'42'_2618
  = C_IsNearSemiring'42''46'constructor_35767 MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
                                              MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
                                              (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                              (AgdaAny -> AgdaAny)
-- Algebra.Structures.Biased.IsNearSemiring*.+-isMonoid
d_'43''45'isMonoid_2634 ::
  T_IsNearSemiring'42'_2618 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'43''45'isMonoid_2634 v0
  = case coe v0 of
      C_IsNearSemiring'42''46'constructor_35767 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsNearSemiring*.*-isSemigroup
d_'42''45'isSemigroup_2636 ::
  T_IsNearSemiring'42'_2618 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_2636 v0
  = case coe v0 of
      C_IsNearSemiring'42''46'constructor_35767 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsNearSemiring*.distribʳ
d_distrib'691'_2638 ::
  T_IsNearSemiring'42'_2618 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2638 v0
  = case coe v0 of
      C_IsNearSemiring'42''46'constructor_35767 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsNearSemiring*.zeroˡ
d_zero'737'_2640 :: T_IsNearSemiring'42'_2618 -> AgdaAny -> AgdaAny
d_zero'737'_2640 v0
  = case coe v0 of
      C_IsNearSemiring'42''46'constructor_35767 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsNearSemiring*.isNearSemiring
d_isNearSemiring_2642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNearSemiring'42'_2618 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_2642 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isNearSemiring_2642 v7
du_isNearSemiring_2642 ::
  T_IsNearSemiring'42'_2618 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_2642 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsNearSemiring'46'constructor_30479
      (coe d_'43''45'isMonoid_2634 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe d_'42''45'isSemigroup_2636 (coe v0))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (coe d_'42''45'isSemigroup_2636 (coe v0)))
      (coe d_distrib'691'_2638 (coe v0)) (coe d_zero'737'_2640 (coe v0))
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*
d_IsSemiringWithoutAnnihilatingZero'42'_2682 a0 a1 a2 a3 a4 a5 a6
                                             a7
  = ()
data T_IsSemiringWithoutAnnihilatingZero'42'_2682
  = C_IsSemiringWithoutAnnihilatingZero'42''46'constructor_37563 MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
                                                                 MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2698 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2698 v0
  = case coe v0 of
      C_IsSemiringWithoutAnnihilatingZero'42''46'constructor_37563 v1 v2 v3
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*.*-isMonoid
d_'42''45'isMonoid_2700 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_2700 v0
  = case coe v0 of
      C_IsSemiringWithoutAnnihilatingZero'42''46'constructor_37563 v1 v2 v3
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*.distrib
d_distrib_2702 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2702 v0
  = case coe v0 of
      C_IsSemiringWithoutAnnihilatingZero'42''46'constructor_37563 v1 v2 v3
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2704 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
                                         ~v6 ~v7 v8
  = du_isSemiringWithoutAnnihilatingZero_2704 v8
du_isSemiringWithoutAnnihilatingZero_2704 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
du_isSemiringWithoutAnnihilatingZero_2704 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
      (coe d_'43''45'isCommutativeMonoid_2698 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_'42''45'isMonoid_2700 (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe d_'42''45'isMonoid_2700 (coe v0))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_identity_612
         (coe d_'42''45'isMonoid_2700 (coe v0)))
      (coe d_distrib_2702 (coe v0))
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*._._.identityʳ
d_identity'691'_2716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 -> AgdaAny -> AgdaAny
d_identity'691'_2716 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_2716 v8
du_identity'691'_2716 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 -> AgdaAny -> AgdaAny
du_identity'691'_2716 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe d_'42''45'isMonoid_2700 (coe v0))
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*._._.identityˡ
d_identity'737'_2718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 -> AgdaAny -> AgdaAny
d_identity'737'_2718 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_2718 v8
du_identity'737'_2718 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 -> AgdaAny -> AgdaAny
du_identity'737'_2718 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe d_'42''45'isMonoid_2700 (coe v0))
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*._._.isPartialEquivalence
d_isPartialEquivalence_2724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2724 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_2724 v8
du_isPartialEquivalence_2724 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2724 v0
  = let v1 = d_'42''45'isMonoid_2700 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*._._.isUnitalMagma
d_isUnitalMagma_2728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_2728 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isUnitalMagma_2728 v8
du_isUnitalMagma_2728 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_2728 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe d_'42''45'isMonoid_2700 (coe v0))
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*._._.reflexive
d_reflexive_2732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2732 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_2732 v8
du_reflexive_2732 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2732 v0
  = let v1 = d_'42''45'isMonoid_2700 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*._._.setoid
d_setoid_2734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2734 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_2734 v8
du_setoid_2734 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2734 v0
  = let v1 = d_'42''45'isMonoid_2700 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*._._.∙-congʳ
d_'8729''45'cong'691'_2742 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2742 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_2742 v8
du_'8729''45'cong'691'_2742 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2742 v0
  = let v1 = d_'42''45'isMonoid_2700 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Structures.Biased.IsSemiringWithoutAnnihilatingZero*._._.∙-congˡ
d_'8729''45'cong'737'_2744 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2744 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_2744 v8
du_'8729''45'cong'737'_2744 ::
  T_IsSemiringWithoutAnnihilatingZero'42'_2682 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2744 v0
  = let v1 = d_'42''45'isMonoid_2700 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Structures.Biased.IsCommutativeSemiringˡ
d_IsCommutativeSemiring'737'_2754 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsCommutativeSemiring'737'_2754
  = C_IsCommutativeSemiring'737''46'constructor_39845 MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
                                                      MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
                                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                                      (AgdaAny -> AgdaAny)
-- Algebra.Structures.Biased.IsCommutativeSemiringˡ.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2772 ::
  T_IsCommutativeSemiring'737'_2754 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2772 v0
  = case coe v0 of
      C_IsCommutativeSemiring'737''46'constructor_39845 v1 v2 v3 v4
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeSemiringˡ.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_2774 ::
  T_IsCommutativeSemiring'737'_2754 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_2774 v0
  = case coe v0 of
      C_IsCommutativeSemiring'737''46'constructor_39845 v1 v2 v3 v4
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeSemiringˡ.distribʳ
d_distrib'691'_2776 ::
  T_IsCommutativeSemiring'737'_2754 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2776 v0
  = case coe v0 of
      C_IsCommutativeSemiring'737''46'constructor_39845 v1 v2 v3 v4
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeSemiringˡ.zeroˡ
d_zero'737'_2778 ::
  T_IsCommutativeSemiring'737'_2754 -> AgdaAny -> AgdaAny
d_zero'737'_2778 v0
  = case coe v0 of
      C_IsCommutativeSemiring'737''46'constructor_39845 v1 v2 v3 v4
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeSemiringˡ.isCommutativeSemiring
d_isCommutativeSemiring_2780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring'737'_2754 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_2780 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 ~v7 v8
  = du_isCommutativeSemiring_2780 v4 v5 v6 v8
du_isCommutativeSemiring_2780 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiring'737'_2754 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_isCommutativeSemiring_2780 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiring'46'constructor_46125
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsSemiring'46'constructor_42303
         (coe
            MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
            (coe d_'43''45'isCommutativeMonoid_2772 (coe v3))
            (coe
               MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMagma_444
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                        (coe d_'42''45'isCommutativeMonoid_2774 (coe v3))))))
            (coe
               MAlonzo.Code.Algebra.Structures.d_assoc_446
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                     (coe d_'42''45'isCommutativeMonoid_2774 (coe v3)))))
            (coe
               MAlonzo.Code.Algebra.Structures.d_identity_612
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe d_'42''45'isCommutativeMonoid_2774 (coe v3))))
            (coe
               MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'distr'691''8658'distr_414
               (let v4 = d_'43''45'isCommutativeMonoid_2772 (coe v3) in
                let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
                let v6
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
               (coe v1) (coe v0)
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isMagma_444
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                           (coe d_'43''45'isCommutativeMonoid_2772 (coe v3))))))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_comm_662
                  (coe d_'42''45'isCommutativeMonoid_2774 (coe v3)))
               (coe d_distrib'691'_2776 (coe v3))))
         (coe
            MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'ze'737''8658'ze_258
            (let v4 = d_'43''45'isCommutativeMonoid_2772 (coe v3) in
             let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
             let v6
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
            (coe v1)
            (coe
               MAlonzo.Code.Algebra.Structures.d_comm_662
               (coe d_'42''45'isCommutativeMonoid_2774 (coe v3)))
            (coe v2) (coe d_zero'737'_2778 (coe v3))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_comm_662
         (coe d_'42''45'isCommutativeMonoid_2774 (coe v3)))
-- Algebra.Structures.Biased.IsCommutativeSemiringʳ
d_IsCommutativeSemiring'691'_2882 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsCommutativeSemiring'691'_2882
  = C_IsCommutativeSemiring'691''46'constructor_44893 MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
                                                      MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
                                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                                      (AgdaAny -> AgdaAny)
-- Algebra.Structures.Biased.IsCommutativeSemiringʳ.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_2900 ::
  T_IsCommutativeSemiring'691'_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_2900 v0
  = case coe v0 of
      C_IsCommutativeSemiring'691''46'constructor_44893 v1 v2 v3 v4
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeSemiringʳ.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_2902 ::
  T_IsCommutativeSemiring'691'_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_2902 v0
  = case coe v0 of
      C_IsCommutativeSemiring'691''46'constructor_44893 v1 v2 v3 v4
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeSemiringʳ.distribˡ
d_distrib'737'_2904 ::
  T_IsCommutativeSemiring'691'_2882 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_2904 v0
  = case coe v0 of
      C_IsCommutativeSemiring'691''46'constructor_44893 v1 v2 v3 v4
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeSemiringʳ.zeroʳ
d_zero'691'_2906 ::
  T_IsCommutativeSemiring'691'_2882 -> AgdaAny -> AgdaAny
d_zero'691'_2906 v0
  = case coe v0 of
      C_IsCommutativeSemiring'691''46'constructor_44893 v1 v2 v3 v4
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsCommutativeSemiringʳ.isCommutativeSemiring
d_isCommutativeSemiring_2908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring'691'_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_2908 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 ~v7 v8
  = du_isCommutativeSemiring_2908 v4 v5 v6 v8
du_isCommutativeSemiring_2908 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiring'691'_2882 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_isCommutativeSemiring_2908 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiring'46'constructor_46125
      (coe
         MAlonzo.Code.Algebra.Structures.C_IsSemiring'46'constructor_42303
         (coe
            MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
            (coe d_'43''45'isCommutativeMonoid_2900 (coe v3))
            (coe
               MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMagma_444
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                        (coe d_'42''45'isCommutativeMonoid_2902 (coe v3))))))
            (coe
               MAlonzo.Code.Algebra.Structures.d_assoc_446
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                     (coe d_'42''45'isCommutativeMonoid_2902 (coe v3)))))
            (coe
               MAlonzo.Code.Algebra.Structures.d_identity_612
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe d_'42''45'isCommutativeMonoid_2902 (coe v3))))
            (coe
               MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'distr'737''8658'distr_410
               (let v4 = d_'43''45'isCommutativeMonoid_2900 (coe v3) in
                let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
                let v6
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
               (coe v1) (coe v0)
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isMagma_444
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                           (coe d_'43''45'isCommutativeMonoid_2900 (coe v3))))))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_comm_662
                  (coe d_'42''45'isCommutativeMonoid_2902 (coe v3)))
               (coe d_distrib'737'_2904 (coe v3))))
         (coe
            MAlonzo.Code.Algebra.Consequences.Setoid.du_comm'43'ze'691''8658'ze_262
            (let v4 = d_'43''45'isCommutativeMonoid_2900 (coe v3) in
             let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
             let v6
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
            (coe v1)
            (coe
               MAlonzo.Code.Algebra.Structures.d_comm_662
               (coe d_'42''45'isCommutativeMonoid_2902 (coe v3)))
            (coe v2) (coe d_zero'691'_2906 (coe v3))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_comm_662
         (coe d_'42''45'isCommutativeMonoid_2902 (coe v3)))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero
d_IsRingWithoutAnnihilatingZero_3012 a0 a1 a2 a3 a4 a5 a6 a7 a8
  = ()
data T_IsRingWithoutAnnihilatingZero_3012
  = C_IsRingWithoutAnnihilatingZero'46'constructor_49975 MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
                                                         MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
                                                         MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+-isAbelianGroup
d_'43''45'isAbelianGroup_3030 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_3030 v0
  = case coe v0 of
      C_IsRingWithoutAnnihilatingZero'46'constructor_49975 v1 v2 v3
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*-isMonoid
d_'42''45'isMonoid_3032 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_3032 v0
  = case coe v0 of
      C_IsRingWithoutAnnihilatingZero'46'constructor_49975 v1 v2 v3
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.distrib
d_distrib_3034 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_3034 v0
  = case coe v0 of
      C_IsRingWithoutAnnihilatingZero'46'constructor_49975 v1 v2 v3
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+._-_
d__'45'__3038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'45'__3038 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 ~v7 ~v8 ~v9
  = du__'45'__3038 v4 v6
du__'45'__3038 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__3038 v0 v1
  = coe
      MAlonzo.Code.Algebra.Structures.du__'45'__944 (coe v0) (coe v1)
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.assoc
d_assoc_3040 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_3040 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe d_'43''45'isAbelianGroup_3030 (coe v0)))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.comm
d_comm_3042 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_3042 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_990
      (coe d_'43''45'isAbelianGroup_3030 (coe v0))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.identity
d_identity_3044 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3044 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe d_'43''45'isAbelianGroup_3030 (coe v0))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.identityʳ
d_identity'691'_3046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_identity'691'_3046 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_3046 v9
du_identity'691'_3046 ::
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
du_identity'691'_3046 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.identityˡ
d_identity'737'_3048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_identity'737'_3048 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_3048 v9
du_identity'737'_3048 ::
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
du_identity'737'_3048 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.inverse
d_inverse_3050 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_3050 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_inverse_904
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe d_'43''45'isAbelianGroup_3030 (coe v0)))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.inverseʳ
d_inverse'691'_3052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_inverse'691'_3052 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_inverse'691'_3052 v9
du_inverse'691'_3052 ::
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
du_inverse'691'_3052 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.inverseˡ
d_inverse'737'_3054 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_inverse'737'_3054 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_inverse'737'_3054 v9
du_inverse'737'_3054 ::
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
du_inverse'737'_3054 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_inverse'737'_950
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isCommutativeMagma
d_isCommutativeMagma_3056 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_3056 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMagma_3056 v9
du_isCommutativeMagma_3056 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_3056 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v2))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isCommutativeMonoid
d_isCommutativeMonoid_3058 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_3058 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMonoid_3058 v9
du_isCommutativeMonoid_3058 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_3058 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
      (coe d_'43''45'isAbelianGroup_3030 (coe v0))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isCommutativeSemigroup
d_isCommutativeSemigroup_3060 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_3060 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                              v9
  = du_isCommutativeSemigroup_3060 v9
du_isCommutativeSemigroup_3060 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_3060 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeMonoid_1048
         (coe v1))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isEquivalence
d_isEquivalence_3062 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3062 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe d_'43''45'isAbelianGroup_3030 (coe v0))))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isGroup
d_isGroup_3064 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_3064 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isGroup_988
      (coe d_'43''45'isAbelianGroup_3030 (coe v0))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isInvertibleMagma
d_isInvertibleMagma_3066 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
d_isInvertibleMagma_3066 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isInvertibleMagma_3066 v9
du_isInvertibleMagma_3066 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleMagma_776
du_isInvertibleMagma_3066 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleMagma_966
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_3068 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_3068 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               v9
  = du_isInvertibleUnitalMagma_3068 v9
du_isInvertibleUnitalMagma_3068 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_3068 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isInvertibleUnitalMagma_968
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isMagma
d_isMagma_3070 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_3070 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_902
            (coe
               MAlonzo.Code.Algebra.Structures.d_isGroup_988
               (coe d_'43''45'isAbelianGroup_3030 (coe v0)))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isMonoid
d_isMonoid_3072 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_3072 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_902
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe d_'43''45'isAbelianGroup_3030 (coe v0)))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isPartialEquivalence
d_isPartialEquivalence_3074 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3074 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_3074 v9
du_isPartialEquivalence_3074 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3074 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isSemigroup
d_isSemigroup_3076 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_3076 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_902
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe d_'43''45'isAbelianGroup_3030 (coe v0))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.isUnitalMagma
d_isUnitalMagma_3078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_3078 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isUnitalMagma_3078 v9
du_isUnitalMagma_3078 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_3078 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.refl
d_refl_3080 ::
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_refl_3080 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe d_'43''45'isAbelianGroup_3030 (coe v0)))))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.reflexive
d_reflexive_3082 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3082 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_3082 v9
du_reflexive_3082 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3082 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.setoid
d_setoid_3084 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3084 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_3084 v9
du_setoid_3084 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3084 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.sym
d_sym_3086 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3086 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe d_'43''45'isAbelianGroup_3030 (coe v0)))))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.trans
d_trans_3088 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3088 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe d_'43''45'isAbelianGroup_3030 (coe v0)))))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_3090 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_3090 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 ~v8
                                  v9
  = du_unique'691''45''8315''185'_3090 v4 v6 v7 v9
du_unique'691''45''8315''185'_3090 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_3090 v0 v1 v2 v3
  = let v4 = d_'43''45'isAbelianGroup_3030 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'691''45''8315''185'_964
      (coe v0) (coe v2) (coe v1)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_3092 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_3092 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 ~v8
                                  v9
  = du_unique'737''45''8315''185'_3092 v4 v6 v7 v9
du_unique'737''45''8315''185'_3092 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_3092 v0 v1 v2 v3
  = let v4 = d_'43''45'isAbelianGroup_3030 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_unique'737''45''8315''185'_958
      (coe v0) (coe v2) (coe v1)
      (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.⁻¹-cong
d_'8315''185''45'cong_3094 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_3094 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
      (coe
         MAlonzo.Code.Algebra.Structures.d_isGroup_988
         (coe d_'43''45'isAbelianGroup_3030 (coe v0)))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.∙-cong
d_'8729''45'cong_3096 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3096 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe d_'43''45'isAbelianGroup_3030 (coe v0))))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.∙-congʳ
d_'8729''45'cong'691'_3098 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3098 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_3098 v9
du_'8729''45'cong'691'_3098 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3098 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.+.∙-congˡ
d_'8729''45'cong'737'_3100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3100 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_3100 v9
du_'8729''45'cong'737'_3100 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3100 v0
  = let v1 = d_'43''45'isAbelianGroup_3030 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.assoc
d_assoc_3104 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_3104 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe d_'42''45'isMonoid_3032 (coe v0)))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.identity
d_identity_3106 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3106 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe d_'42''45'isMonoid_3032 (coe v0))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.identityʳ
d_identity'691'_3108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_identity'691'_3108 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_3108 v9
du_identity'691'_3108 ::
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
du_identity'691'_3108 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe d_'42''45'isMonoid_3032 (coe v0))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.identityˡ
d_identity'737'_3110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_identity'737'_3110 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_3110 v9
du_identity'737'_3110 ::
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
du_identity'737'_3110 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe d_'42''45'isMonoid_3032 (coe v0))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.isEquivalence
d_isEquivalence_3112 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3112 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe d_'42''45'isMonoid_3032 (coe v0))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.isMagma
d_isMagma_3114 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_3114 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe d_'42''45'isMonoid_3032 (coe v0)))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.isPartialEquivalence
d_isPartialEquivalence_3116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3116 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_3116 v9
du_isPartialEquivalence_3116 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3116 v0
  = let v1 = d_'42''45'isMonoid_3032 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.isSemigroup
d_isSemigroup_3118 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_3118 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe d_'42''45'isMonoid_3032 (coe v0))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.isUnitalMagma
d_isUnitalMagma_3120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_3120 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isUnitalMagma_3120 v9
du_isUnitalMagma_3120 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_3120 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe d_'42''45'isMonoid_3032 (coe v0))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.refl
d_refl_3122 ::
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_refl_3122 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_'42''45'isMonoid_3032 (coe v0)))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.reflexive
d_reflexive_3124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3124 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_3124 v9
du_reflexive_3124 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3124 v0
  = let v1 = d_'42''45'isMonoid_3032 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.setoid
d_setoid_3126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3126 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_3126 v9
du_setoid_3126 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3126 v0
  = let v1 = d_'42''45'isMonoid_3032 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.sym
d_sym_3128 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3128 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_'42''45'isMonoid_3032 (coe v0)))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.trans
d_trans_3130 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3130 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_'42''45'isMonoid_3032 (coe v0)))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.∙-cong
d_'8729''45'cong_3132 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3132 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe d_'42''45'isMonoid_3032 (coe v0))))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.∙-congʳ
d_'8729''45'cong'691'_3134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_3134 v9
du_'8729''45'cong'691'_3134 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3134 v0
  = let v1 = d_'42''45'isMonoid_3032 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.*.∙-congˡ
d_'8729''45'cong'737'_3136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3136 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_3136 v9
du_'8729''45'cong'737'_3136 ::
  T_IsRingWithoutAnnihilatingZero_3012 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3136 v0
  = let v1 = d_'42''45'isMonoid_3032 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.zeroˡ
d_zero'737'_3138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_zero'737'_3138 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 ~v8 v9
  = du_zero'737'_3138 v4 v5 v6 v7 v9
du_zero'737'_3138 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
du_zero'737'_3138 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_assoc'43'distrib'691''43'id'691''43'inv'691''8658'ze'737'_480
      (let v5 = d_'43''45'isAbelianGroup_3030 (coe v4) in
       let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
       let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
       let v8
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))
      (coe v0) (coe v1) (coe v2) (coe v3)
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe d_'43''45'isAbelianGroup_3030 (coe v4)))))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_'42''45'isMonoid_3032 (coe v4)))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe d_'43''45'isAbelianGroup_3030 (coe v4))))))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
         (coe d_distrib_3034 (coe v4)))
      (let v5 = d_'43''45'isAbelianGroup_3030 (coe v4) in
       let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
       coe
         MAlonzo.Code.Algebra.Structures.du_identity'691'_642
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6)))
      (let v5 = d_'43''45'isAbelianGroup_3030 (coe v4) in
       coe
         MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5)))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.zeroʳ
d_zero'691'_3140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
d_zero'691'_3140 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 ~v8 v9
  = du_zero'691'_3140 v4 v5 v6 v7 v9
du_zero'691'_3140 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 -> AgdaAny -> AgdaAny
du_zero'691'_3140 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_assoc'43'distrib'737''43'id'691''43'inv'691''8658'ze'691'_492
      (let v5 = d_'43''45'isAbelianGroup_3030 (coe v4) in
       let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
       let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6) in
       let v8
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8)))
      (coe v0) (coe v1) (coe v2) (coe v3)
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isGroup_988
                     (coe d_'43''45'isAbelianGroup_3030 (coe v4)))))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_'42''45'isMonoid_3032 (coe v4)))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_902
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isGroup_988
                  (coe d_'43''45'isAbelianGroup_3030 (coe v4))))))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
         (coe d_distrib_3034 (coe v4)))
      (let v5 = d_'43''45'isAbelianGroup_3030 (coe v4) in
       let v6 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5) in
       coe
         MAlonzo.Code.Algebra.Structures.du_identity'691'_642
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v6)))
      (let v5 = d_'43''45'isAbelianGroup_3030 (coe v4) in
       coe
         MAlonzo.Code.Algebra.Structures.du_inverse'691'_952
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v5)))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.zero
d_zero_3142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_3142 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 ~v8 v9
  = du_zero_3142 v4 v5 v6 v7 v9
du_zero_3142 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_3142 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_zero'737'_3138 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
      (coe
         du_zero'691'_3140 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
-- Algebra.Structures.Biased.IsRingWithoutAnnihilatingZero.isRing
d_isRing_3144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_3144 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 ~v8 v9
  = du_isRing_3144 v4 v5 v6 v7 v9
du_isRing_3144 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutAnnihilatingZero_3012 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
du_isRing_3144 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsRing'46'constructor_80853
      (coe d_'43''45'isAbelianGroup_3030 (coe v4))
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_'42''45'isMonoid_3032 (coe v4)))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe d_'42''45'isMonoid_3032 (coe v4))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_identity_612
         (coe d_'42''45'isMonoid_3032 (coe v4)))
      (coe d_distrib_3034 (coe v4))
      (coe du_zero_3142 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
-- Algebra.Structures.Biased.IsRing*
d_IsRing'42'_3156 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsRing'42'_3156
  = C_IsRing'42''46'constructor_58675 MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
                                      MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
                                      MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                      MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.Biased.IsRing*.+-isAbelianGroup
d_'43''45'isAbelianGroup_3176 ::
  T_IsRing'42'_3156 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_3176 v0
  = case coe v0 of
      C_IsRing'42''46'constructor_58675 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsRing*.*-isMonoid
d_'42''45'isMonoid_3178 ::
  T_IsRing'42'_3156 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_3178 v0
  = case coe v0 of
      C_IsRing'42''46'constructor_58675 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsRing*.distrib
d_distrib_3180 ::
  T_IsRing'42'_3156 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_3180 v0
  = case coe v0 of
      C_IsRing'42''46'constructor_58675 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsRing*.zero
d_zero_3182 ::
  T_IsRing'42'_3156 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_3182 v0
  = case coe v0 of
      C_IsRing'42''46'constructor_58675 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.Biased.IsRing*.isRing
d_isRing_3184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing'42'_3156 -> MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_3184 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isRing_3184 v9
du_isRing_3184 ::
  T_IsRing'42'_3156 -> MAlonzo.Code.Algebra.Structures.T_IsRing_2394
du_isRing_3184 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsRing'46'constructor_80853
      (coe d_'43''45'isAbelianGroup_3176 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe d_'42''45'isMonoid_3178 (coe v0)))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_assoc_446
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe d_'42''45'isMonoid_3178 (coe v0))))
      (coe
         MAlonzo.Code.Algebra.Structures.d_identity_612
         (coe d_'42''45'isMonoid_3178 (coe v0)))
      (coe d_distrib_3180 (coe v0)) (coe d_zero_3182 (coe v0))
-- Algebra.Structures.Biased.IsRing*._._.identityʳ
d_identity'691'_3196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing'42'_3156 -> AgdaAny -> AgdaAny
d_identity'691'_3196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_3196 v9
du_identity'691'_3196 :: T_IsRing'42'_3156 -> AgdaAny -> AgdaAny
du_identity'691'_3196 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe d_'42''45'isMonoid_3178 (coe v0))
-- Algebra.Structures.Biased.IsRing*._._.identityˡ
d_identity'737'_3198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing'42'_3156 -> AgdaAny -> AgdaAny
d_identity'737'_3198 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_3198 v9
du_identity'737'_3198 :: T_IsRing'42'_3156 -> AgdaAny -> AgdaAny
du_identity'737'_3198 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe d_'42''45'isMonoid_3178 (coe v0))
-- Algebra.Structures.Biased.IsRing*._._.isPartialEquivalence
d_isPartialEquivalence_3204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing'42'_3156 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3204 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_3204 v9
du_isPartialEquivalence_3204 ::
  T_IsRing'42'_3156 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3204 v0
  = let v1 = d_'42''45'isMonoid_3178 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
-- Algebra.Structures.Biased.IsRing*._._.isUnitalMagma
d_isUnitalMagma_3208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing'42'_3156 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_3208 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isUnitalMagma_3208 v9
du_isUnitalMagma_3208 ::
  T_IsRing'42'_3156 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_3208 v0
  = coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe d_'42''45'isMonoid_3178 (coe v0))
-- Algebra.Structures.Biased.IsRing*._._.reflexive
d_reflexive_3212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing'42'_3156 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3212 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_3212 v9
du_reflexive_3212 ::
  T_IsRing'42'_3156 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3212 v0
  = let v1 = d_'42''45'isMonoid_3178 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v3))
        v4
-- Algebra.Structures.Biased.IsRing*._._.setoid
d_setoid_3214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing'42'_3156 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3214 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_3214 v9
du_setoid_3214 ::
  T_IsRing'42'_3156 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3214 v0
  = let v1 = d_'42''45'isMonoid_3178 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Structures.Biased.IsRing*._._.∙-congʳ
d_'8729''45'cong'691'_3222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing'42'_3156 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3222 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_3222 v9
du_'8729''45'cong'691'_3222 ::
  T_IsRing'42'_3156 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3222 v0
  = let v1 = d_'42''45'isMonoid_3178 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
-- Algebra.Structures.Biased.IsRing*._._.∙-congˡ
d_'8729''45'cong'737'_3224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing'42'_3156 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3224 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_3224 v9
du_'8729''45'cong'737'_3224 ::
  T_IsRing'42'_3156 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3224 v0
  = let v1 = d_'42''45'isMonoid_3178 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v2))
