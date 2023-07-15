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

module MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters where

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
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing

-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff
d_RawCoeff_14 a0 a1 = ()
data T_RawCoeff_14
  = C_RawCoeff'46'constructor_73 MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
                                 (AgdaAny -> Bool)
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff.rawRing
d_rawRing_24 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_24 v0
  = case coe v0 of
      C_RawCoeff'46'constructor_73 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff.isZero
d_isZero_26 :: T_RawCoeff_14 -> AgdaAny -> Bool
d_isZero_26 v0
  = case coe v0 of
      C_RawCoeff'46'constructor_73 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._._*_
d__'42'__30 :: T_RawCoeff_14 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__30 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._._+_
d__'43'__32 :: T_RawCoeff_14 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__32 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._._≈_
d__'8776'__34 :: T_RawCoeff_14 -> AgdaAny -> AgdaAny -> ()
d__'8776'__34 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._._≉_
d__'8777'__36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 -> AgdaAny -> AgdaAny -> ()
d__'8777'__36 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.*-rawMagma
d_'42''45'rawMagma_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'42''45'rawMagma_38 ~v0 ~v1 v2 = du_'42''45'rawMagma_38 v2
du_'42''45'rawMagma_38 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'42''45'rawMagma_38 v0
  = let v1 = d_rawRing_24 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMagma_142
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178 (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.*-rawMonoid
d_'42''45'rawMonoid_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'42''45'rawMonoid_40 ~v0 ~v1 v2 = du_'42''45'rawMonoid_40 v2
du_'42''45'rawMonoid_40 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_'42''45'rawMonoid_40 v0
  = let v1 = d_rawRing_24 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.+-rawGroup
d_'43''45'rawGroup_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
d_'43''45'rawGroup_42 ~v0 ~v1 v2 = du_'43''45'rawGroup_42 v2
du_'43''45'rawGroup_42 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
du_'43''45'rawGroup_42 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.rawMagma
d_rawMagma_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_44 ~v0 ~v1 v2 = du_rawMagma_44 v2
du_rawMagma_44 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_44 v0
  = let v1 = d_rawRing_24 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134 (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.+-rawMonoid
d_'43''45'rawMonoid_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'43''45'rawMonoid_46 ~v0 ~v1 v2 = du_'43''45'rawMonoid_46 v2
du_'43''45'rawMonoid_46 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_'43''45'rawMonoid_46 v0
  = let v1 = d_rawRing_24 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178 (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.-_
d_'45'__48 :: T_RawCoeff_14 -> AgdaAny -> AgdaAny
d_'45'__48 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.0#
d_0'35'_50 :: T_RawCoeff_14 -> AgdaAny
d_0'35'_50 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.1#
d_1'35'_52 :: T_RawCoeff_14 -> AgdaAny
d_1'35'_52 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.Carrier
d_Carrier_54 :: T_RawCoeff_14 -> ()
d_Carrier_54 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.RawCoeff._.rawSemiring
d_rawSemiring_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_56 ~v0 ~v1 v2 = du_rawSemiring_56 v2
du_rawSemiring_56 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_56 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism
d_Homomorphism_66 a0 a1 a2 a3 = ()
data T_Homomorphism_66
  = C_Homomorphism'46'constructor_1691 T_RawCoeff_14
                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178
                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__372
                                       (AgdaAny -> AgdaAny -> AgdaAny)
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw._*_
d__'42'__82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__82 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'42'__82 v4
du__'42'__82 :: T_RawCoeff_14 -> AgdaAny -> AgdaAny -> AgdaAny
du__'42'__82 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw._+_
d__'43'__84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43'__84 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'43'__84 v4
du__'43'__84 :: T_RawCoeff_14 -> AgdaAny -> AgdaAny -> AgdaAny
du__'43'__84 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw._≈_
d__'8776'__86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__86 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw._≉_
d__'8777'__88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> ()
d__'8777'__88 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.*-rawMagma
d_'42''45'rawMagma_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'42''45'rawMagma_90 ~v0 ~v1 ~v2 ~v3 v4 ~v5
  = du_'42''45'rawMagma_90 v4
du_'42''45'rawMagma_90 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'42''45'rawMagma_90 v0
  = let v1 = d_rawRing_24 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMagma_142
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178 (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.*-rawMonoid
d_'42''45'rawMonoid_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'42''45'rawMonoid_92 ~v0 ~v1 ~v2 ~v3 v4 ~v5
  = du_'42''45'rawMonoid_92 v4
du_'42''45'rawMonoid_92 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_'42''45'rawMonoid_92 v0
  = let v1 = d_rawRing_24 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.+-rawGroup
d_'43''45'rawGroup_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
d_'43''45'rawGroup_94 ~v0 ~v1 ~v2 ~v3 v4 ~v5
  = du_'43''45'rawGroup_94 v4
du_'43''45'rawGroup_94 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
du_'43''45'rawGroup_94 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.rawMagma
d_rawMagma_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_96 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_rawMagma_96 v4
du_rawMagma_96 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_96 v0
  = let v1 = d_rawRing_24 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134 (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.+-rawMonoid
d_'43''45'rawMonoid_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'43''45'rawMonoid_98 ~v0 ~v1 ~v2 ~v3 v4 ~v5
  = du_'43''45'rawMonoid_98 v4
du_'43''45'rawMonoid_98 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_'43''45'rawMonoid_98 v0
  = let v1 = d_rawRing_24 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178 (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.-_
d_'45'__100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny
d_'45'__100 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_'45'__100 v4
du_'45'__100 :: T_RawCoeff_14 -> AgdaAny -> AgdaAny
du_'45'__100 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.0#
d_0'35'_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny
d_0'35'_102 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_0'35'_102 v4
du_0'35'_102 :: T_RawCoeff_14 -> AgdaAny
du_0'35'_102 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.1#
d_1'35'_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny
d_1'35'_104 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_1'35'_104 v4
du_1'35'_104 :: T_RawCoeff_14 -> AgdaAny
du_1'35'_104 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.Carrier
d_Carrier_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  ()
d_Carrier_106 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.isZero
d_isZero_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> Bool
d_isZero_108 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_isZero_108 v4
du_isZero_108 :: T_RawCoeff_14 -> AgdaAny -> Bool
du_isZero_108 v0 = coe d_isZero_26 (coe v0)
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.rawRing
d_rawRing_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_110 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_rawRing_110 v4
du_rawRing_110 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_110 v0 = coe d_rawRing_24 (coe v0)
-- Tactic.RingSolver.Core.Polynomial.Parameters.Raw.rawSemiring
d_rawSemiring_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawCoeff_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_112 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_rawSemiring_112 v4
du_rawSemiring_112 ::
  T_RawCoeff_14 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_112 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
      (coe d_rawRing_24 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.from
d_from_280 :: T_Homomorphism_66 -> T_RawCoeff_14
d_from_280 v0
  = case coe v0 of
      C_Homomorphism'46'constructor_1691 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.to
d_to_282 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178
d_to_282 v0
  = case coe v0 of
      C_Homomorphism'46'constructor_1691 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw._*_
d__'42'__286 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__286 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268
      (coe d_rawRing_24 (coe d_from_280 (coe v0)))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw._+_
d__'43'__288 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__288 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266
      (coe d_rawRing_24 (coe d_from_280 (coe v0)))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw._≈_
d__'8776'__290 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny -> ()
d__'8776'__290 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw._≉_
d__'8777'__292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> ()
d__'8777'__292 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.*-rawMagma
d_'42''45'rawMagma_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'42''45'rawMagma_294 ~v0 ~v1 ~v2 ~v3 v4
  = du_'42''45'rawMagma_294 v4
du_'42''45'rawMagma_294 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'42''45'rawMagma_294 v0
  = let v1 = d_from_280 (coe v0) in
    let v2 = d_rawRing_24 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMagma_142
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178 (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.*-rawMonoid
d_'42''45'rawMonoid_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'42''45'rawMonoid_296 ~v0 ~v1 ~v2 ~v3 v4
  = du_'42''45'rawMonoid_296 v4
du_'42''45'rawMonoid_296 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_'42''45'rawMonoid_296 v0
  = let v1 = d_from_280 (coe v0) in
    let v2 = d_rawRing_24 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.+-rawGroup
d_'43''45'rawGroup_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
d_'43''45'rawGroup_298 ~v0 ~v1 ~v2 ~v3 v4
  = du_'43''45'rawGroup_298 v4
du_'43''45'rawGroup_298 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
du_'43''45'rawGroup_298 v0
  = let v1 = d_from_280 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
      (coe d_rawRing_24 (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.rawMagma
d_rawMagma_300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_300 ~v0 ~v1 ~v2 ~v3 v4 = du_rawMagma_300 v4
du_rawMagma_300 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_300 v0
  = let v1 = d_from_280 (coe v0) in
    let v2 = d_rawRing_24 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134 (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.+-rawMonoid
d_'43''45'rawMonoid_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'43''45'rawMonoid_302 ~v0 ~v1 ~v2 ~v3 v4
  = du_'43''45'rawMonoid_302 v4
du_'43''45'rawMonoid_302 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_'43''45'rawMonoid_302 v0
  = let v1 = d_from_280 (coe v0) in
    let v2 = d_rawRing_24 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178 (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.-_
d_'45'__304 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_'45'__304 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270
      (coe d_rawRing_24 (coe d_from_280 (coe v0)))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.0#
d_0'35'_306 :: T_Homomorphism_66 -> AgdaAny
d_0'35'_306 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
      (coe d_rawRing_24 (coe d_from_280 (coe v0)))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.1#
d_1'35'_308 :: T_Homomorphism_66 -> AgdaAny
d_1'35'_308 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
      (coe d_rawRing_24 (coe d_from_280 (coe v0)))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.Carrier
d_Carrier_310 :: T_Homomorphism_66 -> ()
d_Carrier_310 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.isZero
d_isZero_312 :: T_Homomorphism_66 -> AgdaAny -> Bool
d_isZero_312 v0 = coe d_isZero_26 (coe d_from_280 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.rawRing
d_rawRing_314 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_314 v0 = coe d_rawRing_24 (coe d_from_280 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Raw.rawSemiring
d_rawSemiring_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_316 ~v0 ~v1 ~v2 ~v3 v4 = du_rawSemiring_316 v4
du_rawSemiring_316 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_316 v0
  = let v1 = d_from_280 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
      (coe d_rawRing_24 (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._._*_
d__'42'__320 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__320 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._._+_
d__'43'__322 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__322 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'43'__208
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._._-_
d__'45'__324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__324 ~v0 ~v1 ~v2 ~v3 v4 = du__'45'__324 v4
du__'45'__324 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__324 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du__'45'__350
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._._^_
d__'94'__326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> Integer -> AgdaAny
d__'94'__326 ~v0 ~v1 ~v2 ~v3 v4 = du__'94'__326 v4
du__'94'__326 :: T_Homomorphism_66 -> AgdaAny -> Integer -> AgdaAny
du__'94'__326 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du__'94'__348
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._._≈_
d__'8776'__328 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny -> ()
d__'8776'__328 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-assoc
d_'42''45'assoc_330 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_330 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_1294
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                  (coe d_to_282 (coe v0))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-comm
d_'42''45'comm_332 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_332 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
         (coe
            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
            (coe d_to_282 (coe v0))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-commutativeMonoid
d_'42''45'commutativeMonoid_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'42''45'commutativeMonoid_334 ~v0 ~v1 ~v2 ~v3 v4
  = du_'42''45'commutativeMonoid_334 v4
du_'42''45'commutativeMonoid_334 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'42''45'commutativeMonoid_334 v0
  = let v1 = d_to_282 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'commutativeMonoid_2324
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
         (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-cong
d_'42''45'cong_336 ::
  T_Homomorphism_66 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_336 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                  (coe d_to_282 (coe v0))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.∙-congʳ
d_'8729''45'cong'691'_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_338 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8729''45'cong'691'_338 v4
du_'8729''45'cong'691'_338 ::
  T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_338 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.∙-congˡ
d_'8729''45'cong'737'_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_340 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8729''45'cong'737'_340 v4
du_'8729''45'cong'737'_340 ::
  T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_340 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
              (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-identity
d_'42''45'identity_342 ::
  T_Homomorphism_66 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_342 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'42''45'identity_1296
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                  (coe d_to_282 (coe v0))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.identityʳ
d_identity'691'_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_identity'691'_344 ~v0 ~v1 ~v2 ~v3 v4 = du_identity'691'_344 v4
du_identity'691'_344 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
du_identity'691'_344 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v5))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.identityˡ
d_identity'737'_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_identity'737'_346 ~v0 ~v1 ~v2 ~v3 v4 = du_identity'737'_346 v4
du_identity'737'_346 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
du_identity'737'_346 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v5))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isCommutativeMagma
d_isCommutativeMagma_348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_348 ~v0 ~v1 ~v2 ~v3 v4
  = du_isCommutativeMagma_348 v4
du_isCommutativeMagma_348 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_348 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_350 ~v0 ~v1 ~v2 ~v3 v4
  = du_'42''45'isCommutativeMonoid_350 v4
du_'42''45'isCommutativeMonoid_350 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_350 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeMonoid_1590
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
         (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_352 ~v0 ~v1 ~v2 ~v3 v4
  = du_'42''45'isCommutativeSemigroup_352 v4
du_'42''45'isCommutativeSemigroup_352 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_352 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isCommutativeSemigroup_1256
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
         (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-isMagma
d_'42''45'isMagma_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'42''45'isMagma_354 ~v0 ~v1 ~v2 ~v3 v4
  = du_'42''45'isMagma_354 v4
du_'42''45'isMagma_354 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'42''45'isMagma_354 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_1348
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-isMonoid
d_'42''45'isMonoid_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'42''45'isMonoid_356 ~v0 ~v1 ~v2 ~v3 v4
  = du_'42''45'isMonoid_356 v4
du_'42''45'isMonoid_356 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'42''45'isMonoid_356 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-isSemigroup
d_'42''45'isSemigroup_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'42''45'isSemigroup_358 ~v0 ~v1 ~v2 ~v3 v4
  = du_'42''45'isSemigroup_358 v4
du_'42''45'isSemigroup_358 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'42''45'isSemigroup_358 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'42''45'isSemigroup_1350
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-monoid
d_'42''45'monoid_360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'42''45'monoid_360 ~v0 ~v1 ~v2 ~v3 v4 = du_'42''45'monoid_360 v4
du_'42''45'monoid_360 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'42''45'monoid_360 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
              (coe v1) in
    let v3
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.semigroup
d_semigroup_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_362 ~v0 ~v1 ~v2 ~v3 v4 = du_semigroup_362 v4
du_semigroup_362 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_362 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
              (coe v1) in
    let v3
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970 (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.assoc
d_assoc_364 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_364 v0
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
                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                           (coe d_to_282 (coe v0)))))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.comm
d_comm_366 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_366 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
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
                     (coe d_to_282 (coe v0)))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.+-commutativeMonoid
d_'43''45'commutativeMonoid_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'43''45'commutativeMonoid_368 ~v0 ~v1 ~v2 ~v3 v4
  = du_'43''45'commutativeMonoid_368 v4
du_'43''45'commutativeMonoid_368 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'43''45'commutativeMonoid_368 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
              (coe v1) in
    let v3
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.∙-cong
d_'8729''45'cong_370 ::
  T_Homomorphism_66 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_370 v0
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
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                              (coe d_to_282 (coe v0))))))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.∙-congʳ
d_'8729''45'cong'691'_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_372 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8729''45'cong'691'_372 v4
du_'8729''45'cong'691'_372 ::
  T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_372 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v6) in
    let v8
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.∙-congˡ
d_'8729''45'cong'737'_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_374 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8729''45'cong'737'_374 v4
du_'8729''45'cong'737'_374 ::
  T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_374 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v6) in
    let v8
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.identity
d_identity_376 ::
  T_Homomorphism_66 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_376 v0
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
                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                        (coe d_to_282 (coe v0))))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.identityʳ
d_identity'691'_378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_identity'691'_378 ~v0 ~v1 ~v2 ~v3 v4 = du_identity'691'_378 v4
du_identity'691'_378 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
du_identity'691'_378 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v6))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.identityˡ
d_identity'737'_380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_identity'737'_380 ~v0 ~v1 ~v2 ~v3 v4 = du_identity'737'_380 v4
du_identity'737'_380 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
du_identity'737'_380 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v6))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isCommutativeMagma
d_isCommutativeMagma_382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_382 ~v0 ~v1 ~v2 ~v3 v4
  = du_isCommutativeMagma_382 v4
du_isCommutativeMagma_382 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_382 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v6))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_384 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_384 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                  (coe d_to_282 (coe v0))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isCommutativeSemigroup
d_isCommutativeSemigroup_386 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_386 ~v0 ~v1 ~v2 ~v3 v4
  = du_isCommutativeSemigroup_386 v4
du_isCommutativeSemigroup_386 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_386 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
         (coe v5))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isMagma
d_isMagma_388 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_388 v0
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
                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                           (coe d_to_282 (coe v0)))))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isMonoid
d_isMonoid_390 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_390 v0
  = coe
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
                     (coe d_to_282 (coe v0)))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isSemigroup
d_isSemigroup_392 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_392 v0
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
                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                        (coe d_to_282 (coe v0))))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isUnitalMagma
d_isUnitalMagma_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_394 ~v0 ~v1 ~v2 ~v3 v4 = du_isUnitalMagma_394 v4
du_isUnitalMagma_394 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_394 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v5) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v6))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.monoid
d_monoid_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_monoid_396 ~v0 ~v1 ~v2 ~v3 v4 = du_monoid_396 v4
du_monoid_396 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_monoid_396 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
              (coe v1) in
    let v3
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_monoid_890
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.semigroup
d_semigroup_398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_398 ~v0 ~v1 ~v2 ~v3 v4 = du_semigroup_398 v4
du_semigroup_398 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_398 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
              (coe v1) in
    let v3
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v2) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
              (coe v3) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.du_'43''45'commutativeMonoid_1948
              (coe v4) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_802
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v5))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.-_
d_'45'__400 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_'45'__400 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.-‿*-distribˡ
d_'45''8255''42''45'distrib'737'_402 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''42''45'distrib'737'_402 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255''42''45'distrib'737'_70
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
         (coe d_to_282 (coe v0)))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.-‿+-comm
d_'45''8255''43''45'comm_404 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255''43''45'comm_404 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255''43''45'comm_76
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
         (coe d_to_282 (coe v0)))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.-‿cong
d_'45''8255'cong_406 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'45''8255'cong_406 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255'cong_64
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
         (coe d_to_282 (coe v0)))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.0#
d_0'35'_408 :: T_Homomorphism_66 -> AgdaAny
d_0'35'_408 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.0≟_
d_0'8799'__410 :: T_Homomorphism_66 -> AgdaAny -> Maybe AgdaAny
d_0'8799'__410 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'8799'__218
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.1#
d_1'35'_412 :: T_Homomorphism_66 -> AgdaAny
d_1'35'_412 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'35'_220
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.Carrier
d_Carrier_414 :: T_Homomorphism_66 -> ()
d_Carrier_414 = erased
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.commutativeSemiring
d_commutativeSemiring_416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_commutativeSemiring_416 ~v0 ~v1 ~v2 ~v3 v4
  = du_commutativeSemiring_416 v4
du_commutativeSemiring_416 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_commutativeSemiring_416 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.distrib
d_distrib_418 ::
  T_Homomorphism_66 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_418 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_distrib_1298
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                  (coe d_to_282 (coe v0))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.distribʳ
d_distrib'691'_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_420 ~v0 ~v1 ~v2 ~v3 v4 = du_distrib'691'_420 v4
du_distrib'691'_420 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_420 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'691'_1302
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.distribˡ
d_distrib'737'_422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_422 ~v0 ~v1 ~v2 ~v3 v4 = du_distrib'737'_422 v4
du_distrib'737'_422 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_422 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_distrib'737'_1300
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isAlmostCommutativeRing
d_isAlmostCommutativeRing_424 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_IsAlmostCommutativeRing_26
d_isAlmostCommutativeRing_424 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isCommutativeSemiring
d_isCommutativeSemiring_426 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_426 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
         (coe d_to_282 (coe v0)))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_428 ~v0 ~v1 ~v2 ~v3 v4
  = du_isCommutativeSemiringWithoutOne_428 v4
du_isCommutativeSemiringWithoutOne_428 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_428 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemiringWithoutOne_1582
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
         (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isEquivalence
d_isEquivalence_430 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_430 v0
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
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                              (coe d_to_282 (coe v0))))))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isNearSemiring
d_isNearSemiring_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
d_isNearSemiring_432 ~v0 ~v1 ~v2 ~v3 v4 = du_isNearSemiring_432 v4
du_isNearSemiring_432 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsNearSemiring_1062
du_isNearSemiring_432 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isNearSemiring_1196
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isPartialEquivalence
d_isPartialEquivalence_434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_434 ~v0 ~v1 ~v2 ~v3 v4
  = du_isPartialEquivalence_434 v4
du_isPartialEquivalence_434 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_434 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v6) in
    let v8
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
    let v9 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v9))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isSemiring
d_isSemiring_436 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_isSemiring_436 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
         (coe
            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
            (coe d_to_282 (coe v0))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_438 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_438 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
               (coe d_to_282 (coe v0)))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.isSemiringWithoutOne
d_isSemiringWithoutOne_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_440 ~v0 ~v1 ~v2 ~v3 v4
  = du_isSemiringWithoutOne_440 v4
du_isSemiringWithoutOne_440 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_440 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
      (coe MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.rawRing
d_rawRing_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_rawRing_442 ~v0 ~v1 ~v2 ~v3 v4 = du_rawRing_442 v4
du_rawRing_442 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
du_rawRing_442 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.rawSemiring
d_rawSemiring_444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_rawSemiring_444 ~v0 ~v1 ~v2 ~v3 v4 = du_rawSemiring_444 v4
du_rawSemiring_444 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
du_rawSemiring_444 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
              (coe v1) in
    let v3
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
      (coe
         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
         (coe v3))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.refl
d_refl_446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_refl_446 ~v0 ~v1 ~v2 ~v3 v4 = du_refl_446 v4
du_refl_446 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
du_refl_446 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
      (coe d_to_282 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.reflexive
d_reflexive_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_448 ~v0 ~v1 ~v2 ~v3 v4 = du_reflexive_448 v4
du_reflexive_448 ::
  T_Homomorphism_66 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_448 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v6) in
    let v8
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
    let v9 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8) in
    \ v10 v11 v12 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v9))
        v10
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.semiring
d_semiring_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_semiring_450 ~v0 ~v1 ~v2 ~v3 v4 = du_semiring_450 v4
du_semiring_450 ::
  T_Homomorphism_66 -> MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
du_semiring_450 v0
  = let v1 = d_to_282 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
         (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.setoid
d_setoid_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_452 ~v0 ~v1 ~v2 ~v3 v4 = du_setoid_452 v4
du_setoid_452 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_452 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
              (coe v4) in
    let v6
          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
              (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v6) in
    let v8
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v7) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v8))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.sym
d_sym_454 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_454 v0
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
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                 (coe d_to_282 (coe v0)))))))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.trans
d_trans_456 ::
  T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_456 v0
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
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                 (coe d_to_282 (coe v0)))))))))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.zero
d_zero_458 ::
  T_Homomorphism_66 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_458 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_zero_1388
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
         (coe
            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
               (coe d_to_282 (coe v0)))))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.zeroʳ
d_zero'691'_460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_zero'691'_460 ~v0 ~v1 ~v2 ~v3 v4 = du_zero'691'_460 v4
du_zero'691'_460 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
du_zero'691'_460 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.zeroˡ
d_zero'737'_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_zero'737'_462 ~v0 ~v1 ~v2 ~v3 v4 = du_zero'737'_462 v4
du_zero'737'_462 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
du_zero'737'_462 v0
  = let v1 = d_to_282 (coe v0) in
    let v2
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
              (coe v1) in
    let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
              (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
      (coe
         MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
         (coe v4))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.morphism
d_morphism_464 ::
  T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__372
d_morphism_464 v0
  = case coe v0 of
      C_Homomorphism'46'constructor_1691 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.*-homo
d_'42''45'homo_468 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_468 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'42''45'homo_774
      (coe d_morphism_464 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.+-homo
d_'43''45'homo_470 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d_'43''45'homo_470 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'43''45'homo_772
      (coe d_morphism_464 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.-‿homo
d_'45''8255'homo_472 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_'45''8255'homo_472 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255'homo_776
      (coe d_morphism_464 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.0-homo
d_0'45'homo_474 :: T_Homomorphism_66 -> AgdaAny
d_0'45'homo_474 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'45'homo_778
      (coe d_morphism_464 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.1-homo
d_1'45'homo_476 :: T_Homomorphism_66 -> AgdaAny
d_1'45'homo_476 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'45'homo_780
      (coe d_morphism_464 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism._.⟦_⟧
d_'10214'_'10215'_478 :: T_Homomorphism_66 -> AgdaAny -> AgdaAny
d_'10214'_'10215'_478 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
      (coe d_morphism_464 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Parameters.Homomorphism.Zero-C⟶Zero-R
d_Zero'45'C'10230'Zero'45'R_482 ::
  T_Homomorphism_66 -> AgdaAny -> AgdaAny -> AgdaAny
d_Zero'45'C'10230'Zero'45'R_482 v0
  = case coe v0 of
      C_Homomorphism'46'constructor_1691 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
