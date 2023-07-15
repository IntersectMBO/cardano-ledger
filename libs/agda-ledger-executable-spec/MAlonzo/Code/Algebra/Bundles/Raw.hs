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

module MAlonzo.Code.Algebra.Bundles.Raw where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

-- Algebra.Bundles.Raw.RawMagma
d_RawMagma_10 a0 a1 = ()
newtype T_RawMagma_10
  = C_RawMagma'46'constructor_77 (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Bundles.Raw.RawMagma.Carrier
d_Carrier_22 :: T_RawMagma_10 -> ()
d_Carrier_22 = erased
-- Algebra.Bundles.Raw.RawMagma._≈_
d__'8776'__24 :: T_RawMagma_10 -> AgdaAny -> AgdaAny -> ()
d__'8776'__24 = erased
-- Algebra.Bundles.Raw.RawMagma._∙_
d__'8729'__26 :: T_RawMagma_10 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__26 v0
  = case coe v0 of
      C_RawMagma'46'constructor_77 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawMagma._≉_
d__'8777'__28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawMagma_10 -> AgdaAny -> AgdaAny -> ()
d__'8777'__28 = erased
-- Algebra.Bundles.Raw.RawMonoid
d_RawMonoid_38 a0 a1 = ()
data T_RawMonoid_38
  = C_RawMonoid'46'constructor_473 (AgdaAny -> AgdaAny -> AgdaAny)
                                   AgdaAny
-- Algebra.Bundles.Raw.RawMonoid.Carrier
d_Carrier_52 :: T_RawMonoid_38 -> ()
d_Carrier_52 = erased
-- Algebra.Bundles.Raw.RawMonoid._≈_
d__'8776'__54 :: T_RawMonoid_38 -> AgdaAny -> AgdaAny -> ()
d__'8776'__54 = erased
-- Algebra.Bundles.Raw.RawMonoid._∙_
d__'8729'__56 :: T_RawMonoid_38 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__56 v0
  = case coe v0 of
      C_RawMonoid'46'constructor_473 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawMonoid.ε
d_ε_58 :: T_RawMonoid_38 -> AgdaAny
d_ε_58 v0
  = case coe v0 of
      C_RawMonoid'46'constructor_473 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawMonoid.rawMagma
d_rawMagma_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawMonoid_38 -> T_RawMagma_10
d_rawMagma_60 ~v0 ~v1 v2 = du_rawMagma_60 v2
du_rawMagma_60 :: T_RawMonoid_38 -> T_RawMagma_10
du_rawMagma_60 v0
  = coe C_RawMagma'46'constructor_77 (d__'8729'__56 (coe v0))
-- Algebra.Bundles.Raw.RawMonoid._._≉_
d__'8777'__64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawMonoid_38 -> AgdaAny -> AgdaAny -> ()
d__'8777'__64 = erased
-- Algebra.Bundles.Raw.RawGroup
d_RawGroup_70 a0 a1 = ()
data T_RawGroup_70
  = C_RawGroup'46'constructor_921 (AgdaAny -> AgdaAny -> AgdaAny)
                                  AgdaAny (AgdaAny -> AgdaAny)
-- Algebra.Bundles.Raw.RawGroup.Carrier
d_Carrier_86 :: T_RawGroup_70 -> ()
d_Carrier_86 = erased
-- Algebra.Bundles.Raw.RawGroup._≈_
d__'8776'__88 :: T_RawGroup_70 -> AgdaAny -> AgdaAny -> ()
d__'8776'__88 = erased
-- Algebra.Bundles.Raw.RawGroup._∙_
d__'8729'__90 :: T_RawGroup_70 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__90 v0
  = case coe v0 of
      C_RawGroup'46'constructor_921 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawGroup.ε
d_ε_92 :: T_RawGroup_70 -> AgdaAny
d_ε_92 v0
  = case coe v0 of
      C_RawGroup'46'constructor_921 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawGroup._⁻¹
d__'8315''185'_94 :: T_RawGroup_70 -> AgdaAny -> AgdaAny
d__'8315''185'_94 v0
  = case coe v0 of
      C_RawGroup'46'constructor_921 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawGroup.rawMonoid
d_rawMonoid_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawGroup_70 -> T_RawMonoid_38
d_rawMonoid_96 ~v0 ~v1 v2 = du_rawMonoid_96 v2
du_rawMonoid_96 :: T_RawGroup_70 -> T_RawMonoid_38
du_rawMonoid_96 v0
  = coe
      C_RawMonoid'46'constructor_473 (d__'8729'__90 (coe v0))
      (d_ε_92 (coe v0))
-- Algebra.Bundles.Raw.RawGroup._._≉_
d__'8777'__100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawGroup_70 -> AgdaAny -> AgdaAny -> ()
d__'8777'__100 = erased
-- Algebra.Bundles.Raw.RawGroup._.rawMagma
d_rawMagma_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawGroup_70 -> T_RawMagma_10
d_rawMagma_102 ~v0 ~v1 v2 = du_rawMagma_102 v2
du_rawMagma_102 :: T_RawGroup_70 -> T_RawMagma_10
du_rawMagma_102 v0
  = coe du_rawMagma_60 (coe du_rawMonoid_96 (coe v0))
-- Algebra.Bundles.Raw.RawNearSemiring
d_RawNearSemiring_108 a0 a1 = ()
data T_RawNearSemiring_108
  = C_RawNearSemiring'46'constructor_1421 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
-- Algebra.Bundles.Raw.RawNearSemiring.Carrier
d_Carrier_124 :: T_RawNearSemiring_108 -> ()
d_Carrier_124 = erased
-- Algebra.Bundles.Raw.RawNearSemiring._≈_
d__'8776'__126 :: T_RawNearSemiring_108 -> AgdaAny -> AgdaAny -> ()
d__'8776'__126 = erased
-- Algebra.Bundles.Raw.RawNearSemiring._+_
d__'43'__128 ::
  T_RawNearSemiring_108 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__128 v0
  = case coe v0 of
      C_RawNearSemiring'46'constructor_1421 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawNearSemiring._*_
d__'42'__130 ::
  T_RawNearSemiring_108 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__130 v0
  = case coe v0 of
      C_RawNearSemiring'46'constructor_1421 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawNearSemiring.0#
d_0'35'_132 :: T_RawNearSemiring_108 -> AgdaAny
d_0'35'_132 v0
  = case coe v0 of
      C_RawNearSemiring'46'constructor_1421 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawNearSemiring.+-rawMonoid
d_'43''45'rawMonoid_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawNearSemiring_108 -> T_RawMonoid_38
d_'43''45'rawMonoid_134 ~v0 ~v1 v2 = du_'43''45'rawMonoid_134 v2
du_'43''45'rawMonoid_134 :: T_RawNearSemiring_108 -> T_RawMonoid_38
du_'43''45'rawMonoid_134 v0
  = coe
      C_RawMonoid'46'constructor_473 (d__'43'__128 (coe v0))
      (d_0'35'_132 (coe v0))
-- Algebra.Bundles.Raw.RawNearSemiring._._≉_
d__'8777'__138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawNearSemiring_108 -> AgdaAny -> AgdaAny -> ()
d__'8777'__138 = erased
-- Algebra.Bundles.Raw.RawNearSemiring._.rawMagma
d_rawMagma_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawNearSemiring_108 -> T_RawMagma_10
d_rawMagma_140 ~v0 ~v1 v2 = du_rawMagma_140 v2
du_rawMagma_140 :: T_RawNearSemiring_108 -> T_RawMagma_10
du_rawMagma_140 v0
  = coe du_rawMagma_60 (coe du_'43''45'rawMonoid_134 (coe v0))
-- Algebra.Bundles.Raw.RawNearSemiring.*-rawMagma
d_'42''45'rawMagma_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawNearSemiring_108 -> T_RawMagma_10
d_'42''45'rawMagma_142 ~v0 ~v1 v2 = du_'42''45'rawMagma_142 v2
du_'42''45'rawMagma_142 :: T_RawNearSemiring_108 -> T_RawMagma_10
du_'42''45'rawMagma_142 v0
  = coe C_RawMagma'46'constructor_77 (d__'42'__130 (coe v0))
-- Algebra.Bundles.Raw.RawSemiring
d_RawSemiring_148 a0 a1 = ()
data T_RawSemiring_148
  = C_RawSemiring'46'constructor_2023 (AgdaAny -> AgdaAny -> AgdaAny)
                                      (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
-- Algebra.Bundles.Raw.RawSemiring.Carrier
d_Carrier_166 :: T_RawSemiring_148 -> ()
d_Carrier_166 = erased
-- Algebra.Bundles.Raw.RawSemiring._≈_
d__'8776'__168 :: T_RawSemiring_148 -> AgdaAny -> AgdaAny -> ()
d__'8776'__168 = erased
-- Algebra.Bundles.Raw.RawSemiring._+_
d__'43'__170 :: T_RawSemiring_148 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__170 v0
  = case coe v0 of
      C_RawSemiring'46'constructor_2023 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawSemiring._*_
d__'42'__172 :: T_RawSemiring_148 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__172 v0
  = case coe v0 of
      C_RawSemiring'46'constructor_2023 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawSemiring.0#
d_0'35'_174 :: T_RawSemiring_148 -> AgdaAny
d_0'35'_174 v0
  = case coe v0 of
      C_RawSemiring'46'constructor_2023 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawSemiring.1#
d_1'35'_176 :: T_RawSemiring_148 -> AgdaAny
d_1'35'_176 v0
  = case coe v0 of
      C_RawSemiring'46'constructor_2023 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawSemiring.rawNearSemiring
d_rawNearSemiring_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawSemiring_148 -> T_RawNearSemiring_108
d_rawNearSemiring_178 ~v0 ~v1 v2 = du_rawNearSemiring_178 v2
du_rawNearSemiring_178 ::
  T_RawSemiring_148 -> T_RawNearSemiring_108
du_rawNearSemiring_178 v0
  = coe
      C_RawNearSemiring'46'constructor_1421 (d__'43'__170 (coe v0))
      (d__'42'__172 (coe v0)) (d_0'35'_174 (coe v0))
-- Algebra.Bundles.Raw.RawSemiring._._≉_
d__'8777'__182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawSemiring_148 -> AgdaAny -> AgdaAny -> ()
d__'8777'__182 = erased
-- Algebra.Bundles.Raw.RawSemiring._.*-rawMagma
d_'42''45'rawMagma_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawSemiring_148 -> T_RawMagma_10
d_'42''45'rawMagma_184 ~v0 ~v1 v2 = du_'42''45'rawMagma_184 v2
du_'42''45'rawMagma_184 :: T_RawSemiring_148 -> T_RawMagma_10
du_'42''45'rawMagma_184 v0
  = coe du_'42''45'rawMagma_142 (coe du_rawNearSemiring_178 (coe v0))
-- Algebra.Bundles.Raw.RawSemiring._.rawMagma
d_rawMagma_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawSemiring_148 -> T_RawMagma_10
d_rawMagma_186 ~v0 ~v1 v2 = du_rawMagma_186 v2
du_rawMagma_186 :: T_RawSemiring_148 -> T_RawMagma_10
du_rawMagma_186 v0
  = let v1 = coe du_rawNearSemiring_178 (coe v0) in
    coe du_rawMagma_60 (coe du_'43''45'rawMonoid_134 (coe v1))
-- Algebra.Bundles.Raw.RawSemiring._.+-rawMonoid
d_'43''45'rawMonoid_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawSemiring_148 -> T_RawMonoid_38
d_'43''45'rawMonoid_188 ~v0 ~v1 v2 = du_'43''45'rawMonoid_188 v2
du_'43''45'rawMonoid_188 :: T_RawSemiring_148 -> T_RawMonoid_38
du_'43''45'rawMonoid_188 v0
  = coe
      du_'43''45'rawMonoid_134 (coe du_rawNearSemiring_178 (coe v0))
-- Algebra.Bundles.Raw.RawSemiring.*-rawMonoid
d_'42''45'rawMonoid_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawSemiring_148 -> T_RawMonoid_38
d_'42''45'rawMonoid_190 ~v0 ~v1 v2 = du_'42''45'rawMonoid_190 v2
du_'42''45'rawMonoid_190 :: T_RawSemiring_148 -> T_RawMonoid_38
du_'42''45'rawMonoid_190 v0
  = coe
      C_RawMonoid'46'constructor_473 (d__'42'__172 (coe v0))
      (d_1'35'_176 (coe v0))
-- Algebra.Bundles.Raw.RawRingWithoutOne
d_RawRingWithoutOne_196 a0 a1 = ()
data T_RawRingWithoutOne_196
  = C_RawRingWithoutOne'46'constructor_2743 (AgdaAny ->
                                             AgdaAny -> AgdaAny)
                                            (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                            AgdaAny
-- Algebra.Bundles.Raw.RawRingWithoutOne.Carrier
d_Carrier_214 :: T_RawRingWithoutOne_196 -> ()
d_Carrier_214 = erased
-- Algebra.Bundles.Raw.RawRingWithoutOne._≈_
d__'8776'__216 ::
  T_RawRingWithoutOne_196 -> AgdaAny -> AgdaAny -> ()
d__'8776'__216 = erased
-- Algebra.Bundles.Raw.RawRingWithoutOne._+_
d__'43'__218 ::
  T_RawRingWithoutOne_196 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__218 v0
  = case coe v0 of
      C_RawRingWithoutOne'46'constructor_2743 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawRingWithoutOne._*_
d__'42'__220 ::
  T_RawRingWithoutOne_196 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__220 v0
  = case coe v0 of
      C_RawRingWithoutOne'46'constructor_2743 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawRingWithoutOne.-_
d_'45'__222 :: T_RawRingWithoutOne_196 -> AgdaAny -> AgdaAny
d_'45'__222 v0
  = case coe v0 of
      C_RawRingWithoutOne'46'constructor_2743 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawRingWithoutOne.0#
d_0'35'_224 :: T_RawRingWithoutOne_196 -> AgdaAny
d_0'35'_224 v0
  = case coe v0 of
      C_RawRingWithoutOne'46'constructor_2743 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawRingWithoutOne.+-rawGroup
d_'43''45'rawGroup_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRingWithoutOne_196 -> T_RawGroup_70
d_'43''45'rawGroup_226 ~v0 ~v1 v2 = du_'43''45'rawGroup_226 v2
du_'43''45'rawGroup_226 :: T_RawRingWithoutOne_196 -> T_RawGroup_70
du_'43''45'rawGroup_226 v0
  = coe
      C_RawGroup'46'constructor_921 (d__'43'__218 (coe v0))
      (d_0'35'_224 (coe v0)) (d_'45'__222 (coe v0))
-- Algebra.Bundles.Raw.RawRingWithoutOne._._≉_
d__'8777'__230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRingWithoutOne_196 -> AgdaAny -> AgdaAny -> ()
d__'8777'__230 = erased
-- Algebra.Bundles.Raw.RawRingWithoutOne._.rawMagma
d_rawMagma_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRingWithoutOne_196 -> T_RawMagma_10
d_rawMagma_232 ~v0 ~v1 v2 = du_rawMagma_232 v2
du_rawMagma_232 :: T_RawRingWithoutOne_196 -> T_RawMagma_10
du_rawMagma_232 v0
  = let v1 = coe du_'43''45'rawGroup_226 (coe v0) in
    coe du_rawMagma_60 (coe du_rawMonoid_96 (coe v1))
-- Algebra.Bundles.Raw.RawRingWithoutOne._.rawMonoid
d_rawMonoid_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRingWithoutOne_196 -> T_RawMonoid_38
d_rawMonoid_234 ~v0 ~v1 v2 = du_rawMonoid_234 v2
du_rawMonoid_234 :: T_RawRingWithoutOne_196 -> T_RawMonoid_38
du_rawMonoid_234 v0
  = coe du_rawMonoid_96 (coe du_'43''45'rawGroup_226 (coe v0))
-- Algebra.Bundles.Raw.RawRingWithoutOne.*-rawMagma
d_'42''45'rawMagma_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRingWithoutOne_196 -> T_RawMagma_10
d_'42''45'rawMagma_236 ~v0 ~v1 v2 = du_'42''45'rawMagma_236 v2
du_'42''45'rawMagma_236 :: T_RawRingWithoutOne_196 -> T_RawMagma_10
du_'42''45'rawMagma_236 v0
  = coe C_RawMagma'46'constructor_77 (d__'42'__220 (coe v0))
-- Algebra.Bundles.Raw.RawRing
d_RawRing_242 a0 a1 = ()
data T_RawRing_242
  = C_RawRing'46'constructor_3463 (AgdaAny -> AgdaAny -> AgdaAny)
                                  (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny) AgdaAny
                                  AgdaAny
-- Algebra.Bundles.Raw.RawRing.Carrier
d_Carrier_262 :: T_RawRing_242 -> ()
d_Carrier_262 = erased
-- Algebra.Bundles.Raw.RawRing._≈_
d__'8776'__264 :: T_RawRing_242 -> AgdaAny -> AgdaAny -> ()
d__'8776'__264 = erased
-- Algebra.Bundles.Raw.RawRing._+_
d__'43'__266 :: T_RawRing_242 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43'__266 v0
  = case coe v0 of
      C_RawRing'46'constructor_3463 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawRing._*_
d__'42'__268 :: T_RawRing_242 -> AgdaAny -> AgdaAny -> AgdaAny
d__'42'__268 v0
  = case coe v0 of
      C_RawRing'46'constructor_3463 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawRing.-_
d_'45'__270 :: T_RawRing_242 -> AgdaAny -> AgdaAny
d_'45'__270 v0
  = case coe v0 of
      C_RawRing'46'constructor_3463 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawRing.0#
d_0'35'_272 :: T_RawRing_242 -> AgdaAny
d_0'35'_272 v0
  = case coe v0 of
      C_RawRing'46'constructor_3463 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawRing.1#
d_1'35'_274 :: T_RawRing_242 -> AgdaAny
d_1'35'_274 v0
  = case coe v0 of
      C_RawRing'46'constructor_3463 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawRing.rawSemiring
d_rawSemiring_276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRing_242 -> T_RawSemiring_148
d_rawSemiring_276 ~v0 ~v1 v2 = du_rawSemiring_276 v2
du_rawSemiring_276 :: T_RawRing_242 -> T_RawSemiring_148
du_rawSemiring_276 v0
  = coe
      C_RawSemiring'46'constructor_2023 (d__'43'__266 (coe v0))
      (d__'42'__268 (coe v0)) (d_0'35'_272 (coe v0))
      (d_1'35'_274 (coe v0))
-- Algebra.Bundles.Raw.RawRing._._≉_
d__'8777'__280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRing_242 -> AgdaAny -> AgdaAny -> ()
d__'8777'__280 = erased
-- Algebra.Bundles.Raw.RawRing._.*-rawMagma
d_'42''45'rawMagma_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRing_242 -> T_RawMagma_10
d_'42''45'rawMagma_282 ~v0 ~v1 v2 = du_'42''45'rawMagma_282 v2
du_'42''45'rawMagma_282 :: T_RawRing_242 -> T_RawMagma_10
du_'42''45'rawMagma_282 v0
  = let v1 = coe du_rawSemiring_276 (coe v0) in
    coe du_'42''45'rawMagma_142 (coe du_rawNearSemiring_178 (coe v1))
-- Algebra.Bundles.Raw.RawRing._.*-rawMonoid
d_'42''45'rawMonoid_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRing_242 -> T_RawMonoid_38
d_'42''45'rawMonoid_284 ~v0 ~v1 v2 = du_'42''45'rawMonoid_284 v2
du_'42''45'rawMonoid_284 :: T_RawRing_242 -> T_RawMonoid_38
du_'42''45'rawMonoid_284 v0
  = coe du_'42''45'rawMonoid_190 (coe du_rawSemiring_276 (coe v0))
-- Algebra.Bundles.Raw.RawRing._.rawMagma
d_rawMagma_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRing_242 -> T_RawMagma_10
d_rawMagma_286 ~v0 ~v1 v2 = du_rawMagma_286 v2
du_rawMagma_286 :: T_RawRing_242 -> T_RawMagma_10
du_rawMagma_286 v0
  = let v1 = coe du_rawSemiring_276 (coe v0) in
    let v2 = coe du_rawNearSemiring_178 (coe v1) in
    coe du_rawMagma_60 (coe du_'43''45'rawMonoid_134 (coe v2))
-- Algebra.Bundles.Raw.RawRing._.+-rawMonoid
d_'43''45'rawMonoid_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRing_242 -> T_RawMonoid_38
d_'43''45'rawMonoid_288 ~v0 ~v1 v2 = du_'43''45'rawMonoid_288 v2
du_'43''45'rawMonoid_288 :: T_RawRing_242 -> T_RawMonoid_38
du_'43''45'rawMonoid_288 v0
  = let v1 = coe du_rawSemiring_276 (coe v0) in
    coe du_'43''45'rawMonoid_134 (coe du_rawNearSemiring_178 (coe v1))
-- Algebra.Bundles.Raw.RawRing.+-rawGroup
d_'43''45'rawGroup_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawRing_242 -> T_RawGroup_70
d_'43''45'rawGroup_290 ~v0 ~v1 v2 = du_'43''45'rawGroup_290 v2
du_'43''45'rawGroup_290 :: T_RawRing_242 -> T_RawGroup_70
du_'43''45'rawGroup_290 v0
  = coe
      C_RawGroup'46'constructor_921 (d__'43'__266 (coe v0))
      (d_0'35'_272 (coe v0)) (d_'45'__270 (coe v0))
-- Algebra.Bundles.Raw.RawQuasigroup
d_RawQuasigroup_296 a0 a1 = ()
data T_RawQuasigroup_296
  = C_RawQuasigroup'46'constructor_4237 (AgdaAny ->
                                         AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Bundles.Raw.RawQuasigroup.Carrier
d_Carrier_312 :: T_RawQuasigroup_296 -> ()
d_Carrier_312 = erased
-- Algebra.Bundles.Raw.RawQuasigroup._≈_
d__'8776'__314 :: T_RawQuasigroup_296 -> AgdaAny -> AgdaAny -> ()
d__'8776'__314 = erased
-- Algebra.Bundles.Raw.RawQuasigroup._∙_
d__'8729'__316 ::
  T_RawQuasigroup_296 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__316 v0
  = case coe v0 of
      C_RawQuasigroup'46'constructor_4237 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawQuasigroup._\\_
d__'92''92'__318 ::
  T_RawQuasigroup_296 -> AgdaAny -> AgdaAny -> AgdaAny
d__'92''92'__318 v0
  = case coe v0 of
      C_RawQuasigroup'46'constructor_4237 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawQuasigroup._//_
d__'47''47'__320 ::
  T_RawQuasigroup_296 -> AgdaAny -> AgdaAny -> AgdaAny
d__'47''47'__320 v0
  = case coe v0 of
      C_RawQuasigroup'46'constructor_4237 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawQuasigroup.∙-rawMagma
d_'8729''45'rawMagma_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawQuasigroup_296 -> T_RawMagma_10
d_'8729''45'rawMagma_322 ~v0 ~v1 v2 = du_'8729''45'rawMagma_322 v2
du_'8729''45'rawMagma_322 :: T_RawQuasigroup_296 -> T_RawMagma_10
du_'8729''45'rawMagma_322 v0
  = coe C_RawMagma'46'constructor_77 (d__'8729'__316 (coe v0))
-- Algebra.Bundles.Raw.RawQuasigroup.\\-rawMagma
d_'92''92''45'rawMagma_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawQuasigroup_296 -> T_RawMagma_10
d_'92''92''45'rawMagma_324 ~v0 ~v1 v2
  = du_'92''92''45'rawMagma_324 v2
du_'92''92''45'rawMagma_324 :: T_RawQuasigroup_296 -> T_RawMagma_10
du_'92''92''45'rawMagma_324 v0
  = coe C_RawMagma'46'constructor_77 (d__'92''92'__318 (coe v0))
-- Algebra.Bundles.Raw.RawQuasigroup.//-rawMagma
d_'47''47''45'rawMagma_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawQuasigroup_296 -> T_RawMagma_10
d_'47''47''45'rawMagma_326 ~v0 ~v1 v2
  = du_'47''47''45'rawMagma_326 v2
du_'47''47''45'rawMagma_326 :: T_RawQuasigroup_296 -> T_RawMagma_10
du_'47''47''45'rawMagma_326 v0
  = coe C_RawMagma'46'constructor_77 (d__'47''47'__320 (coe v0))
-- Algebra.Bundles.Raw.RawQuasigroup._._≉_
d__'8777'__330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawQuasigroup_296 -> AgdaAny -> AgdaAny -> ()
d__'8777'__330 = erased
-- Algebra.Bundles.Raw.RawLoop
d_RawLoop_336 a0 a1 = ()
data T_RawLoop_336
  = C_RawLoop'46'constructor_4949 (AgdaAny -> AgdaAny -> AgdaAny)
                                  (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny -> AgdaAny)
                                  AgdaAny
-- Algebra.Bundles.Raw.RawLoop.Carrier
d_Carrier_354 :: T_RawLoop_336 -> ()
d_Carrier_354 = erased
-- Algebra.Bundles.Raw.RawLoop._≈_
d__'8776'__356 :: T_RawLoop_336 -> AgdaAny -> AgdaAny -> ()
d__'8776'__356 = erased
-- Algebra.Bundles.Raw.RawLoop._∙_
d__'8729'__358 :: T_RawLoop_336 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__358 v0
  = case coe v0 of
      C_RawLoop'46'constructor_4949 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawLoop._\\_
d__'92''92'__360 :: T_RawLoop_336 -> AgdaAny -> AgdaAny -> AgdaAny
d__'92''92'__360 v0
  = case coe v0 of
      C_RawLoop'46'constructor_4949 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawLoop._//_
d__'47''47'__362 :: T_RawLoop_336 -> AgdaAny -> AgdaAny -> AgdaAny
d__'47''47'__362 v0
  = case coe v0 of
      C_RawLoop'46'constructor_4949 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawLoop.ε
d_ε_364 :: T_RawLoop_336 -> AgdaAny
d_ε_364 v0
  = case coe v0 of
      C_RawLoop'46'constructor_4949 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Bundles.Raw.RawLoop.rawQuasigroup
d_rawQuasigroup_366 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawLoop_336 -> T_RawQuasigroup_296
d_rawQuasigroup_366 ~v0 ~v1 v2 = du_rawQuasigroup_366 v2
du_rawQuasigroup_366 :: T_RawLoop_336 -> T_RawQuasigroup_296
du_rawQuasigroup_366 v0
  = coe
      C_RawQuasigroup'46'constructor_4237 (d__'8729'__358 (coe v0))
      (d__'92''92'__360 (coe v0)) (d__'47''47'__362 (coe v0))
-- Algebra.Bundles.Raw.RawLoop._._≉_
d__'8777'__370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawLoop_336 -> AgdaAny -> AgdaAny -> ()
d__'8777'__370 = erased
-- Algebra.Bundles.Raw.RawLoop._.//-rawMagma
d_'47''47''45'rawMagma_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawLoop_336 -> T_RawMagma_10
d_'47''47''45'rawMagma_372 ~v0 ~v1 v2
  = du_'47''47''45'rawMagma_372 v2
du_'47''47''45'rawMagma_372 :: T_RawLoop_336 -> T_RawMagma_10
du_'47''47''45'rawMagma_372 v0
  = coe
      du_'47''47''45'rawMagma_326 (coe du_rawQuasigroup_366 (coe v0))
-- Algebra.Bundles.Raw.RawLoop._.\\-rawMagma
d_'92''92''45'rawMagma_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawLoop_336 -> T_RawMagma_10
d_'92''92''45'rawMagma_374 ~v0 ~v1 v2
  = du_'92''92''45'rawMagma_374 v2
du_'92''92''45'rawMagma_374 :: T_RawLoop_336 -> T_RawMagma_10
du_'92''92''45'rawMagma_374 v0
  = coe
      du_'92''92''45'rawMagma_324 (coe du_rawQuasigroup_366 (coe v0))
-- Algebra.Bundles.Raw.RawLoop._.∙-rawMagma
d_'8729''45'rawMagma_376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_RawLoop_336 -> T_RawMagma_10
d_'8729''45'rawMagma_376 ~v0 ~v1 v2 = du_'8729''45'rawMagma_376 v2
du_'8729''45'rawMagma_376 :: T_RawLoop_336 -> T_RawMagma_10
du_'8729''45'rawMagma_376 v0
  = coe du_'8729''45'rawMagma_322 (coe du_rawQuasigroup_366 (coe v0))
