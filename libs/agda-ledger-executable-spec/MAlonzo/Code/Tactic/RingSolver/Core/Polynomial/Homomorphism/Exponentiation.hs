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

module MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Definitions.RawMonoid
import qualified MAlonzo.Code.Algebra.Properties.CommutativeSemiring.Exp.TCOptimised
import qualified MAlonzo.Code.Algebra.Properties.Semiring.Exp.TCOptimised
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Kleene.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Multiplication
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics

-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._._⊡_
d__'8865'__290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
d__'8865'__290 v0 v1 ~v2 ~v3 v4 = du__'8865'__290 v0 v1 v4
du__'8865'__290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
du__'8865'__290 v0 v1 v2
  = let v3
          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
              (coe v2) in
    coe
      (\ v4 v5 v6 ->
         case coe v6 of
           0 -> coe
                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                  (coe (0 :: Integer))
                  (coe
                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                           (coe v3))))
                  (coe
                     MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v4))
           _ -> let v7 = subInt (coe v6) (coe (1 :: Integer)) in
                coe
                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                  (coe v0) (coe v1) (coe v3) (coe v4) (coe v5) (coe v7))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._._⊡_+1
d__'8865'_'43'1_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
d__'8865'_'43'1_292 v0 v1 ~v2 ~v3 v4
  = du__'8865'_'43'1_292 v0 v1 v4
du__'8865'_'43'1_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
du__'8865'_'43'1_292 v0 v1 v2
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
      (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
         (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._.Poly
d_Poly_310 a0 a1 a2 a3 a4 a5 = ()
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._.PowInd
d_PowInd_314 a0 a1 a2 a3 a4 a5 a6 = ()
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._.⊡-mult
d_'8865''45'mult_390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
d_'8865''45'mult_390 v0 v1 ~v2 ~v3 v4
  = du_'8865''45'mult_390 v0 v1 v4
du_'8865''45'mult_390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
du_'8865''45'mult_390 v0 v1 v2
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d_'8865''45'mult_754
      (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
         (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._._*⟨_⟩^_
d__'42''10216'_'10217''94'__486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> Integer -> AgdaAny
d__'42''10216'_'10217''94'__486 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du__'42''10216'_'10217''94'__486 v4 v5 v6 v7
du__'42''10216'_'10217''94'__486 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> Integer -> AgdaAny
du__'42''10216'_'10217''94'__486 v0 v1 v2 v3
  = case coe v3 of
      0 -> coe v1
      _ -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                (coe v0))
             (let v4
                    = coe
                        MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                        (coe
                           MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                           (let v4
                                  = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                      (coe v0) in
                            coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe v4)))) in
              let v5
                    = coe
                        MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                        (coe v4) in
              let v6 = subInt (coe v3) (coe (1 :: Integer)) in
              let v7
                    = coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                        (coe
                           MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                           (coe v5) (coe v6) (coe v2))
                        v2 in
              case coe v3 of
                0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                1 -> coe v2
                _ -> coe v7)
             v1
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._.⟦_⟧
d_'10214'_'10215'_496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'_496 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7
  = du_'10214'_'10215'_496 v4 v6 v7
du_'10214'_'10215'_496 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'10214'_'10215'_496 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v3 v4 v5
        -> case coe v4 of
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v6
               -> coe
                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                       (coe v0))
                    v6
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v7 v8
               -> let v9
                        = coe
                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                               (coe v5) (coe v2)) in
                  case coe v7 of
                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v10 v11
                      -> case coe v10 of
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v12 v13
                             -> case coe v12 of
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v14
                                    -> case coe v9 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v16 v17
                                           -> let v18
                                                    = coe
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                        (coe v0)
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                           (coe v14) (coe v11))
                                                        (coe v9) in
                                              case coe v13 of
                                                0 -> coe v18
                                                _ -> coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                          (coe v0))
                                                       (let v19
                                                              = coe
                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                     (let v19
                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                (coe v0) in
                                                                      coe
                                                                        MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                        (coe
                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                           (coe v19)))) in
                                                        let v20
                                                              = coe
                                                                  MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                  (coe v19) in
                                                        let v21
                                                              = subInt
                                                                  (coe v13) (coe (1 :: Integer)) in
                                                        let v22
                                                              = coe
                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                  v20
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                     (coe v20) (coe v21) (coe v16))
                                                                  v16 in
                                                        case coe v13 of
                                                          0 -> coe
                                                                 MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                 (coe v20)
                                                          1 -> coe v16
                                                          _ -> coe v22)
                                                       v18
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation.RawPow._^′_
d__'94''8242'__500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> Integer -> AgdaAny
d__'94''8242'__500 ~v0 ~v1 ~v2 ~v3 v4 = du__'94''8242'__500 v4
du__'94''8242'__500 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> Integer -> AgdaAny
du__'94''8242'__500 v0
  = let v1
          = coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
              (coe
                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                 (coe v0)) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
              (coe
                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                 (coe v2)) in
    coe
      (\ v4 v5 ->
         let v6
               = coe
                   MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                   (coe v3) in
         let v7 = subInt (coe v5) (coe (1 :: Integer)) in
         let v8
               = coe
                   MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v6
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                      (coe v6) (coe v7) (coe v4))
                   v4 in
         case coe v5 of
           0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v6)
           1 -> coe v4
           _ -> coe v8)
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation.CoPow._^′_
d__'94''8242'__520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> Integer -> AgdaAny
d__'94''8242'__520 ~v0 ~v1 ~v2 ~v3 v4 = du__'94''8242'__520 v4
du__'94''8242'__520 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> Integer -> AgdaAny
du__'94''8242'__520 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
              (coe
                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                 (coe
                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                    (coe v0))) in
    coe
      (\ v2 v3 ->
         let v4
               = coe
                   MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                   (coe v1) in
         let v5 = subInt (coe v3) (coe (1 :: Integer)) in
         let v6
               = coe
                   MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                      (coe v4) (coe v5) (coe v2))
                   v2 in
         case coe v3 of
           0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
           1 -> coe v2
           _ -> coe v6)
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation.pow-eval-hom
d_pow'45'eval'45'hom_562 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> Integer -> AgdaAny
d_pow'45'eval'45'hom_562 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_pow'45'eval'45'hom_562 v4 v5 v6
du_pow'45'eval'45'hom_562 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> Integer -> AgdaAny
du_pow'45'eval'45'hom_562 v0 v1 v2
  = case coe v2 of
      0 -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                (coe v0))
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                   (coe v0))
                (let v3
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                 (coe v0))) in
                 let v4 = 1 :: Integer in
                 let v5
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                           (coe v3) in
                 let v6 = 0 :: Integer in
                 let v7
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                           (coe
                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                              (coe v5) (coe v6) (coe v1))
                           v1 in
                 case coe v4 of
                   0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                   1 -> coe v1
                   _ -> coe v7))
      _ -> let v3 = subInt (coe v2) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'42''45'homo_774
                (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                   (coe v0))
                (let v4
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                 (coe v0))) in
                 let v5
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                           (coe v4) in
                 let v6 = subInt (coe v2) (coe (1 :: Integer)) in
                 let v7
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                           (coe
                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                              (coe v5) (coe v6) (coe v1))
                           v1 in
                 case coe v2 of
                   0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                   1 -> coe v1
                   _ -> coe v7)
                v1)
             (coe
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
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0)))))))))))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                      (coe v0))
                   (let v4
                          = let v4
                                  = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                      (coe v0) in
                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                              (coe v4) in
                    coe
                      MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v4
                      (let v5
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                       (coe v0))) in
                       let v6
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                 (coe v5) in
                       let v7 = subInt (coe v2) (coe (1 :: Integer)) in
                       let v8
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v6
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v6) (coe v7) (coe v1))
                                 v1 in
                       case coe v2 of
                         0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v6)
                         1 -> coe v1
                         _ -> coe v8)
                      v1))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                      (coe v0))
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                      (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                         (coe v0))
                      (let v4
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                       (coe v0))) in
                       let v5
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                 (coe v4) in
                       let v6 = subInt (coe v2) (coe (1 :: Integer)) in
                       let v7
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v5) (coe v6) (coe v1))
                                 v1 in
                       case coe v2 of
                         0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                         1 -> coe v1
                         _ -> coe v7))
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                      (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                         (coe v0))
                      v1))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                      (coe v0))
                   (let v4
                          = coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                 (coe v0)) in
                    let v5
                          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v4) in
                    let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v5)) in
                    let v7
                          = coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                 (coe v0))
                              v1 in
                    let v8
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v6) in
                    let v9 = subInt (coe v2) (coe (1 :: Integer)) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v8
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v8) (coe v9) (coe v7))
                              v7 in
                    case coe v2 of
                      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v8)
                      1 -> coe v7
                      _ -> coe v10)
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                      (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                         (coe v0))
                      v1)))
             (coe
                MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                         (coe
                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                               (coe v0))))))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                      (coe v0))
                   (let v4
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                    (coe v0))) in
                    let v5
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v4) in
                    let v6 = subInt (coe v2) (coe (1 :: Integer)) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v5) (coe v6) (coe v1))
                              v1 in
                    case coe v2 of
                      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                      1 -> coe v1
                      _ -> coe v7))
                (let v4
                       = coe
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                              (coe v0)) in
                 let v5
                       = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v4) in
                 let v6
                       = coe
                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                           (coe
                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                              (coe v5)) in
                 let v7
                       = coe
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                              (coe v0))
                           v1 in
                 let v8
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                           (coe v6) in
                 let v9 = subInt (coe v2) (coe (1 :: Integer)) in
                 let v10
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v8
                           (coe
                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                              (coe v8) (coe v9) (coe v7))
                           v7 in
                 case coe v2 of
                   0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v8)
                   1 -> coe v7
                   _ -> coe v10)
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                      (coe v0))
                   v1)
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                      (coe v0))
                   v1)
                (coe du_pow'45'eval'45'hom_562 (coe v0) (coe v1) (coe v3))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                      (coe v0))
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                      (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                         (coe v0))
                      v1)))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation.⊡-mult-hom
d_'8865''45'mult'45'hom_578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8865''45'mult'45'hom_578 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = case coe v6 of
      0 -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                (coe v4))
             (let v9
                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d_'8865''45'mult_754
                        (coe v0) (coe v1)
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                           (coe v4))
                        (coe v5) (coe (0 :: Integer)) (coe v7) in
              case coe v9 of
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                  -> case coe v11 of
                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                         -> coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                 (coe v4))
                              v13
                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                         -> let v16
                                  = coe
                                      MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                      (coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                         (coe v12) (coe v8)) in
                            case coe v14 of
                              MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                -> case coe v17 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                       -> case coe v19 of
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                              -> case coe v16 of
                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                     -> let v25
                                                              = coe
                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                  (coe v4)
                                                                  (coe
                                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                     (coe v21) (coe v18))
                                                                  (coe v16) in
                                                        case coe v20 of
                                                          0 -> coe v25
                                                          _ -> coe
                                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                    (coe v4))
                                                                 (let v26
                                                                        = coe
                                                                            MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                            (coe
                                                                               MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                               (let v26
                                                                                      = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                          (coe
                                                                                             v4) in
                                                                                coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                  (coe
                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                     (coe v26)))) in
                                                                  let v27
                                                                        = coe
                                                                            MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                            (coe v26) in
                                                                  let v28
                                                                        = subInt
                                                                            (coe v20)
                                                                            (coe (1 :: Integer)) in
                                                                  let v29
                                                                        = coe
                                                                            MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                            v27
                                                                            (coe
                                                                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                               (coe v27) (coe v28)
                                                                               (coe v23))
                                                                            v23 in
                                                                  case coe v20 of
                                                                    0 -> coe
                                                                           MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                           (coe v27)
                                                                    1 -> coe v23
                                                                    _ -> coe v29)
                                                                 v25
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError
                       _ -> MAlonzo.RTE.mazUnreachableError
                _ -> MAlonzo.RTE.mazUnreachableError)
      _ -> let v9 = subInt (coe v6) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Multiplication.du_'8864''45'hom_882
                v0 v1 v2 v3 v4
                (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d_'8865''45'mult_754
                   (coe v0) (coe v1)
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                      (coe v4))
                   (coe v5) (coe v9) (coe v7))
                v7 v8)
             (coe
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
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v4)))))))))))
                (let v10
                       = let v10
                               = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                   (coe v4) in
                         coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8864''45'step'8242'_558
                           v0 v1 v10 v5
                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d_'8865''45'mult_754
                              (coe v0) (coe v1)
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                 (coe v4))
                              (coe v5) (coe v9) (coe v7))
                           v7 in
                 case coe v10 of
                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v11 v12 v13
                     -> case coe v12 of
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v14
                            -> coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                    (coe v4))
                                 v14
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v15 v16
                            -> let v17
                                     = coe
                                         MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                            (coe v13) (coe v8)) in
                               case coe v15 of
                                 MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v18 v19
                                   -> case coe v18 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v20 v21
                                          -> case coe v20 of
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v22
                                                 -> case coe v17 of
                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v24 v25
                                                        -> let v26
                                                                 = coe
                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                     (coe v4)
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                        (coe v22) (coe v19))
                                                                     (coe v17) in
                                                           case coe v21 of
                                                             0 -> coe v26
                                                             _ -> coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v4))
                                                                    (let v27
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                  (let v27
                                                                                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                             (coe
                                                                                                v4) in
                                                                                   coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                     (coe
                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                        (coe
                                                                                           v27)))) in
                                                                     let v28
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                               (coe v27) in
                                                                     let v29
                                                                           = subInt
                                                                               (coe v21)
                                                                               (coe
                                                                                  (1 :: Integer)) in
                                                                     let v30
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                               v28
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                  (coe v28)
                                                                                  (coe v29)
                                                                                  (coe v24))
                                                                               v24 in
                                                                     case coe v21 of
                                                                       0 -> coe
                                                                              MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                              (coe v28)
                                                                       1 -> coe v24
                                                                       _ -> coe v30)
                                                                    v26
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError
                          _ -> MAlonzo.RTE.mazUnreachableError
                   _ -> MAlonzo.RTE.mazUnreachableError)
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                      (coe v4))
                   (let v10
                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d_'8865''45'mult_754
                              (coe v0) (coe v1)
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                 (coe v4))
                              (coe v5) (coe v9) (coe v7) in
                    case coe v10 of
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v11 v12 v13
                        -> case coe v12 of
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v14
                               -> coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                       (coe v4))
                                    v14
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v15 v16
                               -> let v17
                                        = coe
                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                               (coe v13) (coe v8)) in
                                  case coe v15 of
                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v18 v19
                                      -> case coe v18 of
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v20 v21
                                             -> case coe v20 of
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v22
                                                    -> case coe v17 of
                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v24 v25
                                                           -> let v26
                                                                    = coe
                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                        (coe v4)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                           (coe v22) (coe v19))
                                                                        (coe v17) in
                                                              case coe v21 of
                                                                0 -> coe v26
                                                                _ -> coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v4))
                                                                       (let v27
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                     (let v27
                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                (coe
                                                                                                   v4) in
                                                                                      coe
                                                                                        MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                        (coe
                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                           (coe
                                                                                              v27)))) in
                                                                        let v28
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                  (coe v27) in
                                                                        let v29
                                                                              = subInt
                                                                                  (coe v21)
                                                                                  (coe
                                                                                     (1 ::
                                                                                        Integer)) in
                                                                        let v30
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                  v28
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                     (coe v28)
                                                                                     (coe v29)
                                                                                     (coe v24))
                                                                                  v24 in
                                                                        case coe v21 of
                                                                          0 -> coe
                                                                                 MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                 (coe v28)
                                                                          1 -> coe v24
                                                                          _ -> coe v30)
                                                                       v26
                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             _ -> MAlonzo.RTE.mazUnreachableError
                      _ -> MAlonzo.RTE.mazUnreachableError)
                   (case coe v7 of
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                        -> case coe v11 of
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                               -> coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                       (coe v4))
                                    v13
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                               -> let v16
                                        = coe
                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                               (coe v12) (coe v8)) in
                                  case coe v14 of
                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                      -> case coe v17 of
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                             -> case coe v19 of
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                                    -> case coe v16 of
                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                           -> let v25
                                                                    = coe
                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                        (coe v4)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                           (coe v21) (coe v18))
                                                                        (coe v16) in
                                                              case coe v20 of
                                                                0 -> coe v25
                                                                _ -> coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v4))
                                                                       (let v26
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                     (let v26
                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                (coe
                                                                                                   v4) in
                                                                                      coe
                                                                                        MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                        (coe
                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                           (coe
                                                                                              v26)))) in
                                                                        let v27
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                  (coe v26) in
                                                                        let v28
                                                                              = subInt
                                                                                  (coe v20)
                                                                                  (coe
                                                                                     (1 ::
                                                                                        Integer)) in
                                                                        let v29
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                  v27
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                     (coe v27)
                                                                                     (coe v28)
                                                                                     (coe v23))
                                                                                  v23 in
                                                                        case coe v20 of
                                                                          0 -> coe
                                                                                 MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                 (coe v27)
                                                                          1 -> coe v23
                                                                          _ -> coe v29)
                                                                       v25
                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             _ -> MAlonzo.RTE.mazUnreachableError
                      _ -> MAlonzo.RTE.mazUnreachableError))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                      (coe v4))
                   (let v10
                          = coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                 (coe v4)) in
                    let v11
                          = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v10) in
                    let v12
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v11)) in
                    let v13
                          = case coe v7 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v13 v14 v15
                                -> case coe v14 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v16
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v4))
                                            v16
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v17 v18
                                       -> let v19
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                       (coe v15) (coe v8)) in
                                          case coe v17 of
                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v20 v21
                                              -> case coe v20 of
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v22 v23
                                                     -> case coe v22 of
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v24
                                                            -> case coe v19 of
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v26 v27
                                                                   -> let v28
                                                                            = coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                (coe v4)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v24)
                                                                                   (coe v21))
                                                                                (coe v19) in
                                                                      case coe v23 of
                                                                        0 -> coe v28
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v4))
                                                                               (let v29
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v29
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v4) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v29)))) in
                                                                                let v30
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v29) in
                                                                                let v31
                                                                                      = subInt
                                                                                          (coe v23)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v32
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v30
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v30)
                                                                                             (coe
                                                                                                v31)
                                                                                             (coe
                                                                                                v26))
                                                                                          v26 in
                                                                                case coe v23 of
                                                                                  0 -> coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                         (coe v30)
                                                                                  1 -> coe v26
                                                                                  _ -> coe v32)
                                                                               v28
                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError in
                    let v14
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v12) in
                    let v15 = subInt (coe v6) (coe (1 :: Integer)) in
                    let v16
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v14
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v14) (coe v15) (coe v13))
                              v13 in
                    case coe v6 of
                      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v14)
                      1 -> coe v13
                      _ -> coe v16)
                   (case coe v7 of
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                        -> case coe v11 of
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                               -> coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                       (coe v4))
                                    v13
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                               -> let v16
                                        = coe
                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                               (coe v12) (coe v8)) in
                                  case coe v14 of
                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                      -> case coe v17 of
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                             -> case coe v19 of
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                                    -> case coe v16 of
                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                           -> let v25
                                                                    = coe
                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                        (coe v4)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                           (coe v21) (coe v18))
                                                                        (coe v16) in
                                                              case coe v20 of
                                                                0 -> coe v25
                                                                _ -> coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v4))
                                                                       (let v26
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                     (let v26
                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                (coe
                                                                                                   v4) in
                                                                                      coe
                                                                                        MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                        (coe
                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                           (coe
                                                                                              v26)))) in
                                                                        let v27
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                  (coe v26) in
                                                                        let v28
                                                                              = subInt
                                                                                  (coe v20)
                                                                                  (coe
                                                                                     (1 ::
                                                                                        Integer)) in
                                                                        let v29
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                  v27
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                     (coe v27)
                                                                                     (coe v28)
                                                                                     (coe v23))
                                                                                  v23 in
                                                                        case coe v20 of
                                                                          0 -> coe
                                                                                 MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                 (coe v27)
                                                                          1 -> coe v23
                                                                          _ -> coe v29)
                                                                       v25
                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             _ -> MAlonzo.RTE.mazUnreachableError
                      _ -> MAlonzo.RTE.mazUnreachableError)))
             (coe
                MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                         (coe
                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                               (coe v4))))))
                (let v10
                       = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d_'8865''45'mult_754
                           (coe v0) (coe v1)
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                              (coe v4))
                           (coe v5) (coe v9) (coe v7) in
                 case coe v10 of
                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v11 v12 v13
                     -> case coe v12 of
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v14
                            -> coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                    (coe v4))
                                 v14
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v15 v16
                            -> let v17
                                     = coe
                                         MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                            (coe v13) (coe v8)) in
                               case coe v15 of
                                 MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v18 v19
                                   -> case coe v18 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v20 v21
                                          -> case coe v20 of
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v22
                                                 -> case coe v17 of
                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v24 v25
                                                        -> let v26
                                                                 = coe
                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                     (coe v4)
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                        (coe v22) (coe v19))
                                                                     (coe v17) in
                                                           case coe v21 of
                                                             0 -> coe v26
                                                             _ -> coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v4))
                                                                    (let v27
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                  (let v27
                                                                                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                             (coe
                                                                                                v4) in
                                                                                   coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                     (coe
                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                        (coe
                                                                                           v27)))) in
                                                                     let v28
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                               (coe v27) in
                                                                     let v29
                                                                           = subInt
                                                                               (coe v21)
                                                                               (coe
                                                                                  (1 :: Integer)) in
                                                                     let v30
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                               v28
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                  (coe v28)
                                                                                  (coe v29)
                                                                                  (coe v24))
                                                                               v24 in
                                                                     case coe v21 of
                                                                       0 -> coe
                                                                              MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                              (coe v28)
                                                                       1 -> coe v24
                                                                       _ -> coe v30)
                                                                    v26
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError
                          _ -> MAlonzo.RTE.mazUnreachableError
                   _ -> MAlonzo.RTE.mazUnreachableError)
                (let v10
                       = coe
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                              (coe v4)) in
                 let v11
                       = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v10) in
                 let v12
                       = coe
                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                           (coe
                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                              (coe v11)) in
                 let v13
                       = case coe v7 of
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v13 v14 v15
                             -> case coe v14 of
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v16
                                    -> coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                            (coe v4))
                                         v16
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v17 v18
                                    -> let v19
                                             = coe
                                                 MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                 (coe
                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                    (coe v15) (coe v8)) in
                                       case coe v17 of
                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v20 v21
                                           -> case coe v20 of
                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v22 v23
                                                  -> case coe v22 of
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v24
                                                         -> case coe v19 of
                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v26 v27
                                                                -> let v28
                                                                         = coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                             (coe v4)
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                (coe v24) (coe v21))
                                                                             (coe v19) in
                                                                   case coe v23 of
                                                                     0 -> coe v28
                                                                     _ -> coe
                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                               (coe v4))
                                                                            (let v29
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                       (coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                          (let v29
                                                                                                 = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                     (coe
                                                                                                        v4) in
                                                                                           coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                             (coe
                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                (coe
                                                                                                   v29)))) in
                                                                             let v30
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                       (coe v29) in
                                                                             let v31
                                                                                   = subInt
                                                                                       (coe v23)
                                                                                       (coe
                                                                                          (1 ::
                                                                                             Integer)) in
                                                                             let v32
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                       v30
                                                                                       (coe
                                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                          (coe v30)
                                                                                          (coe v31)
                                                                                          (coe v26))
                                                                                       v26 in
                                                                             case coe v23 of
                                                                               0 -> coe
                                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                      (coe v30)
                                                                               1 -> coe v26
                                                                               _ -> coe v32)
                                                                            v28
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError in
                 let v14
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                           (coe v12) in
                 let v15 = subInt (coe v6) (coe (1 :: Integer)) in
                 let v16
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v14
                           (coe
                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                              (coe v14) (coe v15) (coe v13))
                           v13 in
                 case coe v6 of
                   0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v14)
                   1 -> coe v13
                   _ -> coe v16)
                (case coe v7 of
                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                     -> case coe v11 of
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                            -> coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                    (coe v4))
                                 v13
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                            -> let v16
                                     = coe
                                         MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                            (coe v12) (coe v8)) in
                               case coe v14 of
                                 MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                   -> case coe v17 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                          -> case coe v19 of
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                                 -> case coe v16 of
                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                        -> let v25
                                                                 = coe
                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                     (coe v4)
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                        (coe v21) (coe v18))
                                                                     (coe v16) in
                                                           case coe v20 of
                                                             0 -> coe v25
                                                             _ -> coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v4))
                                                                    (let v26
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                  (let v26
                                                                                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                             (coe
                                                                                                v4) in
                                                                                   coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                     (coe
                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                        (coe
                                                                                           v26)))) in
                                                                     let v27
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                               (coe v26) in
                                                                     let v28
                                                                           = subInt
                                                                               (coe v20)
                                                                               (coe
                                                                                  (1 :: Integer)) in
                                                                     let v29
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                               v27
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                  (coe v27)
                                                                                  (coe v28)
                                                                                  (coe v23))
                                                                               v23 in
                                                                     case coe v20 of
                                                                       0 -> coe
                                                                              MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                              (coe v27)
                                                                       1 -> coe v23
                                                                       _ -> coe v29)
                                                                    v25
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError
                          _ -> MAlonzo.RTE.mazUnreachableError
                   _ -> MAlonzo.RTE.mazUnreachableError)
                (case coe v7 of
                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                     -> case coe v11 of
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                            -> coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                    (coe v4))
                                 v13
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                            -> let v16
                                     = coe
                                         MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                            (coe v12) (coe v8)) in
                               case coe v14 of
                                 MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                   -> case coe v17 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                          -> case coe v19 of
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                                 -> case coe v16 of
                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                        -> let v25
                                                                 = coe
                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                     (coe v4)
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                        (coe v21) (coe v18))
                                                                     (coe v16) in
                                                           case coe v20 of
                                                             0 -> coe v25
                                                             _ -> coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v4))
                                                                    (let v26
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                  (let v26
                                                                                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                             (coe
                                                                                                v4) in
                                                                                   coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                     (coe
                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                        (coe
                                                                                           v26)))) in
                                                                     let v27
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                               (coe v26) in
                                                                     let v28
                                                                           = subInt
                                                                               (coe v20)
                                                                               (coe
                                                                                  (1 :: Integer)) in
                                                                     let v29
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                               v27
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                  (coe v27)
                                                                                  (coe v28)
                                                                                  (coe v23))
                                                                               v23 in
                                                                     case coe v20 of
                                                                       0 -> coe
                                                                              MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                              (coe v27)
                                                                       1 -> coe v23
                                                                       _ -> coe v29)
                                                                    v25
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError
                          _ -> MAlonzo.RTE.mazUnreachableError
                   _ -> MAlonzo.RTE.mazUnreachableError)
                (d_'8865''45'mult'45'hom_578
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v9)
                   (coe v7) (coe v8))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                      (coe v4))
                   (case coe v7 of
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                        -> case coe v11 of
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                               -> coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                       (coe v4))
                                    v13
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                               -> let v16
                                        = coe
                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                               (coe v12) (coe v8)) in
                                  case coe v14 of
                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                      -> case coe v17 of
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                             -> case coe v19 of
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                                    -> case coe v16 of
                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                           -> let v25
                                                                    = coe
                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                        (coe v4)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                           (coe v21) (coe v18))
                                                                        (coe v16) in
                                                              case coe v20 of
                                                                0 -> coe v25
                                                                _ -> coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v4))
                                                                       (let v26
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                     (let v26
                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                (coe
                                                                                                   v4) in
                                                                                      coe
                                                                                        MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                        (coe
                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                           (coe
                                                                                              v26)))) in
                                                                        let v27
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                  (coe v26) in
                                                                        let v28
                                                                              = subInt
                                                                                  (coe v20)
                                                                                  (coe
                                                                                     (1 ::
                                                                                        Integer)) in
                                                                        let v29
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                  v27
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                     (coe v27)
                                                                                     (coe v28)
                                                                                     (coe v23))
                                                                                  v23 in
                                                                        case coe v20 of
                                                                          0 -> coe
                                                                                 MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                 (coe v27)
                                                                          1 -> coe v23
                                                                          _ -> coe v29)
                                                                       v25
                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             _ -> MAlonzo.RTE.mazUnreachableError
                      _ -> MAlonzo.RTE.mazUnreachableError)))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation.⊡-+1-hom
d_'8865''45''43'1'45'hom_598 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8865''45''43'1'45'hom_598 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = case coe v6 of
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
        -> case coe v10 of
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
               -> coe du_pow'45'eval'45'hom_562 (coe v4) (coe v12) (coe v7)
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
               -> let v15 = subInt (coe v9) (coe (1 :: Integer)) in
                  case coe v13 of
                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                      -> case coe v17 of
                           MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                             -> case coe v16 of
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                    -> case coe v18 of
                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                           -> coe
                                                MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                                (coe
                                                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                                   (let v22
                                                          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                              (coe
                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                 (coe v4)) in
                                                    let v23
                                                          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                              (coe v22) in
                                                    let v24
                                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                              (coe v23) in
                                                    let v25
                                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                              (coe v24) in
                                                    let v26
                                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                              (coe v25) in
                                                    let v27
                                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                              (coe v26) in
                                                    let v28
                                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                              (coe v27) in
                                                    coe
                                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                         (coe v28)))
                                                   (let v22
                                                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                              (coe v4) in
                                                    let v23
                                                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                              (coe v4) in
                                                    let v24
                                                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                                                              (coe v0) (coe v1)
                                                              (coe
                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                 (coe v4))
                                                              (coe v15) (coe v20) (coe v7) in
                                                    let v25
                                                          = addInt
                                                              (coe mulInt (coe v7) (coe v19))
                                                              (coe v19) in
                                                    case coe v24 of
                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v26 v27 v28
                                                        -> case coe v27 of
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v29
                                                               -> let v30
                                                                        = let v30
                                                                                = coe
                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                                                    v23 v29 in
                                                                          seq
                                                                            (coe v30)
                                                                            (if coe v30
                                                                               then coe
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                                                                                      (coe v17)
                                                                                      (coe
                                                                                         addInt
                                                                                         (coe
                                                                                            (1 ::
                                                                                               Integer))
                                                                                         (coe v25))
                                                                               else coe
                                                                                      MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                         (coe
                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                                                            (coe
                                                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                                                               v24)
                                                                                            (coe
                                                                                               v25))
                                                                                         (coe
                                                                                            v17))) in
                                                                  case coe v30 of
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                      -> let v31
                                                                               = MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                                                                   (coe
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                                                                      (coe v22)) in
                                                                         coe
                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                              (coe v4))
                                                                           v31
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v31
                                                                      -> case coe v31 of
                                                                           MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v32 v33
                                                                             -> case coe v32 of
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v34 v35
                                                                                    -> case coe
                                                                                              v34 of
                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v36
                                                                                           -> case coe
                                                                                                     v35 of
                                                                                                0 -> case coe
                                                                                                            v33 of
                                                                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                                                         -> case coe
                                                                                                                   v36 of
                                                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v38 v39 v40
                                                                                                                -> let v41
                                                                                                                         = coe
                                                                                                                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                                                             (coe
                                                                                                                                MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                                                                v40)
                                                                                                                             (coe
                                                                                                                                MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                                                             (coe
                                                                                                                                v11) in
                                                                                                                   case coe
                                                                                                                          v39 of
                                                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v42
                                                                                                                       -> coe
                                                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                                                               (coe
                                                                                                                                  v4))
                                                                                                                            v42
                                                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v43 v44
                                                                                                                       -> let v45
                                                                                                                                = coe
                                                                                                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                                                                    (coe
                                                                                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                                                       (coe
                                                                                                                                          v41)
                                                                                                                                       (coe
                                                                                                                                          v8)) in
                                                                                                                          case coe
                                                                                                                                 v43 of
                                                                                                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v46 v47
                                                                                                                              -> case coe
                                                                                                                                        v46 of
                                                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v48 v49
                                                                                                                                     -> case coe
                                                                                                                                               v48 of
                                                                                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v50
                                                                                                                                            -> case coe
                                                                                                                                                      v45 of
                                                                                                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v52 v53
                                                                                                                                                   -> let v54
                                                                                                                                                            = coe
                                                                                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                                                                (coe
                                                                                                                                                                   v4)
                                                                                                                                                                (coe
                                                                                                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                                                                   (coe
                                                                                                                                                                      v50)
                                                                                                                                                                   (coe
                                                                                                                                                                      v47))
                                                                                                                                                                (coe
                                                                                                                                                                   v45) in
                                                                                                                                                      case coe
                                                                                                                                                             v49 of
                                                                                                                                                        0 -> coe
                                                                                                                                                               v54
                                                                                                                                                        _ -> coe
                                                                                                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                                  (coe
                                                                                                                                                                     v4))
                                                                                                                                                               (let v55
                                                                                                                                                                      = coe
                                                                                                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                                                          (coe
                                                                                                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                                                             (let v55
                                                                                                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                                                        (coe
                                                                                                                                                                                           v4) in
                                                                                                                                                                              coe
                                                                                                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                                                                (coe
                                                                                                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                                                                   (coe
                                                                                                                                                                                      v55)))) in
                                                                                                                                                                let v56
                                                                                                                                                                      = coe
                                                                                                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                                                          (coe
                                                                                                                                                                             v55) in
                                                                                                                                                                let v57
                                                                                                                                                                      = subInt
                                                                                                                                                                          (coe
                                                                                                                                                                             v49)
                                                                                                                                                                          (coe
                                                                                                                                                                             (1 ::
                                                                                                                                                                                Integer)) in
                                                                                                                                                                let v58
                                                                                                                                                                      = coe
                                                                                                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                                                          v56
                                                                                                                                                                          (coe
                                                                                                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                                                             (coe
                                                                                                                                                                                v56)
                                                                                                                                                                             (coe
                                                                                                                                                                                v57)
                                                                                                                                                                             (coe
                                                                                                                                                                                v52))
                                                                                                                                                                          v52 in
                                                                                                                                                                case coe
                                                                                                                                                                       v49 of
                                                                                                                                                                  0 -> coe
                                                                                                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                                                         (coe
                                                                                                                                                                            v56)
                                                                                                                                                                  1 -> coe
                                                                                                                                                                         v52
                                                                                                                                                                  _ -> coe
                                                                                                                                                                         v58)
                                                                                                                                                               v54
                                                                                                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v38
                                                                                                         -> let v39
                                                                                                                  = coe
                                                                                                                      MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                                                      (coe
                                                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                                         (coe
                                                                                                                            v11)
                                                                                                                         (coe
                                                                                                                            v8)) in
                                                                                                            let v40
                                                                                                                  = 0 ::
                                                                                                                      Integer in
                                                                                                            case coe
                                                                                                                   v39 of
                                                                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v41 v42
                                                                                                                -> let v43
                                                                                                                         = coe
                                                                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                             (coe
                                                                                                                                v4)
                                                                                                                             (coe
                                                                                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                                (coe
                                                                                                                                   v36)
                                                                                                                                (coe
                                                                                                                                   v33))
                                                                                                                             (coe
                                                                                                                                v39) in
                                                                                                                   case coe
                                                                                                                          v40 of
                                                                                                                     0 -> coe
                                                                                                                            v43
                                                                                                                     _ -> coe
                                                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                               (coe
                                                                                                                                  v4))
                                                                                                                            (let v44
                                                                                                                                   = coe
                                                                                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                       (coe
                                                                                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                          (let v44
                                                                                                                                                 = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                     (coe
                                                                                                                                                        v4) in
                                                                                                                                           coe
                                                                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                             (coe
                                                                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                                (coe
                                                                                                                                                   v44)))) in
                                                                                                                             let v45
                                                                                                                                   = coe
                                                                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                       (coe
                                                                                                                                          v44) in
                                                                                                                             let v46
                                                                                                                                   = -1 ::
                                                                                                                                       Integer in
                                                                                                                             let v47
                                                                                                                                   = coe
                                                                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                       v45
                                                                                                                                       (coe
                                                                                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                          (coe
                                                                                                                                             v45)
                                                                                                                                          (coe
                                                                                                                                             v46)
                                                                                                                                          (coe
                                                                                                                                             v41))
                                                                                                                                       v41 in
                                                                                                                             case coe
                                                                                                                                    v40 of
                                                                                                                               0 -> coe
                                                                                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                      (coe
                                                                                                                                         v45)
                                                                                                                               1 -> coe
                                                                                                                                      v41
                                                                                                                               _ -> coe
                                                                                                                                      v47)
                                                                                                                            v43
                                                                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                _ -> let v38
                                                                                                           = coe
                                                                                                               MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                                               (coe
                                                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                                  (coe
                                                                                                                     v11)
                                                                                                                  (coe
                                                                                                                     v8)) in
                                                                                                     case coe
                                                                                                            v38 of
                                                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v39 v40
                                                                                                         -> let v41
                                                                                                                  = coe
                                                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                      (coe
                                                                                                                         v4)
                                                                                                                      (coe
                                                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                         (coe
                                                                                                                            v36)
                                                                                                                         (coe
                                                                                                                            v33))
                                                                                                                      (coe
                                                                                                                         v38) in
                                                                                                            case coe
                                                                                                                   v35 of
                                                                                                              0 -> coe
                                                                                                                     v41
                                                                                                              _ -> coe
                                                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                        (coe
                                                                                                                           v4))
                                                                                                                     (let v42
                                                                                                                            = coe
                                                                                                                                MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                (coe
                                                                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                   (let v42
                                                                                                                                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                              (coe
                                                                                                                                                 v4) in
                                                                                                                                    coe
                                                                                                                                      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                      (coe
                                                                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                         (coe
                                                                                                                                            v42)))) in
                                                                                                                      let v43
                                                                                                                            = coe
                                                                                                                                MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                (coe
                                                                                                                                   v42) in
                                                                                                                      let v44
                                                                                                                            = subInt
                                                                                                                                (coe
                                                                                                                                   v35)
                                                                                                                                (coe
                                                                                                                                   (1 ::
                                                                                                                                      Integer)) in
                                                                                                                      let v45
                                                                                                                            = coe
                                                                                                                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                v43
                                                                                                                                (coe
                                                                                                                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                   (coe
                                                                                                                                      v43)
                                                                                                                                   (coe
                                                                                                                                      v44)
                                                                                                                                   (coe
                                                                                                                                      v39))
                                                                                                                                v39 in
                                                                                                                      case coe
                                                                                                                             v35 of
                                                                                                                        0 -> coe
                                                                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                               (coe
                                                                                                                                  v43)
                                                                                                                        1 -> coe
                                                                                                                               v39
                                                                                                                        _ -> coe
                                                                                                                               v45)
                                                                                                                     v41
                                                                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v30 v31
                                                               -> case coe v25 of
                                                                    0 -> case coe v24 of
                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v32 v33 v34
                                                                             -> let v35
                                                                                      = coe
                                                                                          MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                          (coe
                                                                                             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                             v34)
                                                                                          (coe
                                                                                             MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                          (coe
                                                                                             v11) in
                                                                                case coe v33 of
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v36
                                                                                    -> coe
                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                            (coe
                                                                                               v4))
                                                                                         v36
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v37 v38
                                                                                    -> let v39
                                                                                             = coe
                                                                                                 MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                    (coe
                                                                                                       v35)
                                                                                                    (coe
                                                                                                       v8)) in
                                                                                       case coe
                                                                                              v37 of
                                                                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v40 v41
                                                                                           -> case coe
                                                                                                     v40 of
                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v42 v43
                                                                                                  -> case coe
                                                                                                            v42 of
                                                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v44
                                                                                                         -> case coe
                                                                                                                   v39 of
                                                                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v46 v47
                                                                                                                -> let v48
                                                                                                                         = coe
                                                                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                             (coe
                                                                                                                                v4)
                                                                                                                             (coe
                                                                                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                                (coe
                                                                                                                                   v44)
                                                                                                                                (coe
                                                                                                                                   v41))
                                                                                                                             (coe
                                                                                                                                v39) in
                                                                                                                   case coe
                                                                                                                          v43 of
                                                                                                                     0 -> coe
                                                                                                                            v48
                                                                                                                     _ -> coe
                                                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                               (coe
                                                                                                                                  v4))
                                                                                                                            (let v49
                                                                                                                                   = coe
                                                                                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                       (coe
                                                                                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                          (let v49
                                                                                                                                                 = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                     (coe
                                                                                                                                                        v4) in
                                                                                                                                           coe
                                                                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                             (coe
                                                                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                                (coe
                                                                                                                                                   v49)))) in
                                                                                                                             let v50
                                                                                                                                   = coe
                                                                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                       (coe
                                                                                                                                          v49) in
                                                                                                                             let v51
                                                                                                                                   = subInt
                                                                                                                                       (coe
                                                                                                                                          v43)
                                                                                                                                       (coe
                                                                                                                                          (1 ::
                                                                                                                                             Integer)) in
                                                                                                                             let v52
                                                                                                                                   = coe
                                                                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                       v50
                                                                                                                                       (coe
                                                                                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                          (coe
                                                                                                                                             v50)
                                                                                                                                          (coe
                                                                                                                                             v51)
                                                                                                                                          (coe
                                                                                                                                             v46))
                                                                                                                                       v46 in
                                                                                                                             case coe
                                                                                                                                    v43 of
                                                                                                                               0 -> coe
                                                                                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                      (coe
                                                                                                                                         v50)
                                                                                                                               1 -> coe
                                                                                                                                      v46
                                                                                                                               _ -> coe
                                                                                                                                      v52)
                                                                                                                            v48
                                                                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                    _ -> let v32
                                                                               = coe
                                                                                   MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                   (coe
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                      (coe v11)
                                                                                      (coe v8)) in
                                                                         case coe v32 of
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v33 v34
                                                                             -> let v35
                                                                                      = coe
                                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                          (coe v4)
                                                                                          (coe
                                                                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                             (coe
                                                                                                v24)
                                                                                             (coe
                                                                                                v17))
                                                                                          (coe
                                                                                             v32) in
                                                                                case coe v25 of
                                                                                  0 -> coe v35
                                                                                  _ -> coe
                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                            (coe
                                                                                               v4))
                                                                                         (let v36
                                                                                                = coe
                                                                                                    MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                       (let v36
                                                                                                              = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                  (coe
                                                                                                                     v4) in
                                                                                                        coe
                                                                                                          MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                             (coe
                                                                                                                v36)))) in
                                                                                          let v37
                                                                                                = coe
                                                                                                    MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                    (coe
                                                                                                       v36) in
                                                                                          let v38
                                                                                                = subInt
                                                                                                    (coe
                                                                                                       v25)
                                                                                                    (coe
                                                                                                       (1 ::
                                                                                                          Integer)) in
                                                                                          let v39
                                                                                                = coe
                                                                                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                    v37
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                       (coe
                                                                                                          v37)
                                                                                                       (coe
                                                                                                          v38)
                                                                                                       (coe
                                                                                                          v33))
                                                                                                    v33 in
                                                                                          case coe
                                                                                                 v25 of
                                                                                            0 -> coe
                                                                                                   MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                   (coe
                                                                                                      v37)
                                                                                            1 -> coe
                                                                                                   v33
                                                                                            _ -> coe
                                                                                                   v39)
                                                                                         v35
                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                      _ -> MAlonzo.RTE.mazUnreachableError)
                                                   (coe
                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8512''63''10214'_'10215'_606
                                                      v4
                                                      (let v22
                                                             = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                 (coe v4) in
                                                       let v23
                                                             = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                                                                 (coe v0) (coe v1)
                                                                 (coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                    (coe v4))
                                                                 (coe v15) (coe v20) (coe v7) in
                                                       let v24
                                                             = addInt
                                                                 (coe mulInt (coe v7) (coe v19))
                                                                 (coe v19) in
                                                       case coe v23 of
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                           -> case coe v26 of
                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                  -> let v29
                                                                           = coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                                               v22 v28 in
                                                                     coe
                                                                       seq (coe v29)
                                                                       (if coe v29
                                                                          then coe
                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                                                                                 (coe v17)
                                                                                 (coe
                                                                                    addInt
                                                                                    (coe
                                                                                       (1 ::
                                                                                          Integer))
                                                                                    (coe v24))
                                                                          else coe
                                                                                 MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                                 (coe
                                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                    (coe
                                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                                                       (coe
                                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                                                          v23)
                                                                                       (coe v24))
                                                                                    (coe v17)))
                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                  -> coe
                                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                       (coe
                                                                          MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                                             (coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                                                v23)
                                                                             (coe v24))
                                                                          (coe v17))
                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                         _ -> MAlonzo.RTE.mazUnreachableError)
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                         (coe
                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                            (coe v11) (coe v8))))
                                                   (let v22
                                                          = coe
                                                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                              (coe
                                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                 (coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                    (coe v4))) in
                                                    let v23
                                                          = coe
                                                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                              (coe
                                                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                 (coe v22)) in
                                                    let v24
                                                          = let v24
                                                                  = let v24
                                                                          = coe
                                                                              du_Ρ_634 (coe v11)
                                                                              (coe v8) in
                                                                    case coe v20 of
                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                                        -> case coe v26 of
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                               -> coe
                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                       (coe v4))
                                                                                    v28
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                               -> let v31
                                                                                        = coe
                                                                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                            (coe
                                                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                               (coe
                                                                                                  v27)
                                                                                               (coe
                                                                                                  v24)) in
                                                                                  case coe v29 of
                                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v32 v33
                                                                                      -> case coe
                                                                                                v32 of
                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v34 v35
                                                                                             -> case coe
                                                                                                       v34 of
                                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v36
                                                                                                    -> case coe
                                                                                                              v31 of
                                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v38 v39
                                                                                                           -> let v40
                                                                                                                    = coe
                                                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                        (coe
                                                                                                                           v4)
                                                                                                                        (coe
                                                                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                           (coe
                                                                                                                              v36)
                                                                                                                           (coe
                                                                                                                              v33))
                                                                                                                        (coe
                                                                                                                           v31) in
                                                                                                              case coe
                                                                                                                     v35 of
                                                                                                                0 -> coe
                                                                                                                       v40
                                                                                                                _ -> coe
                                                                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                          (coe
                                                                                                                             v4))
                                                                                                                       (let v41
                                                                                                                              = coe
                                                                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                  (coe
                                                                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                     (let v41
                                                                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                (coe
                                                                                                                                                   v4) in
                                                                                                                                      coe
                                                                                                                                        MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                        (coe
                                                                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                           (coe
                                                                                                                                              v41)))) in
                                                                                                                        let v42
                                                                                                                              = coe
                                                                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                  (coe
                                                                                                                                     v41) in
                                                                                                                        let v43
                                                                                                                              = subInt
                                                                                                                                  (coe
                                                                                                                                     v35)
                                                                                                                                  (coe
                                                                                                                                     (1 ::
                                                                                                                                        Integer)) in
                                                                                                                        let v44
                                                                                                                              = coe
                                                                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                  v42
                                                                                                                                  (coe
                                                                                                                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                     (coe
                                                                                                                                        v42)
                                                                                                                                     (coe
                                                                                                                                        v43)
                                                                                                                                     (coe
                                                                                                                                        v38))
                                                                                                                                  v38 in
                                                                                                                        case coe
                                                                                                                               v35 of
                                                                                                                          0 -> coe
                                                                                                                                 MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                 (coe
                                                                                                                                    v42)
                                                                                                                          1 -> coe
                                                                                                                                 v38
                                                                                                                          _ -> coe
                                                                                                                                 v44)
                                                                                                                       v40
                                                                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                      _ -> MAlonzo.RTE.mazUnreachableError in
                                                            let v25
                                                                  = coe
                                                                      du_ρ'8242'_632 (coe v11)
                                                                      (coe v8) in
                                                            case coe v19 of
                                                              0 -> coe v24
                                                              _ -> coe
                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                        (coe v4))
                                                                     (let v26
                                                                            = coe
                                                                                MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                (coe
                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                   (let v26
                                                                                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                              (coe
                                                                                                 v4) in
                                                                                    coe
                                                                                      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                      (coe
                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                         (coe
                                                                                            v26)))) in
                                                                      let v27
                                                                            = coe
                                                                                MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                (coe v26) in
                                                                      let v28
                                                                            = subInt
                                                                                (coe v19)
                                                                                (coe
                                                                                   (1 ::
                                                                                      Integer)) in
                                                                      let v29
                                                                            = coe
                                                                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                v27
                                                                                (coe
                                                                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                   (coe v27)
                                                                                   (coe v28)
                                                                                   (coe v25))
                                                                                v25 in
                                                                      case coe v19 of
                                                                        0 -> coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                               (coe v27)
                                                                        1 -> coe v25
                                                                        _ -> coe v29)
                                                                     v24 in
                                                    let v25
                                                          = coe
                                                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                              (coe v23) in
                                                    let v26
                                                          = coe
                                                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                              v25
                                                              (coe
                                                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                 (coe v25) (coe v7) (coe v24))
                                                              v24 in
                                                    case coe v7 of
                                                      0 -> coe v24
                                                      _ -> coe v26)
                                                   (coe
                                                      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                                      (let v22
                                                             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                                 (coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                    (coe v4)) in
                                                       let v23
                                                             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                 (coe v22) in
                                                       let v24
                                                             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                 (coe v23) in
                                                       let v25
                                                             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                                 (coe v24) in
                                                       let v26
                                                             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                                 (coe v25) in
                                                       let v27
                                                             = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                                 (coe v26) in
                                                       let v28
                                                             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                                 (coe v27) in
                                                       coe
                                                         MAlonzo.Code.Algebra.Structures.du_setoid_164
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                            (coe v28)))
                                                      (coe
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8512''63''10214'_'10215'_606
                                                         v4
                                                         (let v22
                                                                = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                    (coe v4) in
                                                          let v23
                                                                = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                                                                    (coe v0) (coe v1)
                                                                    (coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                       (coe v4))
                                                                    (coe v15) (coe v20) (coe v7) in
                                                          let v24
                                                                = addInt
                                                                    (coe mulInt (coe v7) (coe v19))
                                                                    (coe v19) in
                                                          case coe v23 of
                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                              -> case coe v26 of
                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                     -> let v29
                                                                              = coe
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                                                  v22 v28 in
                                                                        coe
                                                                          seq (coe v29)
                                                                          (if coe v29
                                                                             then coe
                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                                                                                    (coe v17)
                                                                                    (coe
                                                                                       addInt
                                                                                       (coe
                                                                                          (1 ::
                                                                                             Integer))
                                                                                       (coe v24))
                                                                             else coe
                                                                                    MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                                    (coe
                                                                                       MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                       (coe
                                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                                                          (coe
                                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                                                             v23)
                                                                                          (coe v24))
                                                                                       (coe v17)))
                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                     -> coe
                                                                          MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                          (coe
                                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                             (coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                                                (coe
                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                                                   v23)
                                                                                (coe v24))
                                                                             (coe v17))
                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                            _ -> MAlonzo.RTE.mazUnreachableError)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                            (coe
                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                               (coe v11) (coe v8))))
                                                      (coe
                                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                            (coe v4))
                                                         (let v22
                                                                = coe
                                                                    MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                    (coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v4))) in
                                                          let v23
                                                                = coe
                                                                    MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                    (coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                       (coe v22)) in
                                                          let v24
                                                                = coe
                                                                    du_ρ'8242'_632 (coe v11)
                                                                    (coe v8) in
                                                          let v25
                                                                = addInt
                                                                    (coe mulInt (coe v7) (coe v19))
                                                                    (coe v19) in
                                                          let v26
                                                                = coe
                                                                    MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                    (coe v23) in
                                                          let v27
                                                                = subInt
                                                                    (coe v25)
                                                                    (coe (1 :: Integer)) in
                                                          let v28
                                                                = coe
                                                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                    v26
                                                                    (coe
                                                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                       (coe v26) (coe v27)
                                                                       (coe v24))
                                                                    v24 in
                                                          case coe v25 of
                                                            0 -> coe
                                                                   MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                   (coe v26)
                                                            1 -> coe v24
                                                            _ -> coe v28)
                                                         (let v22
                                                                = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                                                                    (coe v0) (coe v1)
                                                                    (coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                       (coe v4))
                                                                    (coe v15) (coe v20) (coe v7) in
                                                          let v23
                                                                = coe du_Ρ_634 (coe v11) (coe v8) in
                                                          case coe v22 of
                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v24 v25 v26
                                                              -> case coe v25 of
                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v27
                                                                     -> coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                          (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                             (coe v4))
                                                                          v27
                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v28 v29
                                                                     -> let v30
                                                                              = coe
                                                                                  MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                  (coe
                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                     (coe v26)
                                                                                     (coe v23)) in
                                                                        case coe v28 of
                                                                          MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v31 v32
                                                                            -> case coe v31 of
                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v33 v34
                                                                                   -> case coe
                                                                                             v33 of
                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v35
                                                                                          -> case coe
                                                                                                    v30 of
                                                                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v37 v38
                                                                                                 -> let v39
                                                                                                          = coe
                                                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                              (coe
                                                                                                                 v4)
                                                                                                              (coe
                                                                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                 (coe
                                                                                                                    v35)
                                                                                                                 (coe
                                                                                                                    v32))
                                                                                                              (coe
                                                                                                                 v30) in
                                                                                                    case coe
                                                                                                           v34 of
                                                                                                      0 -> coe
                                                                                                             v39
                                                                                                      _ -> coe
                                                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                (coe
                                                                                                                   v4))
                                                                                                             (let v40
                                                                                                                    = coe
                                                                                                                        MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                        (coe
                                                                                                                           MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                           (let v40
                                                                                                                                  = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                      (coe
                                                                                                                                         v4) in
                                                                                                                            coe
                                                                                                                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                              (coe
                                                                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                 (coe
                                                                                                                                    v40)))) in
                                                                                                              let v41
                                                                                                                    = coe
                                                                                                                        MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                        (coe
                                                                                                                           v40) in
                                                                                                              let v42
                                                                                                                    = subInt
                                                                                                                        (coe
                                                                                                                           v34)
                                                                                                                        (coe
                                                                                                                           (1 ::
                                                                                                                              Integer)) in
                                                                                                              let v43
                                                                                                                    = coe
                                                                                                                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                        v41
                                                                                                                        (coe
                                                                                                                           MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                           (coe
                                                                                                                              v41)
                                                                                                                           (coe
                                                                                                                              v42)
                                                                                                                           (coe
                                                                                                                              v37))
                                                                                                                        v37 in
                                                                                                              case coe
                                                                                                                     v34 of
                                                                                                                0 -> coe
                                                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                       (coe
                                                                                                                          v41)
                                                                                                                1 -> coe
                                                                                                                       v37
                                                                                                                _ -> coe
                                                                                                                       v43)
                                                                                                             v39
                                                                                               _ -> MAlonzo.RTE.mazUnreachableError
                                                                                        _ -> MAlonzo.RTE.mazUnreachableError
                                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                            _ -> MAlonzo.RTE.mazUnreachableError))
                                                      (let v22
                                                             = coe
                                                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                 (coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                    (coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v4))) in
                                                       let v23
                                                             = coe
                                                                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                 (coe
                                                                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                    (coe v22)) in
                                                       let v24
                                                             = let v24
                                                                     = let v24
                                                                             = coe
                                                                                 du_Ρ_634 (coe v11)
                                                                                 (coe v8) in
                                                                       case coe v20 of
                                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                                           -> case coe v26 of
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                                  -> coe
                                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                          (coe v4))
                                                                                       v28
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                                  -> let v31
                                                                                           = coe
                                                                                               MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                               (coe
                                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                  (coe
                                                                                                     v27)
                                                                                                  (coe
                                                                                                     v24)) in
                                                                                     case coe v29 of
                                                                                       MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v32 v33
                                                                                         -> case coe
                                                                                                   v32 of
                                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v34 v35
                                                                                                -> case coe
                                                                                                          v34 of
                                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v36
                                                                                                       -> case coe
                                                                                                                 v31 of
                                                                                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v38 v39
                                                                                                              -> let v40
                                                                                                                       = coe
                                                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                           (coe
                                                                                                                              v4)
                                                                                                                           (coe
                                                                                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                              (coe
                                                                                                                                 v36)
                                                                                                                              (coe
                                                                                                                                 v33))
                                                                                                                           (coe
                                                                                                                              v31) in
                                                                                                                 case coe
                                                                                                                        v35 of
                                                                                                                   0 -> coe
                                                                                                                          v40
                                                                                                                   _ -> coe
                                                                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                          (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                             (coe
                                                                                                                                v4))
                                                                                                                          (let v41
                                                                                                                                 = coe
                                                                                                                                     MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                     (coe
                                                                                                                                        MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                        (let v41
                                                                                                                                               = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                   (coe
                                                                                                                                                      v4) in
                                                                                                                                         coe
                                                                                                                                           MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                           (coe
                                                                                                                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                              (coe
                                                                                                                                                 v41)))) in
                                                                                                                           let v42
                                                                                                                                 = coe
                                                                                                                                     MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                     (coe
                                                                                                                                        v41) in
                                                                                                                           let v43
                                                                                                                                 = subInt
                                                                                                                                     (coe
                                                                                                                                        v35)
                                                                                                                                     (coe
                                                                                                                                        (1 ::
                                                                                                                                           Integer)) in
                                                                                                                           let v44
                                                                                                                                 = coe
                                                                                                                                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                     v42
                                                                                                                                     (coe
                                                                                                                                        MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                        (coe
                                                                                                                                           v42)
                                                                                                                                        (coe
                                                                                                                                           v43)
                                                                                                                                        (coe
                                                                                                                                           v38))
                                                                                                                                     v38 in
                                                                                                                           case coe
                                                                                                                                  v35 of
                                                                                                                             0 -> coe
                                                                                                                                    MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                    (coe
                                                                                                                                       v42)
                                                                                                                             1 -> coe
                                                                                                                                    v38
                                                                                                                             _ -> coe
                                                                                                                                    v44)
                                                                                                                          v40
                                                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                                         _ -> MAlonzo.RTE.mazUnreachableError in
                                                               let v25
                                                                     = coe
                                                                         du_ρ'8242'_632 (coe v11)
                                                                         (coe v8) in
                                                               case coe v19 of
                                                                 0 -> coe v24
                                                                 _ -> coe
                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                        (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                           (coe v4))
                                                                        (let v26
                                                                               = coe
                                                                                   MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                   (coe
                                                                                      MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                      (let v26
                                                                                             = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                 (coe
                                                                                                    v4) in
                                                                                       coe
                                                                                         MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                         (coe
                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                            (coe
                                                                                               v26)))) in
                                                                         let v27
                                                                               = coe
                                                                                   MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                   (coe v26) in
                                                                         let v28
                                                                               = subInt
                                                                                   (coe v19)
                                                                                   (coe
                                                                                      (1 ::
                                                                                         Integer)) in
                                                                         let v29
                                                                               = coe
                                                                                   MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                   v27
                                                                                   (coe
                                                                                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                      (coe v27)
                                                                                      (coe v28)
                                                                                      (coe v25))
                                                                                   v25 in
                                                                         case coe v19 of
                                                                           0 -> coe
                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                  (coe v27)
                                                                           1 -> coe v25
                                                                           _ -> coe v29)
                                                                        v24 in
                                                       let v25
                                                             = coe
                                                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                 (coe v23) in
                                                       let v26
                                                             = coe
                                                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                 v25
                                                                 (coe
                                                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                    (coe v25) (coe v7) (coe v24))
                                                                 v24 in
                                                       case coe v7 of
                                                         0 -> coe v24
                                                         _ -> coe v26)
                                                      (coe
                                                         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                                         (let v22
                                                                = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                                    (coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v4)) in
                                                          let v23
                                                                = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                    (coe v22) in
                                                          let v24
                                                                = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                    (coe v23) in
                                                          let v25
                                                                = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                                    (coe v24) in
                                                          let v26
                                                                = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                                    (coe v25) in
                                                          let v27
                                                                = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                                    (coe v26) in
                                                          let v28
                                                                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                                    (coe v27) in
                                                          coe
                                                            MAlonzo.Code.Algebra.Structures.du_setoid_164
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                               (coe v28)))
                                                         (coe
                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                               (coe v4))
                                                            (let v22
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                             (coe v4))) in
                                                             let v23
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                          (coe v22)) in
                                                             let v24
                                                                   = coe
                                                                       du_ρ'8242'_632 (coe v11)
                                                                       (coe v8) in
                                                             let v25
                                                                   = addInt
                                                                       (coe
                                                                          mulInt (coe v7) (coe v19))
                                                                       (coe v19) in
                                                             let v26
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                       (coe v23) in
                                                             let v27
                                                                   = subInt
                                                                       (coe v25)
                                                                       (coe (1 :: Integer)) in
                                                             let v28
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                       v26
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                          (coe v26) (coe v27)
                                                                          (coe v24))
                                                                       v24 in
                                                             case coe v25 of
                                                               0 -> coe
                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                      (coe v26)
                                                               1 -> coe v24
                                                               _ -> coe v28)
                                                            (let v22
                                                                   = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                                                                       (coe v0) (coe v1)
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                          (coe v4))
                                                                       (coe v15) (coe v20)
                                                                       (coe v7) in
                                                             let v23
                                                                   = coe
                                                                       du_Ρ_634 (coe v11)
                                                                       (coe v8) in
                                                             case coe v22 of
                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v24 v25 v26
                                                                 -> case coe v25 of
                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v27
                                                                        -> coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                (coe v4))
                                                                             v27
                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v28 v29
                                                                        -> let v30
                                                                                 = coe
                                                                                     MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                     (coe
                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                        (coe v26)
                                                                                        (coe
                                                                                           v23)) in
                                                                           case coe v28 of
                                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v31 v32
                                                                               -> case coe v31 of
                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v33 v34
                                                                                      -> case coe
                                                                                                v33 of
                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v35
                                                                                             -> case coe
                                                                                                       v30 of
                                                                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v37 v38
                                                                                                    -> let v39
                                                                                                             = coe
                                                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                 (coe
                                                                                                                    v4)
                                                                                                                 (coe
                                                                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                    (coe
                                                                                                                       v35)
                                                                                                                    (coe
                                                                                                                       v32))
                                                                                                                 (coe
                                                                                                                    v30) in
                                                                                                       case coe
                                                                                                              v34 of
                                                                                                         0 -> coe
                                                                                                                v39
                                                                                                         _ -> coe
                                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                   (coe
                                                                                                                      v4))
                                                                                                                (let v40
                                                                                                                       = coe
                                                                                                                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                           (coe
                                                                                                                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                              (let v40
                                                                                                                                     = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                         (coe
                                                                                                                                            v4) in
                                                                                                                               coe
                                                                                                                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                 (coe
                                                                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                    (coe
                                                                                                                                       v40)))) in
                                                                                                                 let v41
                                                                                                                       = coe
                                                                                                                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                           (coe
                                                                                                                              v40) in
                                                                                                                 let v42
                                                                                                                       = subInt
                                                                                                                           (coe
                                                                                                                              v34)
                                                                                                                           (coe
                                                                                                                              (1 ::
                                                                                                                                 Integer)) in
                                                                                                                 let v43
                                                                                                                       = coe
                                                                                                                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                           v41
                                                                                                                           (coe
                                                                                                                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                              (coe
                                                                                                                                 v41)
                                                                                                                              (coe
                                                                                                                                 v42)
                                                                                                                              (coe
                                                                                                                                 v37))
                                                                                                                           v37 in
                                                                                                                 case coe
                                                                                                                        v34 of
                                                                                                                   0 -> coe
                                                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                          (coe
                                                                                                                             v41)
                                                                                                                   1 -> coe
                                                                                                                          v37
                                                                                                                   _ -> coe
                                                                                                                          v43)
                                                                                                                v39
                                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                                               _ -> MAlonzo.RTE.mazUnreachableError))
                                                         (coe
                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                               (coe v4))
                                                            (let v22
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                             (coe v4))) in
                                                             let v23
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                          (coe v22)) in
                                                             let v24
                                                                   = coe
                                                                       du_ρ'8242'_632 (coe v11)
                                                                       (coe v8) in
                                                             let v25
                                                                   = addInt
                                                                       (coe
                                                                          mulInt (coe v7) (coe v19))
                                                                       (coe v19) in
                                                             let v26
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                       (coe v23) in
                                                             let v27
                                                                   = subInt
                                                                       (coe v25)
                                                                       (coe (1 :: Integer)) in
                                                             let v28
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                       v26
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                          (coe v26) (coe v27)
                                                                          (coe v24))
                                                                       v24 in
                                                             case coe v25 of
                                                               0 -> coe
                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                      (coe v26)
                                                               1 -> coe v24
                                                               _ -> coe v28)
                                                            (let v22
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                             (coe v4))) in
                                                             let v23
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                          (coe v22)) in
                                                             let v24
                                                                   = let v24
                                                                           = coe
                                                                               du_Ρ_634 (coe v11)
                                                                               (coe v8) in
                                                                     case coe v20 of
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                                         -> case coe v26 of
                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                                -> coe
                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                        (coe v4))
                                                                                     v28
                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                                -> let v31
                                                                                         = coe
                                                                                             MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                             (coe
                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                (coe
                                                                                                   v27)
                                                                                                (coe
                                                                                                   v24)) in
                                                                                   case coe v29 of
                                                                                     MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v32 v33
                                                                                       -> case coe
                                                                                                 v32 of
                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v34 v35
                                                                                              -> case coe
                                                                                                        v34 of
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v36
                                                                                                     -> case coe
                                                                                                               v31 of
                                                                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v38 v39
                                                                                                            -> let v40
                                                                                                                     = coe
                                                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                         (coe
                                                                                                                            v4)
                                                                                                                         (coe
                                                                                                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                            (coe
                                                                                                                               v36)
                                                                                                                            (coe
                                                                                                                               v33))
                                                                                                                         (coe
                                                                                                                            v31) in
                                                                                                               case coe
                                                                                                                      v35 of
                                                                                                                 0 -> coe
                                                                                                                        v40
                                                                                                                 _ -> coe
                                                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                        (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                           (coe
                                                                                                                              v4))
                                                                                                                        (let v41
                                                                                                                               = coe
                                                                                                                                   MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                   (coe
                                                                                                                                      MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                      (let v41
                                                                                                                                             = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                 (coe
                                                                                                                                                    v4) in
                                                                                                                                       coe
                                                                                                                                         MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                         (coe
                                                                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                            (coe
                                                                                                                                               v41)))) in
                                                                                                                         let v42
                                                                                                                               = coe
                                                                                                                                   MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                   (coe
                                                                                                                                      v41) in
                                                                                                                         let v43
                                                                                                                               = subInt
                                                                                                                                   (coe
                                                                                                                                      v35)
                                                                                                                                   (coe
                                                                                                                                      (1 ::
                                                                                                                                         Integer)) in
                                                                                                                         let v44
                                                                                                                               = coe
                                                                                                                                   MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                   v42
                                                                                                                                   (coe
                                                                                                                                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                      (coe
                                                                                                                                         v42)
                                                                                                                                      (coe
                                                                                                                                         v43)
                                                                                                                                      (coe
                                                                                                                                         v38))
                                                                                                                                   v38 in
                                                                                                                         case coe
                                                                                                                                v35 of
                                                                                                                           0 -> coe
                                                                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                  (coe
                                                                                                                                     v42)
                                                                                                                           1 -> coe
                                                                                                                                  v38
                                                                                                                           _ -> coe
                                                                                                                                  v44)
                                                                                                                        v40
                                                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                                       _ -> MAlonzo.RTE.mazUnreachableError in
                                                             let v25
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                       (coe v23) in
                                                             let v26
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                       v25
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                          (coe v25) (coe v7)
                                                                          (coe v24))
                                                                       v24 in
                                                             case coe v7 of
                                                               0 -> coe v24
                                                               _ -> coe v26))
                                                         (let v22
                                                                = coe
                                                                    MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                    (coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v4))) in
                                                          let v23
                                                                = coe
                                                                    MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                    (coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                       (coe v22)) in
                                                          let v24
                                                                = let v24
                                                                        = let v24
                                                                                = coe
                                                                                    du_Ρ_634
                                                                                    (coe v11)
                                                                                    (coe v8) in
                                                                          case coe v20 of
                                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                                              -> case coe v26 of
                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                                     -> coe
                                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                          (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                             (coe
                                                                                                v4))
                                                                                          v28
                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                                     -> let v31
                                                                                              = coe
                                                                                                  MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                     (coe
                                                                                                        v27)
                                                                                                     (coe
                                                                                                        v24)) in
                                                                                        case coe
                                                                                               v29 of
                                                                                          MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v32 v33
                                                                                            -> case coe
                                                                                                      v32 of
                                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v34 v35
                                                                                                   -> case coe
                                                                                                             v34 of
                                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v36
                                                                                                          -> case coe
                                                                                                                    v31 of
                                                                                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v38 v39
                                                                                                                 -> let v40
                                                                                                                          = coe
                                                                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                              (coe
                                                                                                                                 v4)
                                                                                                                              (coe
                                                                                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                                 (coe
                                                                                                                                    v36)
                                                                                                                                 (coe
                                                                                                                                    v33))
                                                                                                                              (coe
                                                                                                                                 v31) in
                                                                                                                    case coe
                                                                                                                           v35 of
                                                                                                                      0 -> coe
                                                                                                                             v40
                                                                                                                      _ -> coe
                                                                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                (coe
                                                                                                                                   v4))
                                                                                                                             (let v41
                                                                                                                                    = coe
                                                                                                                                        MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                        (coe
                                                                                                                                           MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                           (let v41
                                                                                                                                                  = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                      (coe
                                                                                                                                                         v4) in
                                                                                                                                            coe
                                                                                                                                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                              (coe
                                                                                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                                 (coe
                                                                                                                                                    v41)))) in
                                                                                                                              let v42
                                                                                                                                    = coe
                                                                                                                                        MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                        (coe
                                                                                                                                           v41) in
                                                                                                                              let v43
                                                                                                                                    = subInt
                                                                                                                                        (coe
                                                                                                                                           v35)
                                                                                                                                        (coe
                                                                                                                                           (1 ::
                                                                                                                                              Integer)) in
                                                                                                                              let v44
                                                                                                                                    = coe
                                                                                                                                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                        v42
                                                                                                                                        (coe
                                                                                                                                           MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                           (coe
                                                                                                                                              v42)
                                                                                                                                           (coe
                                                                                                                                              v43)
                                                                                                                                           (coe
                                                                                                                                              v38))
                                                                                                                                        v38 in
                                                                                                                              case coe
                                                                                                                                     v35 of
                                                                                                                                0 -> coe
                                                                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                       (coe
                                                                                                                                          v42)
                                                                                                                                1 -> coe
                                                                                                                                       v38
                                                                                                                                _ -> coe
                                                                                                                                       v44)
                                                                                                                             v40
                                                                                                               _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                        _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            _ -> MAlonzo.RTE.mazUnreachableError in
                                                                  let v25
                                                                        = coe
                                                                            du_ρ'8242'_632 (coe v11)
                                                                            (coe v8) in
                                                                  case coe v19 of
                                                                    0 -> coe v24
                                                                    _ -> coe
                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                              (coe v4))
                                                                           (let v26
                                                                                  = coe
                                                                                      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                      (coe
                                                                                         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                         (let v26
                                                                                                = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                    (coe
                                                                                                       v4) in
                                                                                          coe
                                                                                            MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                            (coe
                                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                               (coe
                                                                                                  v26)))) in
                                                                            let v27
                                                                                  = coe
                                                                                      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                      (coe v26) in
                                                                            let v28
                                                                                  = subInt
                                                                                      (coe v19)
                                                                                      (coe
                                                                                         (1 ::
                                                                                            Integer)) in
                                                                            let v29
                                                                                  = coe
                                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                      v27
                                                                                      (coe
                                                                                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                         (coe v27)
                                                                                         (coe v28)
                                                                                         (coe v25))
                                                                                      v25 in
                                                                            case coe v19 of
                                                                              0 -> coe
                                                                                     MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                     (coe v27)
                                                                              1 -> coe v25
                                                                              _ -> coe v29)
                                                                           v24 in
                                                          let v25
                                                                = coe
                                                                    MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                    (coe v23) in
                                                          let v26
                                                                = coe
                                                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                    v25
                                                                    (coe
                                                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                       (coe v25) (coe v7) (coe v24))
                                                                    v24 in
                                                          case coe v7 of
                                                            0 -> coe v24
                                                            _ -> coe v26)
                                                         (coe
                                                            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                                            (let v22
                                                                   = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v4)) in
                                                             let v23
                                                                   = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                       (coe v22) in
                                                             let v24
                                                                   = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                       (coe v23) in
                                                             let v25
                                                                   = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                                       (coe v24) in
                                                             let v26
                                                                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                                       (coe v25) in
                                                             let v27
                                                                   = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                                       (coe v26) in
                                                             let v28
                                                                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                                       (coe v27) in
                                                             coe
                                                               MAlonzo.Code.Algebra.Structures.du_setoid_164
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                                  (coe v28)))
                                                            (coe
                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                  (coe v4))
                                                               (let v22
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                             (coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                (coe v4))) in
                                                                let v23
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                          (coe
                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                             (coe v22)) in
                                                                let v24
                                                                      = coe
                                                                          du_ρ'8242'_632 (coe v11)
                                                                          (coe v8) in
                                                                let v25
                                                                      = addInt
                                                                          (coe
                                                                             mulInt (coe v7)
                                                                             (coe v19))
                                                                          (coe v19) in
                                                                let v26
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                          (coe v23) in
                                                                let v27
                                                                      = subInt
                                                                          (coe v25)
                                                                          (coe (1 :: Integer)) in
                                                                let v28
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                          v26
                                                                          (coe
                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                             (coe v26) (coe v27)
                                                                             (coe v24))
                                                                          v24 in
                                                                case coe v25 of
                                                                  0 -> coe
                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                         (coe v26)
                                                                  1 -> coe v24
                                                                  _ -> coe v28)
                                                               (let v22
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                             (coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                (coe v4))) in
                                                                let v23
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                          (coe
                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                             (coe v22)) in
                                                                let v24
                                                                      = let v24
                                                                              = coe
                                                                                  du_Ρ_634 (coe v11)
                                                                                  (coe v8) in
                                                                        case coe v20 of
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                                            -> case coe v26 of
                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                                   -> coe
                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                        (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                           (coe v4))
                                                                                        v28
                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                                   -> let v31
                                                                                            = coe
                                                                                                MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                   (coe
                                                                                                      v27)
                                                                                                   (coe
                                                                                                      v24)) in
                                                                                      case coe
                                                                                             v29 of
                                                                                        MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v32 v33
                                                                                          -> case coe
                                                                                                    v32 of
                                                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v34 v35
                                                                                                 -> case coe
                                                                                                           v34 of
                                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v36
                                                                                                        -> case coe
                                                                                                                  v31 of
                                                                                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v38 v39
                                                                                                               -> let v40
                                                                                                                        = coe
                                                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                            (coe
                                                                                                                               v4)
                                                                                                                            (coe
                                                                                                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                               (coe
                                                                                                                                  v36)
                                                                                                                               (coe
                                                                                                                                  v33))
                                                                                                                            (coe
                                                                                                                               v31) in
                                                                                                                  case coe
                                                                                                                         v35 of
                                                                                                                    0 -> coe
                                                                                                                           v40
                                                                                                                    _ -> coe
                                                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                              (coe
                                                                                                                                 v4))
                                                                                                                           (let v41
                                                                                                                                  = coe
                                                                                                                                      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                      (coe
                                                                                                                                         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                         (let v41
                                                                                                                                                = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                    (coe
                                                                                                                                                       v4) in
                                                                                                                                          coe
                                                                                                                                            MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                            (coe
                                                                                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                               (coe
                                                                                                                                                  v41)))) in
                                                                                                                            let v42
                                                                                                                                  = coe
                                                                                                                                      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                      (coe
                                                                                                                                         v41) in
                                                                                                                            let v43
                                                                                                                                  = subInt
                                                                                                                                      (coe
                                                                                                                                         v35)
                                                                                                                                      (coe
                                                                                                                                         (1 ::
                                                                                                                                            Integer)) in
                                                                                                                            let v44
                                                                                                                                  = coe
                                                                                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                      v42
                                                                                                                                      (coe
                                                                                                                                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                         (coe
                                                                                                                                            v42)
                                                                                                                                         (coe
                                                                                                                                            v43)
                                                                                                                                         (coe
                                                                                                                                            v38))
                                                                                                                                      v38 in
                                                                                                                            case coe
                                                                                                                                   v35 of
                                                                                                                              0 -> coe
                                                                                                                                     MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                     (coe
                                                                                                                                        v42)
                                                                                                                              1 -> coe
                                                                                                                                     v38
                                                                                                                              _ -> coe
                                                                                                                                     v44)
                                                                                                                           v40
                                                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                                                                               _ -> MAlonzo.RTE.mazUnreachableError
                                                                                        _ -> MAlonzo.RTE.mazUnreachableError
                                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                                          _ -> MAlonzo.RTE.mazUnreachableError in
                                                                let v25
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                          (coe v23) in
                                                                let v26
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                          v25
                                                                          (coe
                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                             (coe v25) (coe v7)
                                                                             (coe v24))
                                                                          v24 in
                                                                case coe v7 of
                                                                  0 -> coe v24
                                                                  _ -> coe v26))
                                                            (let v22
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                             (coe v4))) in
                                                             let v23
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                          (coe v22)) in
                                                             let v24
                                                                   = let v24
                                                                           = let v24
                                                                                   = coe
                                                                                       du_Ρ_634
                                                                                       (coe v11)
                                                                                       (coe v8) in
                                                                             case coe v20 of
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                                                 -> case coe v26 of
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                                        -> coe
                                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                                (coe
                                                                                                   v4))
                                                                                             v28
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                                        -> let v31
                                                                                                 = coe
                                                                                                     MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                        (coe
                                                                                                           v27)
                                                                                                        (coe
                                                                                                           v24)) in
                                                                                           case coe
                                                                                                  v29 of
                                                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v32 v33
                                                                                               -> case coe
                                                                                                         v32 of
                                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v34 v35
                                                                                                      -> case coe
                                                                                                                v34 of
                                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v36
                                                                                                             -> case coe
                                                                                                                       v31 of
                                                                                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v38 v39
                                                                                                                    -> let v40
                                                                                                                             = coe
                                                                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                                 (coe
                                                                                                                                    v4)
                                                                                                                                 (coe
                                                                                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                                    (coe
                                                                                                                                       v36)
                                                                                                                                    (coe
                                                                                                                                       v33))
                                                                                                                                 (coe
                                                                                                                                    v31) in
                                                                                                                       case coe
                                                                                                                              v35 of
                                                                                                                         0 -> coe
                                                                                                                                v40
                                                                                                                         _ -> coe
                                                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                                (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                   (coe
                                                                                                                                      v4))
                                                                                                                                (let v41
                                                                                                                                       = coe
                                                                                                                                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                           (coe
                                                                                                                                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                              (let v41
                                                                                                                                                     = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                         (coe
                                                                                                                                                            v4) in
                                                                                                                                               coe
                                                                                                                                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                                 (coe
                                                                                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                                    (coe
                                                                                                                                                       v41)))) in
                                                                                                                                 let v42
                                                                                                                                       = coe
                                                                                                                                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                           (coe
                                                                                                                                              v41) in
                                                                                                                                 let v43
                                                                                                                                       = subInt
                                                                                                                                           (coe
                                                                                                                                              v35)
                                                                                                                                           (coe
                                                                                                                                              (1 ::
                                                                                                                                                 Integer)) in
                                                                                                                                 let v44
                                                                                                                                       = coe
                                                                                                                                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                           v42
                                                                                                                                           (coe
                                                                                                                                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                              (coe
                                                                                                                                                 v42)
                                                                                                                                              (coe
                                                                                                                                                 v43)
                                                                                                                                              (coe
                                                                                                                                                 v38))
                                                                                                                                           v38 in
                                                                                                                                 case coe
                                                                                                                                        v35 of
                                                                                                                                   0 -> coe
                                                                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                          (coe
                                                                                                                                             v42)
                                                                                                                                   1 -> coe
                                                                                                                                          v38
                                                                                                                                   _ -> coe
                                                                                                                                          v44)
                                                                                                                                v40
                                                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                                                               _ -> MAlonzo.RTE.mazUnreachableError in
                                                                     let v25
                                                                           = coe
                                                                               du_ρ'8242'_632
                                                                               (coe v11) (coe v8) in
                                                                     case coe v19 of
                                                                       0 -> coe v24
                                                                       _ -> coe
                                                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                 (coe v4))
                                                                              (let v26
                                                                                     = coe
                                                                                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                         (coe
                                                                                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                            (let v26
                                                                                                   = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                       (coe
                                                                                                          v4) in
                                                                                             coe
                                                                                               MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                               (coe
                                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                  (coe
                                                                                                     v26)))) in
                                                                               let v27
                                                                                     = coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                         (coe
                                                                                            v26) in
                                                                               let v28
                                                                                     = subInt
                                                                                         (coe v19)
                                                                                         (coe
                                                                                            (1 ::
                                                                                               Integer)) in
                                                                               let v29
                                                                                     = coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                         v27
                                                                                         (coe
                                                                                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                            (coe
                                                                                               v27)
                                                                                            (coe
                                                                                               v28)
                                                                                            (coe
                                                                                               v25))
                                                                                         v25 in
                                                                               case coe v19 of
                                                                                 0 -> coe
                                                                                        MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                        (coe v27)
                                                                                 1 -> coe v25
                                                                                 _ -> coe v29)
                                                                              v24 in
                                                             let v25
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                       (coe v23) in
                                                             let v26
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                       v25
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                          (coe v25) (coe v7)
                                                                          (coe v24))
                                                                       v24 in
                                                             case coe v7 of
                                                               0 -> coe v24
                                                               _ -> coe v26)
                                                            (let v22
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                             (coe v4))) in
                                                             let v23
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                          (coe v22)) in
                                                             let v24
                                                                   = let v24
                                                                           = let v24
                                                                                   = coe
                                                                                       du_Ρ_634
                                                                                       (coe v11)
                                                                                       (coe v8) in
                                                                             case coe v20 of
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                                                 -> case coe v26 of
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                                        -> coe
                                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                                (coe
                                                                                                   v4))
                                                                                             v28
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                                        -> let v31
                                                                                                 = coe
                                                                                                     MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                        (coe
                                                                                                           v27)
                                                                                                        (coe
                                                                                                           v24)) in
                                                                                           case coe
                                                                                                  v29 of
                                                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v32 v33
                                                                                               -> case coe
                                                                                                         v32 of
                                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v34 v35
                                                                                                      -> case coe
                                                                                                                v34 of
                                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v36
                                                                                                             -> case coe
                                                                                                                       v31 of
                                                                                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v38 v39
                                                                                                                    -> let v40
                                                                                                                             = coe
                                                                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                                 (coe
                                                                                                                                    v4)
                                                                                                                                 (coe
                                                                                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                                    (coe
                                                                                                                                       v36)
                                                                                                                                    (coe
                                                                                                                                       v33))
                                                                                                                                 (coe
                                                                                                                                    v31) in
                                                                                                                       case coe
                                                                                                                              v35 of
                                                                                                                         0 -> coe
                                                                                                                                v40
                                                                                                                         _ -> coe
                                                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                                (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                   (coe
                                                                                                                                      v4))
                                                                                                                                (let v41
                                                                                                                                       = coe
                                                                                                                                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                           (coe
                                                                                                                                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                              (let v41
                                                                                                                                                     = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                         (coe
                                                                                                                                                            v4) in
                                                                                                                                               coe
                                                                                                                                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                                 (coe
                                                                                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                                    (coe
                                                                                                                                                       v41)))) in
                                                                                                                                 let v42
                                                                                                                                       = coe
                                                                                                                                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                           (coe
                                                                                                                                              v41) in
                                                                                                                                 let v43
                                                                                                                                       = subInt
                                                                                                                                           (coe
                                                                                                                                              v35)
                                                                                                                                           (coe
                                                                                                                                              (1 ::
                                                                                                                                                 Integer)) in
                                                                                                                                 let v44
                                                                                                                                       = coe
                                                                                                                                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                           v42
                                                                                                                                           (coe
                                                                                                                                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                              (coe
                                                                                                                                                 v42)
                                                                                                                                              (coe
                                                                                                                                                 v43)
                                                                                                                                              (coe
                                                                                                                                                 v38))
                                                                                                                                           v38 in
                                                                                                                                 case coe
                                                                                                                                        v35 of
                                                                                                                                   0 -> coe
                                                                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                          (coe
                                                                                                                                             v42)
                                                                                                                                   1 -> coe
                                                                                                                                          v38
                                                                                                                                   _ -> coe
                                                                                                                                          v44)
                                                                                                                                v40
                                                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                                                               _ -> MAlonzo.RTE.mazUnreachableError in
                                                                     let v25
                                                                           = coe
                                                                               du_ρ'8242'_632
                                                                               (coe v11) (coe v8) in
                                                                     case coe v19 of
                                                                       0 -> coe v24
                                                                       _ -> coe
                                                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                 (coe v4))
                                                                              (let v26
                                                                                     = coe
                                                                                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                         (coe
                                                                                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                            (let v26
                                                                                                   = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                       (coe
                                                                                                          v4) in
                                                                                             coe
                                                                                               MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                               (coe
                                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                  (coe
                                                                                                     v26)))) in
                                                                               let v27
                                                                                     = coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                         (coe
                                                                                            v26) in
                                                                               let v28
                                                                                     = subInt
                                                                                         (coe v19)
                                                                                         (coe
                                                                                            (1 ::
                                                                                               Integer)) in
                                                                               let v29
                                                                                     = coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                         v27
                                                                                         (coe
                                                                                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                            (coe
                                                                                               v27)
                                                                                            (coe
                                                                                               v28)
                                                                                            (coe
                                                                                               v25))
                                                                                         v25 in
                                                                               case coe v19 of
                                                                                 0 -> coe
                                                                                        MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                        (coe v27)
                                                                                 1 -> coe v25
                                                                                 _ -> coe v29)
                                                                              v24 in
                                                             let v25
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                       (coe v23) in
                                                             let v26
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                       v25
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                          (coe v25) (coe v7)
                                                                          (coe v24))
                                                                       v24 in
                                                             case coe v7 of
                                                               0 -> coe v24
                                                               _ -> coe v26)
                                                            (let v22
                                                                   = let v22
                                                                           = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                                               (coe
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v4)) in
                                                                     let v23
                                                                           = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                               (coe v22) in
                                                                     let v24
                                                                           = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                               (coe v23) in
                                                                     let v25
                                                                           = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                                               (coe v24) in
                                                                     let v26
                                                                           = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                                               (coe v25) in
                                                                     let v27
                                                                           = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                                               (coe v26) in
                                                                     let v28
                                                                           = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                                               (coe v27) in
                                                                     coe
                                                                       MAlonzo.Code.Algebra.Structures.du_setoid_164
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                                          (coe v28)) in
                                                             coe
                                                               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                                               (coe
                                                                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                                  (coe
                                                                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                                     (coe v22)))
                                                               (let v23
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                             (coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                (coe v4))) in
                                                                let v24
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                          (coe
                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                             (coe v23)) in
                                                                let v25
                                                                      = let v25
                                                                              = let v25
                                                                                      = coe
                                                                                          du_Ρ_634
                                                                                          (coe v11)
                                                                                          (coe
                                                                                             v8) in
                                                                                case coe v20 of
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v26 v27 v28
                                                                                    -> case coe
                                                                                              v27 of
                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v29
                                                                                           -> coe
                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                                (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                                   (coe
                                                                                                      v4))
                                                                                                v29
                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v30 v31
                                                                                           -> let v32
                                                                                                    = coe
                                                                                                        MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                           (coe
                                                                                                              v28)
                                                                                                           (coe
                                                                                                              v25)) in
                                                                                              case coe
                                                                                                     v30 of
                                                                                                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v33 v34
                                                                                                  -> case coe
                                                                                                            v33 of
                                                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v35 v36
                                                                                                         -> case coe
                                                                                                                   v35 of
                                                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v37
                                                                                                                -> case coe
                                                                                                                          v32 of
                                                                                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v39 v40
                                                                                                                       -> let v41
                                                                                                                                = coe
                                                                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                                    (coe
                                                                                                                                       v4)
                                                                                                                                    (coe
                                                                                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                                       (coe
                                                                                                                                          v37)
                                                                                                                                       (coe
                                                                                                                                          v34))
                                                                                                                                    (coe
                                                                                                                                       v32) in
                                                                                                                          case coe
                                                                                                                                 v36 of
                                                                                                                            0 -> coe
                                                                                                                                   v41
                                                                                                                            _ -> coe
                                                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                      (coe
                                                                                                                                         v4))
                                                                                                                                   (let v42
                                                                                                                                          = coe
                                                                                                                                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                              (coe
                                                                                                                                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                                 (let v42
                                                                                                                                                        = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                            (coe
                                                                                                                                                               v4) in
                                                                                                                                                  coe
                                                                                                                                                    MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                                    (coe
                                                                                                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                                       (coe
                                                                                                                                                          v42)))) in
                                                                                                                                    let v43
                                                                                                                                          = coe
                                                                                                                                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                              (coe
                                                                                                                                                 v42) in
                                                                                                                                    let v44
                                                                                                                                          = subInt
                                                                                                                                              (coe
                                                                                                                                                 v36)
                                                                                                                                              (coe
                                                                                                                                                 (1 ::
                                                                                                                                                    Integer)) in
                                                                                                                                    let v45
                                                                                                                                          = coe
                                                                                                                                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                              v43
                                                                                                                                              (coe
                                                                                                                                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                                 (coe
                                                                                                                                                    v43)
                                                                                                                                                 (coe
                                                                                                                                                    v44)
                                                                                                                                                 (coe
                                                                                                                                                    v39))
                                                                                                                                              v39 in
                                                                                                                                    case coe
                                                                                                                                           v36 of
                                                                                                                                      0 -> coe
                                                                                                                                             MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                             (coe
                                                                                                                                                v43)
                                                                                                                                      1 -> coe
                                                                                                                                             v39
                                                                                                                                      _ -> coe
                                                                                                                                             v45)
                                                                                                                                   v41
                                                                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                                                  _ -> MAlonzo.RTE.mazUnreachableError in
                                                                        let v26
                                                                              = coe
                                                                                  du_ρ'8242'_632
                                                                                  (coe v11)
                                                                                  (coe v8) in
                                                                        case coe v19 of
                                                                          0 -> coe v25
                                                                          _ -> coe
                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                    (coe v4))
                                                                                 (let v27
                                                                                        = coe
                                                                                            MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                            (coe
                                                                                               MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                               (let v27
                                                                                                      = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                          (coe
                                                                                                             v4) in
                                                                                                coe
                                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                     (coe
                                                                                                        v27)))) in
                                                                                  let v28
                                                                                        = coe
                                                                                            MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                            (coe
                                                                                               v27) in
                                                                                  let v29
                                                                                        = subInt
                                                                                            (coe
                                                                                               v19)
                                                                                            (coe
                                                                                               (1 ::
                                                                                                  Integer)) in
                                                                                  let v30
                                                                                        = coe
                                                                                            MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                            v28
                                                                                            (coe
                                                                                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                               (coe
                                                                                                  v28)
                                                                                               (coe
                                                                                                  v29)
                                                                                               (coe
                                                                                                  v26))
                                                                                            v26 in
                                                                                  case coe v19 of
                                                                                    0 -> coe
                                                                                           MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                           (coe v28)
                                                                                    1 -> coe v26
                                                                                    _ -> coe v30)
                                                                                 v25 in
                                                                let v26
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                          (coe v24) in
                                                                let v27
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                          v26
                                                                          (coe
                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                             (coe v26) (coe v7)
                                                                             (coe v25))
                                                                          v25 in
                                                                case coe v7 of
                                                                  0 -> coe v25
                                                                  _ -> coe v27))
                                                            (coe
                                                               du_rearrange_638 (coe v4) (coe v20)
                                                               (coe v11) (coe v7) (coe v8)
                                                               (coe v19)))
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                                            (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                  (coe
                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                     (coe
                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                                        (coe
                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                           (coe v4))))))
                                                            (let v22
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                             (coe v4))) in
                                                             let v23
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                          (coe v22)) in
                                                             let v24
                                                                   = coe
                                                                       du_ρ'8242'_632 (coe v11)
                                                                       (coe v8) in
                                                             let v25
                                                                   = addInt
                                                                       (coe
                                                                          mulInt (coe v7) (coe v19))
                                                                       (coe v19) in
                                                             let v26
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                       (coe v23) in
                                                             let v27
                                                                   = subInt
                                                                       (coe v25)
                                                                       (coe (1 :: Integer)) in
                                                             let v28
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                       v26
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                          (coe v26) (coe v27)
                                                                          (coe v24))
                                                                       v24 in
                                                             case coe v25 of
                                                               0 -> coe
                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                      (coe v26)
                                                               1 -> coe v24
                                                               _ -> coe v28)
                                                            (let v22
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                             (coe v4))) in
                                                             let v23
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                          (coe v22)) in
                                                             let v24
                                                                   = coe
                                                                       du_ρ'8242'_632 (coe v11)
                                                                       (coe v8) in
                                                             let v25
                                                                   = addInt
                                                                       (coe
                                                                          mulInt (coe v7) (coe v19))
                                                                       (coe v19) in
                                                             let v26
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                       (coe v23) in
                                                             let v27
                                                                   = subInt
                                                                       (coe v25)
                                                                       (coe (1 :: Integer)) in
                                                             let v28
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                       v26
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                          (coe v26) (coe v27)
                                                                          (coe v24))
                                                                       v24 in
                                                             case coe v25 of
                                                               0 -> coe
                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                      (coe v26)
                                                               1 -> coe v24
                                                               _ -> coe v28)
                                                            (let v22
                                                                   = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                                                                       (coe v0) (coe v1)
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                          (coe v4))
                                                                       (coe v15) (coe v20)
                                                                       (coe v7) in
                                                             let v23
                                                                   = coe
                                                                       du_Ρ_634 (coe v11)
                                                                       (coe v8) in
                                                             case coe v22 of
                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v24 v25 v26
                                                                 -> case coe v25 of
                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v27
                                                                        -> coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                (coe v4))
                                                                             v27
                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v28 v29
                                                                        -> let v30
                                                                                 = coe
                                                                                     MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                     (coe
                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                        (coe v26)
                                                                                        (coe
                                                                                           v23)) in
                                                                           case coe v28 of
                                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v31 v32
                                                                               -> case coe v31 of
                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v33 v34
                                                                                      -> case coe
                                                                                                v33 of
                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v35
                                                                                             -> case coe
                                                                                                       v30 of
                                                                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v37 v38
                                                                                                    -> let v39
                                                                                                             = coe
                                                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                 (coe
                                                                                                                    v4)
                                                                                                                 (coe
                                                                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                    (coe
                                                                                                                       v35)
                                                                                                                    (coe
                                                                                                                       v32))
                                                                                                                 (coe
                                                                                                                    v30) in
                                                                                                       case coe
                                                                                                              v34 of
                                                                                                         0 -> coe
                                                                                                                v39
                                                                                                         _ -> coe
                                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                   (coe
                                                                                                                      v4))
                                                                                                                (let v40
                                                                                                                       = coe
                                                                                                                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                           (coe
                                                                                                                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                              (let v40
                                                                                                                                     = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                         (coe
                                                                                                                                            v4) in
                                                                                                                               coe
                                                                                                                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                 (coe
                                                                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                    (coe
                                                                                                                                       v40)))) in
                                                                                                                 let v41
                                                                                                                       = coe
                                                                                                                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                           (coe
                                                                                                                              v40) in
                                                                                                                 let v42
                                                                                                                       = subInt
                                                                                                                           (coe
                                                                                                                              v34)
                                                                                                                           (coe
                                                                                                                              (1 ::
                                                                                                                                 Integer)) in
                                                                                                                 let v43
                                                                                                                       = coe
                                                                                                                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                           v41
                                                                                                                           (coe
                                                                                                                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                              (coe
                                                                                                                                 v41)
                                                                                                                              (coe
                                                                                                                                 v42)
                                                                                                                              (coe
                                                                                                                                 v37))
                                                                                                                           v37 in
                                                                                                                 case coe
                                                                                                                        v34 of
                                                                                                                   0 -> coe
                                                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                          (coe
                                                                                                                             v41)
                                                                                                                   1 -> coe
                                                                                                                          v37
                                                                                                                   _ -> coe
                                                                                                                          v43)
                                                                                                                v39
                                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                                               _ -> MAlonzo.RTE.mazUnreachableError)
                                                            (let v22
                                                                   = coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                       (coe
                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v4)) in
                                                             let v23
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                       (coe v22) in
                                                             let v24
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                          (coe v23)) in
                                                             let v25
                                                                   = let v25
                                                                           = coe
                                                                               du_Ρ_634 (coe v11)
                                                                               (coe v8) in
                                                                     case coe v20 of
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v26 v27 v28
                                                                         -> case coe v27 of
                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v29
                                                                                -> coe
                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                        (coe v4))
                                                                                     v29
                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v30 v31
                                                                                -> let v32
                                                                                         = coe
                                                                                             MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                             (coe
                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                                (coe
                                                                                                   v28)
                                                                                                (coe
                                                                                                   v25)) in
                                                                                   case coe v30 of
                                                                                     MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v33 v34
                                                                                       -> case coe
                                                                                                 v33 of
                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v35 v36
                                                                                              -> case coe
                                                                                                        v35 of
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v37
                                                                                                     -> case coe
                                                                                                               v32 of
                                                                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v39 v40
                                                                                                            -> let v41
                                                                                                                     = coe
                                                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                         (coe
                                                                                                                            v4)
                                                                                                                         (coe
                                                                                                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                            (coe
                                                                                                                               v37)
                                                                                                                            (coe
                                                                                                                               v34))
                                                                                                                         (coe
                                                                                                                            v32) in
                                                                                                               case coe
                                                                                                                      v36 of
                                                                                                                 0 -> coe
                                                                                                                        v41
                                                                                                                 _ -> coe
                                                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                        (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                           (coe
                                                                                                                              v4))
                                                                                                                        (let v42
                                                                                                                               = coe
                                                                                                                                   MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                   (coe
                                                                                                                                      MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                      (let v42
                                                                                                                                             = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                                 (coe
                                                                                                                                                    v4) in
                                                                                                                                       coe
                                                                                                                                         MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                                         (coe
                                                                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                                            (coe
                                                                                                                                               v42)))) in
                                                                                                                         let v43
                                                                                                                               = coe
                                                                                                                                   MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                                   (coe
                                                                                                                                      v42) in
                                                                                                                         let v44
                                                                                                                               = subInt
                                                                                                                                   (coe
                                                                                                                                      v36)
                                                                                                                                   (coe
                                                                                                                                      (1 ::
                                                                                                                                         Integer)) in
                                                                                                                         let v45
                                                                                                                               = coe
                                                                                                                                   MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                                   v43
                                                                                                                                   (coe
                                                                                                                                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                                      (coe
                                                                                                                                         v43)
                                                                                                                                      (coe
                                                                                                                                         v44)
                                                                                                                                      (coe
                                                                                                                                         v39))
                                                                                                                                   v39 in
                                                                                                                         case coe
                                                                                                                                v36 of
                                                                                                                           0 -> coe
                                                                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                                  (coe
                                                                                                                                     v43)
                                                                                                                           1 -> coe
                                                                                                                                  v39
                                                                                                                           _ -> coe
                                                                                                                                  v45)
                                                                                                                        v41
                                                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                                       _ -> MAlonzo.RTE.mazUnreachableError in
                                                             let v26
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                       (coe v24) in
                                                             let v27
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                       v26
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                          (coe v26) (coe v7)
                                                                          (coe v25))
                                                                       v25 in
                                                             case coe v7 of
                                                               0 -> coe v25
                                                               _ -> coe v27)
                                                            (coe
                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
                                                               (coe
                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                  (coe v4))
                                                               (let v22
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                             (coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                (coe v4))) in
                                                                let v23
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                          (coe
                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                             (coe v22)) in
                                                                let v24
                                                                      = coe
                                                                          du_ρ'8242'_632 (coe v11)
                                                                          (coe v8) in
                                                                let v25
                                                                      = addInt
                                                                          (coe
                                                                             mulInt (coe v7)
                                                                             (coe v19))
                                                                          (coe v19) in
                                                                let v26
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                          (coe v23) in
                                                                let v27
                                                                      = subInt
                                                                          (coe v25)
                                                                          (coe (1 :: Integer)) in
                                                                let v28
                                                                      = coe
                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                          v26
                                                                          (coe
                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                             (coe v26) (coe v27)
                                                                             (coe v24))
                                                                          v24 in
                                                                case coe v25 of
                                                                  0 -> coe
                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                         (coe v26)
                                                                  1 -> coe v24
                                                                  _ -> coe v28))
                                                            (d_'8865''45''43'1'45'hom_598
                                                               (coe v0) (coe v1) (coe v2) (coe v3)
                                                               (coe v4) (coe v15) (coe v20) (coe v7)
                                                               (coe du_Ρ_634 (coe v11) (coe v8)))))
                                                      (coe
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8759''8595''45'hom_814
                                                         (coe v4)
                                                         (coe
                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                                                            (coe v0) (coe v1)
                                                            (coe
                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                               (coe v4))
                                                            (coe v15) (coe v20) (coe v7))
                                                         (coe
                                                            addInt (coe mulInt (coe v7) (coe v19))
                                                            (coe v19))
                                                         (coe v17)
                                                         (coe du_ρ'8242'_632 (coe v11) (coe v8))
                                                         (coe du_Ρ_634 (coe v11) (coe v8))))
                                                   (coe
                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8848''8595''45'hom_974
                                                      (coe v4)
                                                      (let v22
                                                             = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                 (coe v4) in
                                                       let v23
                                                             = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                                                                 (coe v0) (coe v1)
                                                                 (coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                                                    (coe v4))
                                                                 (coe v15) (coe v20) (coe v7) in
                                                       let v24
                                                             = addInt
                                                                 (coe mulInt (coe v7) (coe v19))
                                                                 (coe v19) in
                                                       case coe v23 of
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v25 v26 v27
                                                           -> case coe v26 of
                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v28
                                                                  -> let v29
                                                                           = coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                                               v22 v28 in
                                                                     coe
                                                                       seq (coe v29)
                                                                       (if coe v29
                                                                          then coe
                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                                                                                 (coe v17)
                                                                                 (coe
                                                                                    addInt
                                                                                    (coe
                                                                                       (1 ::
                                                                                          Integer))
                                                                                    (coe v24))
                                                                          else coe
                                                                                 MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                                 (coe
                                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                    (coe
                                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                                                       (coe
                                                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                                                          v23)
                                                                                       (coe v24))
                                                                                    (coe v17)))
                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v29 v30
                                                                  -> coe
                                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                       (coe
                                                                          MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                          (coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                                             (coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                                                v23)
                                                                             (coe v24))
                                                                          (coe v17))
                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                         _ -> MAlonzo.RTE.mazUnreachableError)
                                                      (coe v11) (coe v8)))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v18
                             -> coe
                                  d_'8865''45'mult'45'hom_578 (coe v0) (coe v1) (coe v2) (coe v3)
                                  (coe v4) (coe v5) (coe v7)
                                  (coe
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                                     (coe v9)
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216
                                        v13 v14)
                                     (coe v11))
                                  (coe v8)
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._.ρ′,Ρ
d_ρ'8242''44'Ρ_630 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  AgdaAny ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_ρ'8242''44'Ρ_630 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
                   ~v12 v13
  = du_ρ'8242''44'Ρ_630 v11 v13
du_ρ'8242''44'Ρ_630 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_ρ'8242''44'Ρ_630 v0 v1
  = coe
      MAlonzo.Code.Data.Vec.Base.du_uncons_594
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
         (coe v0) (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._.ρ′
d_ρ'8242'_632 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  AgdaAny ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_ρ'8242'_632 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11 ~v12
              v13
  = du_ρ'8242'_632 v11 v13
du_ρ'8242'_632 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_ρ'8242'_632 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe du_ρ'8242''44'Ρ_630 (coe v0) (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._.Ρ
d_Ρ_634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  AgdaAny ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_Ρ_634 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11 ~v12 v13
  = du_Ρ_634 v11 v13
du_Ρ_634 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_Ρ_634 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe du_ρ'8242''44'Ρ_630 (coe v0) (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation._.rearrange
d_rearrange_638 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  AgdaAny ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> Integer -> AgdaAny
d_rearrange_638 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7 ~v8 ~v9 ~v10 v11 v12
                v13 v14
  = du_rearrange_638 v4 v7 v11 v12 v13 v14
du_rearrange_638 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> Integer -> AgdaAny
du_rearrange_638 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      0 -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v6
                       = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                              (coe v0)) in
                 let v7
                       = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe v6) in
                 let v8
                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v7) in
                 let v9
                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                           (coe v8) in
                 let v10
                       = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                           (coe v9) in
                 let v11
                       = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v10) in
                 let v12
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v11) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v12)))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                      (coe v0))
                   (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'35'_220
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                         (coe v0)))
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8
                          = let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                            case coe v1 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                -> case coe v10 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v0))
                                            v12
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                       -> let v15
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                       (coe v11) (coe v8)) in
                                          case coe v13 of
                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                              -> case coe v16 of
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                     -> case coe v18 of
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                            -> case coe v15 of
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                   -> let v24
                                                                            = coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                (coe v0)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v20)
                                                                                   (coe v17))
                                                                                (coe v15) in
                                                                      case coe v19 of
                                                                        0 -> coe v24
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v0))
                                                                               (let v25
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v25
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v0) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v25)))) in
                                                                                let v26
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v25) in
                                                                                let v27
                                                                                      = subInt
                                                                                          (coe v19)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v28
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v26
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v26)
                                                                                             (coe
                                                                                                v27)
                                                                                             (coe
                                                                                                v22))
                                                                                          v22 in
                                                                                case coe v19 of
                                                                                  0 -> coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                         (coe v26)
                                                                                  1 -> coe v22
                                                                                  _ -> coe v28)
                                                                               v24
                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError in
                    let v9
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v9) (coe v3) (coe v8))
                              v8 in
                    case coe v3 of
                      0 -> coe v8
                      _ -> coe v10))
                (let v6
                       = coe
                           MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                 (coe v0))) in
                 let v7
                       = coe
                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                           (coe
                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                              (coe v6)) in
                 let v8
                       = let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                         case coe v1 of
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                             -> case coe v10 of
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                    -> coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                            (coe v0))
                                         v12
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                    -> let v15
                                             = coe
                                                 MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                 (coe
                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                    (coe v11) (coe v8)) in
                                       case coe v13 of
                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                           -> case coe v16 of
                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                  -> case coe v18 of
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                         -> case coe v15 of
                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                -> let v24
                                                                         = coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                             (coe v0)
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                (coe v20) (coe v17))
                                                                             (coe v15) in
                                                                   case coe v19 of
                                                                     0 -> coe v24
                                                                     _ -> coe
                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                               (coe v0))
                                                                            (let v25
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                       (coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                          (let v25
                                                                                                 = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                     (coe
                                                                                                        v0) in
                                                                                           coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                             (coe
                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                (coe
                                                                                                   v25)))) in
                                                                             let v26
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                       (coe v25) in
                                                                             let v27
                                                                                   = subInt
                                                                                       (coe v19)
                                                                                       (coe
                                                                                          (1 ::
                                                                                             Integer)) in
                                                                             let v28
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                       v26
                                                                                       (coe
                                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                          (coe v26)
                                                                                          (coe v27)
                                                                                          (coe v22))
                                                                                       v22 in
                                                                             case coe v19 of
                                                                               0 -> coe
                                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                      (coe v26)
                                                                               1 -> coe v22
                                                                               _ -> coe v28)
                                                                            v24
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError in
                 let v9
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                           (coe v7) in
                 let v10
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                           (coe
                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                              (coe v9) (coe v3) (coe v8))
                           v8 in
                 case coe v3 of
                   0 -> coe v8
                   _ -> coe v10)
                (let v6
                       = coe
                           MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                 (coe v0))) in
                 let v7
                       = coe
                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                           (coe
                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                              (coe v6)) in
                 let v8
                       = let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                         case coe v1 of
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                             -> case coe v10 of
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                    -> coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                            (coe v0))
                                         v12
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                    -> let v15
                                             = coe
                                                 MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                 (coe
                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                    (coe v11) (coe v8)) in
                                       case coe v13 of
                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                           -> case coe v16 of
                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                  -> case coe v18 of
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                         -> case coe v15 of
                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                -> let v24
                                                                         = coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                             (coe v0)
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                (coe v20) (coe v17))
                                                                             (coe v15) in
                                                                   case coe v19 of
                                                                     0 -> coe v24
                                                                     _ -> coe
                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                               (coe v0))
                                                                            (let v25
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                       (coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                          (let v25
                                                                                                 = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                     (coe
                                                                                                        v0) in
                                                                                           coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                             (coe
                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                (coe
                                                                                                   v25)))) in
                                                                             let v26
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                       (coe v25) in
                                                                             let v27
                                                                                   = subInt
                                                                                       (coe v19)
                                                                                       (coe
                                                                                          (1 ::
                                                                                             Integer)) in
                                                                             let v28
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                       v26
                                                                                       (coe
                                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                          (coe v26)
                                                                                          (coe v27)
                                                                                          (coe v22))
                                                                                       v22 in
                                                                             case coe v19 of
                                                                               0 -> coe
                                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                      (coe v26)
                                                                               1 -> coe v22
                                                                               _ -> coe v28)
                                                                            v24
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError in
                 let v9
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                           (coe v7) in
                 let v10
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                           (coe
                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                              (coe v9) (coe v3) (coe v8))
                           v8 in
                 case coe v3 of
                   0 -> coe v8
                   _ -> coe v10)
                (let v6
                       = let v6
                               = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                      (coe v0)) in
                         let v7
                               = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                   (coe v6) in
                         let v8
                               = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v7) in
                         let v9
                               = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                   (coe v8) in
                         let v10
                               = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                   (coe v9) in
                         let v11
                               = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v10) in
                         let v12
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v11) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v12)) in
                 coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                      (coe
                         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6)))
                   (let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v8
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v7)) in
                    let v9
                          = let v9 = coe du_Ρ_634 (coe v2) (coe v4) in
                            case coe v1 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                                -> case coe v11 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v0))
                                            v13
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                                       -> let v16
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                       (coe v12) (coe v9)) in
                                          case coe v14 of
                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                              -> case coe v17 of
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                                     -> case coe v19 of
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                                            -> case coe v16 of
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                                   -> let v25
                                                                            = coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                (coe v0)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v21)
                                                                                   (coe v18))
                                                                                (coe v16) in
                                                                      case coe v20 of
                                                                        0 -> coe v25
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v0))
                                                                               (let v26
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v26
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v0) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v26)))) in
                                                                                let v27
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v26) in
                                                                                let v28
                                                                                      = subInt
                                                                                          (coe v20)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v29
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v27
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v27)
                                                                                             (coe
                                                                                                v28)
                                                                                             (coe
                                                                                                v23))
                                                                                          v23 in
                                                                                case coe v20 of
                                                                                  0 -> coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                         (coe v27)
                                                                                  1 -> coe v23
                                                                                  _ -> coe v29)
                                                                               v25
                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v8) in
                    let v11
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v10
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v10) (coe v3) (coe v9))
                              v9 in
                    case coe v3 of
                      0 -> coe v9
                      _ -> coe v11))
                (let v6
                       = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                           (coe v0) in
                 let v7
                       = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                           (coe v6) in
                 let v8
                       = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe v7) in
                 let v9
                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v8) in
                 let v10
                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                           (coe v9) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_identity'737'_640
                   (coe
                      MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v10))
                   (let v11
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v12
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v11)) in
                    let v13
                          = let v13 = coe du_Ρ_634 (coe v2) (coe v4) in
                            case coe v1 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v14 v15 v16
                                -> case coe v15 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v17
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v0))
                                            v17
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v18 v19
                                       -> let v20
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                       (coe v16) (coe v13)) in
                                          case coe v18 of
                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v21 v22
                                              -> case coe v21 of
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v23 v24
                                                     -> case coe v23 of
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v25
                                                            -> case coe v20 of
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v27 v28
                                                                   -> let v29
                                                                            = coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                (coe v0)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v25)
                                                                                   (coe v22))
                                                                                (coe v20) in
                                                                      case coe v24 of
                                                                        0 -> coe v29
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v0))
                                                                               (let v30
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v30
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v0) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v30)))) in
                                                                                let v31
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v30) in
                                                                                let v32
                                                                                      = subInt
                                                                                          (coe v24)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v33
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v31
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v31)
                                                                                             (coe
                                                                                                v32)
                                                                                             (coe
                                                                                                v27))
                                                                                          v27 in
                                                                                case coe v24 of
                                                                                  0 -> coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                         (coe v31)
                                                                                  1 -> coe v27
                                                                                  _ -> coe v33)
                                                                               v29
                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError in
                    let v14
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v12) in
                    let v15
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v14
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v14) (coe v3) (coe v13))
                              v13 in
                    case coe v3 of
                      0 -> coe v13
                      _ -> coe v15)))
      _ -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v6
                       = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                              (coe v0)) in
                 let v7
                       = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe v6) in
                 let v8
                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v7) in
                 let v9
                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                           (coe v8) in
                 let v10
                       = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                           (coe v9) in
                 let v11
                       = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v10) in
                 let v12
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v11) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v12)))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                      (coe v0))
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                    let v9
                          = mulInt (coe v5) (coe addInt (coe (1 :: Integer)) (coe v3)) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v11 = subInt (coe v9) (coe (1 :: Integer)) in
                    let v12
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v10
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v10) (coe v11) (coe v8))
                              v8 in
                    case coe v9 of
                      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v10)
                      1 -> coe v8
                      _ -> coe v12)
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8
                          = let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                            case coe v1 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                -> case coe v10 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v0))
                                            v12
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                       -> let v15
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                       (coe v11) (coe v8)) in
                                          case coe v13 of
                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                              -> case coe v16 of
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                     -> case coe v18 of
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                            -> case coe v15 of
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                   -> let v24
                                                                            = coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                (coe v0)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v20)
                                                                                   (coe v17))
                                                                                (coe v15) in
                                                                      case coe v19 of
                                                                        0 -> coe v24
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v0))
                                                                               (let v25
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v25
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v0) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v25)))) in
                                                                                let v26
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v25) in
                                                                                let v27
                                                                                      = subInt
                                                                                          (coe v19)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v28
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v26
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v26)
                                                                                             (coe
                                                                                                v27)
                                                                                             (coe
                                                                                                v22))
                                                                                          v22 in
                                                                                case coe v19 of
                                                                                  0 -> coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                         (coe v26)
                                                                                  1 -> coe v22
                                                                                  _ -> coe v28)
                                                                               v24
                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError in
                    let v9
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v9) (coe v3) (coe v8))
                              v8 in
                    case coe v3 of
                      0 -> coe v8
                      _ -> coe v10))
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                      (coe v0))
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8
                          = let v8
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                      (coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                            (coe v0))) in
                            let v9
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                         (coe v8)) in
                            let v10 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                            let v11
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                      (coe v9) in
                            let v12 = subInt (coe v5) (coe (1 :: Integer)) in
                            let v13
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v11
                                      (coe
                                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                         (coe v11) (coe v12) (coe v10))
                                      v10 in
                            case coe v5 of
                              0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v11)
                              1 -> coe v10
                              _ -> coe v13 in
                    let v9
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v9) (coe v3) (coe v8))
                              v8 in
                    case coe v3 of
                      0 -> coe v8
                      _ -> coe v10)
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8
                          = let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                            case coe v1 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                -> case coe v10 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v0))
                                            v12
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                       -> let v15
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                       (coe v11) (coe v8)) in
                                          case coe v13 of
                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                              -> case coe v16 of
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                     -> case coe v18 of
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                            -> case coe v15 of
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                   -> let v24
                                                                            = coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                (coe v0)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v20)
                                                                                   (coe v17))
                                                                                (coe v15) in
                                                                      case coe v19 of
                                                                        0 -> coe v24
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v0))
                                                                               (let v25
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v25
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v0) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v25)))) in
                                                                                let v26
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v25) in
                                                                                let v27
                                                                                      = subInt
                                                                                          (coe v19)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v28
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v26
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v26)
                                                                                             (coe
                                                                                                v27)
                                                                                             (coe
                                                                                                v22))
                                                                                          v22 in
                                                                                case coe v19 of
                                                                                  0 -> coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                         (coe v26)
                                                                                  1 -> coe v22
                                                                                  _ -> coe v28)
                                                                               v24
                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError in
                    let v9
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v9) (coe v3) (coe v8))
                              v8 in
                    case coe v3 of
                      0 -> coe v8
                      _ -> coe v10))
                (let v6
                       = coe
                           MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                 (coe v0))) in
                 let v7
                       = coe
                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                           (coe
                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                              (coe v6)) in
                 let v8
                       = coe
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                              (coe v0))
                           (let v8
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                      (coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                            (coe v0))) in
                            let v9
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                         (coe v8)) in
                            let v10 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                            let v11
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                      (coe v9) in
                            let v12 = subInt (coe v5) (coe (1 :: Integer)) in
                            let v13
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v11
                                      (coe
                                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                         (coe v11) (coe v12) (coe v10))
                                      v10 in
                            case coe v5 of
                              0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v11)
                              1 -> coe v10
                              _ -> coe v13)
                           (let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                            case coe v1 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                -> case coe v10 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v0))
                                            v12
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                       -> let v15
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                       (coe v11) (coe v8)) in
                                          case coe v13 of
                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                              -> case coe v16 of
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                     -> case coe v18 of
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                            -> case coe v15 of
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                   -> let v24
                                                                            = coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                (coe v0)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v20)
                                                                                   (coe v17))
                                                                                (coe v15) in
                                                                      case coe v19 of
                                                                        0 -> coe v24
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v0))
                                                                               (let v25
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v25
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v0) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v25)))) in
                                                                                let v26
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v25) in
                                                                                let v27
                                                                                      = subInt
                                                                                          (coe v19)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v28
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v26
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v26)
                                                                                             (coe
                                                                                                v27)
                                                                                             (coe
                                                                                                v22))
                                                                                          v22 in
                                                                                case coe v19 of
                                                                                  0 -> coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                         (coe v26)
                                                                                  1 -> coe v22
                                                                                  _ -> coe v28)
                                                                               v24
                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError) in
                 let v9
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                           (coe v7) in
                 let v10
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                           (coe
                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                              (coe v9) (coe v3) (coe v8))
                           v8 in
                 case coe v3 of
                   0 -> coe v8
                   _ -> coe v10)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   (let v6
                          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                 (coe v0)) in
                    let v7
                          = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe v6) in
                    let v8
                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v7) in
                    let v9
                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                              (coe v8) in
                    let v10
                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                              (coe v9) in
                    let v11
                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v10) in
                    let v12
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v11) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v12)))
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                      (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                         (coe v0))
                      (let v6
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                       (coe v0))) in
                       let v7
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                    (coe v6)) in
                       let v8
                             = let v8
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))) in
                               let v9
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                         (coe
                                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                            (coe v8)) in
                               let v10 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                               let v11
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                         (coe v9) in
                               let v12 = subInt (coe v5) (coe (1 :: Integer)) in
                               let v13
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v11
                                         (coe
                                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                            (coe v11) (coe v12) (coe v10))
                                         v10 in
                               case coe v5 of
                                 0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v11)
                                 1 -> coe v10
                                 _ -> coe v13 in
                       let v9
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                 (coe v7) in
                       let v10
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v9) (coe v3) (coe v8))
                                 v8 in
                       case coe v3 of
                         0 -> coe v8
                         _ -> coe v10)
                      (let v6
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                       (coe v0))) in
                       let v7
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                    (coe v6)) in
                       let v8
                             = let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                               case coe v1 of
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                   -> case coe v10 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                          -> coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                  (coe v0))
                                               v12
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                          -> let v15
                                                   = coe
                                                       MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                       (coe
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                          (coe v11) (coe v8)) in
                                             case coe v13 of
                                               MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                                 -> case coe v16 of
                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                        -> case coe v18 of
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                               -> case coe v15 of
                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                      -> let v24
                                                                               = coe
                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                   (coe v0)
                                                                                   (coe
                                                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                      (coe v20)
                                                                                      (coe v17))
                                                                                   (coe v15) in
                                                                         case coe v19 of
                                                                           0 -> coe v24
                                                                           _ -> coe
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                     (coe v0))
                                                                                  (let v25
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                             (coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                (let v25
                                                                                                       = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                           (coe
                                                                                                              v0) in
                                                                                                 coe
                                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                   (coe
                                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                      (coe
                                                                                                         v25)))) in
                                                                                   let v26
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                             (coe
                                                                                                v25) in
                                                                                   let v27
                                                                                         = subInt
                                                                                             (coe
                                                                                                v19)
                                                                                             (coe
                                                                                                (1 ::
                                                                                                   Integer)) in
                                                                                   let v28
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                             v26
                                                                                             (coe
                                                                                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                (coe
                                                                                                   v26)
                                                                                                (coe
                                                                                                   v27)
                                                                                                (coe
                                                                                                   v22))
                                                                                             v22 in
                                                                                   case coe v19 of
                                                                                     0 -> coe
                                                                                            MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                            (coe
                                                                                               v26)
                                                                                     1 -> coe v22
                                                                                     _ -> coe v28)
                                                                                  v24
                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError in
                       let v9
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                 (coe v7) in
                       let v10
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v9) (coe v3) (coe v8))
                                 v8 in
                       case coe v3 of
                         0 -> coe v8
                         _ -> coe v10))
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8
                          = coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                 (coe v0))
                              (let v8
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))) in
                               let v9
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                         (coe
                                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                            (coe v8)) in
                               let v10 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                               let v11
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                         (coe v9) in
                               let v12 = subInt (coe v5) (coe (1 :: Integer)) in
                               let v13
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v11
                                         (coe
                                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                            (coe v11) (coe v12) (coe v10))
                                         v10 in
                               case coe v5 of
                                 0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v11)
                                 1 -> coe v10
                                 _ -> coe v13)
                              (let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                               case coe v1 of
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                   -> case coe v10 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                          -> coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                  (coe v0))
                                               v12
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                          -> let v15
                                                   = coe
                                                       MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                       (coe
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                          (coe v11) (coe v8)) in
                                             case coe v13 of
                                               MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                                 -> case coe v16 of
                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                        -> case coe v18 of
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                               -> case coe v15 of
                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                      -> let v24
                                                                               = coe
                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                   (coe v0)
                                                                                   (coe
                                                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                      (coe v20)
                                                                                      (coe v17))
                                                                                   (coe v15) in
                                                                         case coe v19 of
                                                                           0 -> coe v24
                                                                           _ -> coe
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                     (coe v0))
                                                                                  (let v25
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                             (coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                (let v25
                                                                                                       = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                           (coe
                                                                                                              v0) in
                                                                                                 coe
                                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                   (coe
                                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                      (coe
                                                                                                         v25)))) in
                                                                                   let v26
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                             (coe
                                                                                                v25) in
                                                                                   let v27
                                                                                         = subInt
                                                                                             (coe
                                                                                                v19)
                                                                                             (coe
                                                                                                (1 ::
                                                                                                   Integer)) in
                                                                                   let v28
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                             v26
                                                                                             (coe
                                                                                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                (coe
                                                                                                   v26)
                                                                                                (coe
                                                                                                   v27)
                                                                                                (coe
                                                                                                   v22))
                                                                                             v22 in
                                                                                   case coe v19 of
                                                                                     0 -> coe
                                                                                            MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                            (coe
                                                                                               v26)
                                                                                     1 -> coe v22
                                                                                     _ -> coe v28)
                                                                                  v24
                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError) in
                    let v9
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v9) (coe v3) (coe v8))
                              v8 in
                    case coe v3 of
                      0 -> coe v8
                      _ -> coe v10)
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8
                          = coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                 (coe v0))
                              (let v8
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))) in
                               let v9
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                         (coe
                                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                            (coe v8)) in
                               let v10 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                               let v11
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                         (coe v9) in
                               let v12 = subInt (coe v5) (coe (1 :: Integer)) in
                               let v13
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v11
                                         (coe
                                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                            (coe v11) (coe v12) (coe v10))
                                         v10 in
                               case coe v5 of
                                 0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v11)
                                 1 -> coe v10
                                 _ -> coe v13)
                              (let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                               case coe v1 of
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                   -> case coe v10 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                          -> coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                  (coe v0))
                                               v12
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                          -> let v15
                                                   = coe
                                                       MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                       (coe
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                          (coe v11) (coe v8)) in
                                             case coe v13 of
                                               MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                                 -> case coe v16 of
                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                        -> case coe v18 of
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                               -> case coe v15 of
                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                      -> let v24
                                                                               = coe
                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                   (coe v0)
                                                                                   (coe
                                                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                      (coe v20)
                                                                                      (coe v17))
                                                                                   (coe v15) in
                                                                         case coe v19 of
                                                                           0 -> coe v24
                                                                           _ -> coe
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                     (coe v0))
                                                                                  (let v25
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                             (coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                (let v25
                                                                                                       = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                           (coe
                                                                                                              v0) in
                                                                                                 coe
                                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                   (coe
                                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                      (coe
                                                                                                         v25)))) in
                                                                                   let v26
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                             (coe
                                                                                                v25) in
                                                                                   let v27
                                                                                         = subInt
                                                                                             (coe
                                                                                                v19)
                                                                                             (coe
                                                                                                (1 ::
                                                                                                   Integer)) in
                                                                                   let v28
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                             v26
                                                                                             (coe
                                                                                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                (coe
                                                                                                   v26)
                                                                                                (coe
                                                                                                   v27)
                                                                                                (coe
                                                                                                   v22))
                                                                                             v22 in
                                                                                   case coe v19 of
                                                                                     0 -> coe
                                                                                            MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                            (coe
                                                                                               v26)
                                                                                     1 -> coe v22
                                                                                     _ -> coe v28)
                                                                                  v24
                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError) in
                    let v9
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v9) (coe v3) (coe v8))
                              v8 in
                    case coe v3 of
                      0 -> coe v8
                      _ -> coe v10)
                   (let v6
                          = let v6
                                  = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                      (coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                         (coe v0)) in
                            let v7
                                  = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                      (coe v6) in
                            let v8
                                  = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v7) in
                            let v9
                                  = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                      (coe v8) in
                            let v10
                                  = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                      (coe v9) in
                            let v11
                                  = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v10) in
                            let v12
                                  = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v11) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_setoid_164
                              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v12)) in
                    coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6)))
                      (let v7
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                       (coe v0))) in
                       let v8
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                    (coe v7)) in
                       let v9
                             = coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))
                                 (let v9
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))) in
                                  let v10
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                            (coe
                                               MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                               (coe v9)) in
                                  let v11 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                                  let v12
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                            (coe v10) in
                                  let v13 = subInt (coe v5) (coe (1 :: Integer)) in
                                  let v14
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v12
                                            (coe
                                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                               (coe v12) (coe v13) (coe v11))
                                            v11 in
                                  case coe v5 of
                                    0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v12)
                                    1 -> coe v11
                                    _ -> coe v14)
                                 (let v9 = coe du_Ρ_634 (coe v2) (coe v4) in
                                  case coe v1 of
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                                      -> case coe v11 of
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                                             -> coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                     (coe v0))
                                                  v13
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                                             -> let v16
                                                      = coe
                                                          MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                          (coe
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                             (coe v12) (coe v9)) in
                                                case coe v14 of
                                                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                                    -> case coe v17 of
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                                           -> case coe v19 of
                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                                                  -> case coe v16 of
                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                                         -> let v25
                                                                                  = coe
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                      (coe v0)
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                         (coe v21)
                                                                                         (coe v18))
                                                                                      (coe v16) in
                                                                            case coe v20 of
                                                                              0 -> coe v25
                                                                              _ -> coe
                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                        (coe v0))
                                                                                     (let v26
                                                                                            = coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                (coe
                                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                   (let v26
                                                                                                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                              (coe
                                                                                                                 v0) in
                                                                                                    coe
                                                                                                      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                      (coe
                                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                         (coe
                                                                                                            v26)))) in
                                                                                      let v27
                                                                                            = coe
                                                                                                MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                (coe
                                                                                                   v26) in
                                                                                      let v28
                                                                                            = subInt
                                                                                                (coe
                                                                                                   v20)
                                                                                                (coe
                                                                                                   (1 ::
                                                                                                      Integer)) in
                                                                                      let v29
                                                                                            = coe
                                                                                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                v27
                                                                                                (coe
                                                                                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                   (coe
                                                                                                      v27)
                                                                                                   (coe
                                                                                                      v28)
                                                                                                   (coe
                                                                                                      v23))
                                                                                                v23 in
                                                                                      case coe
                                                                                             v20 of
                                                                                        0 -> coe
                                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                               (coe
                                                                                                  v27)
                                                                                        1 -> coe v23
                                                                                        _ -> coe
                                                                                               v29)
                                                                                     v25
                                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError) in
                       let v10
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                 (coe v8) in
                       let v11
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v10
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v10) (coe v3) (coe v9))
                                 v9 in
                       case coe v3 of
                         0 -> coe v9
                         _ -> coe v11))
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
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
                                                 (coe
                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                    (coe v0)))))))))))
                      (let v6
                             = coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0)) in
                       let v7
                             = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v6) in
                       let v8
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                    (coe v7)) in
                       let v9
                             = coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))
                                 (let v9
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))) in
                                  let v10
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                            (coe
                                               MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                               (coe v9)) in
                                  let v11 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                                  let v12
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                            (coe v10) in
                                  let v13 = subInt (coe v5) (coe (1 :: Integer)) in
                                  let v14
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v12
                                            (coe
                                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                               (coe v12) (coe v13) (coe v11))
                                            v11 in
                                  case coe v5 of
                                    0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v12)
                                    1 -> coe v11
                                    _ -> coe v14)
                                 (let v9 = coe du_Ρ_634 (coe v2) (coe v4) in
                                  case coe v1 of
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                                      -> case coe v11 of
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                                             -> coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                     (coe v0))
                                                  v13
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                                             -> let v16
                                                      = coe
                                                          MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                          (coe
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                             (coe v12) (coe v9)) in
                                                case coe v14 of
                                                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                                    -> case coe v17 of
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                                           -> case coe v19 of
                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                                                  -> case coe v16 of
                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                                         -> let v25
                                                                                  = coe
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                      (coe v0)
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                         (coe v21)
                                                                                         (coe v18))
                                                                                      (coe v16) in
                                                                            case coe v20 of
                                                                              0 -> coe v25
                                                                              _ -> coe
                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                        (coe v0))
                                                                                     (let v26
                                                                                            = coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                (coe
                                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                   (let v26
                                                                                                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                              (coe
                                                                                                                 v0) in
                                                                                                    coe
                                                                                                      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                      (coe
                                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                         (coe
                                                                                                            v26)))) in
                                                                                      let v27
                                                                                            = coe
                                                                                                MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                (coe
                                                                                                   v26) in
                                                                                      let v28
                                                                                            = subInt
                                                                                                (coe
                                                                                                   v20)
                                                                                                (coe
                                                                                                   (1 ::
                                                                                                      Integer)) in
                                                                                      let v29
                                                                                            = coe
                                                                                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                v27
                                                                                                (coe
                                                                                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                   (coe
                                                                                                      v27)
                                                                                                   (coe
                                                                                                      v28)
                                                                                                   (coe
                                                                                                      v23))
                                                                                                v23 in
                                                                                      case coe
                                                                                             v20 of
                                                                                        0 -> coe
                                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                               (coe
                                                                                                  v27)
                                                                                        1 -> coe v23
                                                                                        _ -> coe
                                                                                               v29)
                                                                                     v25
                                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError) in
                       let v10
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                 (coe v8) in
                       let v11
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v10
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v10) (coe v3) (coe v9))
                                 v9 in
                       case coe v3 of
                         0 -> coe v9
                         _ -> coe v11)
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                            (coe v0))
                         (let v6
                                = coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                       (coe v0)) in
                          let v7
                                = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v6) in
                          let v8
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                    (coe
                                       MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                       (coe v7)) in
                          let v9
                                = let v9
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))) in
                                  let v10
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                            (coe
                                               MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                               (coe v9)) in
                                  let v11 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                                  let v12
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                            (coe v10) in
                                  let v13 = subInt (coe v5) (coe (1 :: Integer)) in
                                  let v14
                                        = coe
                                            MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v12
                                            (coe
                                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                               (coe v12) (coe v13) (coe v11))
                                            v11 in
                                  case coe v5 of
                                    0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v12)
                                    1 -> coe v11
                                    _ -> coe v14 in
                          let v10
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                    (coe v8) in
                          let v11
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v10
                                    (coe
                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                       (coe v10) (coe v3) (coe v9))
                                    v9 in
                          case coe v3 of
                            0 -> coe v9
                            _ -> coe v11)
                         (let v6
                                = coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                       (coe v0)) in
                          let v7
                                = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v6) in
                          let v8
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                    (coe
                                       MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                       (coe v7)) in
                          let v9
                                = let v9 = coe du_Ρ_634 (coe v2) (coe v4) in
                                  case coe v1 of
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                                      -> case coe v11 of
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                                             -> coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                     (coe v0))
                                                  v13
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                                             -> let v16
                                                      = coe
                                                          MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                          (coe
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                             (coe v12) (coe v9)) in
                                                case coe v14 of
                                                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                                                    -> case coe v17 of
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v19 v20
                                                           -> case coe v19 of
                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v21
                                                                  -> case coe v16 of
                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v23 v24
                                                                         -> let v25
                                                                                  = coe
                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                      (coe v0)
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                         (coe v21)
                                                                                         (coe v18))
                                                                                      (coe v16) in
                                                                            case coe v20 of
                                                                              0 -> coe v25
                                                                              _ -> coe
                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                        (coe v0))
                                                                                     (let v26
                                                                                            = coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                (coe
                                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                   (let v26
                                                                                                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                              (coe
                                                                                                                 v0) in
                                                                                                    coe
                                                                                                      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                      (coe
                                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                         (coe
                                                                                                            v26)))) in
                                                                                      let v27
                                                                                            = coe
                                                                                                MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                (coe
                                                                                                   v26) in
                                                                                      let v28
                                                                                            = subInt
                                                                                                (coe
                                                                                                   v20)
                                                                                                (coe
                                                                                                   (1 ::
                                                                                                      Integer)) in
                                                                                      let v29
                                                                                            = coe
                                                                                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                v27
                                                                                                (coe
                                                                                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                   (coe
                                                                                                      v27)
                                                                                                   (coe
                                                                                                      v28)
                                                                                                   (coe
                                                                                                      v23))
                                                                                                v23 in
                                                                                      case coe
                                                                                             v20 of
                                                                                        0 -> coe
                                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                               (coe
                                                                                                  v27)
                                                                                        1 -> coe v23
                                                                                        _ -> coe
                                                                                               v29)
                                                                                     v25
                                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError in
                          let v10
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                    (coe v8) in
                          let v11
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v10
                                    (coe
                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                       (coe v10) (coe v3) (coe v9))
                                    v9 in
                          case coe v3 of
                            0 -> coe v9
                            _ -> coe v11))
                      (coe
                         MAlonzo.Code.Algebra.Properties.CommutativeSemiring.Exp.TCOptimised.du_'94''45'distrib'45''42'_234
                         (coe
                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                               (coe v0)))
                         (let v6
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                       (coe
                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                          (coe v0))) in
                          let v7
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                    (coe
                                       MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                       (coe v6)) in
                          let v8 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                          let v9
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                    (coe v7) in
                          let v10 = subInt (coe v5) (coe (1 :: Integer)) in
                          let v11
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                                    (coe
                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                       (coe v9) (coe v10) (coe v8))
                                    v8 in
                          case coe v5 of
                            0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v9)
                            1 -> coe v8
                            _ -> coe v11)
                         (let v6 = coe du_Ρ_634 (coe v2) (coe v4) in
                          case coe v1 of
                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v7 v8 v9
                              -> case coe v8 of
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v10
                                     -> coe
                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                          (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                             (coe v0))
                                          v10
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v11 v12
                                     -> let v13
                                              = coe
                                                  MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                     (coe v9) (coe v6)) in
                                        case coe v11 of
                                          MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v14 v15
                                            -> case coe v14 of
                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v16 v17
                                                   -> case coe v16 of
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v18
                                                          -> case coe v13 of
                                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v20 v21
                                                                 -> let v22
                                                                          = coe
                                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                              (coe v0)
                                                                              (coe
                                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                 (coe v18)
                                                                                 (coe v15))
                                                                              (coe v13) in
                                                                    case coe v17 of
                                                                      0 -> coe v22
                                                                      _ -> coe
                                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                (coe v0))
                                                                             (let v23
                                                                                    = coe
                                                                                        MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                        (coe
                                                                                           MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                           (let v23
                                                                                                  = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                      (coe
                                                                                                         v0) in
                                                                                            coe
                                                                                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                              (coe
                                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                 (coe
                                                                                                    v23)))) in
                                                                              let v24
                                                                                    = coe
                                                                                        MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                        (coe v23) in
                                                                              let v25
                                                                                    = subInt
                                                                                        (coe v17)
                                                                                        (coe
                                                                                           (1 ::
                                                                                              Integer)) in
                                                                              let v26
                                                                                    = coe
                                                                                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                        v24
                                                                                        (coe
                                                                                           MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                           (coe v24)
                                                                                           (coe v25)
                                                                                           (coe
                                                                                              v20))
                                                                                        v20 in
                                                                              case coe v17 of
                                                                                0 -> coe
                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                       (coe v24)
                                                                                1 -> coe v20
                                                                                _ -> coe v26)
                                                                             v22
                                                               _ -> MAlonzo.RTE.mazUnreachableError
                                                        _ -> MAlonzo.RTE.mazUnreachableError
                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                          _ -> MAlonzo.RTE.mazUnreachableError
                                   _ -> MAlonzo.RTE.mazUnreachableError
                            _ -> MAlonzo.RTE.mazUnreachableError)
                         (addInt (coe (1 :: Integer)) (coe v3)))))
                (coe
                   MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                   (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                         (coe
                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                               (coe
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                  (coe v0))))))
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                    let v9
                          = mulInt (coe v5) (coe addInt (coe (1 :: Integer)) (coe v3)) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v11 = subInt (coe v9) (coe (1 :: Integer)) in
                    let v12
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v10
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v10) (coe v11) (coe v8))
                              v8 in
                    case coe v9 of
                      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v10)
                      1 -> coe v8
                      _ -> coe v12)
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8
                          = let v8
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                      (coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                            (coe v0))) in
                            let v9
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                         (coe v8)) in
                            let v10 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                            let v11
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                      (coe v9) in
                            let v12 = subInt (coe v5) (coe (1 :: Integer)) in
                            let v13
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v11
                                      (coe
                                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                         (coe v11) (coe v12) (coe v10))
                                      v10 in
                            case coe v5 of
                              0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v11)
                              1 -> coe v10
                              _ -> coe v13 in
                    let v9
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v9) (coe v3) (coe v8))
                              v8 in
                    case coe v3 of
                      0 -> coe v8
                      _ -> coe v10)
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8
                          = let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                            case coe v1 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                -> case coe v10 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v0))
                                            v12
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                       -> let v15
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                       (coe v11) (coe v8)) in
                                          case coe v13 of
                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                              -> case coe v16 of
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                     -> case coe v18 of
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                            -> case coe v15 of
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                   -> let v24
                                                                            = coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                (coe v0)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v20)
                                                                                   (coe v17))
                                                                                (coe v15) in
                                                                      case coe v19 of
                                                                        0 -> coe v24
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v0))
                                                                               (let v25
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v25
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v0) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v25)))) in
                                                                                let v26
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v25) in
                                                                                let v27
                                                                                      = subInt
                                                                                          (coe v19)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v28
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v26
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v26)
                                                                                             (coe
                                                                                                v27)
                                                                                             (coe
                                                                                                v22))
                                                                                          v22 in
                                                                                case coe v19 of
                                                                                  0 -> coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                         (coe v26)
                                                                                  1 -> coe v22
                                                                                  _ -> coe v28)
                                                                               v24
                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError in
                    let v9
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v9) (coe v3) (coe v8))
                              v8 in
                    case coe v3 of
                      0 -> coe v8
                      _ -> coe v10)
                   (let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                    (coe v0))) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                 (coe v6)) in
                    let v8
                          = let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                            case coe v1 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                -> case coe v10 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v0))
                                            v12
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                       -> let v15
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                       (coe v11) (coe v8)) in
                                          case coe v13 of
                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                              -> case coe v16 of
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                     -> case coe v18 of
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                            -> case coe v15 of
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                   -> let v24
                                                                            = coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                (coe v0)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v20)
                                                                                   (coe v17))
                                                                                (coe v15) in
                                                                      case coe v19 of
                                                                        0 -> coe v24
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v0))
                                                                               (let v25
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v25
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v0) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v25)))) in
                                                                                let v26
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v25) in
                                                                                let v27
                                                                                      = subInt
                                                                                          (coe v19)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v28
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v26
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v26)
                                                                                             (coe
                                                                                                v27)
                                                                                             (coe
                                                                                                v22))
                                                                                          v22 in
                                                                                case coe v19 of
                                                                                  0 -> coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                         (coe v26)
                                                                                  1 -> coe v22
                                                                                  _ -> coe v28)
                                                                               v24
                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                            _ -> MAlonzo.RTE.mazUnreachableError
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              _ -> MAlonzo.RTE.mazUnreachableError in
                    let v9
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                              (coe v7) in
                    let v10
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v9) (coe v3) (coe v8))
                              v8 in
                    case coe v3 of
                      0 -> coe v8
                      _ -> coe v10)
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
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
                                                 (coe
                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                    (coe v0)))))))))))
                      (let v6
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                       (coe v0))) in
                       let v7
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                    (coe v6)) in
                       let v8
                             = let v8
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))) in
                               let v9
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                         (coe
                                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                            (coe v8)) in
                               let v10 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                               let v11
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                         (coe v9) in
                               let v12 = subInt (coe v5) (coe (1 :: Integer)) in
                               let v13
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v11
                                         (coe
                                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                            (coe v11) (coe v12) (coe v10))
                                         v10 in
                               case coe v5 of
                                 0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v11)
                                 1 -> coe v10
                                 _ -> coe v13 in
                       let v9
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                 (coe v7) in
                       let v10
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v9) (coe v3) (coe v8))
                                 v8 in
                       case coe v3 of
                         0 -> coe v8
                         _ -> coe v10)
                      (let v6
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                       (coe v0))) in
                       let v7
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                    (coe v6)) in
                       let v8 = coe du_ρ'8242'_632 (coe v2) (coe v4) in
                       let v9
                             = mulInt (coe v5) (coe addInt (coe (1 :: Integer)) (coe v3)) in
                       let v10
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                 (coe v7) in
                       let v11 = subInt (coe v9) (coe (1 :: Integer)) in
                       let v12
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v10
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v10) (coe v11) (coe v8))
                                 v8 in
                       case coe v9 of
                         0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v10)
                         1 -> coe v8
                         _ -> coe v12)
                      (coe
                         MAlonzo.Code.Algebra.Properties.Semiring.Exp.TCOptimised.du_'94''45'assoc'691'_236
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                               (coe
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                  (coe v0))))
                         (coe du_ρ'8242'_632 (coe v2) (coe v4)) (coe v5)
                         (coe addInt (coe (1 :: Integer)) (coe v3))))
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                         (coe v0))
                      (let v6
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                       (coe v0))) in
                       let v7
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                 (coe
                                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                    (coe v6)) in
                       let v8
                             = let v8 = coe du_Ρ_634 (coe v2) (coe v4) in
                               case coe v1 of
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                                   -> case coe v10 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                                          -> coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                  (coe v0))
                                               v12
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                                          -> let v15
                                                   = coe
                                                       MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                       (coe
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                          (coe v11) (coe v8)) in
                                             case coe v13 of
                                               MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v16 v17
                                                 -> case coe v16 of
                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v18 v19
                                                        -> case coe v18 of
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v20
                                                               -> case coe v15 of
                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                      -> let v24
                                                                               = coe
                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                   (coe v0)
                                                                                   (coe
                                                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                      (coe v20)
                                                                                      (coe v17))
                                                                                   (coe v15) in
                                                                         case coe v19 of
                                                                           0 -> coe v24
                                                                           _ -> coe
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                     (coe v0))
                                                                                  (let v25
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                             (coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                (let v25
                                                                                                       = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                           (coe
                                                                                                              v0) in
                                                                                                 coe
                                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                   (coe
                                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                      (coe
                                                                                                         v25)))) in
                                                                                   let v26
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                             (coe
                                                                                                v25) in
                                                                                   let v27
                                                                                         = subInt
                                                                                             (coe
                                                                                                v19)
                                                                                             (coe
                                                                                                (1 ::
                                                                                                   Integer)) in
                                                                                   let v28
                                                                                         = coe
                                                                                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                             v26
                                                                                             (coe
                                                                                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                (coe
                                                                                                   v26)
                                                                                                (coe
                                                                                                   v27)
                                                                                                (coe
                                                                                                   v22))
                                                                                             v22 in
                                                                                   case coe v19 of
                                                                                     0 -> coe
                                                                                            MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                            (coe
                                                                                               v26)
                                                                                     1 -> coe v22
                                                                                     _ -> coe v28)
                                                                                  v24
                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError in
                       let v9
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                 (coe v7) in
                       let v10
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v9
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v9) (coe v3) (coe v8))
                                 v8 in
                       case coe v3 of
                         0 -> coe v8
                         _ -> coe v10))))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation.⊡-hom
d_'8865''45'hom_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8865''45'hom_656 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = case coe v7 of
      0 -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'45'homo_780
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                (coe v4))
      _ -> let v9 = subInt (coe v7) (coe (1 :: Integer)) in
           coe
             d_'8865''45''43'1'45'hom_598 (coe v0) (coe v1) (coe v2) (coe v3)
             (coe v4) (coe v5) (coe v6) (coe v9) (coe v8)
