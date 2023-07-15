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

module MAlonzo.Code.Tactic.RingSolver.NonReflective where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Definitions.RawMonoid
import qualified MAlonzo.Code.Algebra.Properties.Semiring.Exp.TCOptimised
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.List.Kleene.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Reflection
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Expression
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Addition
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Constants
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Multiplication
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics

-- Tactic.RingSolver.NonReflective.Ops.zero-homo
d_zero'45'homo_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_zero'45'homo_174 ~v0 ~v1 v2 v3 ~v4 = du_zero'45'homo_174 v2 v3
du_zero'45'homo_174 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny
du_zero'45'homo_174 v0 v1
  = let v2
          = coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'8799'__218
              v0 v1 in
    case coe v2 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.NonReflective.Ops.homo
d_homo_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66
d_homo_188 ~v0 ~v1 v2 = du_homo_188 v2
du_homo_188 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66
du_homo_188 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.C_Homomorphism'46'constructor_1691
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.C_RawCoeff'46'constructor_73
         (coe
            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
            (coe v0))
         (coe
            (\ v1 ->
               coe
                 MAlonzo.Code.Data.Maybe.Base.du_is'45'just_20
                 (coe
                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'8799'__218
                    v0 v1))))
      (coe v0)
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_788
         (coe v0))
      (\ v1 v2 -> coe du_zero'45'homo_174 (coe v0) v1)
-- Tactic.RingSolver.NonReflective.Ops._.⟦_⟧
d_'10214'_'10215'_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'_194 ~v0 ~v1 v2 = du_'10214'_'10215'_194 v2
du_'10214'_'10215'_194 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'10214'_'10215'_194 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
         (coe v0))
      (coe (\ v4 -> v4)) v2 v3
-- Tactic.RingSolver.NonReflective.Ops._.Poly
d_Poly_234 a0 a1 a2 a3 = ()
-- Tactic.RingSolver.NonReflective.Ops.norm
d_norm_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
d_norm_352 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_Κ_22 v5
        -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
             (coe (0 :: Integer))
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                (coe v5))
             (coe
                MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v3))
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_Ι_24 v5
        -> let v6
                 = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                     (coe du_homo_188 (coe v2)) in
           let v7
                 = coe
                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_go_138
                     (coe v3) (coe v5) in
           let v8
                 = let v8
                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                             (coe du_homo_188 (coe v2)) in
                   let v9 = coe MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46 in
                   let v10
                         = coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                             (coe (0 :: Integer))
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'35'_220
                                   (coe v2)))
                             (coe
                                MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_go_138
                                   (coe v3) (coe v5))) in
                   let v11 = 1 :: Integer in
                   let v12
                         = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'35'_220
                             (coe v2) in
                   let v13
                         = coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                             v8 v12 in
                   seq
                     (coe v13)
                     (if coe v13
                        then coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                               (coe v9) (coe (2 :: Integer))
                        else coe
                               MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                               (coe
                                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                  (coe
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                        v10)
                                     (coe v11))
                                  (coe v9))) in
           let v9
                 = coe
                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                     (coe v5) in
           case coe v8 of
             MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
               -> coe
                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                    (coe (0 :: Integer))
                    (coe
                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                       (coe
                          MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                             (coe v6))))
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v3))
             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v10
               -> case coe v10 of
                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v11 v12
                      -> case coe v11 of
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v13 v14
                             -> case coe v13 of
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v15
                                    -> case coe v14 of
                                         0 -> case coe v12 of
                                                MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                  -> case coe v15 of
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v17 v18 v19
                                                         -> coe
                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                                                              (coe v17) (coe v18)
                                                              (coe
                                                                 MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                 (coe
                                                                    MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                    v19)
                                                                 (coe
                                                                    MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                 (coe v9))
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v17
                                                  -> coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                                                       (coe addInt (coe (1 :: Integer)) (coe v7))
                                                       (coe
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                                (coe
                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                                   v15)
                                                                (coe (0 :: Integer)))
                                                             (coe v12))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                       (coe v9)
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> coe
                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                                                (coe addInt (coe (1 :: Integer)) (coe v7))
                                                (coe
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216
                                                   (coe
                                                      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                      (coe
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                         (coe
                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                            v15)
                                                         (coe v14))
                                                      (coe v12))
                                                   (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                (coe v9)
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C__'8853'__26 v5 v6
        -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8862'__374
             (coe v0) (coe v1)
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                (coe du_homo_188 (coe v2)))
             (coe v3)
             (coe d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v5))
             (coe d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6))
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C__'8855'__28 v5 v6
        -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8864''45'step'8242'_558
             v0 v1
             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                (coe du_homo_188 (coe v2)))
             v3 (d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v5))
             (d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6))
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C__'8859'__30 v5 v6
        -> let v7
                 = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                     (coe du_homo_188 (coe v2)) in
           let v8 = d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v5) in
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
                             (coe v7))))
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v3))
             _ -> let v9 = subInt (coe v6) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                    (coe v0) (coe v1) (coe v7) (coe v3) (coe v8) (coe v9)
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_'8861'__32 v5
        -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8863''45'step_538
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                (coe du_homo_188 (coe v2)))
             (coe v3)
             (coe d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.NonReflective.Ops.⟦_⇓⟧
d_'10214'_'8659''10215'_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'8659''10215'_374 v0 v1 v2 v3 v4
  = let v5 = coe du_homo_188 (coe v2) in
    let v6 = d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) in
    coe
      (\ v7 ->
         case coe v6 of
           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v8 v9 v10
             -> case coe v9 of
                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v11
                    -> coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                            (coe v5))
                         v11
                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v12 v13
                    -> let v14
                             = coe
                                 MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                    (coe v10) (coe v7)) in
                       case coe v12 of
                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v15 v16
                           -> case coe v15 of
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v17 v18
                                  -> case coe v17 of
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v19
                                         -> case coe v14 of
                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v21 v22
                                                -> let v23
                                                         = coe
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                             (coe v5)
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                (coe v19) (coe v16))
                                                             (coe v14) in
                                                   case coe v18 of
                                                     0 -> coe v23
                                                     _ -> coe
                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                               (coe v5))
                                                            (let v24
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                          (let v24
                                                                                 = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                     (coe v5) in
                                                                           coe
                                                                             MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                             (coe
                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                (coe v24)))) in
                                                             let v25
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                       (coe v24) in
                                                             let v26
                                                                   = subInt
                                                                       (coe v18)
                                                                       (coe (1 :: Integer)) in
                                                             let v27
                                                                   = coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                       v25
                                                                       (coe
                                                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                          (coe v25) (coe v26)
                                                                          (coe v21))
                                                                       v21 in
                                                             case coe v18 of
                                                               0 -> coe
                                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                      (coe v25)
                                                               1 -> coe v21
                                                               _ -> coe v27)
                                                            v23
                                              _ -> MAlonzo.RTE.mazUnreachableError
                                       _ -> MAlonzo.RTE.mazUnreachableError
                                _ -> MAlonzo.RTE.mazUnreachableError
                         _ -> MAlonzo.RTE.mazUnreachableError
                  _ -> MAlonzo.RTE.mazUnreachableError
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Tactic.RingSolver.NonReflective.Ops.correct
d_correct_402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_correct_402 v0 v1 v2 v3
  = coe d_go_428 (coe v0) (coe v1) (coe v2) (coe v3)
-- Tactic.RingSolver.NonReflective.Ops._.go
d_go_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_go_428 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_Κ_22 v6
        -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Constants.du_κ'45'hom_396
             (coe du_homo_188 (coe v2)) (coe v6)
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_Ι_24 v6
        -> coe
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables.du_ι'45'hom_480
             (coe du_homo_188 (coe v2)) (coe v3) (coe v6) (coe v5)
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C__'8853'__26 v6 v7
        -> coe
             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Addition.du_'8862''45'hom_492
                v0 v1 v0 v1 (coe du_homo_188 (coe v2))
                (d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6))
                (d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7)) v5)
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
                                           (coe v2))))))))))
                (let v8 = coe du_homo_188 (coe v2) in
                 let v9
                       = let v9 = coe du_homo_188 (coe v2) in
                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8862'__374
                           (coe v0) (coe v1)
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                              (coe v9))
                           (coe v3)
                           (coe d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6))
                           (coe d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7)) in
                 case coe v9 of
                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                     -> case coe v11 of
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                            -> coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                    (coe v8))
                                 v13
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                            -> let v16
                                     = coe
                                         MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                            (coe v12) (coe v5)) in
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
                                                                     (coe v8)
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                        (coe v21) (coe v18))
                                                                     (coe v16) in
                                                           case coe v20 of
                                                             0 -> coe v25
                                                             _ -> coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v8))
                                                                    (let v26
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                  (let v26
                                                                                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                             (coe
                                                                                                v8) in
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
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'43'__208
                   v2
                   (let v8 = coe du_homo_188 (coe v2) in
                    let v9 = d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) in
                    case coe v9 of
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                        -> case coe v11 of
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                               -> coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                       (coe v8))
                                    v13
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                               -> let v16
                                        = coe
                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                               (coe v12) (coe v5)) in
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
                                                                        (coe v8)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                           (coe v21) (coe v18))
                                                                        (coe v16) in
                                                              case coe v20 of
                                                                0 -> coe v25
                                                                _ -> coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v8))
                                                                       (let v26
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                     (let v26
                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                (coe
                                                                                                   v8) in
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
                      _ -> MAlonzo.RTE.mazUnreachableError)
                   (let v8 = coe du_homo_188 (coe v2) in
                    let v9 = d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7) in
                    case coe v9 of
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                        -> case coe v11 of
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                               -> coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                       (coe v8))
                                    v13
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                               -> let v16
                                        = coe
                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                               (coe v12) (coe v5)) in
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
                                                                        (coe v8)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                           (coe v21) (coe v18))
                                                                        (coe v16) in
                                                              case coe v20 of
                                                                0 -> coe v25
                                                                _ -> coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v8))
                                                                       (let v26
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                     (let v26
                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                (coe
                                                                                                   v8) in
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
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'43'__208
                   v2
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v8 -> v8)) (coe v6) (coe v5))
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v8 -> v8)) (coe v7) (coe v5))))
             (coe
                MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                (coe
                   d_go_428 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) (coe v5))
                (coe
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
                                           (coe v2)))))))))
                   (coe d_'10214'_'8659''10215'_374 v0 v1 v2 v3 v6 v5)
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v8 -> v8)) (coe v6) (coe v5))
                   (coe d_'10214'_'8659''10215'_374 v0 v1 v2 v3 v7 v5)
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v8 -> v8)) (coe v7) (coe v5)))
                (coe
                   d_go_428 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7) (coe v5)))
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C__'8855'__28 v6 v7
        -> coe
             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Multiplication.du_'8864''45'hom_882
                v0 v1 v0 v1 (coe du_homo_188 (coe v2))
                (d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6))
                (d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7)) v5)
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
                                           (coe v2))))))))))
                (let v8 = coe du_homo_188 (coe v2) in
                 let v9
                       = let v9 = coe du_homo_188 (coe v2) in
                         let v10
                               = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                   (coe v9) in
                         coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8864''45'step'8242'_558
                           v0 v1 v10 v3
                           (d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6))
                           (d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7)) in
                 case coe v9 of
                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                     -> case coe v11 of
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                            -> coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                    (coe v8))
                                 v13
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                            -> let v16
                                     = coe
                                         MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                            (coe v12) (coe v5)) in
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
                                                                     (coe v8)
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                        (coe v21) (coe v18))
                                                                     (coe v16) in
                                                           case coe v20 of
                                                             0 -> coe v25
                                                             _ -> coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v8))
                                                                    (let v26
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                  (let v26
                                                                                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                             (coe
                                                                                                v8) in
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
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                   v2
                   (let v8 = coe du_homo_188 (coe v2) in
                    let v9 = d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) in
                    case coe v9 of
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                        -> case coe v11 of
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                               -> coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                       (coe v8))
                                    v13
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                               -> let v16
                                        = coe
                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                               (coe v12) (coe v5)) in
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
                                                                        (coe v8)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                           (coe v21) (coe v18))
                                                                        (coe v16) in
                                                              case coe v20 of
                                                                0 -> coe v25
                                                                _ -> coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v8))
                                                                       (let v26
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                     (let v26
                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                (coe
                                                                                                   v8) in
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
                      _ -> MAlonzo.RTE.mazUnreachableError)
                   (let v8 = coe du_homo_188 (coe v2) in
                    let v9 = d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7) in
                    case coe v9 of
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v10 v11 v12
                        -> case coe v11 of
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v13
                               -> coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                       (coe v8))
                                    v13
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v14 v15
                               -> let v16
                                        = coe
                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                               (coe v12) (coe v5)) in
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
                                                                        (coe v8)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                           (coe v21) (coe v18))
                                                                        (coe v16) in
                                                              case coe v20 of
                                                                0 -> coe v25
                                                                _ -> coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v8))
                                                                       (let v26
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                     (let v26
                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                (coe
                                                                                                   v8) in
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
                   v2
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v8 -> v8)) (coe v6) (coe v5))
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v8 -> v8)) (coe v7) (coe v5))))
             (coe
                MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                (coe
                   d_go_428 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) (coe v5))
                (coe
                   MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                   (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                         (coe
                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                               (coe v2)))))
                   (coe d_'10214'_'8659''10215'_374 v0 v1 v2 v3 v6 v5)
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v8 -> v8)) (coe v6) (coe v5))
                   (coe d_'10214'_'8659''10215'_374 v0 v1 v2 v3 v7 v5)
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v8 -> v8)) (coe v7) (coe v5)))
                (coe
                   d_go_428 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7) (coe v5)))
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C__'8859'__30 v6 v7
        -> coe
             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation.d_'8865''45'hom_656
                (coe v0) (coe v1) (coe v0) (coe v1) (coe du_homo_188 (coe v2))
                (coe v3)
                (coe d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6))
                (coe v7) (coe v5))
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
                                           (coe v2))))))))))
                (let v8 = coe du_homo_188 (coe v2) in
                 let v9 = coe du_homo_188 (coe v2) in
                 let v10
                       = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                           (coe v9) in
                 let v11
                       = d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) in
                 case coe v7 of
                   0 -> let v12
                              = MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                                  (coe
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                     (coe v10)) in
                        coe
                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                          (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                             (coe v8))
                          v12
                   _ -> let v12
                              = let v12 = subInt (coe v7) (coe (1 :: Integer)) in
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.d__'8865'_'43'1_764
                                  (coe v0) (coe v1) (coe v10) (coe v3) (coe v11) (coe v12) in
                        case coe v12 of
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v13 v14 v15
                            -> case coe v14 of
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v16
                                   -> coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                        (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                           (coe v8))
                                        v16
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v17 v18
                                   -> let v19
                                            = coe
                                                MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                (coe
                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                   (coe v15) (coe v5)) in
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
                                                                            (coe v8)
                                                                            (coe
                                                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                               (coe v24) (coe v21))
                                                                            (coe v19) in
                                                                  case coe v23 of
                                                                    0 -> coe v28
                                                                    _ -> coe
                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                              (coe v8))
                                                                           (let v29
                                                                                  = coe
                                                                                      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                      (coe
                                                                                         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                         (let v29
                                                                                                = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                    (coe
                                                                                                       v8) in
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
                          _ -> MAlonzo.RTE.mazUnreachableError)
                (let v8 = coe du_homo_188 (coe v2) in
                 let v9
                       = coe
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                              (coe v8)) in
                 let v10
                       = coe MAlonzo.Code.Algebra.Bundles.du_semiring_2282 (coe v9) in
                 let v11
                       = coe
                           MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                           (coe
                              MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                              (coe v10)) in
                 let v12
                       = let v12 = coe du_homo_188 (coe v2) in
                         let v13
                               = d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) in
                         case coe v13 of
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v14 v15 v16
                             -> case coe v15 of
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v17
                                    -> coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                            (coe v12))
                                         v17
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v18 v19
                                    -> let v20
                                             = coe
                                                 MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                 (coe
                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                    (coe v16) (coe v5)) in
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
                                                                             (coe v12)
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                (coe v25) (coe v22))
                                                                             (coe v20) in
                                                                   case coe v24 of
                                                                     0 -> coe v29
                                                                     _ -> coe
                                                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                               (coe v12))
                                                                            (let v30
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                       (coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                          (let v30
                                                                                                 = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                     (coe
                                                                                                        v12) in
                                                                                           coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                             (coe
                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                (coe
                                                                                                   v30)))) in
                                                                             let v31
                                                                                   = coe
                                                                                       MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                       (coe v30) in
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
                                                                                          (coe v31)
                                                                                          (coe v32)
                                                                                          (coe v27))
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
                 let v13
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                           (coe v11) in
                 let v14 = subInt (coe v7) (coe (1 :: Integer)) in
                 let v15
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v13
                           (coe
                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                              (coe v13) (coe v14) (coe v12))
                           v12 in
                 case coe v7 of
                   0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v13)
                   1 -> coe v12
                   _ -> coe v15)
                (coe
                   MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                   (\ v8 v9 -> v9)
                   (\ v8 ->
                      let v9
                            = coe
                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                   (coe v2)) in
                      let v10
                            = coe
                                MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                (coe
                                   MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                   (coe v9)) in
                      let v11
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                (coe v10) in
                      let v12 = subInt (coe v7) (coe (1 :: Integer)) in
                      let v13
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v11
                                (coe
                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                   (coe v11) (coe v12) (coe v8))
                                v8 in
                      case coe v7 of
                        0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v11)
                        1 -> coe v8
                        _ -> coe v13)
                   (coe d_'10214'_'8659''10215'_374 v0 v1 v2 v3 v6 v5)
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v8 -> v8)) (coe v6) (coe v5))))
             (coe
                MAlonzo.Code.Algebra.Properties.Semiring.Exp.TCOptimised.du_'94''45'cong'737'_214
                (coe
                   MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                      (coe v2)))
                v7 (coe d_'10214'_'8659''10215'_374 v0 v1 v2 v3 v6 v5)
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                      (coe v2))
                   (coe (\ v8 -> v8)) (coe v6) (coe v5))
                (d_go_428 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) (coe v5)))
      MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_'8861'__32 v6
        -> coe
             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation.du_'8863''45'hom_518
                (coe du_homo_188 (coe v2))
                (d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6)) v5)
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
                                           (coe v2))))))))))
                (let v7 = coe du_homo_188 (coe v2) in
                 let v8
                       = let v8 = coe du_homo_188 (coe v2) in
                         let v9
                               = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                   (coe v8) in
                         coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8863''45'step_538
                           (coe v9) (coe v3)
                           (coe d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6)) in
                 case coe v8 of
                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                     -> case coe v10 of
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                            -> coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                    (coe v7))
                                 v12
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                            -> let v15
                                     = coe
                                         MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                            (coe v11) (coe v5)) in
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
                                                                     (coe v7)
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                        (coe v20) (coe v17))
                                                                     (coe v15) in
                                                           case coe v19 of
                                                             0 -> coe v24
                                                             _ -> coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v7))
                                                                    (let v25
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                  (let v25
                                                                                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                             (coe
                                                                                                v7) in
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
                                                                                  (1 :: Integer)) in
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
                   _ -> MAlonzo.RTE.mazUnreachableError)
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                   v2
                   (let v7 = coe du_homo_188 (coe v2) in
                    let v8 = d_norm_352 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) in
                    case coe v8 of
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v9 v10 v11
                        -> case coe v10 of
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v12
                               -> coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                       (coe v7))
                                    v12
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v13 v14
                               -> let v15
                                        = coe
                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                               (coe v11) (coe v5)) in
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
                                                                        (coe v7)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                           (coe v20) (coe v17))
                                                                        (coe v15) in
                                                              case coe v19 of
                                                                0 -> coe v24
                                                                _ -> coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                          (coe v7))
                                                                       (let v25
                                                                              = coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                  (coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                     (let v25
                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                (coe
                                                                                                   v7) in
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
                      _ -> MAlonzo.RTE.mazUnreachableError))
                (coe
                   MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                   (\ v7 v8 -> v8)
                   (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                      (coe v2))
                   (coe d_'10214'_'8659''10215'_374 v0 v1 v2 v3 v6 v5)
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                         (coe v2))
                      (coe (\ v7 -> v7)) (coe v6) (coe v5))))
             (coe
                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255'cong_64
                (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                   (coe v2))
                (coe d_'10214'_'8659''10215'_374 v0 v1 v2 v3 v6 v5)
                (coe
                   MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
                      (coe v2))
                   (coe (\ v7 -> v7)) (coe v6) (coe v5))
                (d_go_428 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) (coe v5)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.NonReflective.Ops._._⊜_
d__'8860'__462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d__'8860'__462 ~v0 ~v1 ~v2 = du__'8860'__462
du__'8860'__462 ::
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du__'8860'__462 v0
  = coe MAlonzo.Code.Relation.Binary.Reflection.du__'8860'__142
-- Tactic.RingSolver.NonReflective.Ops._.close
d_close_464 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  () -> Integer -> AgdaAny -> AgdaAny
d_close_464 ~v0 ~v1 ~v2 = du_close_464
du_close_464 :: () -> Integer -> AgdaAny -> AgdaAny
du_close_464 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reflection.du_close_104
      (coe
         (\ v3 ->
            coe MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_Ι_24))
      v1 v2
-- Tactic.RingSolver.NonReflective.Ops._.prove
d_prove_466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  AgdaAny -> AgdaAny
d_prove_466 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reflection.du_prove_90
      (let v3
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                 (coe v2) in
       let v4
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                 (coe v3) in
       let v5
             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v4) in
       let v6
             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                 (coe v5) in
       let v7
             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                 (coe v6) in
       let v8 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v7) in
       let v9
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v8) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v9)))
      (\ v3 v4 v5 ->
         coe
           MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
           (coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
              (coe v2))
           (coe (\ v6 -> v6)) v4 v5)
      (coe d_'10214'_'8659''10215'_374 (coe v0) (coe v1) (coe v2))
      (coe d_correct_402 (coe v0) (coe v1) (coe v2))
-- Tactic.RingSolver.NonReflective.Ops._.solve
d_solve_468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny
d_solve_468 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reflection.du_solve_114
      (let v3
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                 (coe v2) in
       let v4
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                 (coe v3) in
       let v5
             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v4) in
       let v6
             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                 (coe v5) in
       let v7
             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                 (coe v6) in
       let v8 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v7) in
       let v9
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v8) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v9)))
      (coe
         (\ v3 ->
            coe MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_Ι_24))
      (\ v3 v4 v5 ->
         coe
           MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
           (coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
              (coe v2))
           (coe (\ v6 -> v6)) v4 v5)
      (coe d_'10214'_'8659''10215'_374 (coe v0) (coe v1) (coe v2))
      (coe d_correct_402 (coe v0) (coe v1) (coe v2))
-- Tactic.RingSolver.NonReflective.Ops._.solve₁
d_solve'8321'_470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer -> AgdaAny -> AgdaAny
d_solve'8321'_470 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reflection.du_solve'8321'_130
      (let v3
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                 (coe v2) in
       let v4
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                 (coe v3) in
       let v5
             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v4) in
       let v6
             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                 (coe v5) in
       let v7
             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                 (coe v6) in
       let v8 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v7) in
       let v9
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v8) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v9)))
      (coe
         (\ v3 ->
            coe MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_Ι_24))
      (\ v3 v4 v5 ->
         coe
           MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
           (coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
              (coe v2))
           (coe (\ v6 -> v6)) v4 v5)
      (coe d_'10214'_'8659''10215'_374 (coe v0) (coe v1) (coe v2))
      (coe d_correct_402 (coe v0) (coe v1) (coe v2))
-- Tactic.RingSolver.NonReflective.solve
d_solve_476 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny
d_solve_476 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reflection.du_solve_114
      (let v3
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                 (coe v2) in
       let v4
             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                 (coe v3) in
       let v5
             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v4) in
       let v6
             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                 (coe v5) in
       let v7
             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                 (coe v6) in
       let v8 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v7) in
       let v9
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v8) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v9)))
      (coe
         (\ v3 ->
            coe MAlonzo.Code.Tactic.RingSolver.Core.Expression.C_Ι_24))
      (\ v3 v4 v5 ->
         coe
           MAlonzo.Code.Tactic.RingSolver.Core.Expression.du_'10214'_'10215'_88
           (coe
              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_rawRing_346
              (coe v2))
           (coe (\ v6 -> v6)) v4 v5)
      (coe d_'10214'_'8659''10215'_374 (coe v0) (coe v1) (coe v2))
      (coe d_correct_402 (coe v0) (coe v1) (coe v2))
-- Tactic.RingSolver.NonReflective._⊜_
d__'8860'__480 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d__'8860'__480 ~v0 ~v1 = du__'8860'__480
du__'8860'__480 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Expression.T_Expr_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du__'8860'__480 = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
