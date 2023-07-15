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

module MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics where

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
import qualified MAlonzo.Code.Data.List.Kleene.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters

-- Tactic.RingSolver.Core.Polynomial.Semantics._.Coeff
d_Coeff_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer -> ()
d_Coeff_244 = erased
-- Tactic.RingSolver.Core.Polynomial.Semantics._.Poly
d_Poly_256 a0 a1 a2 a3 a4 a5 = ()
-- Tactic.RingSolver.Core.Polynomial.Semantics.drop
d_drop_388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_drop_388 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 = du_drop_388 v7 v8
du_drop_388 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_drop_388 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276 -> coe v1
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v5 v6
               -> coe du_drop_388 (coe v3) (coe v6)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Semantics.drop-1
d_drop'45'1_400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_drop'45'1_400 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8
  = du_drop'45'1_400 v7 v8
du_drop'45'1_400 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_drop'45'1_400 v0 v1
  = coe
      MAlonzo.Code.Data.Vec.Base.du_uncons_594
      (coe du_drop_388 (coe v0) (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Semantics._*⟨_⟩^_
d__'42''10216'_'10217''94'__406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> Integer -> AgdaAny
d__'42''10216'_'10217''94'__406 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du__'42''10216'_'10217''94'__406 v4 v5 v6 v7
du__'42''10216'_'10217''94'__406 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> AgdaAny -> Integer -> AgdaAny
du__'42''10216'_'10217''94'__406 v0 v1 v2 v3
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
-- Tactic.RingSolver.Core.Polynomial.Semantics._⟦∷⟧_
d__'10214''8759''10215'__420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d__'10214''8759''10215'__420 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7
  = du__'10214''8759''10215'__420 v4 v6 v7
du__'10214''8759''10215'__420 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du__'10214''8759''10215'__420 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> case coe v4 of
             MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
                      -> case coe v3 of
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
                                                 (coe du_drop_388 (coe v9) (coe v6)) in
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
                                                                             du__'10214''8759''10215'__420
                                                                             (coe v0)
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                (coe v18) (coe v15))
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
                                                                                          (coe v20))
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
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v5
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                      -> coe
                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'43'__208
                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                              (coe v0))
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                 (coe v0))
                              v6
                              (case coe v5 of
                                 MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v8 v9
                                   -> case coe v8 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v10 v11
                                          -> case coe v10 of
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v12
                                                 -> let v14
                                                          = coe
                                                              du__'10214''8759''10215'__420 (coe v0)
                                                              (coe
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                 (coe v12) (coe v9))
                                                              (coe v2) in
                                                    case coe v11 of
                                                      0 -> coe v14
                                                      _ -> coe
                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                (coe v0))
                                                             (let v15
                                                                    = coe
                                                                        MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                        (coe
                                                                           MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                           (let v15
                                                                                  = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                      (coe v0) in
                                                                            coe
                                                                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                              (coe
                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                 (coe v15)))) in
                                                              let v16
                                                                    = coe
                                                                        MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                        (coe v15) in
                                                              let v17
                                                                    = subInt
                                                                        (coe v11)
                                                                        (coe (1 :: Integer)) in
                                                              let v18
                                                                    = coe
                                                                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                        v16
                                                                        (coe
                                                                           MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                           (coe v16) (coe v17)
                                                                           (coe v6))
                                                                        v6 in
                                                              case coe v11 of
                                                                0 -> coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                       (coe v16)
                                                                1 -> coe v6
                                                                _ -> coe v18)
                                                             v14
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError))
                           (case coe v3 of
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v8 v9 v10
                                -> case coe v9 of
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v11
                                       -> coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                               (coe v0))
                                            v11
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v12 v13
                                       -> let v14
                                                = coe
                                                    MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                    (coe du_drop_388 (coe v10) (coe v7)) in
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
                                                                                du__'10214''8759''10215'__420
                                                                                (coe v0)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe v19)
                                                                                   (coe v16))
                                                                                (coe v14) in
                                                                      case coe v18 of
                                                                        0 -> coe v23
                                                                        _ -> coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                  (coe v0))
                                                                               (let v24
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                             (let v24
                                                                                                    = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                        (coe
                                                                                                           v0) in
                                                                                              coe
                                                                                                MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                (coe
                                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                   (coe
                                                                                                      v24)))) in
                                                                                let v25
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                          (coe
                                                                                             v24) in
                                                                                let v26
                                                                                      = subInt
                                                                                          (coe v18)
                                                                                          (coe
                                                                                             (1 ::
                                                                                                Integer)) in
                                                                                let v27
                                                                                      = coe
                                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                          v25
                                                                                          (coe
                                                                                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                             (coe
                                                                                                v25)
                                                                                             (coe
                                                                                                v26)
                                                                                             (coe
                                                                                                v21))
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
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Semantics.⅀⟦_⟧
d_'8512''10214'_'10215'_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_'8512''10214'_'10215'_424 v0 v1 v2 v3 v4 v5 v6 v7
  = case coe v6 of
      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v8 v9
        -> case coe v8 of
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v10 v11
               -> case coe v10 of
                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v12
                      -> case coe v7 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v14 v15
                             -> let v16
                                      = coe
                                          du__'10214''8759''10215'__420 (coe v4)
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v12)
                                             (coe v9))
                                          (coe v7) in
                                case coe v11 of
                                  0 -> coe v16
                                  _ -> coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                            (coe v4))
                                         (let v17
                                                = coe
                                                    MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                    (coe
                                                       MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                       (let v17
                                                              = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                  (coe v4) in
                                                        coe
                                                          MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                          (coe
                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                             (coe v17)))) in
                                          let v18
                                                = coe
                                                    MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                    (coe v17) in
                                          let v19 = subInt (coe v11) (coe (1 :: Integer)) in
                                          let v20
                                                = coe
                                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                    v18
                                                    (coe
                                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                       (coe v18) (coe v19) (coe v14))
                                                    v14 in
                                          case coe v11 of
                                            0 -> coe
                                                   MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v18)
                                            1 -> coe v14
                                            _ -> coe v20)
                                         v16
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Semantics.⟦_⟧
d_'10214'_'10215'_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'_428 v0 v1 v2 v3 v4 ~v5 v6 v7
  = du_'10214'_'10215'_428 v0 v1 v2 v3 v4 v6 v7
du_'10214'_'10215'_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'10214'_'10215'_428 v0 v1 v2 v3 v4 v5 v6
  = case coe v5 of
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v7 v8 v9
        -> case coe v8 of
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v10
               -> coe
                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                       (coe v4))
                    v10
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v11 v12
               -> let v13 = subInt (coe v7) (coe (1 :: Integer)) in
                  let v14
                        = coe
                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                            (coe du_drop_388 (coe v9) (coe v6)) in
                  case coe v11 of
                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v15 v16
                      -> case coe v15 of
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v17 v18
                             -> case coe v17 of
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v19
                                    -> case coe v14 of
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v21 v22
                                           -> let v23
                                                    = coe
                                                        du__'10214''8759''10215'__420 (coe v4)
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                           (coe v19) (coe v16))
                                                        (coe v14) in
                                              case coe v18 of
                                                0 -> coe v23
                                                _ -> coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                          (coe v4))
                                                       (let v24
                                                              = coe
                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                     (let v24
                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                (coe v4) in
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
                                                                  (coe v18) (coe (1 :: Integer)) in
                                                        let v27
                                                              = coe
                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                  v25
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                     (coe v25) (coe v26) (coe v21))
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
      _ -> MAlonzo.RTE.mazUnreachableError
