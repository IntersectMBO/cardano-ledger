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

module MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Constants where

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
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics

-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Constants._.κ
d_κ_304 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  AgdaAny ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
d_κ_304 ~v0 v1 v2 = du_κ_304 v1 v2
du_κ_304 ::
  Integer ->
  AgdaAny ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
du_κ_304 v0 v1
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
      (coe (0 :: Integer))
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
         (coe v1))
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Constants._.⟦_⟧
d_'10214'_'10215'_388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'_388 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7
  = du_'10214'_'10215'_388 v4 v6 v7
du_'10214'_'10215'_388 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'10214'_'10215'_388 v0 v1 v2
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
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Constants.κ-hom
d_κ'45'hom_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_κ'45'hom_396 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 ~v7
  = du_κ'45'hom_396 v4 v6
du_κ'45'hom_396 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  AgdaAny -> AgdaAny
du_κ'45'hom_396 v0 v1
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
         (coe v0))
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
            (coe v0))
         v1)
