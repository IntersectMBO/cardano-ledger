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

module MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Definitions.RawMonoid
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Fin.Base
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
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics

-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables._.PowInd
d_PowInd_314 a0 a1 a2 a3 a4 a5 a6 = ()
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables._.ι
d_ι_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
d_ι_352 ~v0 ~v1 ~v2 ~v3 v4 = du_ι_352 v4
du_ι_352 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
du_ι_352 v0
  = let v1
          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
              (coe v0) in
    coe
      (\ v2 v3 ->
         let v4
               = coe
                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_go_138
                   (coe v2) (coe v3) in
         let v5
               = let v5 = coe MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46 in
                 let v6
                       = coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                           (coe (0 :: Integer))
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe v1))))
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_go_138
                                 (coe v2) (coe v3))) in
                 let v7 = 1 :: Integer in
                 let v8
                       = MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                              (coe v1)) in
                 let v9
                       = coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                           v1 v8 in
                 seq
                   (coe v9)
                   (if coe v9
                      then coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                             (coe v5) (coe (2 :: Integer))
                      else coe
                             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                             (coe
                                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                      v6)
                                   (coe v7))
                                (coe v5))) in
         let v6
               = coe
                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                   (coe v3) in
         case coe v5 of
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
                           (coe v1))))
                  (coe
                     MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v2))
           MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v7
             -> case coe v7 of
                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v8 v9
                    -> case coe v8 of
                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v10 v11
                           -> case coe v10 of
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v12
                                  -> case coe v11 of
                                       0 -> case coe v9 of
                                              MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                -> case coe v12 of
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v14 v15 v16
                                                       -> coe
                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                                                            (coe v14) (coe v15)
                                                            (coe
                                                               MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                               (coe
                                                                  MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                  v16)
                                                               (coe
                                                                  MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                               (coe v6))
                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                              MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v14
                                                -> coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                                                     (coe addInt (coe (1 :: Integer)) (coe v4))
                                                     (coe
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216
                                                        (coe
                                                           MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                           (coe
                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                              (coe
                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                                 v12)
                                                              (coe (0 :: Integer)))
                                                           (coe v9))
                                                        (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                     (coe v6)
                                              _ -> MAlonzo.RTE.mazUnreachableError
                                       _ -> coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                                              (coe addInt (coe (1 :: Integer)) (coe v4))
                                              (coe
                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216
                                                 (coe
                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                                       (coe
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                                          v12)
                                                       (coe v11))
                                                    (coe v9))
                                                 (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                              (coe v6)
                                _ -> MAlonzo.RTE.mazUnreachableError
                         _ -> MAlonzo.RTE.mazUnreachableError
                  _ -> MAlonzo.RTE.mazUnreachableError
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables._.⟦_⟧
d_'10214'_'10215'_472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'_472 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7
  = du_'10214'_'10215'_472 v4 v6 v7
du_'10214'_'10215'_472 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'10214'_'10215'_472 v0 v1 v2
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
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables.ι-hom
d_ι'45'hom_480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_ι'45'hom_480 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_ι'45'hom_480 v4 v5 v6 v7
du_ι'45'hom_480 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_ι'45'hom_480 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (let v4
                = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                    (coe
                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                       (coe v0)) in
          let v5
                = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                    (coe v4) in
          let v6
                = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v5) in
          let v7
                = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                    (coe v6) in
          let v8
                = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                    (coe v7) in
          let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v8) in
          let v10
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))
         (let v4
                = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                    (coe v0) in
          let v5
                = let v5
                        = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                            (coe v0) in
                  let v6 = coe MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46 in
                  let v7
                        = coe
                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                            (coe (0 :: Integer))
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                                  (coe
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                        (coe v0)))))
                            (coe
                               MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                               (coe
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_go_138
                                  (coe v1) (coe v2))) in
                  let v8 = 1 :: Integer in
                  let v9
                        = MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                               (coe
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                  (coe v0))) in
                  let v10
                        = coe
                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                            v5 v9 in
                  seq
                    (coe v10)
                    (if coe v10
                       then coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                              (coe v6) (coe (2 :: Integer))
                       else coe
                              MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                              (coe
                                 MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                    (coe
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                       v7)
                                    (coe v8))
                                 (coe v6))) in
          let v6
                = coe
                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                    (coe v2) in
          case coe v5 of
            MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
              -> let v7
                       = MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                              (coe v4)) in
                 coe
                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                      (coe v0))
                   v7
            MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v7
              -> case coe v7 of
                   MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v8 v9
                     -> case coe v8 of
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v10 v11
                            -> case coe v10 of
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v12
                                   -> case coe v11 of
                                        0 -> case coe v9 of
                                               MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                 -> case coe v12 of
                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v14 v15 v16
                                                        -> let v17
                                                                 = coe
                                                                     MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                     (coe
                                                                        MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                        v16)
                                                                     (coe
                                                                        MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                     (coe v6) in
                                                           case coe v15 of
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v18
                                                               -> coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                       (coe v0))
                                                                    v18
                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v19 v20
                                                               -> let v21
                                                                        = coe
                                                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                            (coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                               (coe v17)
                                                                               (coe v3)) in
                                                                  case coe v19 of
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v22 v23
                                                                      -> case coe v22 of
                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v24 v25
                                                                             -> case coe v24 of
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v26
                                                                                    -> case coe
                                                                                              v21 of
                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v28 v29
                                                                                           -> let v30
                                                                                                    = coe
                                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                        (coe
                                                                                                           v0)
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                           (coe
                                                                                                              v26)
                                                                                                           (coe
                                                                                                              v23))
                                                                                                        (coe
                                                                                                           v21) in
                                                                                              case coe
                                                                                                     v25 of
                                                                                                0 -> coe
                                                                                                       v30
                                                                                                _ -> coe
                                                                                                       MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                       (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                          (coe
                                                                                                             v0))
                                                                                                       (let v31
                                                                                                              = coe
                                                                                                                  MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                  (coe
                                                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                     (let v31
                                                                                                                            = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                (coe
                                                                                                                                   v0) in
                                                                                                                      coe
                                                                                                                        MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                                                        (coe
                                                                                                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                                                           (coe
                                                                                                                              v31)))) in
                                                                                                        let v32
                                                                                                              = coe
                                                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                                                  (coe
                                                                                                                     v31) in
                                                                                                        let v33
                                                                                                              = subInt
                                                                                                                  (coe
                                                                                                                     v25)
                                                                                                                  (coe
                                                                                                                     (1 ::
                                                                                                                        Integer)) in
                                                                                                        let v34
                                                                                                              = coe
                                                                                                                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                                                  v32
                                                                                                                  (coe
                                                                                                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                                                     (coe
                                                                                                                        v32)
                                                                                                                     (coe
                                                                                                                        v33)
                                                                                                                     (coe
                                                                                                                        v28))
                                                                                                                  v28 in
                                                                                                        case coe
                                                                                                               v25 of
                                                                                                          0 -> coe
                                                                                                                 MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                                                 (coe
                                                                                                                    v32)
                                                                                                          1 -> coe
                                                                                                                 v28
                                                                                                          _ -> coe
                                                                                                                 v34)
                                                                                                       v30
                                                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v14
                                                 -> let v15
                                                          = coe
                                                              MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                              (coe
                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                 (coe v6) (coe v3)) in
                                                    let v16 = 0 :: Integer in
                                                    case coe v15 of
                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v17 v18
                                                        -> let v19
                                                                 = coe
                                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                     (coe v0)
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                        (coe v12) (coe v9))
                                                                     (coe v15) in
                                                           case coe v16 of
                                                             0 -> coe v19
                                                             _ -> coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                       (coe v0))
                                                                    (let v20
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                  (let v20
                                                                                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                             (coe
                                                                                                v0) in
                                                                                   coe
                                                                                     MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                     (coe
                                                                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                        (coe
                                                                                           v20)))) in
                                                                     let v21
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                               (coe v20) in
                                                                     let v22 = -1 :: Integer in
                                                                     let v23
                                                                           = coe
                                                                               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                               v21
                                                                               (coe
                                                                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                  (coe v21)
                                                                                  (coe v22)
                                                                                  (coe v17))
                                                                               v17 in
                                                                     case coe v16 of
                                                                       0 -> coe
                                                                              MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                              (coe v21)
                                                                       1 -> coe v17
                                                                       _ -> coe v23)
                                                                    v19
                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                        _ -> let v14
                                                   = coe
                                                       MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                       (coe
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                          (coe v6) (coe v3)) in
                                             case coe v14 of
                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v15 v16
                                                 -> let v17
                                                          = coe
                                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                              (coe v0)
                                                              (coe
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                 (coe v12) (coe v9))
                                                              (coe v14) in
                                                    case coe v11 of
                                                      0 -> coe v17
                                                      _ -> coe
                                                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                (coe v0))
                                                             (let v18
                                                                    = coe
                                                                        MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                        (coe
                                                                           MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                           (let v18
                                                                                  = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                      (coe v0) in
                                                                            coe
                                                                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                              (coe
                                                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                 (coe v18)))) in
                                                              let v19
                                                                    = coe
                                                                        MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                        (coe v18) in
                                                              let v20
                                                                    = subInt
                                                                        (coe v11)
                                                                        (coe (1 :: Integer)) in
                                                              let v21
                                                                    = coe
                                                                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                        v19
                                                                        (coe
                                                                           MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                           (coe v19) (coe v20)
                                                                           (coe v15))
                                                                        v15 in
                                                              case coe v11 of
                                                                0 -> coe
                                                                       MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                       (coe v19)
                                                                1 -> coe v15
                                                                _ -> coe v21)
                                                             v17
                                               _ -> MAlonzo.RTE.mazUnreachableError
                                 _ -> MAlonzo.RTE.mazUnreachableError
                          _ -> MAlonzo.RTE.mazUnreachableError
                   _ -> MAlonzo.RTE.mazUnreachableError
            _ -> MAlonzo.RTE.mazUnreachableError)
         (coe
            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8512''63''10214'_'10215'_606
            v0
            (let v4
                   = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                       (coe v0) in
             let v5 = coe MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46 in
             let v6
                   = coe
                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                       (coe (0 :: Integer))
                       (coe
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                          (coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                   (coe v0)))))
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_go_138
                             (coe v1) (coe v2))) in
             let v7 = 1 :: Integer in
             let v8
                   = MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                       (coe
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                             (coe v0))) in
             let v9
                   = coe
                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                       v4 v8 in
             seq
               (coe v9)
               (if coe v9
                  then coe
                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                         (coe v5) (coe (2 :: Integer))
                  else coe
                         MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                         (coe
                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                               (coe
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                  v6)
                               (coe v7))
                            (coe v5))))
            (coe
               MAlonzo.Code.Data.Vec.Base.du_uncons_594
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                  (coe
                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                     (coe v2))
                  (coe v3))))
         (coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v3) (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                       (coe
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                          (coe v0)) in
             let v5
                   = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                       (coe v4) in
             let v6
                   = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v5) in
             let v7
                   = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                       (coe v6) in
             let v8
                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                       (coe v7) in
             let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v8) in
             let v10
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8512''63''10214'_'10215'_606
               v0
               (let v4
                      = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                          (coe v0) in
                let v5 = coe MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46 in
                let v6
                      = coe
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                          (coe (0 :: Integer))
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                             (coe
                                MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                      (coe v0)))))
                          (coe
                             MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_go_138
                                (coe v1) (coe v2))) in
                let v7 = 1 :: Integer in
                let v8
                      = MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                (coe v0))) in
                let v9
                      = coe
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                          v4 v8 in
                seq
                  (coe v9)
                  (if coe v9
                     then coe
                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                            (coe v5) (coe (2 :: Integer))
                     else coe
                            MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                            (coe
                               MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                               (coe
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                                  (coe
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                     v6)
                                  (coe v7))
                               (coe v5))))
               (coe
                  MAlonzo.Code.Data.Vec.Base.du_uncons_594
                  (coe
                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                        (coe v2))
                     (coe v3))))
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                  (coe v0))
               (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                  (coe
                     MAlonzo.Code.Data.Vec.Base.du_uncons_594
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                           (coe v2))
                        (coe v3))))
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                (coe v0))) in
                coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                     (coe v0))
                  v4))
            (coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v3) (coe v2))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                             (coe v0)) in
                let v5
                      = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                          (coe v4) in
                let v6
                      = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v5) in
                let v7
                      = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                          (coe v6) in
                let v8
                      = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                          (coe v7) in
                let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v8) in
                let v10
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                     (coe v0))
                  (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                     (coe
                        MAlonzo.Code.Data.Vec.Base.du_uncons_594
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                              (coe v2))
                           (coe v3))))
                  (let v4
                         = MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                   (coe v0))) in
                   coe
                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                        (coe v0))
                     v4))
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                     (coe v0))
                  (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                     (coe
                        MAlonzo.Code.Data.Vec.Base.du_uncons_594
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                              (coe v2))
                           (coe v3))))
                  (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'35'_220
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                        (coe v0))))
               (coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v3) (coe v2))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v4
                         = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                (coe v0)) in
                   let v5
                         = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                             (coe v4) in
                   let v6
                         = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v5) in
                   let v7
                         = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                             (coe v6) in
                   let v8
                         = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                             (coe v7) in
                   let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v8) in
                   let v10
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))
                  (coe
                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                        (coe v0))
                     (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                        (coe
                           MAlonzo.Code.Data.Vec.Base.du_uncons_594
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                                 (coe v2))
                              (coe v3))))
                     (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'35'_220
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                           (coe v0))))
                  (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                     (coe
                        MAlonzo.Code.Data.Vec.Base.du_uncons_594
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                              (coe v2))
                           (coe v3))))
                  (coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v3) (coe v2))
                  (let v4
                         = let v4
                                 = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                        (coe v0)) in
                           let v5
                                 = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v4) in
                           let v6
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v5) in
                           let v7
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v6) in
                           let v8
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v7) in
                           let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v8) in
                           let v10
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)) in
                   coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4)))
                     (coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v3) (coe v2)))
                  (let v4
                         = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                             (coe v0) in
                   let v5
                         = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                             (coe v4) in
                   let v6
                         = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                             (coe v5) in
                   let v7
                         = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v6) in
                   let v8
                         = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                             (coe v7) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_identity'691'_642
                     (coe
                        MAlonzo.Code.Algebra.Structures.du_'42''45'isMonoid_1352 (coe v8))
                     (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                        (coe
                           MAlonzo.Code.Data.Vec.Base.du_uncons_594
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                                 (coe v2))
                              (coe v3))))))
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
                  (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                     (coe
                        MAlonzo.Code.Data.Vec.Base.du_uncons_594
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                              (coe v2))
                           (coe v3))))
                  (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                     (coe
                        MAlonzo.Code.Data.Vec.Base.du_uncons_594
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                              (coe v2))
                           (coe v3))))
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
                      MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274 (coe v4)))
                  (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'35'_220
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                        (coe v0)))
                  (coe
                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_refl_358
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                        (coe v0))
                     (coe
                        MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                        (coe
                           MAlonzo.Code.Data.Vec.Base.du_uncons_594
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                              (coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                                 (coe v2))
                              (coe v3)))))
                  (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_1'45'homo_780
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                        (coe v0)))))
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8759''8595''45'hom'45's_752
               (coe v0)
               (coe
                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                  (coe (0 :: Integer))
                  (coe
                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                           (coe
                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                              (coe v0)))))
                  (coe
                     MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_go_138
                        (coe v1) (coe v2))))
               (coe (0 :: Integer))
               (coe MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46)
               (coe
                  MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                  (coe
                     MAlonzo.Code.Data.Vec.Base.du_uncons_594
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                           (coe v2))
                        (coe v3))))
               (coe
                  MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                  (coe
                     MAlonzo.Code.Data.Vec.Base.du_uncons_594
                     (coe
                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                        (coe
                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
                           (coe v2))
                        (coe v3))))))
         (coe
            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8848''8595''45'hom_974
            (coe v0)
            (let v4
                   = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                       (coe v0) in
             let v5 = coe MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46 in
             let v6
                   = coe
                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206
                       (coe (0 :: Integer))
                       (coe
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208
                          (coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                   (coe v0)))))
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_go_138
                             (coe v1) (coe v2))) in
             let v7 = 1 :: Integer in
             let v8
                   = MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                       (coe
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                             (coe v0))) in
             let v9
                   = coe
                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                       v4 v8 in
             coe
               seq (coe v9)
               (if coe v9
                  then coe
                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du__'9043''42'__252
                         (coe v5) (coe (2 :: Integer))
                  else coe
                         MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                         (coe
                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170
                               (coe
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230
                                  v6)
                               (coe v7))
                            (coe v5))))
            (coe
               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_space'8804''8242'n_148
               (coe v2))
            (coe v3)))
