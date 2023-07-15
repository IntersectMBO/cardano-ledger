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

module MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation where

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
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.List.Kleene.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Induction.WellFounded
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics

-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation._.Poly
d_Poly_344 a0 a1 a2 a3 a4 a5 = ()
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation._.⊟_
d_'8863'__402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
d_'8863'__402 ~v0 ~v1 ~v2 ~v3 v4 = du_'8863'__402 v4
du_'8863'__402 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
du_'8863'__402 v0
  = let v1
          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
              (coe v0) in
    coe
      (\ v2 ->
         coe
           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8863''45'step_538
           (coe v1) (coe v2))
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation._.⊟-step
d_'8863''45'step_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
d_'8863''45'step_404 ~v0 ~v1 ~v2 ~v3 v4 = du_'8863''45'step_404 v4
du_'8863''45'step_404 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174
du_'8863''45'step_404 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8863''45'step_538
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
         (coe v0))
      v1 v3
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation._.⟦_⟧
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
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation.⊟-step-hom
d_'8863''45'step'45'hom_482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8863''45'step'45'hom_482 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7 v8
  = du_'8863''45'step'45'hom_482 v4 v7 v8
du_'8863''45'step'45'hom_482 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'8863''45'step'45'hom_482 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v3 v4 v5
        -> case coe v4 of
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v6
               -> coe
                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255'homo_776
                    (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                       (coe v0))
                    v6
             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v7 v8
               -> let v9 = subInt (coe v3) (coe (1 :: Integer)) in
                  coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v10
                              = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                  (coe
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                     (coe v0)) in
                        let v11
                              = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v10) in
                        let v12
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v11) in
                        let v13
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v12) in
                        let v14
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v13) in
                        let v15
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v14) in
                        let v16
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v15) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v16)))
                       (let v10
                              = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                  (coe v0) in
                        let v11
                              = coe
                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_para_338
                                  (coe
                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                     (coe v0))
                                  (coe
                                     MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8863''45'step_538
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                           (coe v0))
                                        (coe v9)))
                                  (coe v7) in
                        case coe v11 of
                          MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                            -> let v12
                                     = MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                            (coe v10)) in
                               coe
                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                    (coe v0))
                                 v12
                          MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v12
                            -> case coe v12 of
                                 MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v13 v14
                                   -> case coe v13 of
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v15 v16
                                          -> case coe v15 of
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v17
                                                 -> case coe v16 of
                                                      0 -> case coe v14 of
                                                             MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                               -> case coe v17 of
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8848'__206 v19 v20 v21
                                                                      -> let v22
                                                                               = coe
                                                                                   MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                   (coe
                                                                                      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                      v21)
                                                                                   (coe
                                                                                      MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                   (coe v5) in
                                                                         case coe v20 of
                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_Κ_208 v23
                                                                             -> coe
                                                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'10214'_'10215'_770
                                                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_morphism_464
                                                                                     (coe v0))
                                                                                  v23
                                                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C_'8512'_216 v24 v25
                                                                             -> let v26
                                                                                      = coe
                                                                                          MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                                          (coe
                                                                                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                                             (coe
                                                                                                v22)
                                                                                             (coe
                                                                                                v2)) in
                                                                                case coe v24 of
                                                                                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v27 v28
                                                                                    -> case coe
                                                                                              v27 of
                                                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v29 v30
                                                                                           -> case coe
                                                                                                     v29 of
                                                                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v31
                                                                                                  -> case coe
                                                                                                            v26 of
                                                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v33 v34
                                                                                                         -> let v35
                                                                                                                  = coe
                                                                                                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                                                      (coe
                                                                                                                         v0)
                                                                                                                      (coe
                                                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                         (coe
                                                                                                                            v31)
                                                                                                                         (coe
                                                                                                                            v28))
                                                                                                                      (coe
                                                                                                                         v26) in
                                                                                                            case coe
                                                                                                                   v30 of
                                                                                                              0 -> coe
                                                                                                                     v35
                                                                                                              _ -> coe
                                                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                                                                     (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                        (coe
                                                                                                                           v0))
                                                                                                                     (let v36
                                                                                                                            = coe
                                                                                                                                MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                                                                (coe
                                                                                                                                   MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                                                                   (let v36
                                                                                                                                          = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                                                                              (coe
                                                                                                                                                 v0) in
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
                                                                                                                                   v30)
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
                                                                                                                             v30 of
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
                                                                                         _ -> MAlonzo.RTE.mazUnreachableError
                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v19
                                                               -> let v20
                                                                        = coe
                                                                            MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                            (coe
                                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                               (coe v5) (coe v2)) in
                                                                  let v21 = 0 :: Integer in
                                                                  case coe v20 of
                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v22 v23
                                                                      -> let v24
                                                                               = coe
                                                                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                                   (coe v0)
                                                                                   (coe
                                                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                      (coe v17)
                                                                                      (coe v14))
                                                                                   (coe v20) in
                                                                         case coe v21 of
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
                                                                                         = -1 ::
                                                                                             Integer in
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
                                                                                   case coe v21 of
                                                                                     0 -> coe
                                                                                            MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                            (coe
                                                                                               v26)
                                                                                     1 -> coe v22
                                                                                     _ -> coe v28)
                                                                                  v24
                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                      _ -> let v19
                                                                 = coe
                                                                     MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                                                     (coe
                                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                                                        (coe v5) (coe v2)) in
                                                           case coe v19 of
                                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v20 v21
                                                               -> let v22
                                                                        = coe
                                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                            (coe v0)
                                                                            (coe
                                                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                               (coe v17) (coe v14))
                                                                            (coe v19) in
                                                                  case coe v16 of
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
                                                                                      (coe v16)
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
                                                                            case coe v16 of
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
                          _ -> MAlonzo.RTE.mazUnreachableError)
                       (coe
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8512''63''10214'_'10215'_606
                          v0
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_para_338
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                (coe v0))
                             (coe
                                MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8863''45'step_538
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                      (coe v0))
                                   (coe v9)))
                             (coe v7))
                          (coe
                             MAlonzo.Code.Data.Vec.Base.du_uncons_594
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                (coe v5) (coe v2))))
                       (coe
                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                          (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                             (coe v0))
                          (let v10
                                 = coe
                                     MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                        (coe v5) (coe v2)) in
                           case coe v7 of
                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v11 v12
                               -> case coe v11 of
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v13 v14
                                      -> case coe v13 of
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v15
                                             -> case coe v10 of
                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v17 v18
                                                    -> let v19
                                                             = coe
                                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                 (coe v0)
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                    (coe v15) (coe v12))
                                                                 (coe v10) in
                                                       case coe v14 of
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
                                                                                         (coe v0) in
                                                                               coe
                                                                                 MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                 (coe
                                                                                    MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                    (coe v20)))) in
                                                                 let v21
                                                                       = coe
                                                                           MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                           (coe v20) in
                                                                 let v22
                                                                       = subInt
                                                                           (coe v14)
                                                                           (coe (1 :: Integer)) in
                                                                 let v23
                                                                       = coe
                                                                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                           v21
                                                                           (coe
                                                                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                              (coe v21) (coe v22)
                                                                              (coe v17))
                                                                           v17 in
                                                                 case coe v14 of
                                                                   0 -> coe
                                                                          MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                          (coe v21)
                                                                   1 -> coe v17
                                                                   _ -> coe v23)
                                                                v19
                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             _ -> MAlonzo.RTE.mazUnreachableError))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v10
                                 = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                        (coe v0)) in
                           let v11
                                 = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v10) in
                           let v12
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v11) in
                           let v13
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v12) in
                           let v14
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v13) in
                           let v15
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v14) in
                           let v16
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v15) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v16)))
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8512''63''10214'_'10215'_606
                             v0
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_para_338
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                   (coe v0))
                                (coe
                                   MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8863''45'step_538
                                      (coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                         (coe v0))
                                      (coe v9)))
                                (coe v7))
                             (coe
                                MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                   (coe v5) (coe v2))))
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                (coe v0))
                             (let v10
                                    = coe
                                        MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                           (coe v5) (coe v2)) in
                              case coe v7 of
                                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v11 v12
                                  -> case coe v11 of
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v13 v14
                                         -> case coe v13 of
                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v15
                                                -> case coe v10 of
                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v17 v18
                                                       -> let v19
                                                                = coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                    (coe v0)
                                                                    (coe
                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                       (coe v15) (coe v12))
                                                                    (coe v10) in
                                                          case coe v14 of
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
                                                                    let v22
                                                                          = subInt
                                                                              (coe v14)
                                                                              (coe
                                                                                 (1 :: Integer)) in
                                                                    let v23
                                                                          = coe
                                                                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                              v21
                                                                              (coe
                                                                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                 (coe v21) (coe v22)
                                                                                 (coe v17))
                                                                              v17 in
                                                                    case coe v14 of
                                                                      0 -> coe
                                                                             MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                             (coe v21)
                                                                      1 -> coe v17
                                                                      _ -> coe v23)
                                                                   v19
                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                              _ -> MAlonzo.RTE.mazUnreachableError
                                       _ -> MAlonzo.RTE.mazUnreachableError
                                _ -> MAlonzo.RTE.mazUnreachableError))
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                             (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                (coe v0))
                             (let v10
                                    = coe
                                        MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                           (coe v5) (coe v2)) in
                              case coe v7 of
                                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v11 v12
                                  -> case coe v11 of
                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v13 v14
                                         -> case coe v13 of
                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v15
                                                -> case coe v10 of
                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v17 v18
                                                       -> let v19
                                                                = coe
                                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                    (coe v0)
                                                                    (coe
                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                       (coe v15) (coe v12))
                                                                    (coe v10) in
                                                          case coe v14 of
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
                                                                    let v22
                                                                          = subInt
                                                                              (coe v14)
                                                                              (coe
                                                                                 (1 :: Integer)) in
                                                                    let v23
                                                                          = coe
                                                                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                              v21
                                                                              (coe
                                                                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                 (coe v21) (coe v22)
                                                                                 (coe v17))
                                                                              v17 in
                                                                    case coe v14 of
                                                                      0 -> coe
                                                                             MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                             (coe v21)
                                                                      1 -> coe v17
                                                                      _ -> coe v23)
                                                                   v19
                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                              _ -> MAlonzo.RTE.mazUnreachableError
                                       _ -> MAlonzo.RTE.mazUnreachableError
                                _ -> MAlonzo.RTE.mazUnreachableError))
                          (let v10
                                 = let v10
                                         = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                             (coe
                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                (coe v0)) in
                                   let v11
                                         = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                             (coe v10) in
                                   let v12
                                         = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                             (coe v11) in
                                   let v13
                                         = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                             (coe v12) in
                                   let v14
                                         = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                             (coe v13) in
                                   let v15
                                         = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                             (coe v14) in
                                   let v16
                                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                             (coe v15) in
                                   coe
                                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                                     (coe
                                        MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v16)) in
                           coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (coe v10)))
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                   (coe v0))
                                (let v11
                                       = coe
                                           MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                              (coe v5) (coe v2)) in
                                 case coe v7 of
                                   MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v12 v13
                                     -> case coe v12 of
                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__Δ__170 v14 v15
                                            -> case coe v14 of
                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.C__'8800'0_230 v16
                                                   -> case coe v11 of
                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v18 v19
                                                          -> let v20
                                                                   = coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du__'10214''8759''10215'__420
                                                                       (coe v0)
                                                                       (coe
                                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                          (coe v16) (coe v13))
                                                                       (coe v11) in
                                                             case coe v15 of
                                                               0 -> coe v20
                                                               _ -> coe
                                                                      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                                      (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                         (coe v0))
                                                                      (let v21
                                                                             = coe
                                                                                 MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                                                                                 (coe
                                                                                    MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                                                                    (let v21
                                                                                           = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                                               (coe
                                                                                                  v0) in
                                                                                     coe
                                                                                       MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                                                                       (coe
                                                                                          MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_commutativeSemiring_326
                                                                                          (coe
                                                                                             v21)))) in
                                                                       let v22
                                                                             = coe
                                                                                 MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                                                                 (coe v21) in
                                                                       let v23
                                                                             = subInt
                                                                                 (coe v15)
                                                                                 (coe
                                                                                    (1 ::
                                                                                       Integer)) in
                                                                       let v24
                                                                             = coe
                                                                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56
                                                                                 v22
                                                                                 (coe
                                                                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                                                                    (coe v22)
                                                                                    (coe v23)
                                                                                    (coe v18))
                                                                                 v18 in
                                                                       case coe v15 of
                                                                         0 -> coe
                                                                                MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58
                                                                                (coe v22)
                                                                         1 -> coe v18
                                                                         _ -> coe v24)
                                                                      v20
                                                        _ -> MAlonzo.RTE.mazUnreachableError
                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                          _ -> MAlonzo.RTE.mazUnreachableError
                                   _ -> MAlonzo.RTE.mazUnreachableError)))
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_poly'45'mapR_1184
                             (coe v0)
                             (coe
                                MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                (coe
                                   MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                      (coe v5) (coe v2))))
                             (coe
                                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                (coe
                                   MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                      (coe v5) (coe v2))))
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8863''45'step_538
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                   (coe v0))
                                (coe v9))
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                   (coe v0)))
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255'cong_64
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                      (coe v0))))
                             (coe
                                (\ v10 v11 ->
                                   coe
                                     MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                     (coe
                                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                        (coe
                                           MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
                                           (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe
                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                 (coe
                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                    (coe v0))))
                                           v10
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0))
                                              v11))
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
                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0))
                                              v10
                                              (coe
                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                    (coe v0))
                                                 v11))
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0))
                                              (coe
                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                    (coe v0))
                                                 v11)
                                              v10)
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0))
                                              (coe
                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                    (coe v0))
                                                 v11 v10)))
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255''42''45'distrib'737'_70
                                           (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                              (coe
                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0)))
                                           v11 v10))
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
                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0))
                                           v10
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0))
                                              v11))
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0))
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0))
                                              v11 v10))
                                        (coe
                                           MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                                           (\ v12 v13 -> v13)
                                           (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                              (coe
                                                 MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0)))
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0))
                                              v11 v10)
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                              (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                 (coe v0))
                                              v10 v11)))
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255'cong_64
                                        (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0)))
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0))
                                           v11 v10)
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0))
                                           v10 v11)
                                        (coe
                                           MAlonzo.Code.Algebra.Structures.d_'42''45'comm_1496
                                           (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe
                                                 MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                 (coe
                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                    (coe v0))))
                                           v11 v10))))
                             (coe
                                (\ v10 v11 ->
                                   coe
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
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'43'__208
                                        (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                           (coe v0))
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0))
                                           v10)
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0))
                                           v11))
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                        (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                           (coe v0))
                                        (coe
                                           MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'43'__208
                                           (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0))
                                           v10 v11))
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255''43''45'comm_76
                                        (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                              (coe v0)))
                                        v10 v11)))
                             (coe
                                (\ v10 ->
                                   coe
                                     du_'8863''45'step'45'hom_482 (coe v0) (coe v10)
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                        (coe
                                           MAlonzo.Code.Data.Vec.Base.du_uncons_594
                                           (coe
                                              MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Semantics.du_drop_388
                                              (coe v5) (coe v2))))))
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
                                (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                      (coe v0)))
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                   (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                      (coe v0))
                                   (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                      (coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                         (coe v0))))
                                (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                                   (coe
                                      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                      (let v10
                                             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                 (coe
                                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                    (coe v0)) in
                                       let v11
                                             = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                 (coe v10) in
                                       let v12
                                             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                 (coe v11) in
                                       let v13
                                             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                 (coe v12) in
                                       let v14
                                             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                 (coe v13) in
                                       let v15
                                             = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                 (coe v14) in
                                       let v16
                                             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                 (coe v15) in
                                       coe
                                         MAlonzo.Code.Algebra.Structures.du_setoid_164
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                            (coe v16)))
                                      (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                            (coe v0)))
                                      (coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                            (coe v0))
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))
                                            (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))))
                                         (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))))
                                      (coe
                                         MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                         (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                            (coe v0))
                                         (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))))
                                      (coe
                                         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                         (let v10
                                                = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                    (coe
                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                       (coe v0)) in
                                          let v11
                                                = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                    (coe v10) in
                                          let v12
                                                = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                    (coe v11) in
                                          let v13
                                                = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                    (coe v12) in
                                          let v14
                                                = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                    (coe v13) in
                                          let v15
                                                = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                    (coe v14) in
                                          let v16
                                                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                    (coe v15) in
                                          coe
                                            MAlonzo.Code.Algebra.Structures.du_setoid_164
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                               (coe v16)))
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))
                                               (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0))))
                                            (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))))
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))
                                               (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0)))
                                               (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0)))))
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))
                                            (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))))
                                         (coe
                                            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                            (let v10
                                                   = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                       (coe
                                                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                          (coe v0)) in
                                             let v11
                                                   = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                       (coe v10) in
                                             let v12
                                                   = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                       (coe v11) in
                                             let v13
                                                   = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                       (coe v12) in
                                             let v14
                                                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                       (coe v13) in
                                             let v15
                                                   = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                       (coe v14) in
                                             let v16
                                                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                       (coe v15) in
                                             coe
                                               MAlonzo.Code.Algebra.Structures.du_setoid_164
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                  (coe v16)))
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0))
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                     (coe
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                        (coe v0)))
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                     (coe
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                        (coe v0)))))
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))
                                               (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0))))
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))
                                               (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0))))
                                            (let v10
                                                   = let v10
                                                           = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                               (coe
                                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                                  (coe v0)) in
                                                     let v11
                                                           = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                               (coe v10) in
                                                     let v12
                                                           = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                               (coe v11) in
                                                     let v13
                                                           = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                               (coe v12) in
                                                     let v14
                                                           = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                               (coe v13) in
                                                     let v15
                                                           = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                               (coe v14) in
                                                     let v16
                                                           = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                               (coe v15) in
                                                     coe
                                                       MAlonzo.Code.Algebra.Structures.du_setoid_164
                                                       (coe
                                                          MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                          (coe v16)) in
                                             coe
                                               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                               (coe
                                                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                  (coe
                                                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                     (coe v10)))
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0))
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                     (coe
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                        (coe v0)))))
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255'cong_64
                                               (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0)))
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0))
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                     (coe
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                        (coe v0)))
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                     (coe
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                        (coe v0))))
                                               (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0)))
                                               (let v10
                                                      = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                          (coe v0) in
                                                let v11
                                                      = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                          (coe v10) in
                                                let v12
                                                      = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                          (coe v11) in
                                                let v13
                                                      = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                          (coe v12) in
                                                coe
                                                  MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
                                                     (coe v13))
                                                  (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                     (coe
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                        (coe v0))))))
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45''8255''42''45'distrib'737'_70
                                            (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0)))
                                            (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0)))
                                            (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0)))))
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
                                         (coe
                                            MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d__'42'__210
                                            (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0))
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))
                                               (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0))))
                                            (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                               (coe
                                                  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))))
                                         (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                               (coe v0)))
                                         (let v10
                                                = MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                    (coe v0) in
                                          let v11
                                                = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isAlmostCommutativeRing_222
                                                    (coe v10) in
                                          let v12
                                                = MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                    (coe v11) in
                                          let v13
                                                = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                    (coe v12) in
                                          coe
                                            MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
                                               (coe v13))
                                            (coe
                                               MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_'45'__212
                                               (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                  (coe v0))
                                               (MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.d_0'35'_214
                                                  (coe
                                                     MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_to_282
                                                     (coe v0)))))))))
                             (coe v7)))
                       (coe
                          MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas.du_'8848''8595''45'hom_974
                          (coe v0)
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_para_338
                             (coe
                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                (coe v0))
                             (coe
                                MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                                (coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.du_'8863''45'step_538
                                   (coe
                                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_from_280
                                      (coe v0))
                                   (coe v9)))
                             (coe v7))
                          (coe v5) (coe v2)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation.⊟-hom
d_'8863''45'hom_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8863''45'hom_518 ~v0 ~v1 ~v2 ~v3 v4 ~v5
  = du_'8863''45'hom_518 v4
du_'8863''45'hom_518 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'8863''45'hom_518 v0 = coe du_'8863''45'step'45'hom_482 (coe v0)
