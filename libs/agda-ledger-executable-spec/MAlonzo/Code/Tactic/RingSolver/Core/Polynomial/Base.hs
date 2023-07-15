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

module MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Definitions.RawMonoid
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Kleene.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Induction.WellFounded
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters

-- Tactic.RingSolver.Core.Polynomial.Base.InjectionOrdering
d_InjectionOrdering_64 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_InjectionOrdering_64
  = C_inj'45'lt_76 MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 |
    C_inj'45'gt_86 MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 |
    C_inj'45'eq_92
-- Tactic.RingSolver.Core.Polynomial.Base.inj-compare
d_inj'45'compare_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_InjectionOrdering_64
d_inj'45'compare_104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_inj'45'compare_104 v6 v7
du_inj'45'compare_104 ::
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_InjectionOrdering_64
du_inj'45'compare_104 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
        -> case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
               -> coe C_inj'45'eq_92
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282 v3
               -> coe C_inj'45'gt_86 v3
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
               -> coe C_inj'45'lt_76 v3
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282 v5
               -> coe du_inj'45'compare_104 (coe v3) (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.space
d_space_128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> Integer
d_space_128 ~v0 ~v1 ~v2 v3 v4 = du_space_128 v3 v4
du_space_128 ::
  Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> Integer
du_space_128 v0 v1
  = coe addInt (coe (1 :: Integer)) (coe du_go_138 (coe v0) (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Base._.go
d_go_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> Integer
d_go_138 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 = du_go_138 v5 v6
du_go_138 ::
  Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> Integer
du_go_138 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> coe subInt (coe v0) (coe (1 :: Integer))
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v3
        -> let v4 = subInt (coe v0) (coe (1 :: Integer)) in
           coe du_go_138 (coe v4) (coe v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.space≤′n
d_space'8804''8242'n_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_space'8804''8242'n_148 ~v0 ~v1 ~v2 ~v3 v4
  = du_space'8804''8242'n_148 v4
du_space'8804''8242'n_148 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
du_space'8804''8242'n_148 v0
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> coe MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'refl_276
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v2
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
             (coe du_space'8804''8242'n_148 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.PowInd
d_PowInd_156 a0 a1 a2 a3 a4 = ()
data T_PowInd_156 = C__Δ__170 AgdaAny Integer
-- Tactic.RingSolver.Core.Polynomial.Base.PowInd.coeff
d_coeff_166 :: T_PowInd_156 -> AgdaAny
d_coeff_166 v0
  = case coe v0 of
      C__Δ__170 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.PowInd.pow
d_pow_168 :: T_PowInd_156 -> Integer
d_pow_168 v0
  = case coe v0 of
      C__Δ__170 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.Poly
d_Poly_174 a0 a1 a2 a3 = ()
data T_Poly_174
  = C__'8848'__206 Integer T_FlatPoly_176
                   MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
-- Tactic.RingSolver.Core.Polynomial.Base.FlatPoly
d_FlatPoly_176 a0 a1 a2 a3 = ()
data T_FlatPoly_176
  = C_Κ_208 AgdaAny |
    C_'8512'_216 MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 AgdaAny
-- Tactic.RingSolver.Core.Polynomial.Base.Coeff
d_Coeff_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> ()
d_Coeff_178 = erased
-- Tactic.RingSolver.Core.Polynomial.Base.NonZero
d_NonZero_182 a0 a1 a2 a3 = ()
newtype T_NonZero_182 = C__'8800'0_230 T_Poly_174
-- Tactic.RingSolver.Core.Polynomial.Base.Zero
d_Zero_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> T_Poly_174 -> ()
d_Zero_186 = erased
-- Tactic.RingSolver.Core.Polynomial.Base.Normalised
d_Normalised_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 -> ()
d_Normalised_190 = erased
-- Tactic.RingSolver.Core.Polynomial.Base.Poly.i
d_i_200 :: T_Poly_174 -> Integer
d_i_200 v0
  = case coe v0 of
      C__'8848'__206 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.Poly.flat
d_flat_202 :: T_Poly_174 -> T_FlatPoly_176
d_flat_202 v0
  = case coe v0 of
      C__'8848'__206 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.Poly.i≤n
d_i'8804'n_204 ::
  T_Poly_174 -> MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_i'8804'n_204 v0
  = case coe v0 of
      C__'8848'__206 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.NonZero.poly
d_poly_226 :: T_NonZero_182 -> T_Poly_174
d_poly_226 v0
  = case coe v0 of
      C__'8800'0_230 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.zero?
d_zero'63'_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  T_Poly_174 -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_zero'63'_238 ~v0 ~v1 v2 ~v3 v4 = du_zero'63'_238 v2 v4
du_zero'63'_238 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  T_Poly_174 -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_zero'63'_238 v0 v1
  = case coe v1 of
      C__'8848'__206 v2 v3 v4
        -> case coe v3 of
             C_Κ_208 v5
               -> let v6
                        = coe
                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                            v0 v5 in
                  if coe v6
                    then coe
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                           (coe v6)
                           (coe
                              MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                              (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                    else coe
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                           (coe v6) (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             C_'8512'_216 v6 v7
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base._⍓*_
d__'9043''42'__252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  Integer -> MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d__'9043''42'__252 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du__'9043''42'__252 v4 v5
du__'9043''42'__252 ::
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  Integer -> MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
du__'9043''42'__252 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46 -> coe v0
      MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v2
        -> coe
             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
             (coe du__'9043''43'__256 (coe v2) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base._⍓+_
d__'9043''43'__256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  Integer -> MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24
d__'9043''43'__256 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du__'9043''43'__256 v4 v5
du__'9043''43'__256 ::
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  Integer -> MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24
du__'9043''43'__256 v0 v1
  = coe
      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
      (coe
         C__Δ__170
         (coe
            d_coeff_166
            (coe MAlonzo.Code.Data.List.Kleene.Base.d_head_38 (coe v0)))
         (coe
            addInt
            (coe
               d_pow_168
               (coe MAlonzo.Code.Data.List.Kleene.Base.d_head_38 (coe v0)))
            (coe v1)))
      (coe MAlonzo.Code.Data.List.Kleene.Base.d_tail_40 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Base._∷↓_
d__'8759''8595'__276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  T_PowInd_156 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d__'8759''8595'__276 ~v0 ~v1 v2 ~v3 v4 v5
  = du__'8759''8595'__276 v2 v4 v5
du__'8759''8595'__276 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  T_PowInd_156 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
du__'8759''8595'__276 v0 v1 v2
  = case coe v1 of
      C__Δ__170 v3 v4
        -> case coe v3 of
             C__'8848'__206 v5 v6 v7
               -> case coe v6 of
                    C_Κ_208 v8
                      -> let v9
                               = coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                   v0 v8 in
                         coe
                           seq (coe v9)
                           (if coe v9
                              then coe
                                     du__'9043''42'__252 (coe v2)
                                     (coe addInt (coe (1 :: Integer)) (coe v4))
                              else coe
                                     MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                     (coe
                                        MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                        (coe C__Δ__170 (coe C__'8800'0_230 v3) (coe v4)) (coe v2)))
                    C_'8512'_216 v9 v10
                      -> coe
                           MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                           (coe
                              MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                              (coe C__Δ__170 (coe C__'8800'0_230 v3) (coe v4)) (coe v2))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base._⊐↑_
d__'8848''8593'__294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  T_Poly_174 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 -> T_Poly_174
d__'8848''8593'__294 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du__'8848''8593'__294 v5 v6
du__'8848''8593'__294 ::
  T_Poly_174 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 -> T_Poly_174
du__'8848''8593'__294 v0 v1
  = case coe v0 of
      C__'8848'__206 v2 v3 v4
        -> coe
             C__'8848'__206 (coe v2) (coe v3)
             (coe
                MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                (coe MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282 v4)
                (coe
                   MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base._⊐↓_
d__'8848''8595'__306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 -> T_Poly_174
d__'8848''8595'__306 ~v0 ~v1 v2 v3 v4 v5 v6
  = du__'8848''8595'__306 v2 v3 v4 v5 v6
du__'8848''8595'__306 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 -> T_Poly_174
du__'8848''8595'__306 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
        -> coe
             C__'8848'__206 (coe (0 :: Integer))
             (coe
                C_Κ_208
                (coe
                   MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                      (coe v0))))
             (coe
                MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v2))
      MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v5
        -> case coe v5 of
             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v6 v7
               -> case coe v6 of
                    C__Δ__170 v8 v9
                      -> case coe v8 of
                           C__'8800'0_230 v10
                             -> case coe v9 of
                                  0 -> case coe v7 of
                                         MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                           -> case coe v10 of
                                                C__'8848'__206 v12 v13 v14
                                                  -> coe
                                                       C__'8848'__206 (coe v12) (coe v13)
                                                       (coe
                                                          MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                          (coe
                                                             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                             v14)
                                                          (coe
                                                             MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                          (coe v4))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v12
                                           -> coe
                                                C__'8848'__206
                                                (coe addInt (coe (1 :: Integer)) (coe v1))
                                                (coe
                                                   C_'8512'_216
                                                   (coe
                                                      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                      (coe
                                                         C__Δ__170 (coe C__'8800'0_230 v10)
                                                         (coe (0 :: Integer)))
                                                      (coe v7))
                                                   (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                (coe v4)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> coe
                                         C__'8848'__206 (coe addInt (coe (1 :: Integer)) (coe v1))
                                         (coe
                                            C_'8512'_216
                                            (coe
                                               MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                               (coe C__Δ__170 (coe C__'8800'0_230 v10) (coe v9))
                                               (coe v7))
                                            (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                         (coe v4)
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.PolyF
d_PolyF_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> ()
d_PolyF_328 = erased
-- Tactic.RingSolver.Core.Polynomial.Base.Fold
d_Fold_332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> ()
d_Fold_332 = erased
-- Tactic.RingSolver.Core.Polynomial.Base.para
d_para_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d_para_338 ~v0 ~v1 v2 ~v3 v4 v5 = du_para_338 v2 v4 v5
du_para_338 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
du_para_338 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v3 v4
        -> case coe v3 of
             C__Δ__170 v5 v6
               -> case coe v5 of
                    C__'8800'0_230 v7
                      -> case coe v4 of
                           MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                             -> let v9
                                      = coe
                                          v1
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v7)
                                             (coe v4)) in
                                case coe v9 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v10 v11
                                    -> case coe v10 of
                                         C__'8848'__206 v12 v13 v14
                                           -> case coe v13 of
                                                C_Κ_208 v15
                                                  -> let v16
                                                           = coe
                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                               v0 v15 in
                                                     coe
                                                       seq (coe v16)
                                                       (if coe v16
                                                          then coe
                                                                 du__'9043''42'__252 (coe v11)
                                                                 (coe
                                                                    addInt (coe (1 :: Integer))
                                                                    (coe v6))
                                                          else coe
                                                                 MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v10)
                                                                       (coe v6))
                                                                    (coe v11)))
                                                C_'8512'_216 v16 v17
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                       (coe
                                                          MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                          (coe
                                                             C__Δ__170 (coe C__'8800'0_230 v10)
                                                             (coe v6))
                                                          (coe v11))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v9
                             -> let v10
                                      = coe
                                          v1
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v7)
                                             (coe du_para_338 (coe v0) (coe v1) (coe v9))) in
                                case coe v10 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
                                    -> case coe v11 of
                                         C__'8848'__206 v13 v14 v15
                                           -> case coe v14 of
                                                C_Κ_208 v16
                                                  -> let v17
                                                           = coe
                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                               v0 v16 in
                                                     coe
                                                       seq (coe v17)
                                                       (if coe v17
                                                          then coe
                                                                 du__'9043''42'__252 (coe v12)
                                                                 (coe
                                                                    addInt (coe (1 :: Integer))
                                                                    (coe v6))
                                                          else coe
                                                                 MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v11)
                                                                       (coe v6))
                                                                    (coe v12)))
                                                C_'8512'_216 v17 v18
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                       (coe
                                                          MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                          (coe
                                                             C__Δ__170 (coe C__'8800'0_230 v11)
                                                             (coe v6))
                                                          (coe v12))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.poly-map
d_poly'45'map_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  (T_Poly_174 -> T_Poly_174) ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d_poly'45'map_368 ~v0 ~v1 v2 ~v3 v4 = du_poly'45'map_368 v2 v4
du_poly'45'map_368 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  (T_Poly_174 -> T_Poly_174) ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
du_poly'45'map_368 v0 v1
  = coe
      du_para_338 (coe v0)
      (coe MAlonzo.Code.Data.Product.Base.du_map'8321'_114 (coe v1))
-- Tactic.RingSolver.Core.Polynomial.Base._⊞_
d__'8862'__374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> T_Poly_174 -> T_Poly_174 -> T_Poly_174
d__'8862'__374 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      C__'8848'__206 v6 v7 v8
        -> case coe v5 of
             C__'8848'__206 v9 v10 v11
               -> coe
                    d_'8862''45'match_386 (coe v0) (coe v1) (coe v2) (coe v6) (coe v9)
                    (coe v3) (coe v8) (coe v11)
                    (coe du_inj'45'compare_104 (coe v8) (coe v11)) (coe v7) (coe v10)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊞-match
d_'8862''45'match_386 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_InjectionOrdering_64 ->
  T_FlatPoly_176 -> T_FlatPoly_176 -> T_Poly_174
d_'8862''45'match_386 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v8 of
      C_inj'45'lt_76 v13
        -> let v15 = subInt (coe v4) (coe (1 :: Integer)) in
           case coe v10 of
             C_'8512'_216 v17 v18
               -> let v19
                        = d_'8862''45'inj_392
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v15) (coe v13) (coe v9)
                            (coe v17) in
                  case coe v19 of
                    MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                      -> coe
                           C__'8848'__206 (coe (0 :: Integer))
                           (coe
                              C_Κ_208
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe v2))))
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v5))
                    MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v20
                      -> case coe v20 of
                           MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v21 v22
                             -> case coe v21 of
                                  C__Δ__170 v23 v24
                                    -> case coe v23 of
                                         C__'8800'0_230 v25
                                           -> case coe v24 of
                                                0 -> case coe v22 of
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                         -> case coe v25 of
                                                              C__'8848'__206 v27 v28 v29
                                                                -> coe
                                                                     C__'8848'__206 (coe v27)
                                                                     (coe v28)
                                                                     (coe
                                                                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                           v29)
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                        (coe v7))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v27
                                                         -> coe
                                                              C__'8848'__206 (coe v4)
                                                              (coe
                                                                 C_'8512'_216
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v25)
                                                                       (coe (0 :: Integer)))
                                                                    (coe v22))
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                              (coe v7)
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> coe
                                                       C__'8848'__206 (coe v4)
                                                       (coe
                                                          C_'8512'_216
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                C__Δ__170 (coe C__'8800'0_230 v25)
                                                                (coe v24))
                                                             (coe v22))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                       (coe v7)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_inj'45'gt_86 v14
        -> let v15 = subInt (coe v3) (coe (1 :: Integer)) in
           case coe v9 of
             C_'8512'_216 v17 v18
               -> let v19
                        = d_'8862''45'inj_392
                            (coe v0) (coe v1) (coe v2) (coe v4) (coe v15) (coe v14) (coe v10)
                            (coe v17) in
                  case coe v19 of
                    MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                      -> coe
                           C__'8848'__206 (coe (0 :: Integer))
                           (coe
                              C_Κ_208
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe v2))))
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v5))
                    MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v20
                      -> case coe v20 of
                           MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v21 v22
                             -> case coe v21 of
                                  C__Δ__170 v23 v24
                                    -> case coe v23 of
                                         C__'8800'0_230 v25
                                           -> case coe v24 of
                                                0 -> case coe v22 of
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                         -> case coe v25 of
                                                              C__'8848'__206 v27 v28 v29
                                                                -> coe
                                                                     C__'8848'__206 (coe v27)
                                                                     (coe v28)
                                                                     (coe
                                                                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                           v29)
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                        (coe v6))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v27
                                                         -> coe
                                                              C__'8848'__206 (coe v3)
                                                              (coe
                                                                 C_'8512'_216
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v25)
                                                                       (coe (0 :: Integer)))
                                                                    (coe v22))
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                              (coe v6)
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> coe
                                                       C__'8848'__206 (coe v3)
                                                       (coe
                                                          C_'8512'_216
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                C__Δ__170 (coe C__'8800'0_230 v25)
                                                                (coe v24))
                                                             (coe v22))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                       (coe v6)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_inj'45'eq_92
        -> case coe v9 of
             C_Κ_208 v13
               -> case coe v10 of
                    C_Κ_208 v14
                      -> coe
                           C__'8848'__206 (coe (0 :: Integer))
                           (coe
                              C_Κ_208
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266
                                 (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe v2))
                                 v13 v14))
                           (coe v6)
                    _ -> MAlonzo.RTE.mazUnreachableError
             C_'8512'_216 v14 v15
               -> let v16 = subInt (coe v3) (coe (1 :: Integer)) in
                  case coe v14 of
                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                      -> case coe v17 of
                           C__Δ__170 v19 v20
                             -> case coe v10 of
                                  C_'8512'_216 v22 v23
                                    -> case coe v22 of
                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v24 v25
                                           -> case coe v24 of
                                                C__Δ__170 v26 v27
                                                  -> let v28
                                                           = MAlonzo.Code.Data.Nat.Base.d_compare_400
                                                               (coe v20) (coe v27) in
                                                     case coe v28 of
                                                       MAlonzo.Code.Data.Nat.Base.C_less_384 v30
                                                         -> let v31
                                                                  = d_'8862''45'zip'45'r_408
                                                                      (coe v0) (coe v1) (coe v2)
                                                                      (coe v16) (coe v26) (coe v30)
                                                                      (coe v25) (coe v18) in
                                                            case coe v19 of
                                                              C__'8800'0_230 v32
                                                                -> case coe v20 of
                                                                     0 -> case coe v31 of
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                              -> case coe v32 of
                                                                                   C__'8848'__206 v34 v35 v36
                                                                                     -> coe
                                                                                          C__'8848'__206
                                                                                          (coe v34)
                                                                                          (coe v35)
                                                                                          (coe
                                                                                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                                v36)
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                             (coe
                                                                                                v6))
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v34
                                                                              -> coe
                                                                                   C__'8848'__206
                                                                                   (coe v3)
                                                                                   (coe
                                                                                      C_'8512'_216
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                         (coe
                                                                                            C__Δ__170
                                                                                            (coe
                                                                                               C__'8800'0_230
                                                                                               v32)
                                                                                            (coe
                                                                                               (0 ::
                                                                                                  Integer)))
                                                                                         (coe v31))
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                   (coe v6)
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     _ -> coe
                                                                            C__'8848'__206 (coe v3)
                                                                            (coe
                                                                               C_'8512'_216
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                  (coe
                                                                                     C__Δ__170
                                                                                     (coe
                                                                                        C__'8800'0_230
                                                                                        v32)
                                                                                     (coe v20))
                                                                                  (coe v31))
                                                                               (coe
                                                                                  MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                            (coe v6)
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       MAlonzo.Code.Data.Nat.Base.C_equal_388
                                                         -> let v30
                                                                  = d_'8862''45'coeffs_396
                                                                      (coe v0) (coe v1) (coe v2)
                                                                      (coe v16) (coe v18)
                                                                      (coe v25) in
                                                            let v31
                                                                  = d__'8862'__374
                                                                      (coe v0) (coe v1) (coe v2)
                                                                      (coe v16)
                                                                      (coe d_poly_226 (coe v19))
                                                                      (coe d_poly_226 (coe v26)) in
                                                            case coe v31 of
                                                              C__'8848'__206 v32 v33 v34
                                                                -> case coe v33 of
                                                                     C_Κ_208 v35
                                                                       -> let v36
                                                                                = let v36
                                                                                        = coe
                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                                                            v2
                                                                                            v35 in
                                                                                  seq
                                                                                    (coe v36)
                                                                                    (if coe v36
                                                                                       then coe
                                                                                              du__'9043''42'__252
                                                                                              (coe
                                                                                                 v30)
                                                                                              (coe
                                                                                                 addInt
                                                                                                 (coe
                                                                                                    (1 ::
                                                                                                       Integer))
                                                                                                 (coe
                                                                                                    v20))
                                                                                       else coe
                                                                                              MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                                              (coe
                                                                                                 MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                                 (coe
                                                                                                    C__Δ__170
                                                                                                    (coe
                                                                                                       C__'8800'0_230
                                                                                                       v31)
                                                                                                    (coe
                                                                                                       v20))
                                                                                                 (coe
                                                                                                    v30))) in
                                                                          case coe v36 of
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                              -> coe
                                                                                   C__'8848'__206
                                                                                   (coe
                                                                                      (0 ::
                                                                                         Integer))
                                                                                   (coe
                                                                                      C_Κ_208
                                                                                      (coe
                                                                                         MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                                                                         (coe
                                                                                            MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                                                                            (coe
                                                                                               v2))))
                                                                                   (coe
                                                                                      MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                                                                                      (coe v5))
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v37
                                                                              -> case coe v37 of
                                                                                   MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v38 v39
                                                                                     -> case coe
                                                                                               v38 of
                                                                                          C__Δ__170 v40 v41
                                                                                            -> case coe
                                                                                                      v40 of
                                                                                                 C__'8800'0_230 v42
                                                                                                   -> case coe
                                                                                                             v41 of
                                                                                                        0 -> case coe
                                                                                                                    v39 of
                                                                                                               MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                                                                 -> case coe
                                                                                                                           v42 of
                                                                                                                      C__'8848'__206 v44 v45 v46
                                                                                                                        -> coe
                                                                                                                             C__'8848'__206
                                                                                                                             (coe
                                                                                                                                v44)
                                                                                                                             (coe
                                                                                                                                v45)
                                                                                                                             (coe
                                                                                                                                MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                                                                (coe
                                                                                                                                   MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                                                                   v46)
                                                                                                                                (coe
                                                                                                                                   MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                                                                (coe
                                                                                                                                   v6))
                                                                                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                               MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v44
                                                                                                                 -> coe
                                                                                                                      C__'8848'__206
                                                                                                                      (coe
                                                                                                                         v3)
                                                                                                                      (coe
                                                                                                                         C_'8512'_216
                                                                                                                         (coe
                                                                                                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                                                            (coe
                                                                                                                               C__Δ__170
                                                                                                                               (coe
                                                                                                                                  C__'8800'0_230
                                                                                                                                  v42)
                                                                                                                               (coe
                                                                                                                                  (0 ::
                                                                                                                                     Integer)))
                                                                                                                            (coe
                                                                                                                               v39))
                                                                                                                         (coe
                                                                                                                            MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                                                      (coe
                                                                                                                         v6)
                                                                                                               _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                        _ -> coe
                                                                                                               C__'8848'__206
                                                                                                               (coe
                                                                                                                  v3)
                                                                                                               (coe
                                                                                                                  C_'8512'_216
                                                                                                                  (coe
                                                                                                                     MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                                                     (coe
                                                                                                                        C__Δ__170
                                                                                                                        (coe
                                                                                                                           C__'8800'0_230
                                                                                                                           v42)
                                                                                                                        (coe
                                                                                                                           v41))
                                                                                                                     (coe
                                                                                                                        v39))
                                                                                                                  (coe
                                                                                                                     MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                                               (coe
                                                                                                                  v6)
                                                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     C_'8512'_216 v36 v37
                                                                       -> case coe v20 of
                                                                            0 -> case coe v30 of
                                                                                   MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                                     -> case coe
                                                                                               v31 of
                                                                                          C__'8848'__206 v38 v39 v40
                                                                                            -> coe
                                                                                                 C__'8848'__206
                                                                                                 (coe
                                                                                                    v38)
                                                                                                 (coe
                                                                                                    v39)
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                                       v40)
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                                    (coe
                                                                                                       v6))
                                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                                   MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v38
                                                                                     -> coe
                                                                                          C__'8848'__206
                                                                                          (coe v3)
                                                                                          (coe
                                                                                             C_'8512'_216
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                                (coe
                                                                                                   C__Δ__170
                                                                                                   (coe
                                                                                                      C__'8800'0_230
                                                                                                      v31)
                                                                                                   (coe
                                                                                                      (0 ::
                                                                                                         Integer)))
                                                                                                (coe
                                                                                                   v30))
                                                                                             (coe
                                                                                                MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                          (coe v6)
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            _ -> coe
                                                                                   C__'8848'__206
                                                                                   (coe v3)
                                                                                   (coe
                                                                                      C_'8512'_216
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                         (coe
                                                                                            C__Δ__170
                                                                                            (coe
                                                                                               C__'8800'0_230
                                                                                               v31)
                                                                                            (coe
                                                                                               v20))
                                                                                         (coe v30))
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                   (coe v6)
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       MAlonzo.Code.Data.Nat.Base.C_greater_394 v30
                                                         -> let v31
                                                                  = d_'8862''45'zip'45'r_408
                                                                      (coe v0) (coe v1) (coe v2)
                                                                      (coe v16) (coe v19) (coe v30)
                                                                      (coe v18) (coe v25) in
                                                            case coe v26 of
                                                              C__'8800'0_230 v32
                                                                -> case coe v27 of
                                                                     0 -> case coe v31 of
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                              -> case coe v32 of
                                                                                   C__'8848'__206 v34 v35 v36
                                                                                     -> coe
                                                                                          C__'8848'__206
                                                                                          (coe v34)
                                                                                          (coe v35)
                                                                                          (coe
                                                                                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                                v36)
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                             (coe
                                                                                                v6))
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v34
                                                                              -> coe
                                                                                   C__'8848'__206
                                                                                   (coe v3)
                                                                                   (coe
                                                                                      C_'8512'_216
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                         (coe
                                                                                            C__Δ__170
                                                                                            (coe
                                                                                               C__'8800'0_230
                                                                                               v32)
                                                                                            (coe
                                                                                               (0 ::
                                                                                                  Integer)))
                                                                                         (coe v31))
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                   (coe v6)
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     _ -> coe
                                                                            C__'8848'__206 (coe v3)
                                                                            (coe
                                                                               C_'8512'_216
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                  (coe
                                                                                     C__Δ__170
                                                                                     (coe
                                                                                        C__'8800'0_230
                                                                                        v32)
                                                                                     (coe v27))
                                                                                  (coe v31))
                                                                               (coe
                                                                                  MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                            (coe v6)
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊞-inj
d_'8862''45'inj_392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_FlatPoly_176 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d_'8862''45'inj_392 v0 v1 v2 v3 v4 v5 v6 v7
  = case coe v7 of
      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v8 v9
        -> case coe v8 of
             C__Δ__170 v10 v11
               -> case coe v10 of
                    C__'8800'0_230 v12
                      -> case coe v12 of
                           C__'8848'__206 v14 v15 v16
                             -> case coe v11 of
                                  0 -> let v17
                                             = d_'8862''45'match_386
                                                 (coe v0) (coe v1) (coe v2) (coe v14) (coe v3)
                                                 (coe v4) (coe v16) (coe v5)
                                                 (coe du_inj'45'compare_104 (coe v16) (coe v5))
                                                 (coe v15) (coe v6) in
                                       let v18 = 0 :: Integer in
                                       case coe v17 of
                                         C__'8848'__206 v19 v20 v21
                                           -> case coe v20 of
                                                C_Κ_208 v22
                                                  -> let v23
                                                           = coe
                                                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                               v2 v22 in
                                                     coe
                                                       seq (coe v23)
                                                       (if coe v23
                                                          then coe
                                                                 du__'9043''42'__252 (coe v9)
                                                                 (coe (1 :: Integer))
                                                          else coe
                                                                 MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v17)
                                                                       (coe v18))
                                                                    (coe v9)))
                                                C_'8512'_216 v23 v24
                                                  -> coe
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                       (coe
                                                          MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                          (coe
                                                             C__Δ__170 (coe C__'8800'0_230 v17)
                                                             (coe v18))
                                                          (coe v9))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> let v17 = subInt (coe v11) (coe (1 :: Integer)) in
                                       let v18
                                             = coe
                                                 MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                 (coe
                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                    (coe
                                                       C__Δ__170 (coe C__'8800'0_230 v12) (coe v17))
                                                    (coe v9)) in
                                       let v19 = coe C__'8848'__206 (coe v3) (coe v6) (coe v5) in
                                       let v20 = 0 :: Integer in
                                       case coe v6 of
                                         C_Κ_208 v21
                                           -> let v22
                                                    = coe
                                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                        v2 v21 in
                                              coe
                                                seq (coe v22)
                                                (if coe v22
                                                   then coe
                                                          du__'9043''42'__252 (coe v18)
                                                          (coe (1 :: Integer))
                                                   else coe
                                                          MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                C__Δ__170 (coe C__'8800'0_230 v19)
                                                                (coe v20))
                                                             (coe v18)))
                                         C_'8512'_216 v22 v23
                                           -> coe
                                                MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                (coe
                                                   MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                   (coe
                                                      C__Δ__170 (coe C__'8800'0_230 v19) (coe v20))
                                                   (coe v18))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊞-coeffs
d_'8862''45'coeffs_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d_'8862''45'coeffs_396 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46 -> coe v5
      MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v6
        -> case coe v6 of
             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v7 v8
               -> case coe v7 of
                    C__Δ__170 v9 v10
                      -> coe
                           d_'8862''45'zip'45'r_408 (coe v0) (coe v1) (coe v2) (coe v3)
                           (coe v9) (coe v10) (coe v8) (coe v5)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊞-zip
d_'8862''45'zip_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_Ordering_378 ->
  T_NonZero_182 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  T_NonZero_182 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d_'8862''45'zip_404 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v6 of
      MAlonzo.Code.Data.Nat.Base.C_less_384 v12
        -> coe
             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
             (coe
                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                (coe C__Δ__170 (coe v7) (coe v3))
                (coe
                   d_'8862''45'zip'45'r_408 (coe v0) (coe v1) (coe v2) (coe v5)
                   (coe v9) (coe v12) (coe v10) (coe v8)))
      MAlonzo.Code.Data.Nat.Base.C_equal_388
        -> let v12
                 = d_'8862''45'coeffs_396
                     (coe v0) (coe v1) (coe v2) (coe v5) (coe v8) (coe v10) in
           let v13
                 = d__'8862'__374
                     (coe v0) (coe v1) (coe v2) (coe v5) (coe d_poly_226 (coe v7))
                     (coe d_poly_226 (coe v9)) in
           case coe v13 of
             C__'8848'__206 v14 v15 v16
               -> case coe v15 of
                    C_Κ_208 v17
                      -> let v18
                               = coe
                                   MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                   v2 v17 in
                         coe
                           seq (coe v18)
                           (if coe v18
                              then coe
                                     du__'9043''42'__252 (coe v12)
                                     (coe addInt (coe (1 :: Integer)) (coe v3))
                              else coe
                                     MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                     (coe
                                        MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                        (coe C__Δ__170 (coe C__'8800'0_230 v13) (coe v3))
                                        (coe v12)))
                    C_'8512'_216 v18 v19
                      -> coe
                           MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                           (coe
                              MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                              (coe C__Δ__170 (coe C__'8800'0_230 v13) (coe v3)) (coe v12))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Nat.Base.C_greater_394 v12
        -> coe
             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
             (coe
                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                (coe C__Δ__170 (coe v9) (coe v4))
                (coe
                   d_'8862''45'zip'45'r_408 (coe v0) (coe v1) (coe v2) (coe v5)
                   (coe v7) (coe v12) (coe v8) (coe v10)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊞-zip-r
d_'8862''45'zip'45'r_408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  T_NonZero_182 ->
  Integer ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d_'8862''45'zip'45'r_408 v0 v1 v2 v3 v4 v5 v6 v7
  = case coe v7 of
      MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
        -> coe
             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
             (coe
                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                (coe C__Δ__170 (coe v4) (coe v5)) (coe v6))
      MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v8
        -> case coe v8 of
             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v9 v10
               -> case coe v9 of
                    C__Δ__170 v11 v12
                      -> coe
                           d_'8862''45'zip_404 (coe v0) (coe v1) (coe v2) (coe v5) (coe v12)
                           (coe v3)
                           (coe MAlonzo.Code.Data.Nat.Base.d_compare_400 (coe v5) (coe v12))
                           (coe v4) (coe v6) (coe v11) (coe v10)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊟-step
d_'8863''45'step_538 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  T_Poly_174 -> T_Poly_174
d_'8863''45'step_538 ~v0 ~v1 v2 v3 ~v4 v5
  = du_'8863''45'step_538 v2 v3 v5
du_'8863''45'step_538 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> T_Poly_174 -> T_Poly_174
du_'8863''45'step_538 v0 v1 v2
  = case coe v2 of
      C__'8848'__206 v3 v4 v5
        -> case coe v4 of
             C_Κ_208 v6
               -> coe
                    C__'8848'__206 (coe (0 :: Integer))
                    (coe
                       C_Κ_208
                       (coe
                          MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270
                          (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                             (coe v0))
                          v6))
                    (coe v5)
             C_'8512'_216 v7 v8
               -> let v9 = subInt (coe v3) (coe (1 :: Integer)) in
                  let v10
                        = coe
                            du_para_338 (coe v0)
                            (coe
                               MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                               (coe du_'8863''45'step_538 (coe v0) (coe v9)))
                            (coe v7) in
                  case coe v10 of
                    MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                      -> coe
                           C__'8848'__206 (coe (0 :: Integer))
                           (coe
                              C_Κ_208
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe v0))))
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v1))
                    MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v11
                      -> case coe v11 of
                           MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v12 v13
                             -> case coe v12 of
                                  C__Δ__170 v14 v15
                                    -> case coe v14 of
                                         C__'8800'0_230 v16
                                           -> case coe v15 of
                                                0 -> case coe v13 of
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                         -> case coe v16 of
                                                              C__'8848'__206 v18 v19 v20
                                                                -> coe
                                                                     C__'8848'__206 (coe v18)
                                                                     (coe v19)
                                                                     (coe
                                                                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                           v20)
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                        (coe v5))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v18
                                                         -> coe
                                                              C__'8848'__206 (coe v3)
                                                              (coe
                                                                 C_'8512'_216
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v16)
                                                                       (coe (0 :: Integer)))
                                                                    (coe v13))
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                              (coe v5)
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> coe
                                                       C__'8848'__206 (coe v3)
                                                       (coe
                                                          C_'8512'_216
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                C__Δ__170 (coe C__'8800'0_230 v16)
                                                                (coe v15))
                                                             (coe v13))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                       (coe v5)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊟_
d_'8863'__554 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> T_Poly_174 -> T_Poly_174
d_'8863'__554 ~v0 ~v1 v2 v3 = du_'8863'__554 v2 v3
du_'8863'__554 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> T_Poly_174 -> T_Poly_174
du_'8863'__554 v0 v1 = coe du_'8863''45'step_538 (coe v0) (coe v1)
-- Tactic.RingSolver.Core.Polynomial.Base.⊠-step′
d_'8864''45'step'8242'_558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  T_Poly_174 -> T_Poly_174 -> T_Poly_174
d_'8864''45'step'8242'_558 v0 v1 v2 v3 ~v4 v5
  = du_'8864''45'step'8242'_558 v0 v1 v2 v3 v5
du_'8864''45'step'8242'_558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> T_Poly_174 -> T_Poly_174 -> T_Poly_174
du_'8864''45'step'8242'_558 v0 v1 v2 v3 v4
  = case coe v4 of
      C__'8848'__206 v5 v6 v7
        -> coe du_'8864''45'step_564 v0 v1 v2 v5 v3 v6 v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊠-step
d_'8864''45'step_564 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  T_FlatPoly_176 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_Poly_174 -> T_Poly_174
d_'8864''45'step_564 v0 v1 v2 v3 v4 ~v5 v6
  = du_'8864''45'step_564 v0 v1 v2 v3 v4 v6
du_'8864''45'step_564 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  T_FlatPoly_176 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_Poly_174 -> T_Poly_174
du_'8864''45'step_564 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      C_Κ_208 v6
        -> coe
             (\ v7 v8 ->
                case coe v8 of
                  C__'8848'__206 v9 v10 v11
                    -> case coe v10 of
                         C_Κ_208 v12
                           -> coe
                                C__'8848'__206 (coe (0 :: Integer))
                                (coe
                                   C_Κ_208
                                   (coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268
                                      (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                         (coe v2))
                                      v6 v12))
                                (coe v11)
                         C_'8512'_216 v13 v14
                           -> let v15 = subInt (coe v9) (coe (1 :: Integer)) in
                              let v16
                                    = coe
                                        du_'8864''45'Κ'45'inj_578 (coe v0) (coe v1) (coe v2)
                                        (coe v15) (coe v6) (coe v13) in
                              case coe v16 of
                                MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                  -> coe
                                       C__'8848'__206 (coe (0 :: Integer))
                                       (coe
                                          C_Κ_208
                                          (coe
                                             MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                             (coe
                                                MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                                (coe v2))))
                                       (coe
                                          MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                                          (coe v4))
                                MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v17
                                  -> case coe v17 of
                                       MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v18 v19
                                         -> case coe v18 of
                                              C__Δ__170 v20 v21
                                                -> case coe v20 of
                                                     C__'8800'0_230 v22
                                                       -> case coe v21 of
                                                            0 -> case coe v19 of
                                                                   MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                     -> case coe v22 of
                                                                          C__'8848'__206 v24 v25 v26
                                                                            -> coe
                                                                                 C__'8848'__206
                                                                                 (coe v24) (coe v25)
                                                                                 (coe
                                                                                    MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                    (coe
                                                                                       MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                       v26)
                                                                                    (coe
                                                                                       MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                    (coe v11))
                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                   MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v24
                                                                     -> coe
                                                                          C__'8848'__206 (coe v9)
                                                                          (coe
                                                                             C_'8512'_216
                                                                             (coe
                                                                                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                (coe
                                                                                   C__Δ__170
                                                                                   (coe
                                                                                      C__'8800'0_230
                                                                                      v22)
                                                                                   (coe
                                                                                      (0 ::
                                                                                         Integer)))
                                                                                (coe v19))
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                          (coe v11)
                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                            _ -> coe
                                                                   C__'8848'__206 (coe v9)
                                                                   (coe
                                                                      C_'8512'_216
                                                                      (coe
                                                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                         (coe
                                                                            C__Δ__170
                                                                            (coe C__'8800'0_230 v22)
                                                                            (coe v21))
                                                                         (coe v19))
                                                                      (coe
                                                                         MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                   (coe v11)
                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                              _ -> MAlonzo.RTE.mazUnreachableError
                                       _ -> MAlonzo.RTE.mazUnreachableError
                                _ -> MAlonzo.RTE.mazUnreachableError
                         _ -> MAlonzo.RTE.mazUnreachableError
                  _ -> MAlonzo.RTE.mazUnreachableError)
      C_'8512'_216 v7 v8
        -> let v9 = subInt (coe v3) (coe (1 :: Integer)) in
           coe
             du_'8864''45''8512'_574 (coe v0) (coe v1) (coe v2) (coe v9)
             (coe v4) (coe v7)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊠-Κ
d_'8864''45'Κ_568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  AgdaAny -> T_Poly_174 -> T_Poly_174
d_'8864''45'Κ_568 v0 v1 v2 v3 ~v4 v5 v6
  = du_'8864''45'Κ_568 v0 v1 v2 v3 v5 v6
du_'8864''45'Κ_568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> AgdaAny -> T_Poly_174 -> T_Poly_174
du_'8864''45'Κ_568 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      C__'8848'__206 v6 v7 v8
        -> case coe v7 of
             C_Κ_208 v9
               -> coe
                    C__'8848'__206 (coe (0 :: Integer))
                    (coe
                       C_Κ_208
                       (coe
                          MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268
                          (MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                             (coe v2))
                          v4 v9))
                    (coe v8)
             C_'8512'_216 v10 v11
               -> let v12 = subInt (coe v6) (coe (1 :: Integer)) in
                  let v13
                        = coe
                            du_'8864''45'Κ'45'inj_578 (coe v0) (coe v1) (coe v2) (coe v12)
                            (coe v4) (coe v10) in
                  case coe v13 of
                    MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                      -> coe
                           C__'8848'__206 (coe (0 :: Integer))
                           (coe
                              C_Κ_208
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe v2))))
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v3))
                    MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v14
                      -> case coe v14 of
                           MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v15 v16
                             -> case coe v15 of
                                  C__Δ__170 v17 v18
                                    -> case coe v17 of
                                         C__'8800'0_230 v19
                                           -> case coe v18 of
                                                0 -> case coe v16 of
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                         -> case coe v19 of
                                                              C__'8848'__206 v21 v22 v23
                                                                -> coe
                                                                     C__'8848'__206 (coe v21)
                                                                     (coe v22)
                                                                     (coe
                                                                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                           v23)
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                        (coe v8))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v21
                                                         -> coe
                                                              C__'8848'__206 (coe v6)
                                                              (coe
                                                                 C_'8512'_216
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v19)
                                                                       (coe (0 :: Integer)))
                                                                    (coe v16))
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                              (coe v8)
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> coe
                                                       C__'8848'__206 (coe v6)
                                                       (coe
                                                          C_'8512'_216
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                C__Δ__170 (coe C__'8800'0_230 v19)
                                                                (coe v18))
                                                             (coe v16))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                       (coe v8)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊠-⅀
d_'8864''45''8512'_574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_Poly_174 -> T_Poly_174
d_'8864''45''8512'_574 v0 v1 v2 v3 v4 ~v5 v6 v7 v8
  = du_'8864''45''8512'_574 v0 v1 v2 v3 v4 v6 v7 v8
du_'8864''45''8512'_574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_Poly_174 -> T_Poly_174
du_'8864''45''8512'_574 v0 v1 v2 v3 v4 v5 v6 v7
  = case coe v7 of
      C__'8848'__206 v8 v9 v10
        -> case coe v9 of
             C_Κ_208 v11
               -> let v12
                        = coe
                            du_'8864''45'Κ'45'inj_578 (coe v0) (coe v1) (coe v2) (coe v3)
                            (coe v11) (coe v5) in
                  case coe v12 of
                    MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                      -> coe
                           C__'8848'__206 (coe (0 :: Integer))
                           (coe
                              C_Κ_208
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe v2))))
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v4))
                    MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v13
                      -> case coe v13 of
                           MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v14 v15
                             -> case coe v14 of
                                  C__Δ__170 v16 v17
                                    -> case coe v16 of
                                         C__'8800'0_230 v18
                                           -> case coe v17 of
                                                0 -> case coe v15 of
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                         -> case coe v18 of
                                                              C__'8848'__206 v20 v21 v22
                                                                -> coe
                                                                     C__'8848'__206 (coe v20)
                                                                     (coe v21)
                                                                     (coe
                                                                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                           v22)
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                        (coe v6))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v20
                                                         -> coe
                                                              C__'8848'__206
                                                              (coe
                                                                 addInt (coe (1 :: Integer))
                                                                 (coe v3))
                                                              (coe
                                                                 C_'8512'_216
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v18)
                                                                       (coe (0 :: Integer)))
                                                                    (coe v15))
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                              (coe v6)
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> coe
                                                       C__'8848'__206
                                                       (coe addInt (coe (1 :: Integer)) (coe v3))
                                                       (coe
                                                          C_'8512'_216
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                C__Δ__170 (coe C__'8800'0_230 v18)
                                                                (coe v17))
                                                             (coe v15))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                       (coe v6)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             C_'8512'_216 v12 v13
               -> let v14 = subInt (coe v8) (coe (1 :: Integer)) in
                  coe
                    du_'8864''45'match_596 (coe v0) (coe v1) (coe v2) (coe v3)
                    (coe v14) (coe v4) (coe v6) (coe v10)
                    (coe du_inj'45'compare_104 (coe v6) (coe v10)) (coe v5) (coe v12)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊠-Κ-inj
d_'8864''45'Κ'45'inj_578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d_'8864''45'Κ'45'inj_578 v0 v1 v2 v3 ~v4 v5 v6
  = du_'8864''45'Κ'45'inj_578 v0 v1 v2 v3 v5 v6
du_'8864''45'Κ'45'inj_578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
du_'8864''45'Κ'45'inj_578 v0 v1 v2 v3 v4 v5
  = coe
      du_para_338 (coe v2)
      (coe
         MAlonzo.Code.Data.Product.Base.du_map'8321'_114
         (coe
            du_'8864''45'Κ_568 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)))
      (coe v5)
-- Tactic.RingSolver.Core.Polynomial.Base.⊠-⅀-inj
d_'8864''45''8512''45'inj_584 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  T_Poly_174 -> T_Poly_174
d_'8864''45''8512''45'inj_584 v0 v1 v2 v3 v4 ~v5 v6 v7 v8
  = du_'8864''45''8512''45'inj_584 v0 v1 v2 v3 v4 v6 v7 v8
du_'8864''45''8512''45'inj_584 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  T_Poly_174 -> T_Poly_174
du_'8864''45''8512''45'inj_584 v0 v1 v2 v3 v4 v5 v6 v7
  = case coe v7 of
      C__'8848'__206 v8 v9 v10
        -> case coe v9 of
             C_Κ_208 v11
               -> let v12
                        = coe
                            du_'8864''45'Κ'45'inj_578 (coe v0) (coe v1) (coe v2) (coe v3)
                            (coe v11) (coe v6) in
                  case coe v12 of
                    MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                      -> coe
                           C__'8848'__206 (coe (0 :: Integer))
                           (coe
                              C_Κ_208
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                 (coe
                                    MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                    (coe v2))))
                           (coe
                              MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v4))
                    MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v13
                      -> case coe v13 of
                           MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v14 v15
                             -> case coe v14 of
                                  C__Δ__170 v16 v17
                                    -> case coe v16 of
                                         C__'8800'0_230 v18
                                           -> case coe v17 of
                                                0 -> case coe v15 of
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                         -> case coe v18 of
                                                              C__'8848'__206 v20 v21 v22
                                                                -> coe
                                                                     C__'8848'__206 (coe v20)
                                                                     (coe v21)
                                                                     (coe
                                                                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                           v22)
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                        (coe v5))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v20
                                                         -> coe
                                                              C__'8848'__206
                                                              (coe
                                                                 addInt (coe (1 :: Integer))
                                                                 (coe v3))
                                                              (coe
                                                                 C_'8512'_216
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v18)
                                                                       (coe (0 :: Integer)))
                                                                    (coe v15))
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                              (coe v5)
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> coe
                                                       C__'8848'__206
                                                       (coe addInt (coe (1 :: Integer)) (coe v3))
                                                       (coe
                                                          C_'8512'_216
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                C__Δ__170 (coe C__'8800'0_230 v18)
                                                                (coe v17))
                                                             (coe v15))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                       (coe v5)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             C_'8512'_216 v12 v13
               -> let v14 = subInt (coe v8) (coe (1 :: Integer)) in
                  coe
                    du_'8864''45'match_596 (coe v0) (coe v1) (coe v2) (coe v3)
                    (coe v14) (coe v4) (coe v5) (coe v10)
                    (coe du_inj'45'compare_104 (coe v5) (coe v10)) (coe v6) (coe v12)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊠-match
d_'8864''45'match_596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_InjectionOrdering_64 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 -> T_Poly_174
d_'8864''45'match_596 v0 v1 v2 v3 v4 v5 ~v6 v7 v8 v9 v10 v11
  = du_'8864''45'match_596 v0 v1 v2 v3 v4 v5 v7 v8 v9 v10 v11
du_'8864''45'match_596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272 ->
  T_InjectionOrdering_64 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 -> T_Poly_174
du_'8864''45'match_596 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v8 of
      C_inj'45'lt_76 v13
        -> let v15
                 = coe
                     du_para_338 (coe v2)
                     (coe
                        MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                        (coe
                           du_'8864''45''8512''45'inj_584 (coe v0) (coe v1) (coe v2) (coe v3)
                           (coe v4) (coe v13) (coe v9)))
                     (coe v10) in
           case coe v15 of
             MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
               -> coe
                    C__'8848'__206 (coe (0 :: Integer))
                    (coe
                       C_Κ_208
                       (coe
                          MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                             (coe v2))))
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v5))
             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v16
               -> case coe v16 of
                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                      -> case coe v17 of
                           C__Δ__170 v19 v20
                             -> case coe v19 of
                                  C__'8800'0_230 v21
                                    -> case coe v20 of
                                         0 -> case coe v18 of
                                                MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                  -> case coe v21 of
                                                       C__'8848'__206 v23 v24 v25
                                                         -> coe
                                                              C__'8848'__206 (coe v23) (coe v24)
                                                              (coe
                                                                 MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                 (coe
                                                                    MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                    v25)
                                                                 (coe
                                                                    MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                 (coe v7))
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v23
                                                  -> coe
                                                       C__'8848'__206
                                                       (coe addInt (coe (1 :: Integer)) (coe v4))
                                                       (coe
                                                          C_'8512'_216
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                C__Δ__170 (coe C__'8800'0_230 v21)
                                                                (coe (0 :: Integer)))
                                                             (coe v18))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                       (coe v7)
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> coe
                                                C__'8848'__206
                                                (coe addInt (coe (1 :: Integer)) (coe v4))
                                                (coe
                                                   C_'8512'_216
                                                   (coe
                                                      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                      (coe
                                                         C__Δ__170 (coe C__'8800'0_230 v21)
                                                         (coe v20))
                                                      (coe v18))
                                                   (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                (coe v7)
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_inj'45'gt_86 v14
        -> let v15
                 = coe
                     du_para_338 (coe v2)
                     (coe
                        MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                        (coe
                           du_'8864''45''8512''45'inj_584 (coe v0) (coe v1) (coe v2) (coe v4)
                           (coe v3) (coe v14) (coe v10)))
                     (coe v9) in
           case coe v15 of
             MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
               -> coe
                    C__'8848'__206 (coe (0 :: Integer))
                    (coe
                       C_Κ_208
                       (coe
                          MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                          (coe
                             MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                             (coe v2))))
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v5))
             MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v16
               -> case coe v16 of
                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v17 v18
                      -> case coe v17 of
                           C__Δ__170 v19 v20
                             -> case coe v19 of
                                  C__'8800'0_230 v21
                                    -> case coe v20 of
                                         0 -> case coe v18 of
                                                MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                  -> case coe v21 of
                                                       C__'8848'__206 v23 v24 v25
                                                         -> coe
                                                              C__'8848'__206 (coe v23) (coe v24)
                                                              (coe
                                                                 MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                 (coe
                                                                    MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                    v25)
                                                                 (coe
                                                                    MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                 (coe v6))
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v23
                                                  -> coe
                                                       C__'8848'__206
                                                       (coe addInt (coe (1 :: Integer)) (coe v3))
                                                       (coe
                                                          C_'8512'_216
                                                          (coe
                                                             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                             (coe
                                                                C__Δ__170 (coe C__'8800'0_230 v21)
                                                                (coe (0 :: Integer)))
                                                             (coe v18))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                       (coe v6)
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> coe
                                                C__'8848'__206
                                                (coe addInt (coe (1 :: Integer)) (coe v3))
                                                (coe
                                                   C_'8512'_216
                                                   (coe
                                                      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                      (coe
                                                         C__Δ__170 (coe C__'8800'0_230 v21)
                                                         (coe v20))
                                                      (coe v18))
                                                   (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                (coe v6)
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_inj'45'eq_92
        -> case coe v10 of
             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v13 v14
               -> case coe v13 of
                    C__Δ__170 v15 v16
                      -> case coe v15 of
                           C__'8800'0_230 v17
                             -> case coe v14 of
                                  MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                    -> let v19
                                             = coe
                                                 du__'9043''42'__252
                                                 (coe
                                                    du_para_338 (coe v2)
                                                    (coe
                                                       MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                                                       (coe
                                                          du_'8864''45'step'8242'_558 (coe v0)
                                                          (coe v1) (coe v2) (coe v3) (coe v17)))
                                                    (coe v9))
                                                 (coe v16) in
                                       case coe v19 of
                                         MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                           -> coe
                                                C__'8848'__206 (coe (0 :: Integer))
                                                (coe
                                                   C_Κ_208
                                                   (coe
                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                                      (coe
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                                         (coe v2))))
                                                (coe
                                                   MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                                                   (coe v5))
                                         MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v20
                                           -> case coe v20 of
                                                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v21 v22
                                                  -> case coe v21 of
                                                       C__Δ__170 v23 v24
                                                         -> case coe v23 of
                                                              C__'8800'0_230 v25
                                                                -> case coe v24 of
                                                                     0 -> case coe v22 of
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                              -> case coe v25 of
                                                                                   C__'8848'__206 v27 v28 v29
                                                                                     -> coe
                                                                                          C__'8848'__206
                                                                                          (coe v27)
                                                                                          (coe v28)
                                                                                          (coe
                                                                                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                                v29)
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                             (coe
                                                                                                v6))
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v27
                                                                              -> coe
                                                                                   C__'8848'__206
                                                                                   (coe
                                                                                      addInt
                                                                                      (coe
                                                                                         (1 ::
                                                                                            Integer))
                                                                                      (coe v3))
                                                                                   (coe
                                                                                      C_'8512'_216
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                         (coe
                                                                                            C__Δ__170
                                                                                            (coe
                                                                                               C__'8800'0_230
                                                                                               v25)
                                                                                            (coe
                                                                                               (0 ::
                                                                                                  Integer)))
                                                                                         (coe v22))
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                   (coe v6)
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     _ -> coe
                                                                            C__'8848'__206
                                                                            (coe
                                                                               addInt
                                                                               (coe (1 :: Integer))
                                                                               (coe v3))
                                                                            (coe
                                                                               C_'8512'_216
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                  (coe
                                                                                     C__Δ__170
                                                                                     (coe
                                                                                        C__'8800'0_230
                                                                                        v25)
                                                                                     (coe v24))
                                                                                  (coe v22))
                                                                               (coe
                                                                                  MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                            (coe v6)
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v19
                                    -> let v20
                                             = coe
                                                 du__'9043''42'__252
                                                 (coe
                                                    du_para_338 (coe v2)
                                                    (coe
                                                       (\ v20 ->
                                                          case coe v20 of
                                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v21 v22
                                                              -> case coe v21 of
                                                                   C__'8848'__206 v23 v24 v25
                                                                     -> coe
                                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                          (coe
                                                                             du_'8864''45'step_564
                                                                             v0 v1 v2 v23 v3 v24 v25
                                                                             v17)
                                                                          (coe
                                                                             d_'8862''45'coeffs_396
                                                                             (coe v0) (coe v1)
                                                                             (coe v2) (coe v3)
                                                                             (coe
                                                                                du_para_338 (coe v2)
                                                                                (coe
                                                                                   MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                                                                                   (coe
                                                                                      du_'8864''45'step_564
                                                                                      v0 v1 v2 v23
                                                                                      v3 v24 v25))
                                                                                (coe v19))
                                                                             (coe v22))
                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                            _ -> MAlonzo.RTE.mazUnreachableError))
                                                    (coe v9))
                                                 (coe v16) in
                                       case coe v20 of
                                         MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                           -> coe
                                                C__'8848'__206 (coe (0 :: Integer))
                                                (coe
                                                   C_Κ_208
                                                   (coe
                                                      MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                                      (coe
                                                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                                         (coe v2))))
                                                (coe
                                                   MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                                                   (coe v5))
                                         MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v21
                                           -> case coe v21 of
                                                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v22 v23
                                                  -> case coe v22 of
                                                       C__Δ__170 v24 v25
                                                         -> case coe v24 of
                                                              C__'8800'0_230 v26
                                                                -> case coe v25 of
                                                                     0 -> case coe v23 of
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                              -> case coe v26 of
                                                                                   C__'8848'__206 v28 v29 v30
                                                                                     -> coe
                                                                                          C__'8848'__206
                                                                                          (coe v28)
                                                                                          (coe v29)
                                                                                          (coe
                                                                                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                                v30)
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                             (coe
                                                                                                v6))
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v28
                                                                              -> coe
                                                                                   C__'8848'__206
                                                                                   (coe
                                                                                      addInt
                                                                                      (coe
                                                                                         (1 ::
                                                                                            Integer))
                                                                                      (coe v3))
                                                                                   (coe
                                                                                      C_'8512'_216
                                                                                      (coe
                                                                                         MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                         (coe
                                                                                            C__Δ__170
                                                                                            (coe
                                                                                               C__'8800'0_230
                                                                                               v26)
                                                                                            (coe
                                                                                               (0 ::
                                                                                                  Integer)))
                                                                                         (coe v23))
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                   (coe v6)
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     _ -> coe
                                                                            C__'8848'__206
                                                                            (coe
                                                                               addInt
                                                                               (coe (1 :: Integer))
                                                                               (coe v3))
                                                                            (coe
                                                                               C_'8512'_216
                                                                               (coe
                                                                                  MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                  (coe
                                                                                     C__Δ__170
                                                                                     (coe
                                                                                        C__'8800'0_230
                                                                                        v26)
                                                                                     (coe v25))
                                                                                  (coe v23))
                                                                               (coe
                                                                                  MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                            (coe v6)
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊠-coeffs
d_'8864''45'coeffs_600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
d_'8864''45'coeffs_600 v0 v1 v2 v3 ~v4 v5 v6
  = du_'8864''45'coeffs_600 v0 v1 v2 v3 v5 v6
du_'8864''45'coeffs_600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'42'_30
du_'8864''45'coeffs_600 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v6 v7
        -> case coe v6 of
             C__Δ__170 v8 v9
               -> case coe v8 of
                    C__'8800'0_230 v10
                      -> case coe v7 of
                           MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                             -> coe
                                  du__'9043''42'__252
                                  (coe
                                     du_para_338 (coe v2)
                                     (coe
                                        MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                                        (coe
                                           du_'8864''45'step'8242'_558 (coe v0) (coe v1) (coe v2)
                                           (coe v3) (coe v10)))
                                     (coe v4))
                                  (coe v9)
                           MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v12
                             -> coe
                                  du__'9043''42'__252
                                  (coe
                                     du_para_338 (coe v2)
                                     (coe
                                        (\ v13 ->
                                           case coe v13 of
                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v14 v15
                                               -> case coe v14 of
                                                    C__'8848'__206 v16 v17 v18
                                                      -> coe
                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                           (coe
                                                              du_'8864''45'step_564 v0 v1 v2 v16 v3
                                                              v17 v18 v10)
                                                           (coe
                                                              d_'8862''45'coeffs_396 (coe v0)
                                                              (coe v1) (coe v2) (coe v3)
                                                              (coe
                                                                 du_para_338 (coe v2)
                                                                 (coe
                                                                    MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                                                                    (coe
                                                                       du_'8864''45'step_564 v0 v1
                                                                       v2 v16 v3 v17 v18))
                                                                 (coe v12))
                                                              (coe v15))
                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                             _ -> MAlonzo.RTE.mazUnreachableError))
                                     (coe v4))
                                  (coe v9)
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊠-cons
d_'8864''45'cons_604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  T_Poly_174 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8864''45'cons_604 v0 v1 v2 v3 ~v4 v5 v6 v7
  = du_'8864''45'cons_604 v0 v1 v2 v3 v5 v6 v7
du_'8864''45'cons_604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer ->
  T_Poly_174 ->
  MAlonzo.Code.Data.List.Kleene.Base.T__'43'_24 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8864''45'cons_604 v0 v1 v2 v3 v4 v5 v6
  = case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
        -> case coe v7 of
             C__'8848'__206 v9 v10 v11
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                    (coe du_'8864''45'step_564 v0 v1 v2 v9 v3 v10 v11 v4)
                    (coe
                       d_'8862''45'coeffs_396 (coe v0) (coe v1) (coe v2) (coe v3)
                       (coe
                          du_para_338 (coe v2)
                          (coe
                             MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                             (coe du_'8864''45'step_564 v0 v1 v2 v9 v3 v10 v11))
                          (coe v5))
                       (coe v8))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base._⊠_
d__'8864'__738 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> T_Poly_174 -> T_Poly_174 -> T_Poly_174
d__'8864'__738 v0 v1 v2 v3
  = coe
      du_'8864''45'step'8242'_558 (coe v0) (coe v1) (coe v2) (coe v3)
-- Tactic.RingSolver.Core.Polynomial.Base.κ
d_κ_742 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> AgdaAny -> T_Poly_174
d_κ_742 ~v0 v1 v2 = du_κ_742 v1 v2
du_κ_742 :: Integer -> AgdaAny -> T_Poly_174
du_κ_742 v0 v1
  = coe
      C__'8848'__206 (coe (0 :: Integer)) (coe C_Κ_208 (coe v1))
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v0))
-- Tactic.RingSolver.Core.Polynomial.Base.ι
d_ι_748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> T_Poly_174
d_ι_748 ~v0 ~v1 v2 v3 v4 = du_ι_748 v2 v3 v4
du_ι_748 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> T_Poly_174
du_ι_748 v0 v1 v2
  = let v3 = coe du_go_138 (coe v1) (coe v2) in
    let v4
          = let v4 = coe MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46 in
            let v5
                  = coe
                      C__'8848'__206 (coe (0 :: Integer))
                      (coe
                         C_Κ_208
                         (coe
                            MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                            (coe
                               MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                               (coe v0))))
                      (coe
                         MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                         (coe du_go_138 (coe v1) (coe v2))) in
            let v6 = 1 :: Integer in
            let v7
                  = MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                      (coe
                         MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                         (coe v0)) in
            let v8
                  = coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                      v0 v7 in
            seq
              (coe v8)
              (if coe v8
                 then coe du__'9043''42'__252 (coe v4) (coe (2 :: Integer))
                 else coe
                        MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                        (coe
                           MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                           (coe C__Δ__170 (coe C__'8800'0_230 v5) (coe v6)) (coe v4))) in
    let v5 = coe du_space'8804''8242'n_148 (coe v2) in
    case coe v4 of
      MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
        -> coe
             C__'8848'__206 (coe (0 :: Integer))
             (coe
                C_Κ_208
                (coe
                   MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                      (coe v0))))
             (coe
                MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v1))
      MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v6
        -> case coe v6 of
             MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v7 v8
               -> case coe v7 of
                    C__Δ__170 v9 v10
                      -> case coe v9 of
                           C__'8800'0_230 v11
                             -> case coe v10 of
                                  0 -> case coe v8 of
                                         MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                           -> case coe v11 of
                                                C__'8848'__206 v13 v14 v15
                                                  -> coe
                                                       C__'8848'__206 (coe v13) (coe v14)
                                                       (coe
                                                          MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                          (coe
                                                             MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                             v15)
                                                          (coe
                                                             MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                          (coe v5))
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v13
                                           -> coe
                                                C__'8848'__206
                                                (coe addInt (coe (1 :: Integer)) (coe v3))
                                                (coe
                                                   C_'8512'_216
                                                   (coe
                                                      MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                      (coe
                                                         C__Δ__170 (coe C__'8800'0_230 v11)
                                                         (coe (0 :: Integer)))
                                                      (coe v8))
                                                   (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                (coe v5)
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> coe
                                         C__'8848'__206 (coe addInt (coe (1 :: Integer)) (coe v3))
                                         (coe
                                            C_'8512'_216
                                            (coe
                                               MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                               (coe C__Δ__170 (coe C__'8800'0_230 v11) (coe v10))
                                               (coe v8))
                                            (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                         (coe v5)
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base.⊡-mult
d_'8865''45'mult_754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> Integer -> T_Poly_174 -> T_Poly_174
d_'8865''45'mult_754 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      0 -> coe v5
      _ -> let v6 = subInt (coe v4) (coe (1 :: Integer)) in
           coe
             du_'8864''45'step'8242'_558 v0 v1 v2 v3
             (d_'8865''45'mult_754
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) (coe v5))
             v5
-- Tactic.RingSolver.Core.Polynomial.Base._⊡_+1
d__'8865'_'43'1_764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> T_Poly_174 -> Integer -> T_Poly_174
d__'8865'_'43'1_764 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      C__'8848'__206 v6 v7 v8
        -> case coe v7 of
             C_Κ_208 v9
               -> coe
                    C__'8848'__206 (coe (0 :: Integer))
                    (coe
                       C_Κ_208
                       (let v10
                              = coe
                                  MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                     (coe
                                        MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                        (coe v2))) in
                        let v11
                              = coe
                                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v10
                                  (coe
                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                     (coe v10) (coe v5) (coe v9))
                                  v9 in
                        case coe v5 of
                          0 -> coe v9
                          _ -> coe v11))
                    (coe v8)
             C_'8512'_216 v10 v11
               -> let v12 = subInt (coe v6) (coe (1 :: Integer)) in
                  case coe v10 of
                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v13 v14
                      -> case coe v13 of
                           C__Δ__170 v15 v16
                             -> case coe v14 of
                                  MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                    -> let v17
                                             = d__'8865'_'43'1_764
                                                 (coe v0) (coe v1) (coe v2) (coe v12)
                                                 (coe d_poly_226 (coe v15)) (coe v5) in
                                       let v18 = addInt (coe mulInt (coe v5) (coe v16)) (coe v16) in
                                       case coe v17 of
                                         C__'8848'__206 v19 v20 v21
                                           -> case coe v20 of
                                                C_Κ_208 v22
                                                  -> let v23
                                                           = let v23
                                                                   = coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_isZero_26
                                                                       v2 v22 in
                                                             seq
                                                               (coe v23)
                                                               (if coe v23
                                                                  then coe
                                                                         du__'9043''42'__252
                                                                         (coe v14)
                                                                         (coe
                                                                            addInt
                                                                            (coe (1 :: Integer))
                                                                            (coe v18))
                                                                  else coe
                                                                         MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48
                                                                         (coe
                                                                            MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                            (coe
                                                                               C__Δ__170
                                                                               (coe
                                                                                  C__'8800'0_230
                                                                                  v17)
                                                                               (coe v18))
                                                                            (coe v14))) in
                                                     case coe v23 of
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                         -> coe
                                                              C__'8848'__206 (coe (0 :: Integer))
                                                              (coe
                                                                 C_Κ_208
                                                                 (coe
                                                                    MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272
                                                                    (coe
                                                                       MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                                                                       (coe v2))))
                                                              (coe
                                                                 MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914
                                                                 (coe v3))
                                                       MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v24
                                                         -> case coe v24 of
                                                              MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42 v25 v26
                                                                -> case coe v25 of
                                                                     C__Δ__170 v27 v28
                                                                       -> case coe v27 of
                                                                            C__'8800'0_230 v29
                                                                              -> case coe v28 of
                                                                                   0 -> case coe
                                                                                               v26 of
                                                                                          MAlonzo.Code.Data.List.Kleene.Base.C_'91''93'_46
                                                                                            -> case coe
                                                                                                      v29 of
                                                                                                 C__'8848'__206 v31 v32 v33
                                                                                                   -> coe
                                                                                                        C__'8848'__206
                                                                                                        (coe
                                                                                                           v31)
                                                                                                        (coe
                                                                                                           v32)
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                                                           (coe
                                                                                                              MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                                                              v33)
                                                                                                           (coe
                                                                                                              MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                                                           (coe
                                                                                                              v8))
                                                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                                                          MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v31
                                                                                            -> coe
                                                                                                 C__'8848'__206
                                                                                                 (coe
                                                                                                    v6)
                                                                                                 (coe
                                                                                                    C_'8512'_216
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                                       (coe
                                                                                                          C__Δ__170
                                                                                                          (coe
                                                                                                             C__'8800'0_230
                                                                                                             v29)
                                                                                                          (coe
                                                                                                             (0 ::
                                                                                                                Integer)))
                                                                                                       (coe
                                                                                                          v26))
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                                 (coe
                                                                                                    v8)
                                                                                          _ -> MAlonzo.RTE.mazUnreachableError
                                                                                   _ -> coe
                                                                                          C__'8848'__206
                                                                                          (coe v6)
                                                                                          (coe
                                                                                             C_'8512'_216
                                                                                             (coe
                                                                                                MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                                                (coe
                                                                                                   C__Δ__170
                                                                                                   (coe
                                                                                                      C__'8800'0_230
                                                                                                      v29)
                                                                                                   (coe
                                                                                                      v28))
                                                                                                (coe
                                                                                                   v26))
                                                                                             (coe
                                                                                                MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                                                          (coe v8)
                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> MAlonzo.RTE.mazUnreachableError
                                                C_'8512'_216 v23 v24
                                                  -> case coe v18 of
                                                       0 -> case coe v17 of
                                                              C__'8848'__206 v25 v26 v27
                                                                -> coe
                                                                     C__'8848'__206 (coe v25)
                                                                     (coe v26)
                                                                     (coe
                                                                        MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Base.C_'8804''8242''45'step_282
                                                                           v27)
                                                                        (coe
                                                                           MAlonzo.Code.Data.Nat.Properties.du_'8804''8242''45'trans_5904)
                                                                        (coe v8))
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> coe
                                                              C__'8848'__206 (coe v6)
                                                              (coe
                                                                 C_'8512'_216
                                                                 (coe
                                                                    MAlonzo.Code.Data.List.Kleene.Base.C__'38'__42
                                                                    (coe
                                                                       C__Δ__170
                                                                       (coe C__'8800'0_230 v17)
                                                                       (coe v18))
                                                                    (coe v14))
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                                                              (coe v8)
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  MAlonzo.Code.Data.List.Kleene.Base.C_'8761'__48 v17
                                    -> coe
                                         d_'8865''45'mult_754 (coe v0) (coe v1) (coe v2) (coe v3)
                                         (coe v5)
                                         (coe
                                            C__'8848'__206 (coe v6) (coe C_'8512'_216 v10 v11)
                                            (coe v8))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.Polynomial.Base._⊡_
d__'8865'__788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_RawCoeff_14 ->
  Integer -> T_Poly_174 -> Integer -> T_Poly_174
d__'8865'__788 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      0 -> coe
             C__'8848'__206 (coe (0 :: Integer))
             (coe
                C_Κ_208
                (coe
                   MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274
                   (coe
                      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.d_rawRing_24
                      (coe v2))))
             (coe
                MAlonzo.Code.Data.Nat.Properties.d_z'8804''8242'n_5914 (coe v3))
      _ -> let v6 = subInt (coe v5) (coe (1 :: Integer)) in
           coe
             d__'8865'_'43'1_764 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
             (coe v6)
