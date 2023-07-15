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

module MAlonzo.Code.Tactic.RingSolver.Core.Expression where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Definitions.RawMonoid
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Vec.Base

-- Tactic.RingSolver.Core.Expression.Expr
d_Expr_14 a0 a1 a2 = ()
data T_Expr_14
  = C_Κ_22 AgdaAny | C_Ι_24 MAlonzo.Code.Data.Fin.Base.T_Fin_10 |
    C__'8853'__26 T_Expr_14 T_Expr_14 |
    C__'8855'__28 T_Expr_14 T_Expr_14 |
    C__'8859'__30 T_Expr_14 Integer | C_'8861'__32 T_Expr_14
-- Tactic.RingSolver.Core.Expression._.Eval.⟦_⟧
d_'10214'_'10215'_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Integer ->
  T_Expr_14 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'_88 ~v0 ~v1 v2 ~v3 ~v4 v5 ~v6 v7 v8
  = du_'10214'_'10215'_88 v2 v5 v7 v8
du_'10214'_'10215'_88 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_Expr_14 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'10214'_'10215'_88 v0 v1 v2 v3
  = case coe v2 of
      C_Κ_22 v4 -> coe v1 v4
      C_Ι_24 v4
        -> coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v3) (coe v4)
      C__'8853'__26 v4 v5
        -> coe
             MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
             (coe du_'10214'_'10215'_88 (coe v0) (coe v1) (coe v4) (coe v3))
             (coe du_'10214'_'10215'_88 (coe v0) (coe v1) (coe v5) (coe v3))
      C__'8855'__28 v4 v5
        -> coe
             MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
             (coe du_'10214'_'10215'_88 (coe v0) (coe v1) (coe v4) (coe v3))
             (coe du_'10214'_'10215'_88 (coe v0) (coe v1) (coe v5) (coe v3))
      C__'8859'__30 v4 v5
        -> let v6
                 = coe
                     MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
           let v7
                 = coe du_'10214'_'10215'_88 (coe v0) (coe v1) (coe v4) (coe v3) in
           let v8 = subInt (coe v5) (coe (1 :: Integer)) in
           let v9
                 = coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v6
                     (coe
                        MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                        (coe v6) (coe v8) (coe v7))
                     v7 in
           case coe v5 of
             0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v6)
             1 -> coe v7
             _ -> coe v9
      C_'8861'__32 v4
        -> coe
             MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0
             (coe du_'10214'_'10215'_88 (coe v0) (coe v1) (coe v4) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
