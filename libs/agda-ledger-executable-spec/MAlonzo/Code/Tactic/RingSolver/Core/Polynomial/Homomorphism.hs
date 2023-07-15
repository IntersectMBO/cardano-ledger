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

module MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Addition
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Constants
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Multiplication
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables
import qualified MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters

-- Tactic.RingSolver.Core.Polynomial.Homomorphism._.⊞-hom
d_'8862''45'hom_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8862''45'hom_22 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Addition.du_'8862''45'hom_492
      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) v6 v7
-- Tactic.RingSolver.Core.Polynomial.Homomorphism._.⊠-hom
d_'8864''45'hom_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8864''45'hom_26 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Multiplication.du_'8864''45'hom_882
      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) v6
-- Tactic.RingSolver.Core.Polynomial.Homomorphism._.⊟-hom
d_'8863''45'hom_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8863''45'hom_30 ~v0 ~v1 ~v2 ~v3 v4 = du_'8863''45'hom_30 v4
du_'8863''45'hom_30 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'8863''45'hom_30 v0 v1
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation.du_'8863''45'hom_518
      (coe v0)
-- Tactic.RingSolver.Core.Polynomial.Homomorphism._.⊡-hom
d_'8865''45'hom_34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Base.T_Poly_174 ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8865''45'hom_34 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation.d_'8865''45'hom_656
      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
-- Tactic.RingSolver.Core.Polynomial.Homomorphism._.κ-hom
d_κ'45'hom_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_κ'45'hom_38 ~v0 ~v1 ~v2 ~v3 v4 = du_κ'45'hom_38 v4
du_κ'45'hom_38 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_κ'45'hom_38 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Constants.du_κ'45'hom_396
      (coe v0) v2
-- Tactic.RingSolver.Core.Polynomial.Homomorphism._.ι-hom
d_ι'45'hom_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_ι'45'hom_42 ~v0 ~v1 ~v2 ~v3 v4 = du_ι'45'hom_42 v4
du_ι'45'hom_42 ::
  MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Parameters.T_Homomorphism_66 ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_ι'45'hom_42 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables.du_ι'45'hom_480
      (coe v0)
