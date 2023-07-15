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

module MAlonzo.Code.Algebra.Construct.NaturalChoice.Base where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Bundles

-- Algebra.Construct.NaturalChoice.Base._._≥_
d__'8805'__82 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  AgdaAny -> AgdaAny -> ()
d__'8805'__82 = erased
-- Algebra.Construct.NaturalChoice.Base._.MinOperator
d_MinOperator_84 a0 a1 a2 a3 = ()
data T_MinOperator_84
  = C_MinOperator'46'constructor_973 (AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Construct.NaturalChoice.Base._.MinOperator._⊓_
d__'8851'__100 :: T_MinOperator_84 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8851'__100 v0
  = case coe v0 of
      C_MinOperator'46'constructor_973 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.Base._.MinOperator.x≤y⇒x⊓y≈x
d_x'8804'y'8658'x'8851'y'8776'x_106 ::
  T_MinOperator_84 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'x'8851'y'8776'x_106 v0
  = case coe v0 of
      C_MinOperator'46'constructor_973 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.Base._.MinOperator.x≥y⇒x⊓y≈y
d_x'8805'y'8658'x'8851'y'8776'y_112 ::
  T_MinOperator_84 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8805'y'8658'x'8851'y'8776'y_112 v0
  = case coe v0 of
      C_MinOperator'46'constructor_973 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.Base._.MaxOperator
d_MaxOperator_114 a0 a1 a2 a3 = ()
data T_MaxOperator_114
  = C_MaxOperator'46'constructor_1501 (AgdaAny -> AgdaAny -> AgdaAny)
                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Construct.NaturalChoice.Base._.MaxOperator._⊔_
d__'8852'__130 ::
  T_MaxOperator_114 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8852'__130 v0
  = case coe v0 of
      C_MaxOperator'46'constructor_1501 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.Base._.MaxOperator.x≤y⇒x⊔y≈y
d_x'8804'y'8658'x'8852'y'8776'y_136 ::
  T_MaxOperator_114 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'x'8852'y'8776'y_136 v0
  = case coe v0 of
      C_MaxOperator'46'constructor_1501 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.Base._.MaxOperator.x≥y⇒x⊔y≈x
d_x'8805'y'8658'x'8852'y'8776'x_142 ::
  T_MaxOperator_114 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8805'y'8658'x'8852'y'8776'x_142 v0
  = case coe v0 of
      C_MaxOperator'46'constructor_1501 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.Base.MinOp⇒MaxOp
d_MinOp'8658'MaxOp_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  T_MinOperator_84 -> T_MaxOperator_114
d_MinOp'8658'MaxOp_144 ~v0 ~v1 ~v2 ~v3 v4
  = du_MinOp'8658'MaxOp_144 v4
du_MinOp'8658'MaxOp_144 :: T_MinOperator_84 -> T_MaxOperator_114
du_MinOp'8658'MaxOp_144 v0
  = coe
      C_MaxOperator'46'constructor_1501 (coe d__'8851'__100 (coe v0))
      (coe d_x'8805'y'8658'x'8851'y'8776'y_112 (coe v0))
      (coe d_x'8804'y'8658'x'8851'y'8776'x_106 (coe v0))
-- Algebra.Construct.NaturalChoice.Base._._._⊓_
d__'8851'__154 :: T_MinOperator_84 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8851'__154 v0 = coe d__'8851'__100 (coe v0)
-- Algebra.Construct.NaturalChoice.Base._._.x≤y⇒x⊓y≈x
d_x'8804'y'8658'x'8851'y'8776'x_156 ::
  T_MinOperator_84 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'x'8851'y'8776'x_156 v0
  = coe d_x'8804'y'8658'x'8851'y'8776'x_106 (coe v0)
-- Algebra.Construct.NaturalChoice.Base._._.x≥y⇒x⊓y≈y
d_x'8805'y'8658'x'8851'y'8776'y_158 ::
  T_MinOperator_84 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8805'y'8658'x'8851'y'8776'y_158 v0
  = coe d_x'8805'y'8658'x'8851'y'8776'y_112 (coe v0)
-- Algebra.Construct.NaturalChoice.Base.MaxOp⇒MinOp
d_MaxOp'8658'MinOp_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  T_MaxOperator_114 -> T_MinOperator_84
d_MaxOp'8658'MinOp_160 ~v0 ~v1 ~v2 ~v3 v4
  = du_MaxOp'8658'MinOp_160 v4
du_MaxOp'8658'MinOp_160 :: T_MaxOperator_114 -> T_MinOperator_84
du_MaxOp'8658'MinOp_160 v0
  = coe
      C_MinOperator'46'constructor_973 (coe d__'8852'__130 (coe v0))
      (coe d_x'8805'y'8658'x'8852'y'8776'x_142 (coe v0))
      (coe d_x'8804'y'8658'x'8852'y'8776'y_136 (coe v0))
-- Algebra.Construct.NaturalChoice.Base._._._⊔_
d__'8852'__170 ::
  T_MaxOperator_114 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8852'__170 v0 = coe d__'8852'__130 (coe v0)
-- Algebra.Construct.NaturalChoice.Base._._.x≤y⇒x⊔y≈y
d_x'8804'y'8658'x'8852'y'8776'y_172 ::
  T_MaxOperator_114 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'x'8852'y'8776'y_172 v0
  = coe d_x'8804'y'8658'x'8852'y'8776'y_136 (coe v0)
-- Algebra.Construct.NaturalChoice.Base._._.x≥y⇒x⊔y≈x
d_x'8805'y'8658'x'8852'y'8776'x_174 ::
  T_MaxOperator_114 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8805'y'8658'x'8852'y'8776'x_174 v0
  = coe d_x'8805'y'8658'x'8852'y'8776'x_142 (coe v0)
