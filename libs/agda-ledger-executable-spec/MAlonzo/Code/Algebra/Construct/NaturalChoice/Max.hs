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

module MAlonzo.Code.Algebra.Construct.NaturalChoice.Max where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.Base
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MaxOp
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.Min
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Construct.Converse

-- Algebra.Construct.NaturalChoice.Max._.totalPreorder
d_totalPreorder_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204
d_totalPreorder_46 ~v0 ~v1 ~v2 v3 = du_totalPreorder_46 v3
du_totalPreorder_46 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204
du_totalPreorder_46 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0)
-- Algebra.Construct.NaturalChoice.Max.Min.x≤y⇒x⊓y≈x
d_x'8804'y'8658'x'8851'y'8776'x_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'x'8851'y'8776'x_88 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8658'x'8851'y'8776'x_88 v3
du_x'8804'y'8658'x'8851'y'8776'x_88 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'x'8851'y'8776'x_88 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Min.du_x'8804'y'8658'x'8851'y'8776'x_106
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalOrder_822
         (coe v0))
-- Algebra.Construct.NaturalChoice.Max.Min.x≤y⇒y⊓x≈x
d_x'8804'y'8658'y'8851'x'8776'x_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'y'8851'x'8776'x_92 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8658'y'8851'x'8776'x_92 v3
du_x'8804'y'8658'y'8851'x'8776'x_92 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'y'8851'x'8776'x_92 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Min.du_x'8804'y'8658'y'8851'x'8776'x_136
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalOrder_822
         (coe v0))
-- Algebra.Construct.NaturalChoice.Max._⊔_
d__'8852'__170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8852'__170 ~v0 ~v1 ~v2 v3 = du__'8852'__170 v3
du__'8852'__170 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8852'__170 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Min.du__'8851'__80
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalOrder_822
         (coe v0))
-- Algebra.Construct.NaturalChoice.Max.maxOperator
d_maxOperator_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MaxOperator_114
d_maxOperator_172 ~v0 ~v1 ~v2 v3 = du_maxOperator_172 v3
du_maxOperator_172 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MaxOperator_114
du_maxOperator_172 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MaxOperator'46'constructor_1501
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Min.du__'8851'__80
         (coe
            MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalOrder_822
            (coe v0)))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.Min.du_x'8804'y'8658'y'8851'x'8776'x_136
              (coe
                 MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalOrder_822
                 (coe v0))
              (coe v2) (coe v1)))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Min.du_x'8804'y'8658'x'8851'y'8776'x_106
         (coe
            MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalOrder_822
            (coe v0)))
-- Algebra.Construct.NaturalChoice.Max._.mono-≤-distrib-⊔
d_mono'45''8804''45'distrib'45''8852'_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
d_mono'45''8804''45'distrib'45''8852'_176 ~v0 ~v1 ~v2 v3
  = du_mono'45''8804''45'distrib'45''8852'_176 v3
du_mono'45''8804''45'distrib'45''8852'_176 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
du_mono'45''8804''45'distrib'45''8852'_176 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MaxOp.du_mono'45''8804''45'distrib'45''8852'_168
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_maxOperator_172 (coe v0))
-- Algebra.Construct.NaturalChoice.Max._.x⊓y≤x
d_x'8851'y'8804'x_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8804'x_178 ~v0 ~v1 ~v2 v3 = du_x'8851'y'8804'x_178 v3
du_x'8851'y'8804'x_178 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8804'x_178 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'x'8851'z'8804'y_180 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8658'x'8851'z'8804'y_180 v3
du_x'8804'y'8658'x'8851'z'8804'y_180 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'x'8851'z'8804'y_180 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'z'8851'x'8804'y_182 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8658'z'8851'x'8804'y_182 v3
du_x'8804'y'8658'z'8851'x'8804'y_182 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'z'8851'x'8804'y_182 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.x⊓y≤y
d_x'8851'y'8804'y_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8804'y_184 ~v0 ~v1 ~v2 v3 = du_x'8851'y'8804'y_184 v3
du_x'8851'y'8804'y_184 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8804'y_184 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8776'x'8658'x'8804'y_186 ~v0 ~v1 ~v2 v3
  = du_x'8851'y'8776'x'8658'x'8804'y_186 v3
du_x'8851'y'8776'x'8658'x'8804'y_186 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8776'x'8658'x'8804'y_186 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8776'y'8658'y'8804'x_188 ~v0 ~v1 ~v2 v3
  = du_x'8851'y'8776'y'8658'y'8804'x_188 v3
du_x'8851'y'8776'y'8658'y'8804'x_188 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8776'y'8658'y'8804'x_188 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8851'z'8658'x'8804'y_190 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8851'z'8658'x'8804'y_190 v3
du_x'8804'y'8851'z'8658'x'8804'y_190 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8851'z'8658'x'8804'y_190 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8851'z'8658'x'8804'z_192 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8851'z'8658'x'8804'z_192 v3
du_x'8804'y'8851'z'8658'x'8804'z_192 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8851'z'8658'x'8804'z_192 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-assoc
d_'8851''45'assoc_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'assoc_194 ~v0 ~v1 ~v2 v3 = du_'8851''45'assoc_194 v3
du_'8851''45'assoc_194 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'assoc_194 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'assoc_2692
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-band
d_'8851''45'band_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_196 ~v0 ~v1 ~v2 v3 = du_'8851''45'band_196 v3
du_'8851''45'band_196 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
du_'8851''45'band_196 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-comm
d_'8851''45'comm_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'comm_198 ~v0 ~v1 ~v2 v3 = du_'8851''45'comm_198 v3
du_'8851''45'comm_198 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'comm_198 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'comm_2604
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_200 ~v0 ~v1 ~v2 v3
  = du_'8851''45'commutativeSemigroup_200 v3
du_'8851''45'commutativeSemigroup_200 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
du_'8851''45'commutativeSemigroup_200 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-cong
d_'8851''45'cong_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'cong_202 ~v0 ~v1 ~v2 v3 = du_'8851''45'cong_202 v3
du_'8851''45'cong_202 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'cong_202 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong_2678
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-congʳ
d_'8851''45'cong'691'_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'cong'691'_204 ~v0 ~v1 ~v2 v3
  = du_'8851''45'cong'691'_204 v3
du_'8851''45'cong'691'_204 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'cong'691'_204 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong'691'_2668
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-congˡ
d_'8851''45'cong'737'_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'cong'737'_206 ~v0 ~v1 ~v2 v3
  = du_'8851''45'cong'737'_206 v3
du_'8851''45'cong'737'_206 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'cong'737'_206 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong'737'_2630
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-idem
d_'8851''45'idem_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny
d_'8851''45'idem_208 ~v0 ~v1 ~v2 v3 = du_'8851''45'idem_208 v3
du_'8851''45'idem_208 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny
du_'8851''45'idem_208 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'idem_2732
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-identity
d_'8851''45'identity_210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'identity_210 ~v0 ~v1 ~v2 v3
  = du_'8851''45'identity_210 v3
du_'8851''45'identity_210 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8851''45'identity_210 v0
  = let v1 = coe du_maxOperator_172 (coe v0) in
    coe
      (\ v2 v3 ->
         coe
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
           (coe
              (\ v4 ->
                 coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8852'y'8776'y_136
                   v1 v2 v4 (coe v3 v4)))
           (coe
              (\ v4 ->
                 coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8852'y'8776'x_142
                   v1 v4 v2 (coe v3 v4))))
-- Algebra.Construct.NaturalChoice.Max._.⊓-identityʳ
d_'8851''45'identity'691'_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'identity'691'_212 ~v0 ~v1 ~v2 v3
  = du_'8851''45'identity'691'_212 v3
du_'8851''45'identity'691'_212 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8851''45'identity'691'_212 v0
  = let v1 = coe du_maxOperator_172 (coe v0) in
    coe
      (\ v2 v3 v4 ->
         coe
           MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8852'y'8776'x_142
           v1 v4 v2 (coe v3 v4))
-- Algebra.Construct.NaturalChoice.Max._.⊓-identityˡ
d_'8851''45'identity'737'_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'identity'737'_214 ~v0 ~v1 ~v2 v3
  = du_'8851''45'identity'737'_214 v3
du_'8851''45'identity'737'_214 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8851''45'identity'737'_214 v0
  = let v1 = coe du_maxOperator_172 (coe v0) in
    coe
      (\ v2 v3 v4 ->
         coe
           MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8852'y'8776'y_136
           v1 v2 v4 (coe v3 v4))
-- Algebra.Construct.NaturalChoice.Max._.⊓-isBand
d_'8851''45'isBand_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_216 ~v0 ~v1 ~v2 v3 = du_'8851''45'isBand_216 v3
du_'8851''45'isBand_216 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_'8851''45'isBand_216 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_218 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isCommutativeSemigroup_218 v3
du_'8851''45'isCommutativeSemigroup_218 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'8851''45'isCommutativeSemigroup_218 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-isMagma
d_'8851''45'isMagma_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_220 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isMagma_220 v3
du_'8851''45'isMagma_220 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8851''45'isMagma_220 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-isMonoid
d_'8851''45'isMonoid_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8851''45'isMonoid_222 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isMonoid_222 v3
du_'8851''45'isMonoid_222 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'8851''45'isMonoid_222 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMonoid_2790
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_224 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isSelectiveMagma_224 v3
du_'8851''45'isSelectiveMagma_224 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
du_'8851''45'isSelectiveMagma_224 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-isSemigroup
d_'8851''45'isSemigroup_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_226 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isSemigroup_226 v3
du_'8851''45'isSemigroup_226 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8851''45'isSemigroup_226 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-glb
d_'8851''45'glb_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'glb_228 ~v0 ~v1 ~v2 v3 = du_'8851''45'glb_228 v3
du_'8851''45'glb_228 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'glb_228 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-magma
d_'8851''45'magma_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_230 ~v0 ~v1 ~v2 v3 = du_'8851''45'magma_230 v3
du_'8851''45'magma_230 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_'8851''45'magma_230 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-mono-≤
d_'8851''45'mono'45''8804'_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'mono'45''8804'_232 ~v0 ~v1 ~v2 v3
  = du_'8851''45'mono'45''8804'_232 v3
du_'8851''45'mono'45''8804'_232 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'mono'45''8804'_232 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-monoid
d_'8851''45'monoid_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'8851''45'monoid_234 ~v0 ~v1 ~v2 v3 = du_'8851''45'monoid_234 v3
du_'8851''45'monoid_234 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'8851''45'monoid_234 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'monoid_2808
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'mono'691''45''8804'_236 ~v0 ~v1 ~v2 v3
  = du_'8851''45'mono'691''45''8804'_236 v3
du_'8851''45'mono'691''45''8804'_236 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'mono'691''45''8804'_236 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'mono'737''45''8804'_238 ~v0 ~v1 ~v2 v3
  = du_'8851''45'mono'737''45''8804'_238 v3
du_'8851''45'mono'737''45''8804'_238 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'mono'737''45''8804'_238 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-sel
d_'8851''45'sel_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_240 ~v0 ~v1 ~v2 v3 = du_'8851''45'sel_240 v3
du_'8851''45'sel_240 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_'8851''45'sel_240 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-selectiveMagma
d_'8851''45'selectiveMagma_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_242 ~v0 ~v1 ~v2 v3
  = du_'8851''45'selectiveMagma_242 v3
du_'8851''45'selectiveMagma_242 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
du_'8851''45'selectiveMagma_242 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-semigroup
d_'8851''45'semigroup_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_244 ~v0 ~v1 ~v2 v3
  = du_'8851''45'semigroup_244 v3
du_'8851''45'semigroup_244 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'8851''45'semigroup_244 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-triangulate
d_'8851''45'triangulate_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'triangulate_246 ~v0 ~v1 ~v2 v3
  = du_'8851''45'triangulate_246 v3
du_'8851''45'triangulate_246 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'triangulate_246 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728
              (coe v0) in
    let v2 = coe du_maxOperator_172 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'triangulate_3040
      (coe
         MAlonzo.Code.Relation.Binary.Construct.Converse.du_totalPreorder_698
         (coe v1))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.du_MaxOp'8658'MinOp_160
         (coe v2))
-- Algebra.Construct.NaturalChoice.Max._.⊓-zero
d_'8851''45'zero_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'zero_248 ~v0 ~v1 ~v2 v3 = du_'8851''45'zero_248 v3
du_'8851''45'zero_248 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8851''45'zero_248 v0
  = let v1 = coe du_maxOperator_172 (coe v0) in
    coe
      (\ v2 v3 ->
         coe
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
           (coe
              (\ v4 ->
                 coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8852'y'8776'x_142
                   v1 v2 v4 (coe v3 v4)))
           (coe
              (\ v4 ->
                 coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8852'y'8776'y_136
                   v1 v4 v2 (coe v3 v4))))
-- Algebra.Construct.NaturalChoice.Max._.⊓-zeroʳ
d_'8851''45'zero'691'_250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'zero'691'_250 ~v0 ~v1 ~v2 v3
  = du_'8851''45'zero'691'_250 v3
du_'8851''45'zero'691'_250 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8851''45'zero'691'_250 v0
  = let v1 = coe du_maxOperator_172 (coe v0) in
    coe
      (\ v2 v3 v4 ->
         coe
           MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8852'y'8776'y_136
           v1 v4 v2 (coe v3 v4))
-- Algebra.Construct.NaturalChoice.Max._.⊓-zeroˡ
d_'8851''45'zero'737'_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'zero'737'_252 ~v0 ~v1 ~v2 v3
  = du_'8851''45'zero'737'_252 v3
du_'8851''45'zero'737'_252 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8851''45'zero'737'_252 v0
  = let v1 = coe du_maxOperator_172 (coe v0) in
    coe
      (\ v2 v3 v4 ->
         coe
           MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8852'y'8776'x_142
           v1 v2 v4 (coe v3 v4))
