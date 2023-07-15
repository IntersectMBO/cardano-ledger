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

module MAlonzo.Code.Algebra.Construct.NaturalChoice.Min where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.Base
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Construct.NaturalChoice.Min._.totalPreorder
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
-- Algebra.Construct.NaturalChoice.Min._⊓_
d__'8851'__80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8851'__80 ~v0 ~v1 ~v2 v3 v4 v5 = du__'8851'__80 v3 v4 v5
du__'8851'__80 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8851'__80 v0 v1 v2
  = let v3
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_390
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0))
              v1 v2 in
    case coe v3 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v4 -> coe v1
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.Min.x≤y⇒x⊓y≈x
d_x'8804'y'8658'x'8851'y'8776'x_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'x'8851'y'8776'x_106 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_x'8804'y'8658'x'8851'y'8776'x_106 v3 v4 v5 v6
du_x'8804'y'8658'x'8851'y'8776'x_106 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'x'8851'y'8776'x_106 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_390
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0))
              v1 v2 in
    case coe v4 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5
        -> let v6
                 = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
           let v7
                 = coe
                     MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v6) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
             (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                (coe
                   MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v7)))
             v1
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
             (MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                (coe
                   MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0)))
             v2 v1 v5 v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.Min.x≤y⇒y⊓x≈x
d_x'8804'y'8658'y'8851'x'8776'x_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'y'8851'x'8776'x_136 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_x'8804'y'8658'y'8851'x'8776'x_136 v3 v4 v5 v6
du_x'8804'y'8658'y'8851'x'8776'x_136 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'y'8851'x'8776'x_136 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_390
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0))
              v2 v1 in
    case coe v4 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
             (MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                (coe
                   MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0)))
             v2 v1 v5 v3
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5
        -> let v6
                 = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
           let v7
                 = coe
                     MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v6) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
             (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                (coe
                   MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v7)))
             v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.Min.minOperator
d_minOperator_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84
d_minOperator_162 ~v0 ~v1 ~v2 v3 = du_minOperator_162 v3
du_minOperator_162 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84
du_minOperator_162 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.C_MinOperator'46'constructor_973
      (coe du__'8851'__80 (coe v0))
      (coe du_x'8804'y'8658'x'8851'y'8776'x_106 (coe v0))
      (coe
         (\ v1 v2 ->
            coe
              du_x'8804'y'8658'y'8851'x'8776'x_136 (coe v0) (coe v2) (coe v1)))
-- Algebra.Construct.NaturalChoice.Min._.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
d_mono'45''8804''45'distrib'45''8851'_166 ~v0 ~v1 ~v2 v3
  = du_mono'45''8804''45'distrib'45''8851'_166 v3
du_mono'45''8804''45'distrib'45''8851'_166 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
du_mono'45''8804''45'distrib'45''8851'_166 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_mono'45''8804''45'distrib'45''8851'_2862
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'x'8851'z'8804'y_168 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8658'x'8851'z'8804'y_168 v3
du_x'8804'y'8658'x'8851'z'8804'y_168 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'x'8851'z'8804'y_168 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'x'8851'z'8804'y_2908
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'z'8851'x'8804'y_170 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8658'z'8851'x'8804'y_170 v3
du_x'8804'y'8658'z'8851'x'8804'y_170 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'z'8851'x'8804'y_170 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8658'z'8851'x'8804'y_2920
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8851'z'8658'x'8804'y_172 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8851'z'8658'x'8804'y_172 v3
du_x'8804'y'8851'z'8658'x'8804'y_172 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8851'z'8658'x'8804'y_172 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'y_2932
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8851'z'8658'x'8804'z_174 ~v0 ~v1 ~v2 v3
  = du_x'8804'y'8851'z'8658'x'8804'z_174 v3
du_x'8804'y'8851'z'8658'x'8804'z_174 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8851'z'8658'x'8804'z_174 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8804'y'8851'z'8658'x'8804'z_2946
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8776'x'8658'x'8804'y_176 ~v0 ~v1 ~v2 v3
  = du_x'8851'y'8776'x'8658'x'8804'y_176 v3
du_x'8851'y'8776'x'8658'x'8804'y_176 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8776'x'8658'x'8804'y_176 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'x'8658'x'8804'y_2816
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8776'y'8658'y'8804'x_178 ~v0 ~v1 ~v2 v3
  = du_x'8851'y'8776'y'8658'y'8804'x_178 v3
du_x'8851'y'8776'y'8658'y'8804'x_178 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8776'y'8658'y'8804'x_178 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8776'y'8658'y'8804'x_2848
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.x⊓y≤x
d_x'8851'y'8804'x_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8804'x_180 ~v0 ~v1 ~v2 v3 = du_x'8851'y'8804'x_180 v3
du_x'8851'y'8804'x_180 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8804'x_180 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'x_2556
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.x⊓y≤y
d_x'8851'y'8804'y_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8804'y_182 ~v0 ~v1 ~v2 v3 = du_x'8851'y'8804'y_182 v3
du_x'8851'y'8804'y_182 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8804'y_182 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_x'8851'y'8804'y_2582
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-assoc
d_'8851''45'assoc_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'assoc_184 ~v0 ~v1 ~v2 v3 = du_'8851''45'assoc_184 v3
du_'8851''45'assoc_184 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'assoc_184 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'assoc_2692
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-band
d_'8851''45'band_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_186 ~v0 ~v1 ~v2 v3 = du_'8851''45'band_186 v3
du_'8851''45'band_186 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
du_'8851''45'band_186 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'band_2800
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-comm
d_'8851''45'comm_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'comm_188 ~v0 ~v1 ~v2 v3 = du_'8851''45'comm_188 v3
du_'8851''45'comm_188 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'comm_188 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'comm_2604
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_190 ~v0 ~v1 ~v2 v3
  = du_'8851''45'commutativeSemigroup_190 v3
du_'8851''45'commutativeSemigroup_190 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
du_'8851''45'commutativeSemigroup_190 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'commutativeSemigroup_2802
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-cong
d_'8851''45'cong_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'cong_192 ~v0 ~v1 ~v2 v3 = du_'8851''45'cong_192 v3
du_'8851''45'cong_192 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'cong_192 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong_2678
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-congʳ
d_'8851''45'cong'691'_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'cong'691'_194 ~v0 ~v1 ~v2 v3
  = du_'8851''45'cong'691'_194 v3
du_'8851''45'cong'691'_194 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'cong'691'_194 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong'691'_2668
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-congˡ
d_'8851''45'cong'737'_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'cong'737'_196 ~v0 ~v1 ~v2 v3
  = du_'8851''45'cong'737'_196 v3
du_'8851''45'cong'737'_196 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'cong'737'_196 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'cong'737'_2630
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-glb
d_'8851''45'glb_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'glb_198 ~v0 ~v1 ~v2 v3 = du_'8851''45'glb_198 v3
du_'8851''45'glb_198 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'glb_198 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'glb_3026
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-idem
d_'8851''45'idem_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny
d_'8851''45'idem_200 ~v0 ~v1 ~v2 v3 = du_'8851''45'idem_200 v3
du_'8851''45'idem_200 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny
du_'8851''45'idem_200 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'idem_2732
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-identity
d_'8851''45'identity_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'identity_202 ~v0 ~v1 ~v2 v3 v4 v5
  = du_'8851''45'identity_202 v3 v4 v5
du_'8851''45'identity_202 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8851''45'identity_202 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         (\ v3 ->
            coe
              du_x'8804'y'8658'y'8851'x'8776'x_136 (coe v0) (coe v3) (coe v1)
              (coe v2 v3)))
      (coe
         (\ v3 ->
            coe
              du_x'8804'y'8658'x'8851'y'8776'x_106 (coe v0) (coe v3) (coe v1)
              (coe v2 v3)))
-- Algebra.Construct.NaturalChoice.Min._.⊓-identityʳ
d_'8851''45'identity'691'_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'identity'691'_204 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_'8851''45'identity'691'_204 v3 v4 v5 v6
du_'8851''45'identity'691'_204 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8851''45'identity'691'_204 v0 v1 v2 v3
  = coe
      du_x'8804'y'8658'x'8851'y'8776'x_106 (coe v0) (coe v3) (coe v1)
      (coe v2 v3)
-- Algebra.Construct.NaturalChoice.Min._.⊓-identityˡ
d_'8851''45'identity'737'_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'identity'737'_206 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_'8851''45'identity'737'_206 v3 v4 v5 v6
du_'8851''45'identity'737'_206 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8851''45'identity'737'_206 v0 v1 v2 v3
  = coe
      du_x'8804'y'8658'y'8851'x'8776'x_136 (coe v0) (coe v3) (coe v1)
      (coe v2 v3)
-- Algebra.Construct.NaturalChoice.Min._.⊓-isBand
d_'8851''45'isBand_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_208 ~v0 ~v1 ~v2 v3 = du_'8851''45'isBand_208 v3
du_'8851''45'isBand_208 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_'8851''45'isBand_208 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isBand_2782
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_210 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isCommutativeSemigroup_210 v3
du_'8851''45'isCommutativeSemigroup_210 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'8851''45'isCommutativeSemigroup_210 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isCommutativeSemigroup_2784
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-isMagma
d_'8851''45'isMagma_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_212 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isMagma_212 v3
du_'8851''45'isMagma_212 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8851''45'isMagma_212 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMagma_2778
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-isMonoid
d_'8851''45'isMonoid_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8851''45'isMonoid_214 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isMonoid_214 v3
du_'8851''45'isMonoid_214 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'8851''45'isMonoid_214 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isMonoid_2790
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_216 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isSelectiveMagma_216 v3
du_'8851''45'isSelectiveMagma_216 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
du_'8851''45'isSelectiveMagma_216 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSelectiveMagma_2786
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-isSemigroup
d_'8851''45'isSemigroup_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_218 ~v0 ~v1 ~v2 v3
  = du_'8851''45'isSemigroup_218 v3
du_'8851''45'isSemigroup_218 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8851''45'isSemigroup_218 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'isSemigroup_2780
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-magma
d_'8851''45'magma_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_220 ~v0 ~v1 ~v2 v3 = du_'8851''45'magma_220 v3
du_'8851''45'magma_220 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_'8851''45'magma_220 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'magma_2796
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-mono-≤
d_'8851''45'mono'45''8804'_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'mono'45''8804'_222 ~v0 ~v1 ~v2 v3
  = du_'8851''45'mono'45''8804'_222 v3
du_'8851''45'mono'45''8804'_222 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'mono'45''8804'_222 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'45''8804'_2954
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-monoid
d_'8851''45'monoid_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'8851''45'monoid_224 ~v0 ~v1 ~v2 v3 = du_'8851''45'monoid_224 v3
du_'8851''45'monoid_224 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'8851''45'monoid_224 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'monoid_2808
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'mono'691''45''8804'_226 ~v0 ~v1 ~v2 v3
  = du_'8851''45'mono'691''45''8804'_226 v3
du_'8851''45'mono'691''45''8804'_226 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'mono'691''45''8804'_226 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'691''45''8804'_3014
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'mono'737''45''8804'_228 ~v0 ~v1 ~v2 v3
  = du_'8851''45'mono'737''45''8804'_228 v3
du_'8851''45'mono'737''45''8804'_228 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'mono'737''45''8804'_228 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'mono'737''45''8804'_3004
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-rawMagma
d_'8851''45'rawMagma_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8851''45'rawMagma_230 ~v0 ~v1 ~v2 v3
  = du_'8851''45'rawMagma_230 v3
du_'8851''45'rawMagma_230 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8851''45'rawMagma_230 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'rawMagma_2794
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-sel
d_'8851''45'sel_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_232 ~v0 ~v1 ~v2 v3 = du_'8851''45'sel_232 v3
du_'8851''45'sel_232 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_'8851''45'sel_232 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'sel_2736
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-selectiveMagma
d_'8851''45'selectiveMagma_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_234 ~v0 ~v1 ~v2 v3
  = du_'8851''45'selectiveMagma_234 v3
du_'8851''45'selectiveMagma_234 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
du_'8851''45'selectiveMagma_234 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'selectiveMagma_2804
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-semigroup
d_'8851''45'semigroup_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_236 ~v0 ~v1 ~v2 v3
  = du_'8851''45'semigroup_236 v3
du_'8851''45'semigroup_236 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'8851''45'semigroup_236 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'semigroup_2798
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-triangulate
d_'8851''45'triangulate_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'triangulate_238 ~v0 ~v1 ~v2 v3
  = du_'8851''45'triangulate_238 v3
du_'8851''45'triangulate_238 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'triangulate_238 v0
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp.du_'8851''45'triangulate_3040
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalPreorder_728 (coe v0))
      (coe du_minOperator_162 (coe v0))
-- Algebra.Construct.NaturalChoice.Min._.⊓-zero
d_'8851''45'zero_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'zero_240 ~v0 ~v1 ~v2 v3 v4 v5
  = du_'8851''45'zero_240 v3 v4 v5
du_'8851''45'zero_240 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8851''45'zero_240 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         (\ v3 ->
            coe
              du_x'8804'y'8658'x'8851'y'8776'x_106 (coe v0) (coe v1) (coe v3)
              (coe v2 v3)))
      (coe
         (\ v3 ->
            coe
              du_x'8804'y'8658'y'8851'x'8776'x_136 (coe v0) (coe v1) (coe v3)
              (coe v2 v3)))
-- Algebra.Construct.NaturalChoice.Min._.⊓-zeroʳ
d_'8851''45'zero'691'_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'zero'691'_242 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_'8851''45'zero'691'_242 v3 v4 v5 v6
du_'8851''45'zero'691'_242 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8851''45'zero'691'_242 v0 v1 v2 v3
  = coe
      du_x'8804'y'8658'y'8851'x'8776'x_136 (coe v0) (coe v1) (coe v3)
      (coe v2 v3)
-- Algebra.Construct.NaturalChoice.Min._.⊓-zeroˡ
d_'8851''45'zero'737'_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'zero'737'_244 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_'8851''45'zero'737'_244 v3 v4 v5 v6
du_'8851''45'zero'737'_244 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_'8851''45'zero'737'_244 v0 v1 v2 v3
  = coe
      du_x'8804'y'8658'x'8851'y'8776'x_106 (coe v0) (coe v1) (coe v3)
      (coe v2 v3)
