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

module MAlonzo.Code.Relation.Binary.Properties.DecTotalOrder where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Construct.Converse
import qualified MAlonzo.Code.Relation.Binary.Construct.NonStrictToStrict
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.Properties.Poset
import qualified MAlonzo.Code.Relation.Binary.Properties.Preorder
import qualified MAlonzo.Code.Relation.Binary.Properties.TotalOrder
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties._<_
d__'60'__142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> ()
d__'60'__142 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties._≥_
d__'8805'__144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> ()
d__'8805'__144 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties._≰_
d__'8816'__146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> ()
d__'8816'__146 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.asym
d_asym_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_148 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.irrefl
d_irrefl_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_150 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_152 ~v0 ~v1 ~v2 v3
  = du_'60''45'isStrictPartialOrder_152 v3
du_'60''45'isStrictPartialOrder_152 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
du_'60''45'isStrictPartialOrder_152 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'isStrictPartialOrder_168
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.<-resp-≈
d_'60''45'resp'45''8776'_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8776'_154 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'45''8776'_154 v3
du_'60''45'resp'45''8776'_154 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'45''8776'_154 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_'60''45'resp'45''8776'_284
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'isStrictPartialOrder_168
         (coe v2))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.<-respʳ-≈
d_'60''45'resp'691''45''8776'_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'691''45''8776'_156 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'691''45''8776'_156 v3
du_'60''45'resp'691''45''8776'_156 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'691''45''8776'_156 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'strictPartialOrder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'691''45''8776'_304
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isStrictPartialOrder_494
         (coe v3))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.<-respˡ-≈
d_'60''45'resp'737''45''8776'_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'737''45''8776'_158 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'737''45''8776'_158 v3
du_'60''45'resp'737''45''8776'_158 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'737''45''8776'_158 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'strictPartialOrder_170
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'737''45''8776'_306
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isStrictPartialOrder_494
         (coe v3))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.<-strictPartialOrder
d_'60''45'strictPartialOrder_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_160 ~v0 ~v1 ~v2 v3
  = du_'60''45'strictPartialOrder_160 v3
du_'60''45'strictPartialOrder_160 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
du_'60''45'strictPartialOrder_160 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'strictPartialOrder_170
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.trans
d_trans_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_trans_162 ~v0 ~v1 ~v2 v3 = du_trans_162 v3
du_trans_162 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_trans_162 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_282
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'isStrictPartialOrder_168
         (coe v2))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.<⇒≉
d_'60''8658''8777'_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8777'_164 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.<⇒≱
d_'60''8658''8817'_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8817'_166 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≤⇒≯
d_'8804''8658''8815'_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8804''8658''8815'_170 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≤∧≉⇒<
d_'8804''8743''8777''8658''60'_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8804''8743''8777''8658''60'_172 ~v0 ~v1 ~v2 ~v3
  = du_'8804''8743''8777''8658''60'_172
du_'8804''8743''8777''8658''60'_172 ::
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8804''8743''8777''8658''60'_172 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'8804''8743''8777''8658''60'_196
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.antisym
d_antisym_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_174 ~v0 ~v1 ~v2 v3 = du_antisym_174 v3
du_antisym_174 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antisym_174 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    coe
      (\ v3 v4 v5 v6 ->
         coe
           MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
           (MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
              (coe v2))
           v3 v4 v6 v5)
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≥-isPartialOrder
d_'8805''45'isPartialOrder_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8805''45'isPartialOrder_176 ~v0 ~v1 ~v2 v3
  = du_'8805''45'isPartialOrder_176 v3
du_'8805''45'isPartialOrder_176 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_'8805''45'isPartialOrder_176 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'8805''45'isPartialOrder_134
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.converse-isPreorder
d_converse'45'isPreorder_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_converse'45'isPreorder_178 ~v0 ~v1 ~v2 v3
  = du_converse'45'isPreorder_178 v3
du_converse'45'isPreorder_178 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_converse'45'isPreorder_178 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v2))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≥-isTotalOrder
d_'8805''45'isTotalOrder_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_'8805''45'isTotalOrder_180 ~v0 ~v1 ~v2 v3
  = du_'8805''45'isTotalOrder_180 v3
du_'8805''45'isTotalOrder_180 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
du_'8805''45'isTotalOrder_180 v0
  = coe
      MAlonzo.Code.Relation.Binary.Properties.TotalOrder.du_'8805''45'isTotalOrder_190
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≥-poset
d_'8805''45'poset_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8805''45'poset_182 ~v0 ~v1 ~v2 v3 = du_'8805''45'poset_182 v3
du_'8805''45'poset_182 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_'8805''45'poset_182 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'8805''45'poset_136
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.converse-preorder
d_converse'45'preorder_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_converse'45'preorder_184 ~v0 ~v1 ~v2 v3
  = du_converse'45'preorder_184 v3
du_converse'45'preorder_184 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_converse'45'preorder_184 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'preorder_66
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v2))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.refl
d_refl_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny
d_refl_186 ~v0 ~v1 ~v2 v3 = du_refl_186 v3
du_refl_186 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny
du_refl_186 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    let v3
          = coe
              MAlonzo.Code.Relation.Binary.Properties.Poset.du_'8805''45'poset_136
              (coe v2) in
    let v4
          = MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v4))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.reflexive
d_reflexive_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_188 ~v0 ~v1 ~v2 v3 = du_reflexive_188 v3
du_reflexive_188 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_reflexive_188 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v2)))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.total
d_total_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_190 ~v0 ~v1 ~v2 v3 = du_total_190 v3
du_total_190 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_total_190 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_total_390
      (coe
         MAlonzo.Code.Relation.Binary.Properties.TotalOrder.du_'8805''45'isTotalOrder_190
         (coe v1))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≥-totalOrder
d_'8805''45'totalOrder_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_'8805''45'totalOrder_192 ~v0 ~v1 ~v2 v3
  = du_'8805''45'totalOrder_192 v3
du_'8805''45'totalOrder_192 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
du_'8805''45'totalOrder_192 v0
  = coe
      MAlonzo.Code.Relation.Binary.Properties.TotalOrder.du_'8805''45'totalOrder_192
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.trans
d_trans_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_194 ~v0 ~v1 ~v2 v3 = du_trans_194 v3
du_trans_194 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_194 v0
  = let v1
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0) in
    let v2
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v2)))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≰-respʳ-≈
d_'8816''45'resp'691''45''8776'_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8816''45'resp'691''45''8776'_196 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≰-respˡ-≈
d_'8816''45'resp'737''45''8776'_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8816''45'resp'737''45''8776'_198 = erased
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≰⇒>
d_'8816''8658''62'_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8816''8658''62'_200 ~v0 ~v1 ~v2 v3 = du_'8816''8658''62'_200 v3
du_'8816''8658''62'_200 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8816''8658''62'_200 v0
  = coe
      MAlonzo.Code.Relation.Binary.Properties.TotalOrder.du_'8816''8658''62'_202
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0))
-- Relation.Binary.Properties.DecTotalOrder.TotalOrderProperties.≰⇒≥
d_'8816''8658''8805'_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
d_'8816''8658''8805'_202 ~v0 ~v1 ~v2 v3
  = du_'8816''8658''8805'_202 v3
du_'8816''8658''8805'_202 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
du_'8816''8658''8805'_202 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Properties.TotalOrder.du_'8816''8658''8805'_208
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0))
      v1 v2
-- Relation.Binary.Properties.DecTotalOrder.≥-isDecTotalOrder
d_'8805''45'isDecTotalOrder_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_'8805''45'isDecTotalOrder_204 ~v0 ~v1 ~v2 v3
  = du_'8805''45'isDecTotalOrder_204 v3
du_'8805''45'isDecTotalOrder_204 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
du_'8805''45'isDecTotalOrder_204 v0
  = coe
      MAlonzo.Code.Relation.Binary.Construct.Converse.du_isDecTotalOrder_424
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
         (coe v0))
-- Relation.Binary.Properties.DecTotalOrder.≥-decTotalOrder
d_'8805''45'decTotalOrder_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_'8805''45'decTotalOrder_206 ~v0 ~v1 ~v2 v3
  = du_'8805''45'decTotalOrder_206 v3
du_'8805''45'decTotalOrder_206 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
du_'8805''45'decTotalOrder_206 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      (coe du_'8805''45'isDecTotalOrder_204 (coe v0))
-- Relation.Binary.Properties.DecTotalOrder._._≤?_
d__'8804''63'__210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__210 ~v0 ~v1 ~v2 v3 = du__'8804''63'__210 v3
du__'8804''63'__210 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8804''63'__210 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
      (coe du_'8805''45'isDecTotalOrder_204 (coe v0))
-- Relation.Binary.Properties.DecTotalOrder.<-isStrictTotalOrder
d_'60''45'isStrictTotalOrder_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_'60''45'isStrictTotalOrder_212 ~v0 ~v1 ~v2 v3
  = du_'60''45'isStrictTotalOrder_212 v3
du_'60''45'isStrictTotalOrder_212 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
du_'60''45'isStrictTotalOrder_212 v0
  = coe
      MAlonzo.Code.Relation.Binary.Construct.NonStrictToStrict.du_'60''45'isStrictTotalOrder'8322'_578
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
         (coe v0))
-- Relation.Binary.Properties.DecTotalOrder.<-strictTotalOrder
d_'60''45'strictTotalOrder_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_'60''45'strictTotalOrder_214 ~v0 ~v1 ~v2 v3
  = du_'60''45'strictTotalOrder_214 v3
du_'60''45'strictTotalOrder_214 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
du_'60''45'strictTotalOrder_214 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      (coe du_'60''45'isStrictTotalOrder_212 (coe v0))
-- Relation.Binary.Properties.DecTotalOrder._.compare
d_compare_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_compare_218 ~v0 ~v1 ~v2 v3 = du_compare_218 v3
du_compare_218 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
du_compare_218 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_compare_512
      (coe du_'60''45'isStrictTotalOrder_212 (coe v0))
-- Relation.Binary.Properties.DecTotalOrder.≮⇒≥
d_'8814''8658''8805'_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
d_'8814''8658''8805'_224 ~v0 ~v1 ~v2 v3 v4 v5
  = du_'8814''8658''8805'_224 v3 v4 v5
du_'8814''8658''8805'_224 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
du_'8814''8658''8805'_224 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Construct.NonStrictToStrict.du_'8814''8658''8805'_126
      (let v4
             = coe
                 MAlonzo.Code.Relation.Binary.Bundles.du_decPoset_820 (coe v0) in
       coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isDecPartialOrder_382
                     (coe v4))))))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d__'8799'__442
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
            (coe v0)))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
                     (coe v0))))))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_total_390
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
               (coe v0))))
      (coe v1) (coe v2)
