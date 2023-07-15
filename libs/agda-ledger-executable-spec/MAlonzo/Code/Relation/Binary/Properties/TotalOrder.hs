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

module MAlonzo.Code.Relation.Binary.Properties.TotalOrder where

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
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Construct.Converse
import qualified MAlonzo.Code.Relation.Binary.Construct.NonStrictToStrict
import qualified MAlonzo.Code.Relation.Binary.Properties.Poset
import qualified MAlonzo.Code.Relation.Binary.Properties.Preorder
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Binary.Properties.TotalOrder.PosetProperties._<_
d__'60'__128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> ()
d__'60'__128 = erased
-- Relation.Binary.Properties.TotalOrder.PosetProperties._≥_
d__'8805'__130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> ()
d__'8805'__130 = erased
-- Relation.Binary.Properties.TotalOrder.PosetProperties._≰_
d__'8816'__132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> ()
d__'8816'__132 = erased
-- Relation.Binary.Properties.TotalOrder.PosetProperties.asym
d_asym_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_134 = erased
-- Relation.Binary.Properties.TotalOrder.PosetProperties.irrefl
d_irrefl_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_136 = erased
-- Relation.Binary.Properties.TotalOrder.PosetProperties.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_138 ~v0 ~v1 ~v2 v3
  = du_'60''45'isStrictPartialOrder_138 v3
du_'60''45'isStrictPartialOrder_138 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
du_'60''45'isStrictPartialOrder_138 v0
  = coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'isStrictPartialOrder_168
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.<-resp-≈
d_'60''45'resp'45''8776'_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8776'_140 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'45''8776'_140 v3
du_'60''45'resp'45''8776'_140 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'45''8776'_140 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_'60''45'resp'45''8776'_284
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'isStrictPartialOrder_168
         (coe v1))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.<-respʳ-≈
d_'60''45'resp'691''45''8776'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'691''45''8776'_142 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'691''45''8776'_142 v3
du_'60''45'resp'691''45''8776'_142 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'691''45''8776'_142 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'strictPartialOrder_170
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'691''45''8776'_304
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isStrictPartialOrder_494
         (coe v2))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.<-respˡ-≈
d_'60''45'resp'737''45''8776'_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'737''45''8776'_144 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'737''45''8776'_144 v3
du_'60''45'resp'737''45''8776'_144 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'737''45''8776'_144 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'strictPartialOrder_170
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'737''45''8776'_306
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isStrictPartialOrder_494
         (coe v2))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.<-strictPartialOrder
d_'60''45'strictPartialOrder_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_146 ~v0 ~v1 ~v2 v3
  = du_'60''45'strictPartialOrder_146 v3
du_'60''45'strictPartialOrder_146 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
du_'60''45'strictPartialOrder_146 v0
  = coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'strictPartialOrder_170
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.trans
d_trans_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_trans_148 ~v0 ~v1 ~v2 v3 = du_trans_148 v3
du_trans_148 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_trans_148 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_282
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Poset.du_'60''45'isStrictPartialOrder_168
         (coe v1))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.<⇒≉
d_'60''8658''8777'_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8777'_150 = erased
-- Relation.Binary.Properties.TotalOrder.PosetProperties.<⇒≱
d_'60''8658''8817'_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8817'_152 = erased
-- Relation.Binary.Properties.TotalOrder.PosetProperties.≤⇒≯
d_'8804''8658''8815'_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8804''8658''8815'_162 = erased
-- Relation.Binary.Properties.TotalOrder.PosetProperties.≤∧≉⇒<
d_'8804''8743''8777''8658''60'_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8804''8743''8777''8658''60'_164 ~v0 ~v1 ~v2 ~v3
  = du_'8804''8743''8777''8658''60'_164
du_'8804''8743''8777''8658''60'_164 ::
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8804''8743''8777''8658''60'_164 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'8804''8743''8777''8658''60'_196
-- Relation.Binary.Properties.TotalOrder.PosetProperties.antisym
d_antisym_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_166 ~v0 ~v1 ~v2 v3 = du_antisym_166 v3
du_antisym_166 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antisym_166 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    coe
      (\ v2 v3 v4 v5 ->
         coe
           MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
           (MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
              (coe v1))
           v2 v3 v5 v4)
-- Relation.Binary.Properties.TotalOrder.PosetProperties.≥-isPartialOrder
d_'8805''45'isPartialOrder_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8805''45'isPartialOrder_168 ~v0 ~v1 ~v2 v3
  = du_'8805''45'isPartialOrder_168 v3
du_'8805''45'isPartialOrder_168 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_'8805''45'isPartialOrder_168 v0
  = coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'8805''45'isPartialOrder_134
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.converse-isPreorder
d_converse'45'isPreorder_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_converse'45'isPreorder_170 ~v0 ~v1 ~v2 v3
  = du_converse'45'isPreorder_170 v3
du_converse'45'isPreorder_170 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_converse'45'isPreorder_170 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v1))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.≥-poset
d_'8805''45'poset_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8805''45'poset_172 ~v0 ~v1 ~v2 v3 = du_'8805''45'poset_172 v3
du_'8805''45'poset_172 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_'8805''45'poset_172 v0
  = coe
      MAlonzo.Code.Relation.Binary.Properties.Poset.du_'8805''45'poset_136
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.converse-preorder
d_converse'45'preorder_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_converse'45'preorder_174 ~v0 ~v1 ~v2 v3
  = du_converse'45'preorder_174 v3
du_converse'45'preorder_174 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_converse'45'preorder_174 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'preorder_66
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v1))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.refl
d_refl_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny
d_refl_176 ~v0 ~v1 ~v2 v3 = du_refl_176 v3
du_refl_176 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny
du_refl_176 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Relation.Binary.Properties.Poset.du_'8805''45'poset_136
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.reflexive
d_reflexive_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_178 ~v0 ~v1 ~v2 v3 = du_reflexive_178 v3
du_reflexive_178 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_reflexive_178 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v1)))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.trans
d_trans_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_180 ~v0 ~v1 ~v2 v3 = du_trans_180 v3
du_trans_180 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_180 v0
  = let v1
          = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v1)))
-- Relation.Binary.Properties.TotalOrder.PosetProperties.≰-respʳ-≈
d_'8816''45'resp'691''45''8776'_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8816''45'resp'691''45''8776'_182 = erased
-- Relation.Binary.Properties.TotalOrder.PosetProperties.≰-respˡ-≈
d_'8816''45'resp'737''45''8776'_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8816''45'resp'737''45''8776'_184 = erased
-- Relation.Binary.Properties.TotalOrder.decTotalOrder
d_decTotalOrder_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_decTotalOrder_186 ~v0 ~v1 ~v2 v3 v4 = du_decTotalOrder_186 v3 v4
du_decTotalOrder_186 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
du_decTotalOrder_186 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0))
         (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Consequences.du_total'8743'dec'8658'dec_204
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                        (coe v0)))))
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_total_390
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0)))
            (coe v1)))
-- Relation.Binary.Properties.TotalOrder.≥-isTotalOrder
d_'8805''45'isTotalOrder_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_'8805''45'isTotalOrder_190 ~v0 ~v1 ~v2 v3
  = du_'8805''45'isTotalOrder_190 v3
du_'8805''45'isTotalOrder_190 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
du_'8805''45'isTotalOrder_190 v0
  = coe
      MAlonzo.Code.Relation.Binary.Construct.Converse.du_isTotalOrder_378
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0))
-- Relation.Binary.Properties.TotalOrder.≥-totalOrder
d_'8805''45'totalOrder_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_'8805''45'totalOrder_192 ~v0 ~v1 ~v2 v3
  = du_'8805''45'totalOrder_192 v3
du_'8805''45'totalOrder_192 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
du_'8805''45'totalOrder_192 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalOrder'46'constructor_12355
      (coe du_'8805''45'isTotalOrder_190 (coe v0))
-- Relation.Binary.Properties.TotalOrder._.total
d_total_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_196 ~v0 ~v1 ~v2 v3 = du_total_196 v3
du_total_196 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_total_196 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_total_390
      (coe du_'8805''45'isTotalOrder_190 (coe v0))
-- Relation.Binary.Properties.TotalOrder.≰⇒>
d_'8816''8658''62'_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8816''8658''62'_202 ~v0 ~v1 ~v2 v3 v4 v5
  = du_'8816''8658''62'_202 v3 v4 v5
du_'8816''8658''62'_202 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8816''8658''62'_202 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Construct.NonStrictToStrict.du_'8816''8658''62'_76
      (let v3
             = coe MAlonzo.Code.Relation.Binary.Bundles.du_poset_698 (coe v0) in
       let v4
             = coe
                 MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v3) in
       coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v4))))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                  (coe v0)))))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_total_390
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0)))
      (coe v1) (coe v2)
-- Relation.Binary.Properties.TotalOrder.≰⇒≥
d_'8816''8658''8805'_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
d_'8816''8658''8805'_208 ~v0 ~v1 ~v2 v3 v4 v5 ~v6
  = du_'8816''8658''8805'_208 v3 v4 v5
du_'8816''8658''8805'_208 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8816''8658''8805'_208 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe du_'8816''8658''62'_202 v0 v1 v2 erased)
