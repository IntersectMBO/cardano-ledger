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

module MAlonzo.Code.Relation.Binary.Properties.Poset where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Construct.NonStrictToStrict
import qualified MAlonzo.Code.Relation.Binary.Properties.Preorder
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Relation.Binary.Properties.Poset._.Eq._≉_
d__'8777'__50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> ()
d__'8777'__50 = erased
-- Relation.Binary.Properties.Poset.PreorderProperties.converse-isPreorder
d_converse'45'isPreorder_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_converse'45'isPreorder_120 ~v0 ~v1 ~v2 v3
  = du_converse'45'isPreorder_120 v3
du_converse'45'isPreorder_120 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_converse'45'isPreorder_120 v0
  = coe
      MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v0))
-- Relation.Binary.Properties.Poset.PreorderProperties.converse-preorder
d_converse'45'preorder_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_converse'45'preorder_122 ~v0 ~v1 ~v2 v3
  = du_converse'45'preorder_122 v3
du_converse'45'preorder_122 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_converse'45'preorder_122 v0
  = coe
      MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'preorder_66
      (coe MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v0))
-- Relation.Binary.Properties.Poset._≥_
d__'8805'__128 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> ()
d__'8805'__128 = erased
-- Relation.Binary.Properties.Poset.≥-isPartialOrder
d_'8805''45'isPartialOrder_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8805''45'isPartialOrder_134 ~v0 ~v1 ~v2 v3
  = du_'8805''45'isPartialOrder_134 v3
du_'8805''45'isPartialOrder_134 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_'8805''45'isPartialOrder_134 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v0)))
      (coe
         (\ v1 v2 v3 v4 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
              (MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
                 (coe v0))
              v1 v2 v4 v3))
-- Relation.Binary.Properties.Poset.≥-poset
d_'8805''45'poset_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8805''45'poset_136 ~v0 ~v1 ~v2 v3 = du_'8805''45'poset_136 v3
du_'8805''45'poset_136 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_'8805''45'poset_136 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      (coe du_'8805''45'isPartialOrder_134 (coe v0))
-- Relation.Binary.Properties.Poset._.antisym
d_antisym_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_140 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7
  = du_antisym_140 v3 v4 v5 v6 v7
du_antisym_140 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antisym_140 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
         (coe v0))
      v1 v2 v4 v3
-- Relation.Binary.Properties.Poset._.refl
d_refl_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny
d_refl_142 ~v0 ~v1 ~v2 v3 = du_refl_142 v3
du_refl_142 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny
du_refl_142 v0
  = let v1 = coe du_'8805''45'poset_136 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Properties.Poset._.reflexive
d_reflexive_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_144 ~v0 ~v1 ~v2 v3 = du_reflexive_144 v3
du_reflexive_144 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_reflexive_144 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v0)))
-- Relation.Binary.Properties.Poset._.trans
d_trans_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_146 ~v0 ~v1 ~v2 v3 = du_trans_146 v3
du_trans_146 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_146 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Preorder.du_converse'45'isPreorder_64
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v0)))
-- Relation.Binary.Properties.Poset._≰_
d__'8816'__148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> ()
d__'8816'__148 = erased
-- Relation.Binary.Properties.Poset.≰-respˡ-≈
d_'8816''45'resp'737''45''8776'_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8816''45'resp'737''45''8776'_154 = erased
-- Relation.Binary.Properties.Poset.≰-respʳ-≈
d_'8816''45'resp'691''45''8776'_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8816''45'resp'691''45''8776'_160 = erased
-- Relation.Binary.Properties.Poset._<_
d__'60'__166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> ()
d__'60'__166 = erased
-- Relation.Binary.Properties.Poset.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_168 ~v0 ~v1 ~v2 v3
  = du_'60''45'isStrictPartialOrder_168 v3
du_'60''45'isStrictPartialOrder_168 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
du_'60''45'isStrictPartialOrder_168 v0
  = coe
      MAlonzo.Code.Relation.Binary.Construct.NonStrictToStrict.du_'60''45'isStrictPartialOrder_438
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304 (coe v0))
-- Relation.Binary.Properties.Poset.<-strictPartialOrder
d_'60''45'strictPartialOrder_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_170 ~v0 ~v1 ~v2 v3
  = du_'60''45'strictPartialOrder_170 v3
du_'60''45'strictPartialOrder_170 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
du_'60''45'strictPartialOrder_170 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      (coe du_'60''45'isStrictPartialOrder_168 (coe v0))
-- Relation.Binary.Properties.Poset._.asym
d_asym_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_174 = erased
-- Relation.Binary.Properties.Poset._.irrefl
d_irrefl_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_176 = erased
-- Relation.Binary.Properties.Poset._.<-resp-≈
d_'60''45'resp'45''8776'_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8776'_178 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'45''8776'_178 v3
du_'60''45'resp'45''8776'_178 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'45''8776'_178 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_'60''45'resp'45''8776'_284
      (coe du_'60''45'isStrictPartialOrder_168 (coe v0))
-- Relation.Binary.Properties.Poset._.<-respʳ-≈
d_'60''45'resp'691''45''8776'_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'691''45''8776'_180 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'691''45''8776'_180 v3
du_'60''45'resp'691''45''8776'_180 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'691''45''8776'_180 v0
  = let v1 = coe du_'60''45'strictPartialOrder_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'691''45''8776'_304
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isStrictPartialOrder_494
         (coe v1))
-- Relation.Binary.Properties.Poset._.<-respˡ-≈
d_'60''45'resp'737''45''8776'_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'737''45''8776'_182 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'737''45''8776'_182 v3
du_'60''45'resp'737''45''8776'_182 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'737''45''8776'_182 v0
  = let v1 = coe du_'60''45'strictPartialOrder_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'737''45''8776'_306
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isStrictPartialOrder_494
         (coe v1))
-- Relation.Binary.Properties.Poset._.trans
d_trans_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_trans_184 ~v0 ~v1 ~v2 v3 = du_trans_184 v3
du_trans_184 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_trans_184 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_282
      (coe du_'60''45'isStrictPartialOrder_168 (coe v0))
-- Relation.Binary.Properties.Poset.<⇒≉
d_'60''8658''8777'_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8777'_190 = erased
-- Relation.Binary.Properties.Poset.≤∧≉⇒<
d_'8804''8743''8777''8658''60'_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8804''8743''8777''8658''60'_196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'8804''8743''8777''8658''60'_196
du_'8804''8743''8777''8658''60'_196 ::
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8804''8743''8777''8658''60'_196
  = coe
      MAlonzo.Code.Relation.Binary.Construct.NonStrictToStrict.du_'8804''8743''8777''8658''60'_44
-- Relation.Binary.Properties.Poset.<⇒≱
d_'60''8658''8817'_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''8658''8817'_202 = erased
-- Relation.Binary.Properties.Poset.≤⇒≯
d_'8804''8658''8815'_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8804''8658''8815'_208 = erased
-- Relation.Binary.Properties.Poset.≤-dec⇒≈-dec
d_'8804''45'dec'8658''8776''45'dec_210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_'8804''45'dec'8658''8776''45'dec_210 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_'8804''45'dec'8658''8776''45'dec_210 v3 v4 v5 v6
du_'8804''45'dec'8658''8776''45'dec_210 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_'8804''45'dec'8658''8776''45'dec_210 v0 v1 v2 v3
  = let v4 = coe v1 v2 v3 in
    let v5 = coe v1 v3 v2 in
    case coe v4 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v6 v7
        -> if coe v6
             then case coe v7 of
                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v8
                      -> case coe v5 of
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v9 v10
                             -> if coe v9
                                  then case coe v10 of
                                         MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v11
                                           -> coe
                                                MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                (coe v9)
                                                (coe
                                                   MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                                   (coe
                                                      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
                                                      (MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
                                                         (coe v0))
                                                      v2 v3 v8 v11))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  else coe
                                         seq (coe v10)
                                         (coe
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                            (coe v9)
                                            (coe
                                               MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             else coe
                    seq (coe v7)
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                       (coe v6)
                       (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Properties.Poset.≤-dec⇒isDecPartialOrder
d_'8804''45'dec'8658'isDecPartialOrder_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
d_'8804''45'dec'8658'isDecPartialOrder_254 ~v0 ~v1 ~v2 v3 v4
  = du_'8804''45'dec'8658'isDecPartialOrder_254 v3 v4
du_'8804''45'dec'8658'isDecPartialOrder_254 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
du_'8804''45'dec'8658'isDecPartialOrder_254 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecPartialOrder'46'constructor_10175
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304 (coe v0))
      (coe du_'8804''45'dec'8658''8776''45'dec_210 (coe v0) (coe v1))
      (coe v1)
-- Relation.Binary.Properties.Poset.mono⇒cong
d_mono'8658'cong_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_mono'8658'cong_260 ~v0 ~v1 ~v2 v3 v4
  = du_mono'8658'cong_260 v3 v4
du_mono'8658'cong_260 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_mono'8658'cong_260 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_mono'8658'cong_276
      (let v2
             = coe
                 MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v0) in
       coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v2))))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
               (coe v0))))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
            (coe v0)))
      (coe v1)
-- Relation.Binary.Properties.Poset.antimono⇒cong
d_antimono'8658'cong_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antimono'8658'cong_264 ~v0 ~v1 ~v2 v3 v4
  = du_antimono'8658'cong_264 v3 v4
du_antimono'8658'cong_264 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antimono'8658'cong_264 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_antimono'8658'cong_290
      (let v2
             = coe
                 MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v0) in
       coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v2))))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
               (coe v0))))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
            (coe v0)))
      (coe v1)
