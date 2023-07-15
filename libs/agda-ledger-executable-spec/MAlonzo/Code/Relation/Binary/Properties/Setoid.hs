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

module MAlonzo.Code.Relation.Binary.Properties.Setoid where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Relation.Binary.Properties.Setoid._._≉_
d__'8777'__18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> AgdaAny -> ()
d__'8777'__18 = erased
-- Relation.Binary.Properties.Setoid.isPreorder
d_isPreorder_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_36 ~v0 ~v1 v2 = du_isPreorder_36 v2
du_isPreorder_36 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_isPreorder_36 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         erased erased erased)
      (\ v1 v2 v3 ->
         coe
           MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
           (coe
              MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
           v1)
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
-- Relation.Binary.Properties.Setoid.≈-isPreorder
d_'8776''45'isPreorder_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8776''45'isPreorder_38 ~v0 ~v1 v2
  = du_'8776''45'isPreorder_38 v2
du_'8776''45'isPreorder_38 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_'8776''45'isPreorder_38 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
      (coe (\ v1 v2 v3 -> v3))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
-- Relation.Binary.Properties.Setoid.≈-isPartialOrder
d_'8776''45'isPartialOrder_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8776''45'isPartialOrder_40 ~v0 ~v1 v2
  = du_'8776''45'isPartialOrder_40 v2
du_'8776''45'isPartialOrder_40 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_'8776''45'isPartialOrder_40 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe du_'8776''45'isPreorder_38 (coe v0))
      (coe (\ v1 v2 v3 v4 -> v3))
-- Relation.Binary.Properties.Setoid.preorder
d_preorder_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_46 ~v0 ~v1 v2 = du_preorder_46 v2
du_preorder_46 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_46 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe du_isPreorder_36 (coe v0))
-- Relation.Binary.Properties.Setoid.≈-preorder
d_'8776''45'preorder_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8776''45'preorder_48 ~v0 ~v1 v2 = du_'8776''45'preorder_48 v2
du_'8776''45'preorder_48 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_'8776''45'preorder_48 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe du_'8776''45'isPreorder_38 (coe v0))
-- Relation.Binary.Properties.Setoid.≈-poset
d_'8776''45'poset_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8776''45'poset_50 ~v0 ~v1 v2 = du_'8776''45'poset_50 v2
du_'8776''45'poset_50 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_'8776''45'poset_50 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      (coe du_'8776''45'isPartialOrder_40 (coe v0))
-- Relation.Binary.Properties.Setoid.≉-sym
d_'8777''45'sym_52 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8777''45'sym_52 = erased
-- Relation.Binary.Properties.Setoid.≉-respˡ
d_'8777''45'resp'737'_56 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8777''45'resp'737'_56 = erased
-- Relation.Binary.Properties.Setoid.≉-respʳ
d_'8777''45'resp'691'_62 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8777''45'resp'691'_62 = erased
-- Relation.Binary.Properties.Setoid.≉-resp₂
d_'8777''45'resp'8322'_70 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8777''45'resp'8322'_70 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         (\ v1 v2 v3 v4 v5 v6 ->
            coe
              v5
              (coe
                 MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                 (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                 v1 v3 v2 v6
                 (coe
                    MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                    (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                    v2 v3 v4))))
      (coe
         (\ v1 v2 v3 v4 v5 v6 ->
            coe
              v5
              (coe
                 MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                 (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                 v2 v3 v1 v4 v6)))
-- Relation.Binary.Properties.Setoid.respʳ-flip
d_resp'691''45'flip_72 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_resp'691''45'flip_72 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
      v1 v2 v3 v5
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
         v3 v2 v4)
-- Relation.Binary.Properties.Setoid.respˡ-flip
d_resp'737''45'flip_78 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_resp'737''45'flip_78 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
      v3 v2 v1
