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

module MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Construct.Trivial where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles
import qualified MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Relation.Binary.Indexed.Heterogeneous.Construct.Trivial._.Aᵢ
d_A'7522'_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> ()
d_A'7522'_24 = erased
-- Relation.Binary.Indexed.Heterogeneous.Construct.Trivial._.isIndexedEquivalence
d_isIndexedEquivalence_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.T_IsIndexedEquivalence_22
d_isIndexedEquivalence_32 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isIndexedEquivalence_32 v6
du_isIndexedEquivalence_32 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.T_IsIndexedEquivalence_22
du_isIndexedEquivalence_32 v0
  = coe
      MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.C_IsIndexedEquivalence'46'constructor_1089
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Binary.Structures.d_refl_34 (coe v0)))
      (coe
         (\ v1 v2 ->
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36 (coe v0)))
      (coe
         (\ v1 v2 v3 ->
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38 (coe v0)))
-- Relation.Binary.Indexed.Heterogeneous.Construct.Trivial._.isIndexedPreorder
d_isIndexedPreorder_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.T_IsIndexedPreorder_44
d_isIndexedPreorder_60 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isIndexedPreorder_60 v8
du_isIndexedPreorder_60 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.T_IsIndexedPreorder_44
du_isIndexedPreorder_60 v0
  = coe
      MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.C_IsIndexedPreorder'46'constructor_4779
      (coe
         du_isIndexedEquivalence_32
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe v0)))
      (coe
         (\ v1 v2 ->
            MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82 (coe v0)))
      (coe
         (\ v1 v2 v3 ->
            MAlonzo.Code.Relation.Binary.Structures.d_trans_84 (coe v0)))
-- Relation.Binary.Indexed.Heterogeneous.Construct.Trivial.indexedSetoid
d_indexedSetoid_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedSetoid_18
d_indexedSetoid_100 ~v0 ~v1 ~v2 ~v3 v4 = du_indexedSetoid_100 v4
du_indexedSetoid_100 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedSetoid_18
du_indexedSetoid_100 v0
  = coe
      MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.C_IndexedSetoid'46'constructor_441
      (coe
         du_isIndexedEquivalence_32
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
-- Relation.Binary.Indexed.Heterogeneous.Construct.Trivial.indexedPreorder
d_indexedPreorder_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedPreorder_60
d_indexedPreorder_136 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_indexedPreorder_136 v5
du_indexedPreorder_136 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedPreorder_60
du_indexedPreorder_136 v0
  = coe
      MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.C_IndexedPreorder'46'constructor_1971
      (coe
         du_isIndexedPreorder_60
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0)))
