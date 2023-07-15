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

module MAlonzo.Code.Relation.Binary.Properties.Preorder where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Construct.Converse
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Relation.Binary.Properties.Preorder.converse-isPreorder
d_converse'45'isPreorder_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_converse'45'isPreorder_64 ~v0 ~v1 ~v2 v3
  = du_converse'45'isPreorder_64 v3
du_converse'45'isPreorder_64 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_converse'45'isPreorder_64 v0
  = coe
      MAlonzo.Code.Relation.Binary.Construct.Converse.du_isPreorder_256
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0))
-- Relation.Binary.Properties.Preorder.converse-preorder
d_converse'45'preorder_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_converse'45'preorder_66 ~v0 ~v1 ~v2 v3
  = du_converse'45'preorder_66 v3
du_converse'45'preorder_66 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_converse'45'preorder_66 v0
  = coe
      MAlonzo.Code.Relation.Binary.Construct.Converse.du_preorder_642
      (coe v0)
-- Relation.Binary.Properties.Preorder.InducedEquivalence
d_InducedEquivalence_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_InducedEquivalence_68 ~v0 ~v1 ~v2 v3
  = du_InducedEquivalence_68 v3
du_InducedEquivalence_68 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_InducedEquivalence_68 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         (coe
            (\ v1 ->
               coe
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                 (coe
                    MAlonzo.Code.Relation.Binary.Structures.du_refl_98
                    (coe
                       MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0))
                    (coe v1))
                 (coe
                    MAlonzo.Code.Relation.Binary.Structures.du_refl_98
                    (coe
                       MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0))
                    (coe v1))))
         (coe (\ v1 v2 -> coe MAlonzo.Code.Data.Product.Base.du_swap_346))
         (coe
            (\ v1 v2 v3 ->
               coe
                 MAlonzo.Code.Data.Product.Base.du_zip_174
                 (coe
                    MAlonzo.Code.Relation.Binary.Structures.d_trans_84
                    (MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0)) v1
                    v2 v3)
                 (coe
                    (\ v4 v5 v6 v7 ->
                       coe
                         MAlonzo.Code.Relation.Binary.Structures.d_trans_84
                         (MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0)) v3
                         v2 v1 v7 v6)))))
-- Relation.Binary.Properties.Preorder.invIsPreorder
d_invIsPreorder_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_invIsPreorder_74 ~v0 ~v1 ~v2 v3 = du_invIsPreorder_74 v3
du_invIsPreorder_74 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_invIsPreorder_74 v0 = coe du_converse'45'isPreorder_64 (coe v0)
-- Relation.Binary.Properties.Preorder.invPreorder
d_invPreorder_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_invPreorder_76 ~v0 ~v1 ~v2 v3 = du_invPreorder_76 v3
du_invPreorder_76 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_invPreorder_76 v0 = coe du_converse'45'preorder_66 (coe v0)
