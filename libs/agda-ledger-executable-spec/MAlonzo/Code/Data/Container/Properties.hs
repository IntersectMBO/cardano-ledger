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

module MAlonzo.Code.Data.Container.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Container.Core
import qualified MAlonzo.Code.Data.Container.Relation.Binary.Equality.Setoid
import qualified MAlonzo.Code.Data.Container.Relation.Binary.Pointwise
import qualified MAlonzo.Code.Relation.Binary.Bundles

-- Data.Container.Properties._.map-identity
d_map'45'identity_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
d_map'45'identity_24 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_map'45'identity_24 v5 v6
du_map'45'identity_24 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
du_map'45'identity_24 v0 v1
  = coe
      MAlonzo.Code.Data.Container.Relation.Binary.Equality.Setoid.du_refl_52
      (coe v0)
      (coe MAlonzo.Code.Data.Container.Core.du_map_56 (\ v2 -> v2) v1)
-- Data.Container.Properties._.map-compose
d_map'45'compose_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
d_map'45'compose_50 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10 v11
                    v12
  = du_map'45'compose_50 v9 v10 v11 v12
du_map'45'compose_50 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
du_map'45'compose_50 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Container.Relation.Binary.Equality.Setoid.du_refl_52
      (coe v0)
      (coe
         MAlonzo.Code.Data.Container.Core.du_map_56 v1
         (coe MAlonzo.Code.Data.Container.Core.du_map_56 v2 v3))
