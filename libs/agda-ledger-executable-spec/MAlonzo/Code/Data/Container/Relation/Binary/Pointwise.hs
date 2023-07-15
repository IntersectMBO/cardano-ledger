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

module MAlonzo.Code.Data.Container.Relation.Binary.Pointwise where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Container.Core

-- Data.Container.Relation.Binary.Pointwise._.Pointwise
d_Pointwise_36 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
newtype T_Pointwise_36 = C__'44'__66 (AgdaAny -> AgdaAny)
-- Data.Container.Relation.Binary.Pointwise._.Pointwise.shape
d_shape_60 ::
  T_Pointwise_36 -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_shape_60 = erased
-- Data.Container.Relation.Binary.Pointwise._.Pointwise.position
d_position_64 :: T_Pointwise_36 -> AgdaAny -> AgdaAny
d_position_64 v0
  = case coe v0 of
      C__'44'__66 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Container.Relation.Binary.Pointwise._.map
d_map_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_Pointwise_36 -> T_Pointwise_36
d_map_94 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11 v12 v13
         v14
  = du_map_94 v11 v12 v13 v14
du_map_94 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_Pointwise_36 -> T_Pointwise_36
du_map_94 v0 v1 v2 v3
  = case coe v3 of
      C__'44'__66 v5
        -> coe
             C__'44'__66
             (\ v6 ->
                coe
                  v0 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v1 v6)
                  (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v2 v6) (coe v5 v6))
      _ -> MAlonzo.RTE.mazUnreachableError
