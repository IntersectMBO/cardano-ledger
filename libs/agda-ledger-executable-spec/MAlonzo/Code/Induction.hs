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

module MAlonzo.Code.Induction where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

-- Induction.RecStruct
d_RecStruct_8 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> ()
d_RecStruct_8 = erased
-- Induction.RecursorBuilder
d_RecursorBuilder_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> ((AgdaAny -> ()) -> AgdaAny -> ()) -> ()
d_RecursorBuilder_24 = erased
-- Induction.Recursor
d_Recursor_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> ((AgdaAny -> ()) -> AgdaAny -> ()) -> ()
d_Recursor_38 = erased
-- Induction.build
d_build_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  ((AgdaAny -> ()) -> AgdaAny -> ()) ->
  ((AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_build_54 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8
  = du_build_54 v5 v6 v7 v8
du_build_54 ::
  ((AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_build_54 v0 v1 v2 v3 = coe v2 v3 (coe v0 v1 v2 v3)
-- Induction.SubsetRecursorBuilder
d_SubsetRecursorBuilder_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> ((AgdaAny -> ()) -> AgdaAny -> ()) -> ()
d_SubsetRecursorBuilder_74 = erased
-- Induction.SubsetRecursor
d_SubsetRecursor_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> ((AgdaAny -> ()) -> AgdaAny -> ()) -> ()
d_SubsetRecursor_92 = erased
-- Induction.subsetBuild
d_subsetBuild_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  ((AgdaAny -> ()) -> AgdaAny -> ()) ->
  ((AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_subsetBuild_114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9 v10 v11
  = du_subsetBuild_114 v7 v8 v9 v10 v11
du_subsetBuild_114 ::
  ((AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_subsetBuild_114 v0 v1 v2 v3 v4 = coe v2 v3 (coe v0 v1 v2 v3 v4)
