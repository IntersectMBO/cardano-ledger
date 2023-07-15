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

module MAlonzo.Code.Interface.DecRel where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Interface.DecRel.DecRel
d_DecRel_16 a0 a1 a2 a3 a4 a5 = ()
newtype T_DecRel_16
  = C_DecRel'46'constructor_307 (AgdaAny ->
                                 AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
-- Interface.DecRel.DecRel._∼?_
d__'8764''63'__20 ::
  T_DecRel_16 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8764''63'__20 v0
  = case coe v0 of
      C_DecRel'46'constructor_307 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.DecRel.DecRel._∼ᵇ_
d__'8764''7495'__22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_DecRel_16 -> AgdaAny -> AgdaAny -> Bool
d__'8764''7495'__22 ~v0 ~v1 v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du__'8764''7495'__22 v2 v6 v7 v8
du__'8764''7495'__22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecRel_16 -> AgdaAny -> AgdaAny -> Bool
du__'8764''7495'__22 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 v0
      erased (coe d__'8764''63'__20 v1 v2 v3)
