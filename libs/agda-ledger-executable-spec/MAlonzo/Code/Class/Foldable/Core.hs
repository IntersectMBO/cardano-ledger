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

module MAlonzo.Code.Class.Foldable.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Monoid.Core

-- Class.Foldable.Core.Foldable
d_Foldable_12 a0 = ()
newtype T_Foldable_12
  = C_Foldable'46'constructor_141 (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                   MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                   () ->
                                   () ->
                                   MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
                                   (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny)
-- Class.Foldable.Core.Foldable.foldMap
d_foldMap_26 ::
  T_Foldable_12 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_foldMap_26 v0
  = case coe v0 of
      C_Foldable'46'constructor_141 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Foldable.Core._.foldMap
d_foldMap_30 ::
  T_Foldable_12 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_foldMap_30 v0 = coe d_foldMap_26 (coe v0)
