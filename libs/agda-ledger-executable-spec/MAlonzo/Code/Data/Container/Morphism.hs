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

module MAlonzo.Code.Data.Container.Morphism where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Container.Core
import qualified MAlonzo.Code.Function.Base

-- Data.Container.Morphism._.id
d_id_16 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74
d_id_16 ~v0 ~v1 ~v2 = du_id_16
du_id_16 :: MAlonzo.Code.Data.Container.Core.T__'8658'__74
du_id_16
  = coe
      MAlonzo.Code.Data.Container.Core.C__'9655'__108 (coe (\ v0 -> v0))
      (coe (\ v0 v1 -> v1))
-- Data.Container.Morphism._._∘_
d__'8728'__40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74
d__'8728'__40 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du__'8728'__40 v9 v10
du__'8728'__40 ::
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74
du__'8728'__40 v0 v1
  = coe
      MAlonzo.Code.Data.Container.Core.C__'9655'__108
      (coe
         MAlonzo.Code.Function.Base.du__'8728''8242'__216
         (coe MAlonzo.Code.Data.Container.Core.d_shape_94 (coe v0))
         (coe MAlonzo.Code.Data.Container.Core.d_shape_94 (coe v1)))
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Function.Base.du__'8728''8242'__216
              (coe MAlonzo.Code.Data.Container.Core.d_position_98 v1 v2)
              (coe
                 MAlonzo.Code.Data.Container.Core.d_position_98 v0
                 (coe MAlonzo.Code.Data.Container.Core.d_shape_94 v1 v2))))
