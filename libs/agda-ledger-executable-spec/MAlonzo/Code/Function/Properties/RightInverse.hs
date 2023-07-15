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

module MAlonzo.Code.Function.Properties.RightInverse where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Relation.Binary.Bundles

-- Function.Properties.RightInverse.RightInverse⇒Surjection
d_RightInverse'8658'Surjection_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
d_RightInverse'8658'Surjection_22 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_RightInverse'8658'Surjection_22 v6
du_RightInverse'8658'Surjection_22 ::
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
du_RightInverse'8658'Surjection_22 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_Surjection'46'constructor_10213
      (coe MAlonzo.Code.Function.Bundles.d_from_1038 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1042 (coe v0))
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
              (coe MAlonzo.Code.Function.Bundles.d_to_1036 v0 v1)
              (coe MAlonzo.Code.Function.Bundles.d_inverse'691'_1044 v0 v1)))
-- Function.Properties.RightInverse.↪⇒↠
d_'8618''8658''8608'_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
d_'8618''8658''8608'_50 ~v0 ~v1 ~v2 ~v3 = du_'8618''8658''8608'_50
du_'8618''8658''8608'_50 ::
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
du_'8618''8658''8608'_50 = coe du_RightInverse'8658'Surjection_22
