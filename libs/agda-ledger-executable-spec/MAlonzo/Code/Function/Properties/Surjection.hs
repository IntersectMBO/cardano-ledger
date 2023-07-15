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

module MAlonzo.Code.Function.Properties.Surjection where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Bundles

-- Function.Properties.Surjection.↠⇒↪
d_'8608''8658''8618'_14 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d_'8608''8658''8618'_14 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8608''8658''8618'_14 v4
du_'8608''8658''8618'_14 ::
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
du_'8608''8658''8618'_14 v0
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8618'_1344
      (coe
         (\ v1 ->
            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
              (coe MAlonzo.Code.Function.Bundles.d_surjective_786 v0 v1)))
      (coe MAlonzo.Code.Function.Bundles.d_to_782 (coe v0)) erased
-- Function.Properties.Surjection.↠⇒⇔
d_'8608''8658''8660'_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8608''8658''8660'_84 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8608''8658''8660'_84 v4
du_'8608''8658''8660'_84 ::
  MAlonzo.Code.Function.Bundles.T_Surjection_774 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8608''8658''8660'_84 v0
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
      (coe MAlonzo.Code.Function.Bundles.d_to_782 (coe v0))
      (coe
         (\ v1 ->
            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
              (coe MAlonzo.Code.Function.Bundles.d_surjective_786 v0 v1)))
