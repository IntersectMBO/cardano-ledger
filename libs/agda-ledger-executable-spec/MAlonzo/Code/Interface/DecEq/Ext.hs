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

module MAlonzo.Code.Interface.DecEq.Ext where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Interface.DecRel
import qualified MAlonzo.Code.Relation.Nullary.Decidable
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Interface.DecEq.Ext._._._∼?_
d__'8764''63'__22 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8764''63'__22 v0
  = coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)
-- Interface.DecEq.Ext._._._∼ᵇ_
d__'8764''7495'__24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> Bool
d__'8764''7495'__24 v0 ~v1 v2 = du__'8764''7495'__24 v0 v2
du__'8764''7495'__24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> Bool
du__'8764''7495'__24 v0 v1
  = coe
      MAlonzo.Code.Interface.DecRel.du__'8764''7495'__22 (coe v0)
      (coe
         MAlonzo.Code.Interface.DecRel.C_DecRel'46'constructor_307
         (coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v1)))
-- Interface.DecEq.Ext._.≡ᵇ-refl
d_'8801''7495''45'refl_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8801''7495''45'refl_28 = erased
-- Interface.DecEq.Ext.↔-DecEq
d_'8596''45'DecEq_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_'8596''45'DecEq_44 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_'8596''45'DecEq_44 v4 v5
du_'8596''45'DecEq_44 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_'8596''45'DecEq_44 v0 v1
  = coe
      MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63
      (case coe v1 of
         MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63 v2
           -> coe
                (\ v3 v4 ->
                   coe
                     MAlonzo.Code.Relation.Nullary.Decidable.du_map_18
                     (coe
                        MAlonzo.Code.Function.Bundles.C_Equivalence'46'constructor_17233
                        erased
                        (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1070 v0 v3 v4)
                        erased erased)
                     (coe
                        v2 (coe MAlonzo.Code.Function.Bundles.d_from_1066 v0 v3)
                        (coe MAlonzo.Code.Function.Bundles.d_from_1066 v0 v4)))
         _ -> MAlonzo.RTE.mazUnreachableError)
