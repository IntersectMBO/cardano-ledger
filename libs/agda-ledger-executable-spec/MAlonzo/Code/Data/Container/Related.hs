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

module MAlonzo.Code.Data.Container.Related where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Container.Core
import qualified MAlonzo.Code.Function.Related
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Relation.Binary.Bundles

-- Data.Container.Related.[_]-Order
d_'91'_'93''45'Order_12 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  () -> MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'91'_'93''45'Order_12 ~v0 ~v1 ~v2 v3 ~v4 ~v5
  = du_'91'_'93''45'Order_12 v3
du_'91'_'93''45'Order_12 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_'91'_'93''45'Order_12 v0
  = coe
      MAlonzo.Code.Function.Related.du_InducedPreorder'8322'_680 (coe v0)
-- Data.Container.Related.[_]-Equality
d_'91'_'93''45'Equality_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  () -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'91'_'93''45'Equality_26 ~v0 ~v1 ~v2 v3 ~v4 ~v5
  = du_'91'_'93''45'Equality_26 v3
du_'91'_'93''45'Equality_26 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_'91'_'93''45'Equality_26 v0
  = coe
      MAlonzo.Code.Function.Related.du_InducedEquivalence'8322'_756
      (coe v0)
-- Data.Container.Related._∼[_]_
d__'8764''91'_'93'__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d__'8764''91'_'93'__44 = erased
