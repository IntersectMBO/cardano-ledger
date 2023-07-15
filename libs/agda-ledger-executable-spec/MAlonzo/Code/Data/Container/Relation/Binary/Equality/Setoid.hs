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

module MAlonzo.Code.Data.Container.Relation.Binary.Equality.Setoid where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Container.Core
import qualified MAlonzo.Code.Data.Container.Relation.Binary.Pointwise
import qualified MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.Properties
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Data.Container.Relation.Binary.Equality.Setoid._.Eq
d_Eq_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d_Eq_50 = erased
-- Data.Container.Relation.Binary.Equality.Setoid._.refl
d_refl_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
d_refl_52 ~v0 ~v1 v2 ~v3 ~v4 ~v5 v6 = du_refl_52 v2 v6
du_refl_52 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
du_refl_52 v0 v1
  = coe
      MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.Properties.du_refl_30
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
      (coe v1)
-- Data.Container.Relation.Binary.Equality.Setoid._.sym
d_sym_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
d_sym_54 ~v0 ~v1 v2 ~v3 ~v4 ~v5 v6 v7 = du_sym_54 v2 v6 v7
du_sym_54 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
du_sym_54 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.Properties.du_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
      (coe v1) (coe v2)
-- Data.Container.Relation.Binary.Equality.Setoid._.trans
d_trans_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
d_trans_56 ~v0 ~v1 v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_trans_56 v2 v6 v7 v8
du_trans_56 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
du_trans_56 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.Properties.du_trans_44
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
      (coe v1) (coe v2) (coe v3)
-- Data.Container.Relation.Binary.Equality.Setoid._.isEquivalence
d_isEquivalence_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_58 ~v0 ~v1 v2 ~v3 ~v4 ~v5 = du_isEquivalence_58 v2
du_isEquivalence_58 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_58 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (coe du_refl_52 (coe v0)) (coe du_sym_54 (coe v0))
      (coe du_trans_56 (coe v0))
-- Data.Container.Relation.Binary.Equality.Setoid._.setoid
d_setoid_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_60 ~v0 ~v1 v2 ~v3 ~v4 ~v5 = du_setoid_60 v2
du_setoid_60 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_60 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe du_isEquivalence_58 (coe v0))
