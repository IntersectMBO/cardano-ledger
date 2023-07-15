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

module MAlonzo.Code.Relation.Binary.Bundles where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Binary.Bundles.PartialSetoid
d_PartialSetoid_10 a0 a1 = ()
newtype T_PartialSetoid_10
  = C_PartialSetoid'46'constructor_129 MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
-- Relation.Binary.Bundles.PartialSetoid.Carrier
d_Carrier_22 :: T_PartialSetoid_10 -> ()
d_Carrier_22 = erased
-- Relation.Binary.Bundles.PartialSetoid._≈_
d__'8776'__24 :: T_PartialSetoid_10 -> AgdaAny -> AgdaAny -> ()
d__'8776'__24 = erased
-- Relation.Binary.Bundles.PartialSetoid.isPartialEquivalence
d_isPartialEquivalence_26 ::
  T_PartialSetoid_10 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_26 v0
  = case coe v0 of
      C_PartialSetoid'46'constructor_129 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.PartialSetoid._.sym
d_sym_30 ::
  T_PartialSetoid_10 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_30 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_22
      (coe d_isPartialEquivalence_26 (coe v0))
-- Relation.Binary.Bundles.PartialSetoid._.trans
d_trans_32 ::
  T_PartialSetoid_10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_32 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_24
      (coe d_isPartialEquivalence_26 (coe v0))
-- Relation.Binary.Bundles.PartialSetoid._≉_
d__'8777'__34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_PartialSetoid_10 -> AgdaAny -> AgdaAny -> ()
d__'8777'__34 = erased
-- Relation.Binary.Bundles.Setoid
d_Setoid_44 a0 a1 = ()
newtype T_Setoid_44
  = C_Setoid'46'constructor_719 MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
-- Relation.Binary.Bundles.Setoid.Carrier
d_Carrier_56 :: T_Setoid_44 -> ()
d_Carrier_56 = erased
-- Relation.Binary.Bundles.Setoid._≈_
d__'8776'__58 :: T_Setoid_44 -> AgdaAny -> AgdaAny -> ()
d__'8776'__58 = erased
-- Relation.Binary.Bundles.Setoid.isEquivalence
d_isEquivalence_60 ::
  T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_60 v0
  = case coe v0 of
      C_Setoid'46'constructor_719 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.Setoid._.isPartialEquivalence
d_isPartialEquivalence_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_64 ~v0 ~v1 v2
  = du_isPartialEquivalence_64 v2
du_isPartialEquivalence_64 ::
  T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_64 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_60 (coe v0))
-- Relation.Binary.Bundles.Setoid._.refl
d_refl_66 :: T_Setoid_44 -> AgdaAny -> AgdaAny
d_refl_66 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_60 (coe v0))
-- Relation.Binary.Bundles.Setoid._.reflexive
d_reflexive_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Setoid_44 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_68 ~v0 ~v1 v2 = du_reflexive_68 v2
du_reflexive_68 ::
  T_Setoid_44 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_68 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
      (coe d_isEquivalence_60 (coe v0)) v1
-- Relation.Binary.Bundles.Setoid._.sym
d_sym_70 :: T_Setoid_44 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_70 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_60 (coe v0))
-- Relation.Binary.Bundles.Setoid._.trans
d_trans_72 ::
  T_Setoid_44 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_72 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_60 (coe v0))
-- Relation.Binary.Bundles.Setoid.partialSetoid
d_partialSetoid_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Setoid_44 -> T_PartialSetoid_10
d_partialSetoid_74 ~v0 ~v1 v2 = du_partialSetoid_74 v2
du_partialSetoid_74 :: T_Setoid_44 -> T_PartialSetoid_10
du_partialSetoid_74 v0
  = coe
      C_PartialSetoid'46'constructor_129
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
         (coe d_isEquivalence_60 (coe v0)))
-- Relation.Binary.Bundles.Setoid._._≉_
d__'8777'__78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Setoid_44 -> AgdaAny -> AgdaAny -> ()
d__'8777'__78 = erased
-- Relation.Binary.Bundles.DecSetoid
d_DecSetoid_84 a0 a1 = ()
newtype T_DecSetoid_84
  = C_DecSetoid'46'constructor_1373 MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
-- Relation.Binary.Bundles.DecSetoid.Carrier
d_Carrier_96 :: T_DecSetoid_84 -> ()
d_Carrier_96 = erased
-- Relation.Binary.Bundles.DecSetoid._≈_
d__'8776'__98 :: T_DecSetoid_84 -> AgdaAny -> AgdaAny -> ()
d__'8776'__98 = erased
-- Relation.Binary.Bundles.DecSetoid.isDecEquivalence
d_isDecEquivalence_100 ::
  T_DecSetoid_84 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_isDecEquivalence_100 v0
  = case coe v0 of
      C_DecSetoid'46'constructor_1373 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.DecSetoid._._≟_
d__'8799'__104 ::
  T_DecSetoid_84 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__104 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8799'__52
      (coe d_isDecEquivalence_100 (coe v0))
-- Relation.Binary.Bundles.DecSetoid._.isEquivalence
d_isEquivalence_106 ::
  T_DecSetoid_84 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_106 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
      (coe d_isDecEquivalence_100 (coe v0))
-- Relation.Binary.Bundles.DecSetoid._.isPartialEquivalence
d_isPartialEquivalence_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecSetoid_84 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_108 ~v0 ~v1 v2
  = du_isPartialEquivalence_108 v2
du_isPartialEquivalence_108 ::
  T_DecSetoid_84 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_108 v0
  = let v1 = d_isDecEquivalence_100 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
         (coe v1))
-- Relation.Binary.Bundles.DecSetoid._.refl
d_refl_110 :: T_DecSetoid_84 -> AgdaAny -> AgdaAny
d_refl_110 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
         (coe d_isDecEquivalence_100 (coe v0)))
-- Relation.Binary.Bundles.DecSetoid._.reflexive
d_reflexive_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecSetoid_84 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_112 ~v0 ~v1 v2 = du_reflexive_112 v2
du_reflexive_112 ::
  T_DecSetoid_84 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_112 v0
  = let v1 = d_isDecEquivalence_100 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
           (coe v1))
        v2
-- Relation.Binary.Bundles.DecSetoid._.sym
d_sym_114 ::
  T_DecSetoid_84 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_114 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
         (coe d_isDecEquivalence_100 (coe v0)))
-- Relation.Binary.Bundles.DecSetoid._.trans
d_trans_116 ::
  T_DecSetoid_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_116 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
         (coe d_isDecEquivalence_100 (coe v0)))
-- Relation.Binary.Bundles.DecSetoid.setoid
d_setoid_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecSetoid_84 -> T_Setoid_44
d_setoid_118 ~v0 ~v1 v2 = du_setoid_118 v2
du_setoid_118 :: T_DecSetoid_84 -> T_Setoid_44
du_setoid_118 v0
  = coe
      C_Setoid'46'constructor_719
      (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
         (coe d_isDecEquivalence_100 (coe v0)))
-- Relation.Binary.Bundles.DecSetoid._._≉_
d__'8777'__122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecSetoid_84 -> AgdaAny -> AgdaAny -> ()
d__'8777'__122 = erased
-- Relation.Binary.Bundles.DecSetoid._.partialSetoid
d_partialSetoid_124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecSetoid_84 -> T_PartialSetoid_10
d_partialSetoid_124 ~v0 ~v1 v2 = du_partialSetoid_124 v2
du_partialSetoid_124 :: T_DecSetoid_84 -> T_PartialSetoid_10
du_partialSetoid_124 v0
  = coe du_partialSetoid_74 (coe du_setoid_118 (coe v0))
-- Relation.Binary.Bundles.Preorder
d_Preorder_132 a0 a1 a2 = ()
newtype T_Preorder_132
  = C_Preorder'46'constructor_2251 MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
-- Relation.Binary.Bundles.Preorder.Carrier
d_Carrier_148 :: T_Preorder_132 -> ()
d_Carrier_148 = erased
-- Relation.Binary.Bundles.Preorder._≈_
d__'8776'__150 :: T_Preorder_132 -> AgdaAny -> AgdaAny -> ()
d__'8776'__150 = erased
-- Relation.Binary.Bundles.Preorder._∼_
d__'8764'__152 :: T_Preorder_132 -> AgdaAny -> AgdaAny -> ()
d__'8764'__152 = erased
-- Relation.Binary.Bundles.Preorder.isPreorder
d_isPreorder_154 ::
  T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_154 v0
  = case coe v0 of
      C_Preorder'46'constructor_2251 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.Preorder._.isEquivalence
d_isEquivalence_158 ::
  T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_158 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe d_isPreorder_154 (coe v0))
-- Relation.Binary.Bundles.Preorder._.refl
d_refl_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 -> AgdaAny -> AgdaAny
d_refl_160 ~v0 ~v1 ~v2 v3 = du_refl_160 v3
du_refl_160 :: T_Preorder_132 -> AgdaAny -> AgdaAny
du_refl_160 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe d_isPreorder_154 (coe v0))
-- Relation.Binary.Bundles.Preorder._.reflexive
d_reflexive_162 ::
  T_Preorder_132 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_162 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe d_isPreorder_154 (coe v0))
-- Relation.Binary.Bundles.Preorder._.trans
d_trans_164 ::
  T_Preorder_132 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_164 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe d_isPreorder_154 (coe v0))
-- Relation.Binary.Bundles.Preorder._.∼-resp-≈
d_'8764''45'resp'45''8776'_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_166 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_166 v3
du_'8764''45'resp'45''8776'_166 ::
  T_Preorder_132 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_166 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe d_isPreorder_154 (coe v0))
-- Relation.Binary.Bundles.Preorder._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_168 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_168 v3
du_'8764''45'resp'691''45''8776'_168 ::
  T_Preorder_132 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_168 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe d_isPreorder_154 (coe v0))
-- Relation.Binary.Bundles.Preorder._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_170 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_170 v3
du_'8764''45'resp'737''45''8776'_170 ::
  T_Preorder_132 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_170 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe d_isPreorder_154 (coe v0))
-- Relation.Binary.Bundles.Preorder.Eq.setoid
d_setoid_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 -> T_Setoid_44
d_setoid_174 ~v0 ~v1 ~v2 v3 = du_setoid_174 v3
du_setoid_174 :: T_Preorder_132 -> T_Setoid_44
du_setoid_174 v0
  = coe
      C_Setoid'46'constructor_719
      (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v0)))
-- Relation.Binary.Bundles.Preorder.Eq._._≈_
d__'8776'__178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 -> AgdaAny -> AgdaAny -> ()
d__'8776'__178 = erased
-- Relation.Binary.Bundles.Preorder.Eq._._≉_
d__'8777'__180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 -> AgdaAny -> AgdaAny -> ()
d__'8777'__180 = erased
-- Relation.Binary.Bundles.Preorder.Eq._.Carrier
d_Carrier_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Preorder_132 -> ()
d_Carrier_182 = erased
-- Relation.Binary.Bundles.Preorder.Eq._.isEquivalence
d_isEquivalence_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_184 ~v0 ~v1 ~v2 v3 = du_isEquivalence_184 v3
du_isEquivalence_184 ::
  T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_184 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe d_isPreorder_154 (coe v0))
-- Relation.Binary.Bundles.Preorder.Eq._.isPartialEquivalence
d_isPartialEquivalence_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_186 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_186 v3
du_isPartialEquivalence_186 ::
  T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_186 v0
  = let v1 = coe du_setoid_174 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_60 (coe v1))
-- Relation.Binary.Bundles.Preorder.Eq._.partialSetoid
d_partialSetoid_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 -> T_PartialSetoid_10
d_partialSetoid_188 ~v0 ~v1 ~v2 v3 = du_partialSetoid_188 v3
du_partialSetoid_188 :: T_Preorder_132 -> T_PartialSetoid_10
du_partialSetoid_188 v0
  = coe du_partialSetoid_74 (coe du_setoid_174 (coe v0))
-- Relation.Binary.Bundles.Preorder.Eq._.refl
d_refl_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 -> AgdaAny -> AgdaAny
d_refl_190 ~v0 ~v1 ~v2 v3 = du_refl_190 v3
du_refl_190 :: T_Preorder_132 -> AgdaAny -> AgdaAny
du_refl_190 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v0)))
-- Relation.Binary.Bundles.Preorder.Eq._.reflexive
d_reflexive_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_192 ~v0 ~v1 ~v2 v3 = du_reflexive_192 v3
du_reflexive_192 ::
  T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_192 v0
  = let v1 = coe du_setoid_174 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_60 (coe v1)) v2
-- Relation.Binary.Bundles.Preorder.Eq._.sym
d_sym_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_194 ~v0 ~v1 ~v2 v3 = du_sym_194 v3
du_sym_194 ::
  T_Preorder_132 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_194 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v0)))
-- Relation.Binary.Bundles.Preorder.Eq._.trans
d_trans_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Preorder_132 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_196 ~v0 ~v1 ~v2 v3 = du_trans_196 v3
du_trans_196 ::
  T_Preorder_132 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_196 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v0)))
-- Relation.Binary.Bundles.TotalPreorder
d_TotalPreorder_204 a0 a1 a2 = ()
newtype T_TotalPreorder_204
  = C_TotalPreorder'46'constructor_3645 MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
-- Relation.Binary.Bundles.TotalPreorder.Carrier
d_Carrier_220 :: T_TotalPreorder_204 -> ()
d_Carrier_220 = erased
-- Relation.Binary.Bundles.TotalPreorder._≈_
d__'8776'__222 :: T_TotalPreorder_204 -> AgdaAny -> AgdaAny -> ()
d__'8776'__222 = erased
-- Relation.Binary.Bundles.TotalPreorder._≲_
d__'8818'__224 :: T_TotalPreorder_204 -> AgdaAny -> AgdaAny -> ()
d__'8818'__224 = erased
-- Relation.Binary.Bundles.TotalPreorder.isTotalPreorder
d_isTotalPreorder_226 ::
  T_TotalPreorder_204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
d_isTotalPreorder_226 v0
  = case coe v0 of
      C_TotalPreorder'46'constructor_3645 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.TotalPreorder._.isEquivalence
d_isEquivalence_230 ::
  T_TotalPreorder_204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_230 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
         (coe d_isTotalPreorder_226 (coe v0)))
-- Relation.Binary.Bundles.TotalPreorder._.isPreorder
d_isPreorder_232 ::
  T_TotalPreorder_204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_232 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
      (coe d_isTotalPreorder_226 (coe v0))
-- Relation.Binary.Bundles.TotalPreorder._.refl
d_refl_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 -> AgdaAny -> AgdaAny
d_refl_234 ~v0 ~v1 ~v2 v3 = du_refl_234 v3
du_refl_234 :: T_TotalPreorder_204 -> AgdaAny -> AgdaAny
du_refl_234 v0
  = let v1 = d_isTotalPreorder_226 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v1))
-- Relation.Binary.Bundles.TotalPreorder._.reflexive
d_reflexive_236 ::
  T_TotalPreorder_204 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_236 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
         (coe d_isTotalPreorder_226 (coe v0)))
-- Relation.Binary.Bundles.TotalPreorder._.total
d_total_238 ::
  T_TotalPreorder_204 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_238 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_total_128
      (coe d_isTotalPreorder_226 (coe v0))
-- Relation.Binary.Bundles.TotalPreorder._.trans
d_trans_240 ::
  T_TotalPreorder_204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_240 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
         (coe d_isTotalPreorder_226 (coe v0)))
-- Relation.Binary.Bundles.TotalPreorder._.∼-resp-≈
d_'8764''45'resp'45''8776'_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_242 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_242 v3
du_'8764''45'resp'45''8776'_242 ::
  T_TotalPreorder_204 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_242 v0
  = let v1 = d_isTotalPreorder_226 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v1))
-- Relation.Binary.Bundles.TotalPreorder._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_244 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_244 v3
du_'8764''45'resp'691''45''8776'_244 ::
  T_TotalPreorder_204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_244 v0
  = let v1 = d_isTotalPreorder_226 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v1))
-- Relation.Binary.Bundles.TotalPreorder._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_246 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_246 v3
du_'8764''45'resp'737''45''8776'_246 ::
  T_TotalPreorder_204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_246 v0
  = let v1 = d_isTotalPreorder_226 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v1))
-- Relation.Binary.Bundles.TotalPreorder.preorder
d_preorder_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 -> T_Preorder_132
d_preorder_248 ~v0 ~v1 ~v2 v3 = du_preorder_248 v3
du_preorder_248 :: T_TotalPreorder_204 -> T_Preorder_132
du_preorder_248 v0
  = coe
      C_Preorder'46'constructor_2251
      (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
         (coe d_isTotalPreorder_226 (coe v0)))
-- Relation.Binary.Bundles.TotalPreorder._.Eq._≈_
d__'8776'__254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 -> AgdaAny -> AgdaAny -> ()
d__'8776'__254 = erased
-- Relation.Binary.Bundles.TotalPreorder._.Eq._≉_
d__'8777'__256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 -> AgdaAny -> AgdaAny -> ()
d__'8777'__256 = erased
-- Relation.Binary.Bundles.TotalPreorder._.Eq.Carrier
d_Carrier_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_TotalPreorder_204 -> ()
d_Carrier_258 = erased
-- Relation.Binary.Bundles.TotalPreorder._.Eq.isEquivalence
d_isEquivalence_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_260 ~v0 ~v1 ~v2 v3 = du_isEquivalence_260 v3
du_isEquivalence_260 ::
  T_TotalPreorder_204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_260 v0
  = let v1 = coe du_preorder_248 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe d_isPreorder_154 (coe v1))
-- Relation.Binary.Bundles.TotalPreorder._.Eq.isPartialEquivalence
d_isPartialEquivalence_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_262 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_262 v3
du_isPartialEquivalence_262 ::
  T_TotalPreorder_204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_262 v0
  = let v1 = coe du_preorder_248 (coe v0) in
    let v2 = coe du_setoid_174 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_60 (coe v2))
-- Relation.Binary.Bundles.TotalPreorder._.Eq.partialSetoid
d_partialSetoid_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 -> T_PartialSetoid_10
d_partialSetoid_264 ~v0 ~v1 ~v2 v3 = du_partialSetoid_264 v3
du_partialSetoid_264 :: T_TotalPreorder_204 -> T_PartialSetoid_10
du_partialSetoid_264 v0
  = let v1 = coe du_preorder_248 (coe v0) in
    coe du_partialSetoid_74 (coe du_setoid_174 (coe v1))
-- Relation.Binary.Bundles.TotalPreorder._.Eq.refl
d_refl_266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 -> AgdaAny -> AgdaAny
d_refl_266 ~v0 ~v1 ~v2 v3 = du_refl_266 v3
du_refl_266 :: T_TotalPreorder_204 -> AgdaAny -> AgdaAny
du_refl_266 v0
  = let v1 = coe du_preorder_248 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v1)))
-- Relation.Binary.Bundles.TotalPreorder._.Eq.reflexive
d_reflexive_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_268 ~v0 ~v1 ~v2 v3 = du_reflexive_268 v3
du_reflexive_268 ::
  T_TotalPreorder_204 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_268 v0
  = let v1 = coe du_preorder_248 (coe v0) in
    let v2 = coe du_setoid_174 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_60 (coe v2)) v3
-- Relation.Binary.Bundles.TotalPreorder._.Eq.setoid
d_setoid_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 -> T_Setoid_44
d_setoid_270 ~v0 ~v1 ~v2 v3 = du_setoid_270 v3
du_setoid_270 :: T_TotalPreorder_204 -> T_Setoid_44
du_setoid_270 v0 = coe du_setoid_174 (coe du_preorder_248 (coe v0))
-- Relation.Binary.Bundles.TotalPreorder._.Eq.sym
d_sym_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_272 ~v0 ~v1 ~v2 v3 = du_sym_272 v3
du_sym_272 ::
  T_TotalPreorder_204 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_272 v0
  = let v1 = coe du_preorder_248 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v1)))
-- Relation.Binary.Bundles.TotalPreorder._.Eq.trans
d_trans_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalPreorder_204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_274 ~v0 ~v1 ~v2 v3 = du_trans_274 v3
du_trans_274 ::
  T_TotalPreorder_204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_274 v0
  = let v1 = coe du_preorder_248 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v1)))
-- Relation.Binary.Bundles.Poset
d_Poset_282 a0 a1 a2 = ()
newtype T_Poset_282
  = C_Poset'46'constructor_5189 MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
-- Relation.Binary.Bundles.Poset.Carrier
d_Carrier_298 :: T_Poset_282 -> ()
d_Carrier_298 = erased
-- Relation.Binary.Bundles.Poset._≈_
d__'8776'__300 :: T_Poset_282 -> AgdaAny -> AgdaAny -> ()
d__'8776'__300 = erased
-- Relation.Binary.Bundles.Poset._≤_
d__'8804'__302 :: T_Poset_282 -> AgdaAny -> AgdaAny -> ()
d__'8804'__302 = erased
-- Relation.Binary.Bundles.Poset.isPartialOrder
d_isPartialOrder_304 ::
  T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_304 v0
  = case coe v0 of
      C_Poset'46'constructor_5189 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.Poset._.antisym
d_antisym_308 ::
  T_Poset_282 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_308 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe d_isPartialOrder_304 (coe v0))
-- Relation.Binary.Bundles.Poset._.isEquivalence
d_isEquivalence_310 ::
  T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_310 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_304 (coe v0)))
-- Relation.Binary.Bundles.Poset._.isPreorder
d_isPreorder_312 ::
  T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_312 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe d_isPartialOrder_304 (coe v0))
-- Relation.Binary.Bundles.Poset._.refl
d_refl_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 -> AgdaAny -> AgdaAny
d_refl_314 ~v0 ~v1 ~v2 v3 = du_refl_314 v3
du_refl_314 :: T_Poset_282 -> AgdaAny -> AgdaAny
du_refl_314 v0
  = let v1 = d_isPartialOrder_304 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Bundles.Poset._.reflexive
d_reflexive_316 ::
  T_Poset_282 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_316 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_304 (coe v0)))
-- Relation.Binary.Bundles.Poset._.trans
d_trans_318 ::
  T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_318 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_304 (coe v0)))
-- Relation.Binary.Bundles.Poset._.∼-resp-≈
d_'8764''45'resp'45''8776'_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_320 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_320 v3
du_'8764''45'resp'45''8776'_320 ::
  T_Poset_282 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_320 v0
  = let v1 = d_isPartialOrder_304 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Bundles.Poset._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_322 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_322 v3
du_'8764''45'resp'691''45''8776'_322 ::
  T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_322 v0
  = let v1 = d_isPartialOrder_304 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Bundles.Poset._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_324 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_324 v3
du_'8764''45'resp'737''45''8776'_324 ::
  T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_324 v0
  = let v1 = d_isPartialOrder_304 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v1))
-- Relation.Binary.Bundles.Poset.preorder
d_preorder_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 -> T_Preorder_132
d_preorder_326 ~v0 ~v1 ~v2 v3 = du_preorder_326 v3
du_preorder_326 :: T_Poset_282 -> T_Preorder_132
du_preorder_326 v0
  = coe
      C_Preorder'46'constructor_2251
      (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe d_isPartialOrder_304 (coe v0)))
-- Relation.Binary.Bundles.Poset._.Eq._≈_
d__'8776'__332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 -> AgdaAny -> AgdaAny -> ()
d__'8776'__332 = erased
-- Relation.Binary.Bundles.Poset._.Eq._≉_
d__'8777'__334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 -> AgdaAny -> AgdaAny -> ()
d__'8777'__334 = erased
-- Relation.Binary.Bundles.Poset._.Eq.Carrier
d_Carrier_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Poset_282 -> ()
d_Carrier_336 = erased
-- Relation.Binary.Bundles.Poset._.Eq.isEquivalence
d_isEquivalence_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_338 ~v0 ~v1 ~v2 v3 = du_isEquivalence_338 v3
du_isEquivalence_338 ::
  T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_338 v0
  = let v1 = coe du_preorder_326 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe d_isPreorder_154 (coe v1))
-- Relation.Binary.Bundles.Poset._.Eq.isPartialEquivalence
d_isPartialEquivalence_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_340 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_340 v3
du_isPartialEquivalence_340 ::
  T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_340 v0
  = let v1 = coe du_preorder_326 (coe v0) in
    let v2 = coe du_setoid_174 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_60 (coe v2))
-- Relation.Binary.Bundles.Poset._.Eq.partialSetoid
d_partialSetoid_342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 -> T_PartialSetoid_10
d_partialSetoid_342 ~v0 ~v1 ~v2 v3 = du_partialSetoid_342 v3
du_partialSetoid_342 :: T_Poset_282 -> T_PartialSetoid_10
du_partialSetoid_342 v0
  = let v1 = coe du_preorder_326 (coe v0) in
    coe du_partialSetoid_74 (coe du_setoid_174 (coe v1))
-- Relation.Binary.Bundles.Poset._.Eq.refl
d_refl_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 -> AgdaAny -> AgdaAny
d_refl_344 ~v0 ~v1 ~v2 v3 = du_refl_344 v3
du_refl_344 :: T_Poset_282 -> AgdaAny -> AgdaAny
du_refl_344 v0
  = let v1 = coe du_preorder_326 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v1)))
-- Relation.Binary.Bundles.Poset._.Eq.reflexive
d_reflexive_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_346 ~v0 ~v1 ~v2 v3 = du_reflexive_346 v3
du_reflexive_346 ::
  T_Poset_282 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_346 v0
  = let v1 = coe du_preorder_326 (coe v0) in
    let v2 = coe du_setoid_174 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_60 (coe v2)) v3
-- Relation.Binary.Bundles.Poset._.Eq.setoid
d_setoid_348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 -> T_Setoid_44
d_setoid_348 ~v0 ~v1 ~v2 v3 = du_setoid_348 v3
du_setoid_348 :: T_Poset_282 -> T_Setoid_44
du_setoid_348 v0 = coe du_setoid_174 (coe du_preorder_326 (coe v0))
-- Relation.Binary.Bundles.Poset._.Eq.sym
d_sym_350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_350 ~v0 ~v1 ~v2 v3 = du_sym_350 v3
du_sym_350 ::
  T_Poset_282 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_350 v0
  = let v1 = coe du_preorder_326 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v1)))
-- Relation.Binary.Bundles.Poset._.Eq.trans
d_trans_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_352 ~v0 ~v1 ~v2 v3 = du_trans_352 v3
du_trans_352 ::
  T_Poset_282 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_352 v0
  = let v1 = coe du_preorder_326 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v1)))
-- Relation.Binary.Bundles.DecPoset
d_DecPoset_360 a0 a1 a2 = ()
newtype T_DecPoset_360
  = C_DecPoset'46'constructor_6741 MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
-- Relation.Binary.Bundles.DecPoset.Carrier
d_Carrier_376 :: T_DecPoset_360 -> ()
d_Carrier_376 = erased
-- Relation.Binary.Bundles.DecPoset._≈_
d__'8776'__378 :: T_DecPoset_360 -> AgdaAny -> AgdaAny -> ()
d__'8776'__378 = erased
-- Relation.Binary.Bundles.DecPoset._≤_
d__'8804'__380 :: T_DecPoset_360 -> AgdaAny -> AgdaAny -> ()
d__'8804'__380 = erased
-- Relation.Binary.Bundles.DecPoset.isDecPartialOrder
d_isDecPartialOrder_382 ::
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
d_isDecPartialOrder_382 v0
  = case coe v0 of
      C_DecPoset'46'constructor_6741 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.DecPoset.DPO._≟_
d__'8799'__386 ::
  T_DecPoset_360 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__386 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8799'__218
      (coe d_isDecPartialOrder_382 (coe v0))
-- Relation.Binary.Bundles.DecPoset.DPO._≤?_
d__'8804''63'__388 ::
  T_DecPoset_360 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__388 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__220
      (coe d_isDecPartialOrder_382 (coe v0))
-- Relation.Binary.Bundles.DecPoset.DPO.antisym
d_antisym_390 ::
  T_DecPoset_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_390 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
         (coe d_isDecPartialOrder_382 (coe v0)))
-- Relation.Binary.Bundles.DecPoset.DPO.isEquivalence
d_isEquivalence_392 ::
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_392 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
            (coe d_isDecPartialOrder_382 (coe v0))))
-- Relation.Binary.Bundles.DecPoset.DPO.isPartialOrder
d_isPartialOrder_394 ::
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_394 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
      (coe d_isDecPartialOrder_382 (coe v0))
-- Relation.Binary.Bundles.DecPoset.DPO.isPreorder
d_isPreorder_396 ::
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_396 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
         (coe d_isDecPartialOrder_382 (coe v0)))
-- Relation.Binary.Bundles.DecPoset.DPO.refl
d_refl_398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> AgdaAny -> AgdaAny
d_refl_398 ~v0 ~v1 ~v2 v3 = du_refl_398 v3
du_refl_398 :: T_DecPoset_360 -> AgdaAny -> AgdaAny
du_refl_398 v0
  = let v1 = d_isDecPartialOrder_382 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Bundles.DecPoset.DPO.reflexive
d_reflexive_400 ::
  T_DecPoset_360 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_400 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
            (coe d_isDecPartialOrder_382 (coe v0))))
-- Relation.Binary.Bundles.DecPoset.DPO.trans
d_trans_402 ::
  T_DecPoset_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_402 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
            (coe d_isDecPartialOrder_382 (coe v0))))
-- Relation.Binary.Bundles.DecPoset.DPO.∼-resp-≈
d_'8764''45'resp'45''8776'_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_404 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_404 v3
du_'8764''45'resp'45''8776'_404 ::
  T_DecPoset_360 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_404 v0
  = let v1 = d_isDecPartialOrder_382 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Bundles.DecPoset.DPO.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_406 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_406 v3
du_'8764''45'resp'691''45''8776'_406 ::
  T_DecPoset_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_406 v0
  = let v1 = d_isDecPartialOrder_382 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Bundles.DecPoset.DPO.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_408 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_408 v3
du_'8764''45'resp'737''45''8776'_408 ::
  T_DecPoset_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_408 v0
  = let v1 = d_isDecPartialOrder_382 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Bundles.DecPoset.poset
d_poset_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> T_Poset_282
d_poset_428 ~v0 ~v1 ~v2 v3 = du_poset_428 v3
du_poset_428 :: T_DecPoset_360 -> T_Poset_282
du_poset_428 v0
  = coe
      C_Poset'46'constructor_5189
      (MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
         (coe d_isDecPartialOrder_382 (coe v0)))
-- Relation.Binary.Bundles.DecPoset._.preorder
d_preorder_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> T_Preorder_132
d_preorder_432 ~v0 ~v1 ~v2 v3 = du_preorder_432 v3
du_preorder_432 :: T_DecPoset_360 -> T_Preorder_132
du_preorder_432 v0
  = coe du_preorder_326 (coe du_poset_428 (coe v0))
-- Relation.Binary.Bundles.DecPoset.Eq.decSetoid
d_decSetoid_436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> T_DecSetoid_84
d_decSetoid_436 ~v0 ~v1 ~v2 v3 = du_decSetoid_436 v3
du_decSetoid_436 :: T_DecPoset_360 -> T_DecSetoid_84
du_decSetoid_436 v0
  = coe
      C_DecSetoid'46'constructor_1373
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du_isDecEquivalence_244
         (coe d_isDecPartialOrder_382 (coe v0)))
-- Relation.Binary.Bundles.DecPoset.Eq._._≈_
d__'8776'__440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> AgdaAny -> AgdaAny -> ()
d__'8776'__440 = erased
-- Relation.Binary.Bundles.DecPoset.Eq._._≉_
d__'8777'__442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> AgdaAny -> AgdaAny -> ()
d__'8777'__442 = erased
-- Relation.Binary.Bundles.DecPoset.Eq._._≟_
d__'8799'__444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__444 ~v0 ~v1 ~v2 v3 = du__'8799'__444 v3
du__'8799'__444 ::
  T_DecPoset_360 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__444 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8799'__218
      (coe d_isDecPartialOrder_382 (coe v0))
-- Relation.Binary.Bundles.DecPoset.Eq._.Carrier
d_Carrier_446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_DecPoset_360 -> ()
d_Carrier_446 = erased
-- Relation.Binary.Bundles.DecPoset.Eq._.isDecEquivalence
d_isDecEquivalence_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_isDecEquivalence_448 ~v0 ~v1 ~v2 v3 = du_isDecEquivalence_448 v3
du_isDecEquivalence_448 ::
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
du_isDecEquivalence_448 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isDecEquivalence_244
      (coe d_isDecPartialOrder_382 (coe v0))
-- Relation.Binary.Bundles.DecPoset.Eq._.isEquivalence
d_isEquivalence_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_450 ~v0 ~v1 ~v2 v3 = du_isEquivalence_450 v3
du_isEquivalence_450 ::
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_450 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
            (coe d_isDecPartialOrder_382 (coe v0))))
-- Relation.Binary.Bundles.DecPoset.Eq._.isPartialEquivalence
d_isPartialEquivalence_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_452 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_452 v3
du_isPartialEquivalence_452 ::
  T_DecPoset_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_452 v0
  = let v1 = coe du_decSetoid_436 (coe v0) in
    let v2 = d_isDecEquivalence_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
         (coe v2))
-- Relation.Binary.Bundles.DecPoset.Eq._.partialSetoid
d_partialSetoid_454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> T_PartialSetoid_10
d_partialSetoid_454 ~v0 ~v1 ~v2 v3 = du_partialSetoid_454 v3
du_partialSetoid_454 :: T_DecPoset_360 -> T_PartialSetoid_10
du_partialSetoid_454 v0
  = let v1 = coe du_decSetoid_436 (coe v0) in
    coe du_partialSetoid_74 (coe du_setoid_118 (coe v1))
-- Relation.Binary.Bundles.DecPoset.Eq._.refl
d_refl_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> AgdaAny -> AgdaAny
d_refl_456 ~v0 ~v1 ~v2 v3 = du_refl_456 v3
du_refl_456 :: T_DecPoset_360 -> AgdaAny -> AgdaAny
du_refl_456 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
               (coe d_isDecPartialOrder_382 (coe v0)))))
-- Relation.Binary.Bundles.DecPoset.Eq._.reflexive
d_reflexive_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_458 ~v0 ~v1 ~v2 v3 = du_reflexive_458 v3
du_reflexive_458 ::
  T_DecPoset_360 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_458 v0
  = let v1 = coe du_decSetoid_436 (coe v0) in
    let v2 = d_isDecEquivalence_100 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
           (coe v2))
        v3
-- Relation.Binary.Bundles.DecPoset.Eq._.setoid
d_setoid_460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> T_Setoid_44
d_setoid_460 ~v0 ~v1 ~v2 v3 = du_setoid_460 v3
du_setoid_460 :: T_DecPoset_360 -> T_Setoid_44
du_setoid_460 v0
  = coe du_setoid_118 (coe du_decSetoid_436 (coe v0))
-- Relation.Binary.Bundles.DecPoset.Eq._.sym
d_sym_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_462 ~v0 ~v1 ~v2 v3 = du_sym_462 v3
du_sym_462 ::
  T_DecPoset_360 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_462 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
               (coe d_isDecPartialOrder_382 (coe v0)))))
-- Relation.Binary.Bundles.DecPoset.Eq._.trans
d_trans_464 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecPoset_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_464 ~v0 ~v1 ~v2 v3 = du_trans_464 v3
du_trans_464 ::
  T_DecPoset_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_464 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
               (coe d_isDecPartialOrder_382 (coe v0)))))
-- Relation.Binary.Bundles.StrictPartialOrder
d_StrictPartialOrder_472 a0 a1 a2 = ()
newtype T_StrictPartialOrder_472
  = C_StrictPartialOrder'46'constructor_8915 MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
-- Relation.Binary.Bundles.StrictPartialOrder.Carrier
d_Carrier_488 :: T_StrictPartialOrder_472 -> ()
d_Carrier_488 = erased
-- Relation.Binary.Bundles.StrictPartialOrder._≈_
d__'8776'__490 ::
  T_StrictPartialOrder_472 -> AgdaAny -> AgdaAny -> ()
d__'8776'__490 = erased
-- Relation.Binary.Bundles.StrictPartialOrder._<_
d__'60'__492 ::
  T_StrictPartialOrder_472 -> AgdaAny -> AgdaAny -> ()
d__'60'__492 = erased
-- Relation.Binary.Bundles.StrictPartialOrder.isStrictPartialOrder
d_isStrictPartialOrder_494 ::
  T_StrictPartialOrder_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_isStrictPartialOrder_494 v0
  = case coe v0 of
      C_StrictPartialOrder'46'constructor_8915 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.StrictPartialOrder._.<-resp-≈
d_'60''45'resp'45''8776'_498 ::
  T_StrictPartialOrder_472 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8776'_498 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_'60''45'resp'45''8776'_284
      (coe d_isStrictPartialOrder_494 (coe v0))
-- Relation.Binary.Bundles.StrictPartialOrder._.<-respʳ-≈
d_'60''45'resp'691''45''8776'_500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'691''45''8776'_500 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'691''45''8776'_500 v3
du_'60''45'resp'691''45''8776'_500 ::
  T_StrictPartialOrder_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'691''45''8776'_500 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'691''45''8776'_304
      (coe d_isStrictPartialOrder_494 (coe v0))
-- Relation.Binary.Bundles.StrictPartialOrder._.<-respˡ-≈
d_'60''45'resp'737''45''8776'_502 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'737''45''8776'_502 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'737''45''8776'_502 v3
du_'60''45'resp'737''45''8776'_502 ::
  T_StrictPartialOrder_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'737''45''8776'_502 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'737''45''8776'_306
      (coe d_isStrictPartialOrder_494 (coe v0))
-- Relation.Binary.Bundles.StrictPartialOrder._.asym
d_asym_504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_504 = erased
-- Relation.Binary.Bundles.StrictPartialOrder._.irrefl
d_irrefl_506 ::
  T_StrictPartialOrder_472 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_506 = erased
-- Relation.Binary.Bundles.StrictPartialOrder._.isEquivalence
d_isEquivalence_508 ::
  T_StrictPartialOrder_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_508 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
      (coe d_isStrictPartialOrder_494 (coe v0))
-- Relation.Binary.Bundles.StrictPartialOrder._.trans
d_trans_510 ::
  T_StrictPartialOrder_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_510 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_282
      (coe d_isStrictPartialOrder_494 (coe v0))
-- Relation.Binary.Bundles.StrictPartialOrder.Eq.setoid
d_setoid_514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 -> T_Setoid_44
d_setoid_514 ~v0 ~v1 ~v2 v3 = du_setoid_514 v3
du_setoid_514 :: T_StrictPartialOrder_472 -> T_Setoid_44
du_setoid_514 v0
  = coe
      C_Setoid'46'constructor_719
      (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe d_isStrictPartialOrder_494 (coe v0)))
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._._≈_
d__'8776'__518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 -> AgdaAny -> AgdaAny -> ()
d__'8776'__518 = erased
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._._≉_
d__'8777'__520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 -> AgdaAny -> AgdaAny -> ()
d__'8777'__520 = erased
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._.Carrier
d_Carrier_522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 -> ()
d_Carrier_522 = erased
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._.isEquivalence
d_isEquivalence_524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_524 ~v0 ~v1 ~v2 v3 = du_isEquivalence_524 v3
du_isEquivalence_524 ::
  T_StrictPartialOrder_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_524 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
      (coe d_isStrictPartialOrder_494 (coe v0))
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._.isPartialEquivalence
d_isPartialEquivalence_526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_526 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_526 v3
du_isPartialEquivalence_526 ::
  T_StrictPartialOrder_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_526 v0
  = let v1 = coe du_setoid_514 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_60 (coe v1))
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._.partialSetoid
d_partialSetoid_528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 -> T_PartialSetoid_10
d_partialSetoid_528 ~v0 ~v1 ~v2 v3 = du_partialSetoid_528 v3
du_partialSetoid_528 ::
  T_StrictPartialOrder_472 -> T_PartialSetoid_10
du_partialSetoid_528 v0
  = coe du_partialSetoid_74 (coe du_setoid_514 (coe v0))
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._.refl
d_refl_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 -> AgdaAny -> AgdaAny
d_refl_530 ~v0 ~v1 ~v2 v3 = du_refl_530 v3
du_refl_530 :: T_StrictPartialOrder_472 -> AgdaAny -> AgdaAny
du_refl_530 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe d_isStrictPartialOrder_494 (coe v0)))
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._.reflexive
d_reflexive_532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_532 ~v0 ~v1 ~v2 v3 = du_reflexive_532 v3
du_reflexive_532 ::
  T_StrictPartialOrder_472 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_532 v0
  = let v1 = coe du_setoid_514 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_60 (coe v1)) v2
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._.sym
d_sym_534 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_534 ~v0 ~v1 ~v2 v3 = du_sym_534 v3
du_sym_534 ::
  T_StrictPartialOrder_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_534 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe d_isStrictPartialOrder_494 (coe v0)))
-- Relation.Binary.Bundles.StrictPartialOrder.Eq._.trans
d_trans_536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictPartialOrder_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_536 ~v0 ~v1 ~v2 v3 = du_trans_536 v3
du_trans_536 ::
  T_StrictPartialOrder_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_536 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe d_isStrictPartialOrder_494 (coe v0)))
-- Relation.Binary.Bundles.DecStrictPartialOrder
d_DecStrictPartialOrder_544 a0 a1 a2 = ()
newtype T_DecStrictPartialOrder_544
  = C_DecStrictPartialOrder'46'constructor_10331 MAlonzo.Code.Relation.Binary.Structures.T_IsDecStrictPartialOrder_312
-- Relation.Binary.Bundles.DecStrictPartialOrder.Carrier
d_Carrier_560 :: T_DecStrictPartialOrder_544 -> ()
d_Carrier_560 = erased
-- Relation.Binary.Bundles.DecStrictPartialOrder._≈_
d__'8776'__562 ::
  T_DecStrictPartialOrder_544 -> AgdaAny -> AgdaAny -> ()
d__'8776'__562 = erased
-- Relation.Binary.Bundles.DecStrictPartialOrder._<_
d__'60'__564 ::
  T_DecStrictPartialOrder_544 -> AgdaAny -> AgdaAny -> ()
d__'60'__564 = erased
-- Relation.Binary.Bundles.DecStrictPartialOrder.isDecStrictPartialOrder
d_isDecStrictPartialOrder_566 ::
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecStrictPartialOrder_312
d_isDecStrictPartialOrder_566 v0
  = case coe v0 of
      C_DecStrictPartialOrder'46'constructor_10331 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO._<?_
d__'60''63'__570 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__570 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'60''63'__326
      (coe d_isDecStrictPartialOrder_566 (coe v0))
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO._≟_
d__'8799'__572 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__572 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8799'__324
      (coe d_isDecStrictPartialOrder_566 (coe v0))
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO.<-resp-≈
d_'60''45'resp'45''8776'_574 ::
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8776'_574 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_'60''45'resp'45''8776'_284
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
         (coe d_isDecStrictPartialOrder_566 (coe v0)))
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO.<-respʳ-≈
d_'60''45'resp'691''45''8776'_576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'691''45''8776'_576 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'691''45''8776'_576 v3
du_'60''45'resp'691''45''8776'_576 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'691''45''8776'_576 v0
  = let v1 = d_isDecStrictPartialOrder_566 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'691''45''8776'_304
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
         (coe v1))
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO.<-respˡ-≈
d_'60''45'resp'737''45''8776'_578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'737''45''8776'_578 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'737''45''8776'_578 v3
du_'60''45'resp'737''45''8776'_578 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'737''45''8776'_578 v0
  = let v1 = d_isDecStrictPartialOrder_566 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'737''45''8776'_306
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
         (coe v1))
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO.asym
d_asym_580 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_580 = erased
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO.irrefl
d_irrefl_582 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_582 = erased
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO.isEquivalence
d_isEquivalence_584 ::
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_584 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
         (coe d_isDecStrictPartialOrder_566 (coe v0)))
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO.isStrictPartialOrder
d_isStrictPartialOrder_586 ::
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_isStrictPartialOrder_586 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
      (coe d_isDecStrictPartialOrder_566 (coe v0))
-- Relation.Binary.Bundles.DecStrictPartialOrder.DSPO.trans
d_trans_588 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_588 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_282
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
         (coe d_isDecStrictPartialOrder_566 (coe v0)))
-- Relation.Binary.Bundles.DecStrictPartialOrder.strictPartialOrder
d_strictPartialOrder_608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 -> T_StrictPartialOrder_472
d_strictPartialOrder_608 ~v0 ~v1 ~v2 v3
  = du_strictPartialOrder_608 v3
du_strictPartialOrder_608 ::
  T_DecStrictPartialOrder_544 -> T_StrictPartialOrder_472
du_strictPartialOrder_608 v0
  = coe
      C_StrictPartialOrder'46'constructor_8915
      (MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
         (coe d_isDecStrictPartialOrder_566 (coe v0)))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq.decSetoid
d_decSetoid_612 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 -> T_DecSetoid_84
d_decSetoid_612 ~v0 ~v1 ~v2 v3 = du_decSetoid_612 v3
du_decSetoid_612 :: T_DecStrictPartialOrder_544 -> T_DecSetoid_84
du_decSetoid_612 v0
  = coe
      C_DecSetoid'46'constructor_1373
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du_isDecEquivalence_358
         (coe d_isDecStrictPartialOrder_566 (coe v0)))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._._≈_
d__'8776'__616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 -> AgdaAny -> AgdaAny -> ()
d__'8776'__616 = erased
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._._≉_
d__'8777'__618 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 -> AgdaAny -> AgdaAny -> ()
d__'8777'__618 = erased
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._._≟_
d__'8799'__620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__620 ~v0 ~v1 ~v2 v3 = du__'8799'__620 v3
du__'8799'__620 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__620 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8799'__324
      (coe d_isDecStrictPartialOrder_566 (coe v0))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.Carrier
d_Carrier_622 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 -> ()
d_Carrier_622 = erased
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.isDecEquivalence
d_isDecEquivalence_624 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_isDecEquivalence_624 ~v0 ~v1 ~v2 v3 = du_isDecEquivalence_624 v3
du_isDecEquivalence_624 ::
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
du_isDecEquivalence_624 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isDecEquivalence_358
      (coe d_isDecStrictPartialOrder_566 (coe v0))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.isEquivalence
d_isEquivalence_626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_626 ~v0 ~v1 ~v2 v3 = du_isEquivalence_626 v3
du_isEquivalence_626 ::
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_626 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
         (coe d_isDecStrictPartialOrder_566 (coe v0)))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.isPartialEquivalence
d_isPartialEquivalence_628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_628 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_628 v3
du_isPartialEquivalence_628 ::
  T_DecStrictPartialOrder_544 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_628 v0
  = let v1 = coe du_decSetoid_612 (coe v0) in
    let v2 = d_isDecEquivalence_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
         (coe v2))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.partialSetoid
d_partialSetoid_630 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 -> T_PartialSetoid_10
d_partialSetoid_630 ~v0 ~v1 ~v2 v3 = du_partialSetoid_630 v3
du_partialSetoid_630 ::
  T_DecStrictPartialOrder_544 -> T_PartialSetoid_10
du_partialSetoid_630 v0
  = let v1 = coe du_decSetoid_612 (coe v0) in
    coe du_partialSetoid_74 (coe du_setoid_118 (coe v1))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.refl
d_refl_632 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 -> AgdaAny -> AgdaAny
d_refl_632 ~v0 ~v1 ~v2 v3 = du_refl_632 v3
du_refl_632 :: T_DecStrictPartialOrder_544 -> AgdaAny -> AgdaAny
du_refl_632 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
            (coe d_isDecStrictPartialOrder_566 (coe v0))))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.reflexive
d_reflexive_634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_634 ~v0 ~v1 ~v2 v3 = du_reflexive_634 v3
du_reflexive_634 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_634 v0
  = let v1 = coe du_decSetoid_612 (coe v0) in
    let v2 = d_isDecEquivalence_100 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
           (coe v2))
        v3
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.setoid
d_setoid_636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 -> T_Setoid_44
d_setoid_636 ~v0 ~v1 ~v2 v3 = du_setoid_636 v3
du_setoid_636 :: T_DecStrictPartialOrder_544 -> T_Setoid_44
du_setoid_636 v0
  = coe du_setoid_118 (coe du_decSetoid_612 (coe v0))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.sym
d_sym_638 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_638 ~v0 ~v1 ~v2 v3 = du_sym_638 v3
du_sym_638 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_638 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
            (coe d_isDecStrictPartialOrder_566 (coe v0))))
-- Relation.Binary.Bundles.DecStrictPartialOrder.Eq._.trans
d_trans_640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecStrictPartialOrder_544 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_640 ~v0 ~v1 ~v2 v3 = du_trans_640 v3
du_trans_640 ::
  T_DecStrictPartialOrder_544 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_640 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isStrictPartialOrder_322
            (coe d_isDecStrictPartialOrder_566 (coe v0))))
-- Relation.Binary.Bundles.TotalOrder
d_TotalOrder_648 a0 a1 a2 = ()
newtype T_TotalOrder_648
  = C_TotalOrder'46'constructor_12355 MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
-- Relation.Binary.Bundles.TotalOrder.Carrier
d_Carrier_664 :: T_TotalOrder_648 -> ()
d_Carrier_664 = erased
-- Relation.Binary.Bundles.TotalOrder._≈_
d__'8776'__666 :: T_TotalOrder_648 -> AgdaAny -> AgdaAny -> ()
d__'8776'__666 = erased
-- Relation.Binary.Bundles.TotalOrder._≤_
d__'8804'__668 :: T_TotalOrder_648 -> AgdaAny -> AgdaAny -> ()
d__'8804'__668 = erased
-- Relation.Binary.Bundles.TotalOrder.isTotalOrder
d_isTotalOrder_670 ::
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_isTotalOrder_670 v0
  = case coe v0 of
      C_TotalOrder'46'constructor_12355 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.TotalOrder._.antisym
d_antisym_674 ::
  T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_674 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
         (coe d_isTotalOrder_670 (coe v0)))
-- Relation.Binary.Bundles.TotalOrder._.isEquivalence
d_isEquivalence_676 ::
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_676 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
            (coe d_isTotalOrder_670 (coe v0))))
-- Relation.Binary.Bundles.TotalOrder._.isPartialOrder
d_isPartialOrder_678 ::
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_678 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
      (coe d_isTotalOrder_670 (coe v0))
-- Relation.Binary.Bundles.TotalOrder._.isPreorder
d_isPreorder_680 ::
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_680 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
         (coe d_isTotalOrder_670 (coe v0)))
-- Relation.Binary.Bundles.TotalOrder._.isTotalPreorder
d_isTotalPreorder_682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
d_isTotalPreorder_682 ~v0 ~v1 ~v2 v3 = du_isTotalPreorder_682 v3
du_isTotalPreorder_682 ::
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
du_isTotalPreorder_682 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isTotalPreorder_424
      (coe d_isTotalOrder_670 (coe v0))
-- Relation.Binary.Bundles.TotalOrder._.refl
d_refl_684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> AgdaAny -> AgdaAny
d_refl_684 ~v0 ~v1 ~v2 v3 = du_refl_684 v3
du_refl_684 :: T_TotalOrder_648 -> AgdaAny -> AgdaAny
du_refl_684 v0
  = let v1 = d_isTotalOrder_670 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Bundles.TotalOrder._.reflexive
d_reflexive_686 ::
  T_TotalOrder_648 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_686 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
            (coe d_isTotalOrder_670 (coe v0))))
-- Relation.Binary.Bundles.TotalOrder._.total
d_total_688 ::
  T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_688 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_total_390
      (coe d_isTotalOrder_670 (coe v0))
-- Relation.Binary.Bundles.TotalOrder._.trans
d_trans_690 ::
  T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_690 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
            (coe d_isTotalOrder_670 (coe v0))))
-- Relation.Binary.Bundles.TotalOrder._.∼-resp-≈
d_'8764''45'resp'45''8776'_692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_692 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_692 v3
du_'8764''45'resp'45''8776'_692 ::
  T_TotalOrder_648 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_692 v0
  = let v1 = d_isTotalOrder_670 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Bundles.TotalOrder._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_694 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_694 v3
du_'8764''45'resp'691''45''8776'_694 ::
  T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_694 v0
  = let v1 = d_isTotalOrder_670 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Bundles.TotalOrder._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_696 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_696 v3
du_'8764''45'resp'737''45''8776'_696 ::
  T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_696 v0
  = let v1 = d_isTotalOrder_670 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v2))
-- Relation.Binary.Bundles.TotalOrder.poset
d_poset_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> T_Poset_282
d_poset_698 ~v0 ~v1 ~v2 v3 = du_poset_698 v3
du_poset_698 :: T_TotalOrder_648 -> T_Poset_282
du_poset_698 v0
  = coe
      C_Poset'46'constructor_5189
      (MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
         (coe d_isTotalOrder_670 (coe v0)))
-- Relation.Binary.Bundles.TotalOrder._.preorder
d_preorder_702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> T_Preorder_132
d_preorder_702 ~v0 ~v1 ~v2 v3 = du_preorder_702 v3
du_preorder_702 :: T_TotalOrder_648 -> T_Preorder_132
du_preorder_702 v0
  = coe du_preorder_326 (coe du_poset_698 (coe v0))
-- Relation.Binary.Bundles.TotalOrder._.Eq._≈_
d__'8776'__706 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> AgdaAny -> AgdaAny -> ()
d__'8776'__706 = erased
-- Relation.Binary.Bundles.TotalOrder._.Eq._≉_
d__'8777'__708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> AgdaAny -> AgdaAny -> ()
d__'8777'__708 = erased
-- Relation.Binary.Bundles.TotalOrder._.Eq.Carrier
d_Carrier_710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_TotalOrder_648 -> ()
d_Carrier_710 = erased
-- Relation.Binary.Bundles.TotalOrder._.Eq.isEquivalence
d_isEquivalence_712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_712 ~v0 ~v1 ~v2 v3 = du_isEquivalence_712 v3
du_isEquivalence_712 ::
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_712 v0
  = let v1 = coe du_poset_698 (coe v0) in
    let v2 = coe du_preorder_326 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe d_isPreorder_154 (coe v2))
-- Relation.Binary.Bundles.TotalOrder._.Eq.isPartialEquivalence
d_isPartialEquivalence_714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_714 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_714 v3
du_isPartialEquivalence_714 ::
  T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_714 v0
  = let v1 = coe du_poset_698 (coe v0) in
    let v2 = coe du_preorder_326 (coe v1) in
    let v3 = coe du_setoid_174 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_60 (coe v3))
-- Relation.Binary.Bundles.TotalOrder._.Eq.partialSetoid
d_partialSetoid_716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> T_PartialSetoid_10
d_partialSetoid_716 ~v0 ~v1 ~v2 v3 = du_partialSetoid_716 v3
du_partialSetoid_716 :: T_TotalOrder_648 -> T_PartialSetoid_10
du_partialSetoid_716 v0
  = let v1 = coe du_poset_698 (coe v0) in
    let v2 = coe du_preorder_326 (coe v1) in
    coe du_partialSetoid_74 (coe du_setoid_174 (coe v2))
-- Relation.Binary.Bundles.TotalOrder._.Eq.refl
d_refl_718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> AgdaAny -> AgdaAny
d_refl_718 ~v0 ~v1 ~v2 v3 = du_refl_718 v3
du_refl_718 :: T_TotalOrder_648 -> AgdaAny -> AgdaAny
du_refl_718 v0
  = let v1 = coe du_poset_698 (coe v0) in
    let v2 = coe du_preorder_326 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v2)))
-- Relation.Binary.Bundles.TotalOrder._.Eq.reflexive
d_reflexive_720 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_720 ~v0 ~v1 ~v2 v3 = du_reflexive_720 v3
du_reflexive_720 ::
  T_TotalOrder_648 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_720 v0
  = let v1 = coe du_poset_698 (coe v0) in
    let v2 = coe du_preorder_326 (coe v1) in
    let v3 = coe du_setoid_174 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_60 (coe v3)) v4
-- Relation.Binary.Bundles.TotalOrder._.Eq.setoid
d_setoid_722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> T_Setoid_44
d_setoid_722 ~v0 ~v1 ~v2 v3 = du_setoid_722 v3
du_setoid_722 :: T_TotalOrder_648 -> T_Setoid_44
du_setoid_722 v0
  = let v1 = coe du_poset_698 (coe v0) in
    coe du_setoid_174 (coe du_preorder_326 (coe v1))
-- Relation.Binary.Bundles.TotalOrder._.Eq.sym
d_sym_724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_724 ~v0 ~v1 ~v2 v3 = du_sym_724 v3
du_sym_724 ::
  T_TotalOrder_648 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_724 v0
  = let v1 = coe du_poset_698 (coe v0) in
    let v2 = coe du_preorder_326 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v2)))
-- Relation.Binary.Bundles.TotalOrder._.Eq.trans
d_trans_726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_726 ~v0 ~v1 ~v2 v3 = du_trans_726 v3
du_trans_726 ::
  T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_726 v0
  = let v1 = coe du_poset_698 (coe v0) in
    let v2 = coe du_preorder_326 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe d_isPreorder_154 (coe v2)))
-- Relation.Binary.Bundles.TotalOrder.totalPreorder
d_totalPreorder_728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_TotalOrder_648 -> T_TotalPreorder_204
d_totalPreorder_728 ~v0 ~v1 ~v2 v3 = du_totalPreorder_728 v3
du_totalPreorder_728 :: T_TotalOrder_648 -> T_TotalPreorder_204
du_totalPreorder_728 v0
  = coe
      C_TotalPreorder'46'constructor_3645
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du_isTotalPreorder_424
         (coe d_isTotalOrder_670 (coe v0)))
-- Relation.Binary.Bundles.DecTotalOrder
d_DecTotalOrder_736 a0 a1 a2 = ()
newtype T_DecTotalOrder_736
  = C_DecTotalOrder'46'constructor_14197 MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
-- Relation.Binary.Bundles.DecTotalOrder.Carrier
d_Carrier_752 :: T_DecTotalOrder_736 -> ()
d_Carrier_752 = erased
-- Relation.Binary.Bundles.DecTotalOrder._≈_
d__'8776'__754 :: T_DecTotalOrder_736 -> AgdaAny -> AgdaAny -> ()
d__'8776'__754 = erased
-- Relation.Binary.Bundles.DecTotalOrder._≤_
d__'8804'__756 :: T_DecTotalOrder_736 -> AgdaAny -> AgdaAny -> ()
d__'8804'__756 = erased
-- Relation.Binary.Bundles.DecTotalOrder.isDecTotalOrder
d_isDecTotalOrder_758 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_isDecTotalOrder_758 v0
  = case coe v0 of
      C_DecTotalOrder'46'constructor_14197 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.DecTotalOrder.DTO._≟_
d__'8799'__762 ::
  T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__762 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8799'__442
      (coe d_isDecTotalOrder_758 (coe v0))
-- Relation.Binary.Bundles.DecTotalOrder.DTO._≤?_
d__'8804''63'__764 ::
  T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__764 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
      (coe d_isDecTotalOrder_758 (coe v0))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.antisym
d_antisym_766 ::
  T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_766 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
            (coe d_isDecTotalOrder_758 (coe v0))))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.isDecPartialOrder
d_isDecPartialOrder_768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
d_isDecPartialOrder_768 ~v0 ~v1 ~v2 v3
  = du_isDecPartialOrder_768 v3
du_isDecPartialOrder_768 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
du_isDecPartialOrder_768 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isDecPartialOrder_472
      (coe d_isDecTotalOrder_758 (coe v0))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.isEquivalence
d_isEquivalence_770 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_770 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
               (coe d_isDecTotalOrder_758 (coe v0)))))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.isPartialOrder
d_isPartialOrder_772 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_772 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
         (coe d_isDecTotalOrder_758 (coe v0)))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.isPreorder
d_isPreorder_774 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_774 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
            (coe d_isDecTotalOrder_758 (coe v0))))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.isTotalOrder
d_isTotalOrder_776 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_isTotalOrder_776 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
      (coe d_isDecTotalOrder_758 (coe v0))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.isTotalPreorder
d_isTotalPreorder_778 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
d_isTotalPreorder_778 ~v0 ~v1 ~v2 v3 = du_isTotalPreorder_778 v3
du_isTotalPreorder_778 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
du_isTotalPreorder_778 v0
  = let v1 = d_isDecTotalOrder_758 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isTotalPreorder_424
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
         (coe v1))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.refl
d_refl_780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> AgdaAny -> AgdaAny
d_refl_780 ~v0 ~v1 ~v2 v3 = du_refl_780 v3
du_refl_780 :: T_DecTotalOrder_736 -> AgdaAny -> AgdaAny
du_refl_780 v0
  = let v1 = d_isDecTotalOrder_758 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_refl_98
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.reflexive
d_reflexive_782 ::
  T_DecTotalOrder_736 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_782 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
               (coe d_isDecTotalOrder_758 (coe v0)))))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.total
d_total_784 ::
  T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_784 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_total_390
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
         (coe d_isDecTotalOrder_758 (coe v0)))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.trans
d_trans_786 ::
  T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_786 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
               (coe d_isDecTotalOrder_758 (coe v0)))))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.∼-resp-≈
d_'8764''45'resp'45''8776'_788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_788 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'45''8776'_788 v3
du_'8764''45'resp'45''8776'_788 ::
  T_DecTotalOrder_736 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_788 v0
  = let v1 = d_isDecTotalOrder_758 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_790 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'691''45''8776'_790 v3
du_'8764''45'resp'691''45''8776'_790 ::
  T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_790 v0
  = let v1 = d_isDecTotalOrder_758 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Bundles.DecTotalOrder.DTO.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_792 ~v0 ~v1 ~v2 v3
  = du_'8764''45'resp'737''45''8776'_792 v3
du_'8764''45'resp'737''45''8776'_792 ::
  T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_792 v0
  = let v1 = d_isDecTotalOrder_758 (coe v0) in
    let v2
          = MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
              (coe v1) in
    let v3
          = MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v3))
-- Relation.Binary.Bundles.DecTotalOrder.totalOrder
d_totalOrder_812 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> T_TotalOrder_648
d_totalOrder_812 ~v0 ~v1 ~v2 v3 = du_totalOrder_812 v3
du_totalOrder_812 :: T_DecTotalOrder_736 -> T_TotalOrder_648
du_totalOrder_812 v0
  = coe
      C_TotalOrder'46'constructor_12355
      (MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
         (coe d_isDecTotalOrder_758 (coe v0)))
-- Relation.Binary.Bundles.DecTotalOrder._.poset
d_poset_816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> T_Poset_282
d_poset_816 ~v0 ~v1 ~v2 v3 = du_poset_816 v3
du_poset_816 :: T_DecTotalOrder_736 -> T_Poset_282
du_poset_816 v0 = coe du_poset_698 (coe du_totalOrder_812 (coe v0))
-- Relation.Binary.Bundles.DecTotalOrder._.preorder
d_preorder_818 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> T_Preorder_132
d_preorder_818 ~v0 ~v1 ~v2 v3 = du_preorder_818 v3
du_preorder_818 :: T_DecTotalOrder_736 -> T_Preorder_132
du_preorder_818 v0
  = let v1 = coe du_totalOrder_812 (coe v0) in
    coe du_preorder_326 (coe du_poset_698 (coe v1))
-- Relation.Binary.Bundles.DecTotalOrder.decPoset
d_decPoset_820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> T_DecPoset_360
d_decPoset_820 ~v0 ~v1 ~v2 v3 = du_decPoset_820 v3
du_decPoset_820 :: T_DecTotalOrder_736 -> T_DecPoset_360
du_decPoset_820 v0
  = coe
      C_DecPoset'46'constructor_6741
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du_isDecPartialOrder_472
         (coe d_isDecTotalOrder_758 (coe v0)))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq._≈_
d__'8776'__826 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> AgdaAny -> AgdaAny -> ()
d__'8776'__826 = erased
-- Relation.Binary.Bundles.DecTotalOrder._.Eq._≉_
d__'8777'__828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> AgdaAny -> AgdaAny -> ()
d__'8777'__828 = erased
-- Relation.Binary.Bundles.DecTotalOrder._.Eq._≟_
d__'8799'__830 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__830 ~v0 ~v1 ~v2 v3 = du__'8799'__830 v3
du__'8799'__830 ::
  T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__830 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d__'8799'__218
      (coe d_isDecPartialOrder_382 (coe v1))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.Carrier
d_Carrier_832 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_DecTotalOrder_736 -> ()
d_Carrier_832 = erased
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.decSetoid
d_decSetoid_834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> T_DecSetoid_84
d_decSetoid_834 ~v0 ~v1 ~v2 v3 = du_decSetoid_834 v3
du_decSetoid_834 :: T_DecTotalOrder_736 -> T_DecSetoid_84
du_decSetoid_834 v0
  = coe du_decSetoid_436 (coe du_decPoset_820 (coe v0))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.isDecEquivalence
d_isDecEquivalence_836 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_isDecEquivalence_836 ~v0 ~v1 ~v2 v3 = du_isDecEquivalence_836 v3
du_isDecEquivalence_836 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
du_isDecEquivalence_836 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isDecEquivalence_244
      (coe d_isDecPartialOrder_382 (coe v1))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.isEquivalence
d_isEquivalence_838 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_838 ~v0 ~v1 ~v2 v3 = du_isEquivalence_838 v3
du_isEquivalence_838 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_838 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
            (coe d_isDecPartialOrder_382 (coe v1))))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.isPartialEquivalence
d_isPartialEquivalence_840 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_840 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_840 v3
du_isPartialEquivalence_840 ::
  T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_840 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    let v2 = coe du_decSetoid_436 (coe v1) in
    let v3 = d_isDecEquivalence_100 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
         (coe v3))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.partialSetoid
d_partialSetoid_842 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> T_PartialSetoid_10
d_partialSetoid_842 ~v0 ~v1 ~v2 v3 = du_partialSetoid_842 v3
du_partialSetoid_842 :: T_DecTotalOrder_736 -> T_PartialSetoid_10
du_partialSetoid_842 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    let v2 = coe du_decSetoid_436 (coe v1) in
    coe du_partialSetoid_74 (coe du_setoid_118 (coe v2))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.refl
d_refl_844 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> AgdaAny -> AgdaAny
d_refl_844 ~v0 ~v1 ~v2 v3 = du_refl_844 v3
du_refl_844 :: T_DecTotalOrder_736 -> AgdaAny -> AgdaAny
du_refl_844 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
               (coe d_isDecPartialOrder_382 (coe v1)))))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.reflexive
d_reflexive_846 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_846 ~v0 ~v1 ~v2 v3 = du_reflexive_846 v3
du_reflexive_846 ::
  T_DecTotalOrder_736 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_846 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    let v2 = coe du_decSetoid_436 (coe v1) in
    let v3 = d_isDecEquivalence_100 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
           (coe v3))
        v4
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.setoid
d_setoid_848 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> T_Setoid_44
d_setoid_848 ~v0 ~v1 ~v2 v3 = du_setoid_848 v3
du_setoid_848 :: T_DecTotalOrder_736 -> T_Setoid_44
du_setoid_848 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    coe du_setoid_118 (coe du_decSetoid_436 (coe v1))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.sym
d_sym_850 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_850 ~v0 ~v1 ~v2 v3 = du_sym_850 v3
du_sym_850 ::
  T_DecTotalOrder_736 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_850 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
               (coe d_isDecPartialOrder_382 (coe v1)))))
-- Relation.Binary.Bundles.DecTotalOrder._.Eq.trans
d_trans_852 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_852 ~v0 ~v1 ~v2 v3 = du_trans_852 v3
du_trans_852 ::
  T_DecTotalOrder_736 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_852 v0
  = let v1 = coe du_decPoset_820 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_216
               (coe d_isDecPartialOrder_382 (coe v1)))))
-- Relation.Binary.Bundles.StrictTotalOrder
d_StrictTotalOrder_860 a0 a1 a2 = ()
newtype T_StrictTotalOrder_860
  = C_StrictTotalOrder'46'constructor_16593 MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
-- Relation.Binary.Bundles.StrictTotalOrder.Carrier
d_Carrier_876 :: T_StrictTotalOrder_860 -> ()
d_Carrier_876 = erased
-- Relation.Binary.Bundles.StrictTotalOrder._≈_
d__'8776'__878 ::
  T_StrictTotalOrder_860 -> AgdaAny -> AgdaAny -> ()
d__'8776'__878 = erased
-- Relation.Binary.Bundles.StrictTotalOrder._<_
d__'60'__880 :: T_StrictTotalOrder_860 -> AgdaAny -> AgdaAny -> ()
d__'60'__880 = erased
-- Relation.Binary.Bundles.StrictTotalOrder.isStrictTotalOrder
d_isStrictTotalOrder_882 ::
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_isStrictTotalOrder_882 v0
  = case coe v0 of
      C_StrictTotalOrder'46'constructor_16593 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.StrictTotalOrder._._<?_
d__'60''63'__886 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__886 ~v0 ~v1 ~v2 v3 = du__'60''63'__886 v3
du__'60''63'__886 ::
  T_StrictTotalOrder_860 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'60''63'__886 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du__'60''63'__516
      (coe d_isStrictTotalOrder_882 (coe v0))
-- Relation.Binary.Bundles.StrictTotalOrder._._≟_
d__'8799'__888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__888 ~v0 ~v1 ~v2 v3 = du__'8799'__888 v3
du__'8799'__888 ::
  T_StrictTotalOrder_860 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__888 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du__'8799'__514
      (coe d_isStrictTotalOrder_882 (coe v0))
-- Relation.Binary.Bundles.StrictTotalOrder._.<-resp-≈
d_'60''45'resp'45''8776'_890 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8776'_890 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'45''8776'_890 v3
du_'60''45'resp'45''8776'_890 ::
  T_StrictTotalOrder_860 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'45''8776'_890 v0
  = let v1 = d_isStrictTotalOrder_882 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Consequences.du_trans'8743'tri'8658'resp_716
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_compare_512 (coe v1))
-- Relation.Binary.Bundles.StrictTotalOrder._.<-respʳ-≈
d_'60''45'resp'691''45''8776'_892 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'691''45''8776'_892 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'691''45''8776'_892 v3
du_'60''45'resp'691''45''8776'_892 ::
  T_StrictTotalOrder_860 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'691''45''8776'_892 v0
  = let v1 = d_isStrictTotalOrder_882 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'691''45''8776'_304
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du_isStrictPartialOrder_536
         (coe v1))
-- Relation.Binary.Bundles.StrictTotalOrder._.<-respˡ-≈
d_'60''45'resp'737''45''8776'_894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'737''45''8776'_894 ~v0 ~v1 ~v2 v3
  = du_'60''45'resp'737''45''8776'_894 v3
du_'60''45'resp'737''45''8776'_894 ::
  T_StrictTotalOrder_860 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'737''45''8776'_894 v0
  = let v1 = d_isStrictTotalOrder_882 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'60''45'resp'737''45''8776'_306
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du_isStrictPartialOrder_536
         (coe v1))
-- Relation.Binary.Bundles.StrictTotalOrder._.asym
d_asym_896 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_896 = erased
-- Relation.Binary.Bundles.StrictTotalOrder._.compare
d_compare_898 ::
  T_StrictTotalOrder_860 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_compare_898 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_compare_512
      (coe d_isStrictTotalOrder_882 (coe v0))
-- Relation.Binary.Bundles.StrictTotalOrder._.irrefl
d_irrefl_900 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_900 = erased
-- Relation.Binary.Bundles.StrictTotalOrder._.isDecEquivalence
d_isDecEquivalence_902 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_isDecEquivalence_902 ~v0 ~v1 ~v2 v3 = du_isDecEquivalence_902 v3
du_isDecEquivalence_902 ::
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
du_isDecEquivalence_902 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isDecEquivalence_518
      (coe d_isStrictTotalOrder_882 (coe v0))
-- Relation.Binary.Bundles.StrictTotalOrder._.isDecStrictPartialOrder
d_isDecStrictPartialOrder_904 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecStrictPartialOrder_312
d_isDecStrictPartialOrder_904 ~v0 ~v1 ~v2 v3
  = du_isDecStrictPartialOrder_904 v3
du_isDecStrictPartialOrder_904 ::
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecStrictPartialOrder_312
du_isDecStrictPartialOrder_904 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isDecStrictPartialOrder_538
      (coe d_isStrictTotalOrder_882 (coe v0))
-- Relation.Binary.Bundles.StrictTotalOrder._.isEquivalence
d_isEquivalence_906 ::
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_906 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_508
      (coe d_isStrictTotalOrder_882 (coe v0))
-- Relation.Binary.Bundles.StrictTotalOrder._.isStrictPartialOrder
d_isStrictPartialOrder_908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_isStrictPartialOrder_908 ~v0 ~v1 ~v2 v3
  = du_isStrictPartialOrder_908 v3
du_isStrictPartialOrder_908 ::
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
du_isStrictPartialOrder_908 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isStrictPartialOrder_536
      (coe d_isStrictTotalOrder_882 (coe v0))
-- Relation.Binary.Bundles.StrictTotalOrder._.trans
d_trans_910 ::
  T_StrictTotalOrder_860 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_910 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_510
      (coe d_isStrictTotalOrder_882 (coe v0))
-- Relation.Binary.Bundles.StrictTotalOrder.strictPartialOrder
d_strictPartialOrder_912 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> T_StrictPartialOrder_472
d_strictPartialOrder_912 ~v0 ~v1 ~v2 v3
  = du_strictPartialOrder_912 v3
du_strictPartialOrder_912 ::
  T_StrictTotalOrder_860 -> T_StrictPartialOrder_472
du_strictPartialOrder_912 v0
  = coe
      C_StrictPartialOrder'46'constructor_8915
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du_isStrictPartialOrder_536
         (coe d_isStrictTotalOrder_882 (coe v0)))
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq._≈_
d__'8776'__918 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> AgdaAny -> AgdaAny -> ()
d__'8776'__918 = erased
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq._≉_
d__'8777'__920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> AgdaAny -> AgdaAny -> ()
d__'8777'__920 = erased
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq.Carrier
d_Carrier_922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> ()
d_Carrier_922 = erased
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq.isEquivalence
d_isEquivalence_924 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_924 ~v0 ~v1 ~v2 v3 = du_isEquivalence_924 v3
du_isEquivalence_924 ::
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_924 v0
  = let v1 = coe du_strictPartialOrder_912 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
      (coe d_isStrictPartialOrder_494 (coe v1))
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq.isPartialEquivalence
d_isPartialEquivalence_926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_926 ~v0 ~v1 ~v2 v3
  = du_isPartialEquivalence_926 v3
du_isPartialEquivalence_926 ::
  T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_926 v0
  = let v1 = coe du_strictPartialOrder_912 (coe v0) in
    let v2 = coe du_setoid_514 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_60 (coe v2))
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq.partialSetoid
d_partialSetoid_928 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> T_PartialSetoid_10
d_partialSetoid_928 ~v0 ~v1 ~v2 v3 = du_partialSetoid_928 v3
du_partialSetoid_928 ::
  T_StrictTotalOrder_860 -> T_PartialSetoid_10
du_partialSetoid_928 v0
  = let v1 = coe du_strictPartialOrder_912 (coe v0) in
    coe du_partialSetoid_74 (coe du_setoid_514 (coe v1))
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq.refl
d_refl_930 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> AgdaAny -> AgdaAny
d_refl_930 ~v0 ~v1 ~v2 v3 = du_refl_930 v3
du_refl_930 :: T_StrictTotalOrder_860 -> AgdaAny -> AgdaAny
du_refl_930 v0
  = let v1 = coe du_strictPartialOrder_912 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe d_isStrictPartialOrder_494 (coe v1)))
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq.reflexive
d_reflexive_932 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_932 ~v0 ~v1 ~v2 v3 = du_reflexive_932 v3
du_reflexive_932 ::
  T_StrictTotalOrder_860 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_932 v0
  = let v1 = coe du_strictPartialOrder_912 (coe v0) in
    let v2 = coe du_setoid_514 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_60 (coe v2)) v3
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq.setoid
d_setoid_934 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> T_Setoid_44
d_setoid_934 ~v0 ~v1 ~v2 v3 = du_setoid_934 v3
du_setoid_934 :: T_StrictTotalOrder_860 -> T_Setoid_44
du_setoid_934 v0
  = coe du_setoid_514 (coe du_strictPartialOrder_912 (coe v0))
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq.sym
d_sym_936 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_936 ~v0 ~v1 ~v2 v3 = du_sym_936 v3
du_sym_936 ::
  T_StrictTotalOrder_860 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_936 v0
  = let v1 = coe du_strictPartialOrder_912 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe d_isStrictPartialOrder_494 (coe v1)))
-- Relation.Binary.Bundles.StrictTotalOrder._.Eq.trans
d_trans_938 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_938 ~v0 ~v1 ~v2 v3 = du_trans_938 v3
du_trans_938 ::
  T_StrictTotalOrder_860 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_938 v0
  = let v1 = coe du_strictPartialOrder_912 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe d_isStrictPartialOrder_494 (coe v1)))
-- Relation.Binary.Bundles.StrictTotalOrder.decSetoid
d_decSetoid_940 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_StrictTotalOrder_860 -> T_DecSetoid_84
d_decSetoid_940 ~v0 ~v1 ~v2 v3 = du_decSetoid_940 v3
du_decSetoid_940 :: T_StrictTotalOrder_860 -> T_DecSetoid_84
du_decSetoid_940 v0
  = coe
      C_DecSetoid'46'constructor_1373
      (coe
         MAlonzo.Code.Relation.Binary.Structures.du_isDecEquivalence_518
         (coe d_isStrictTotalOrder_882 (coe v0)))
-- Relation.Binary.Bundles.ApartnessRelation
d_ApartnessRelation_948 a0 a1 a2 = ()
newtype T_ApartnessRelation_948
  = C_ApartnessRelation'46'constructor_18393 MAlonzo.Code.Relation.Binary.Structures.T_IsApartnessRelation_556
-- Relation.Binary.Bundles.ApartnessRelation.Carrier
d_Carrier_964 :: T_ApartnessRelation_948 -> ()
d_Carrier_964 = erased
-- Relation.Binary.Bundles.ApartnessRelation._≈_
d__'8776'__966 ::
  T_ApartnessRelation_948 -> AgdaAny -> AgdaAny -> ()
d__'8776'__966 = erased
-- Relation.Binary.Bundles.ApartnessRelation._#_
d__'35'__968 :: T_ApartnessRelation_948 -> AgdaAny -> AgdaAny -> ()
d__'35'__968 = erased
-- Relation.Binary.Bundles.ApartnessRelation.isApartnessRelation
d_isApartnessRelation_970 ::
  T_ApartnessRelation_948 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsApartnessRelation_556
d_isApartnessRelation_970 v0
  = case coe v0 of
      C_ApartnessRelation'46'constructor_18393 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Bundles.ApartnessRelation._._¬#_
d__'172''35'__974 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_ApartnessRelation_948 -> AgdaAny -> AgdaAny -> ()
d__'172''35'__974 = erased
-- Relation.Binary.Bundles.ApartnessRelation._.cotrans
d_cotrans_976 ::
  T_ApartnessRelation_948 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_cotrans_976 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_cotrans_570
      (coe d_isApartnessRelation_970 (coe v0))
-- Relation.Binary.Bundles.ApartnessRelation._.irrefl
d_irrefl_978 ::
  T_ApartnessRelation_948 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_978 = erased
-- Relation.Binary.Bundles.ApartnessRelation._.sym
d_sym_980 ::
  T_ApartnessRelation_948 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_980 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_568
      (coe d_isApartnessRelation_970 (coe v0))
