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

module MAlonzo.Code.Function.Structures where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Structures.IsCongruent
d_IsCongruent_22 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsCongruent_22
  = C_IsCongruent'46'constructor_985 (AgdaAny ->
                                      AgdaAny -> AgdaAny -> AgdaAny)
                                     MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
                                     MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
-- Function.Structures.IsCongruent.cong
d_cong_32 ::
  T_IsCongruent_22 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_32 v0
  = case coe v0 of
      C_IsCongruent'46'constructor_985 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsCongruent.isEquivalence₁
d_isEquivalence'8321'_34 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8321'_34 v0
  = case coe v0 of
      C_IsCongruent'46'constructor_985 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsCongruent.isEquivalence₂
d_isEquivalence'8322'_36 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8322'_36 v0
  = case coe v0 of
      C_IsCongruent'46'constructor_985 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsCongruent.Eq₁.setoid
d_setoid_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_40 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_40 v9
du_setoid_40 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_40 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (d_isEquivalence'8321'_34 (coe v0))
-- Function.Structures.IsCongruent.Eq₁._._≈_
d__'8776'__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 -> AgdaAny -> AgdaAny -> ()
d__'8776'__44 = erased
-- Function.Structures.IsCongruent.Eq₁._._≉_
d__'8777'__46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 -> AgdaAny -> AgdaAny -> ()
d__'8777'__46 = erased
-- Function.Structures.IsCongruent.Eq₁._.Carrier
d_Carrier_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsCongruent_22 -> ()
d_Carrier_48 = erased
-- Function.Structures.IsCongruent.Eq₁._.isEquivalence
d_isEquivalence_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_50 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isEquivalence_50 v9
du_isEquivalence_50 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_50 v0 = coe d_isEquivalence'8321'_34 (coe v0)
-- Function.Structures.IsCongruent.Eq₁._.isPartialEquivalence
d_isPartialEquivalence_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_52 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_52 v9
du_isPartialEquivalence_52 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_52 v0
  = let v1 = coe du_setoid_40 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
-- Function.Structures.IsCongruent.Eq₁._.partialSetoid
d_partialSetoid_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_54 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_partialSetoid_54 v9
du_partialSetoid_54 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_54 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_40 (coe v0))
-- Function.Structures.IsCongruent.Eq₁._.refl
d_refl_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsCongruent_22 -> AgdaAny -> AgdaAny
d_refl_56 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_refl_56 v9
du_refl_56 :: T_IsCongruent_22 -> AgdaAny -> AgdaAny
du_refl_56 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8321'_34 (coe v0))
-- Function.Structures.IsCongruent.Eq₁._.reflexive
d_reflexive_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_58 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_58 v9
du_reflexive_58 ::
  T_IsCongruent_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_58 v0
  = let v1 = coe du_setoid_40 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
        v2
-- Function.Structures.IsCongruent.Eq₁._.sym
d_sym_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_60 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_sym_60 v9
du_sym_60 ::
  T_IsCongruent_22 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_60 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8321'_34 (coe v0))
-- Function.Structures.IsCongruent.Eq₁._.trans
d_trans_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_62 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_trans_62 v9
du_trans_62 ::
  T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_62 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8321'_34 (coe v0))
-- Function.Structures.IsCongruent.Eq₂.setoid
d_setoid_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_66 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_66 v9
du_setoid_66 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_66 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (d_isEquivalence'8322'_36 (coe v0))
-- Function.Structures.IsCongruent.Eq₂._._≈_
d__'8776'__70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 -> AgdaAny -> AgdaAny -> ()
d__'8776'__70 = erased
-- Function.Structures.IsCongruent.Eq₂._._≉_
d__'8777'__72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 -> AgdaAny -> AgdaAny -> ()
d__'8777'__72 = erased
-- Function.Structures.IsCongruent.Eq₂._.Carrier
d_Carrier_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsCongruent_22 -> ()
d_Carrier_74 = erased
-- Function.Structures.IsCongruent.Eq₂._.isEquivalence
d_isEquivalence_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_76 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isEquivalence_76 v9
du_isEquivalence_76 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_76 v0 = coe d_isEquivalence'8322'_36 (coe v0)
-- Function.Structures.IsCongruent.Eq₂._.isPartialEquivalence
d_isPartialEquivalence_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_78 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_78 v9
du_isPartialEquivalence_78 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_78 v0
  = let v1 = coe du_setoid_66 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
-- Function.Structures.IsCongruent.Eq₂._.partialSetoid
d_partialSetoid_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_80 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_partialSetoid_80 v9
du_partialSetoid_80 ::
  T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_80 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_66 (coe v0))
-- Function.Structures.IsCongruent.Eq₂._.refl
d_refl_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsCongruent_22 -> AgdaAny -> AgdaAny
d_refl_82 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_refl_82 v9
du_refl_82 :: T_IsCongruent_22 -> AgdaAny -> AgdaAny
du_refl_82 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8322'_36 (coe v0))
-- Function.Structures.IsCongruent.Eq₂._.reflexive
d_reflexive_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_84 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_84 v9
du_reflexive_84 ::
  T_IsCongruent_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_84 v0
  = let v1 = coe du_setoid_66 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
        v2
-- Function.Structures.IsCongruent.Eq₂._.sym
d_sym_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_86 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_sym_86 v9
du_sym_86 ::
  T_IsCongruent_22 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_86 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8322'_36 (coe v0))
-- Function.Structures.IsCongruent.Eq₂._.trans
d_trans_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_88 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_trans_88 v9
du_trans_88 ::
  T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_88 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8322'_36 (coe v0))
-- Function.Structures.IsInjection
d_IsInjection_92 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsInjection_92
  = C_IsInjection'46'constructor_3991 T_IsCongruent_22
                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Structures.IsInjection.isCongruent
d_isCongruent_100 :: T_IsInjection_92 -> T_IsCongruent_22
d_isCongruent_100 v0
  = case coe v0 of
      C_IsInjection'46'constructor_3991 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsInjection.injective
d_injective_102 ::
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_102 v0
  = case coe v0 of
      C_IsInjection'46'constructor_3991 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsInjection._.cong
d_cong_106 ::
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_106 v0 = coe d_cong_32 (coe d_isCongruent_100 (coe v0))
-- Function.Structures.IsInjection._.isEquivalence₁
d_isEquivalence'8321'_108 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8321'_108 v0
  = coe d_isEquivalence'8321'_34 (coe d_isCongruent_100 (coe v0))
-- Function.Structures.IsInjection._.isEquivalence₂
d_isEquivalence'8322'_110 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8322'_110 v0
  = coe d_isEquivalence'8322'_36 (coe d_isCongruent_100 (coe v0))
-- Function.Structures.IsInjection._.Eq₁._≈_
d__'8776'__114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> ()
d__'8776'__114 = erased
-- Function.Structures.IsInjection._.Eq₁._≉_
d__'8777'__116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> ()
d__'8777'__116 = erased
-- Function.Structures.IsInjection._.Eq₁.Carrier
d_Carrier_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsInjection_92 -> ()
d_Carrier_118 = erased
-- Function.Structures.IsInjection._.Eq₁.isEquivalence
d_isEquivalence_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_120 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isEquivalence_120 v9
du_isEquivalence_120 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_120 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe d_isEquivalence'8321'_34 (coe v1)
-- Function.Structures.IsInjection._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_122 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_122 v9
du_isPartialEquivalence_122 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_122 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsInjection._.Eq₁.partialSetoid
d_partialSetoid_124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_124 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_partialSetoid_124 v9
du_partialSetoid_124 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_124 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_40 (coe v1))
-- Function.Structures.IsInjection._.Eq₁.refl
d_refl_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsInjection_92 -> AgdaAny -> AgdaAny
d_refl_126 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_refl_126 v9
du_refl_126 :: T_IsInjection_92 -> AgdaAny -> AgdaAny
du_refl_126 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsInjection._.Eq₁.reflexive
d_reflexive_128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_128 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_128 v9
du_reflexive_128 ::
  T_IsInjection_92 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_128 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsInjection._.Eq₁.setoid
d_setoid_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_130 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_130 v9
du_setoid_130 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_130 v0
  = coe du_setoid_40 (coe d_isCongruent_100 (coe v0))
-- Function.Structures.IsInjection._.Eq₁.sym
d_sym_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_132 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_sym_132 v9
du_sym_132 ::
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_132 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsInjection._.Eq₁.trans
d_trans_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_trans_134 v9
du_trans_134 ::
  T_IsInjection_92 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_134 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsInjection._.Eq₂._≈_
d__'8776'__138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> ()
d__'8776'__138 = erased
-- Function.Structures.IsInjection._.Eq₂._≉_
d__'8777'__140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> ()
d__'8777'__140 = erased
-- Function.Structures.IsInjection._.Eq₂.Carrier
d_Carrier_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsInjection_92 -> ()
d_Carrier_142 = erased
-- Function.Structures.IsInjection._.Eq₂.isEquivalence
d_isEquivalence_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_144 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isEquivalence_144 v9
du_isEquivalence_144 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_144 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe d_isEquivalence'8322'_36 (coe v1)
-- Function.Structures.IsInjection._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_146 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_146 v9
du_isPartialEquivalence_146 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_146 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsInjection._.Eq₂.partialSetoid
d_partialSetoid_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_148 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_partialSetoid_148 v9
du_partialSetoid_148 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_148 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_66 (coe v1))
-- Function.Structures.IsInjection._.Eq₂.refl
d_refl_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsInjection_92 -> AgdaAny -> AgdaAny
d_refl_150 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_refl_150 v9
du_refl_150 :: T_IsInjection_92 -> AgdaAny -> AgdaAny
du_refl_150 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsInjection._.Eq₂.reflexive
d_reflexive_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_152 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_152 v9
du_reflexive_152 ::
  T_IsInjection_92 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_152 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsInjection._.Eq₂.setoid
d_setoid_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_154 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_154 v9
du_setoid_154 ::
  T_IsInjection_92 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_154 v0
  = coe du_setoid_66 (coe d_isCongruent_100 (coe v0))
-- Function.Structures.IsInjection._.Eq₂.sym
d_sym_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_156 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_sym_156 v9
du_sym_156 ::
  T_IsInjection_92 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_156 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsInjection._.Eq₂.trans
d_trans_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInjection_92 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_158 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_trans_158 v9
du_trans_158 ::
  T_IsInjection_92 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_158 v0
  = let v1 = d_isCongruent_100 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsSurjection
d_IsSurjection_162 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsSurjection_162
  = C_IsSurjection'46'constructor_6455 T_IsCongruent_22
                                       (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Function.Structures.IsSurjection.isCongruent
d_isCongruent_170 :: T_IsSurjection_162 -> T_IsCongruent_22
d_isCongruent_170 v0
  = case coe v0 of
      C_IsSurjection'46'constructor_6455 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsSurjection.surjective
d_surjective_172 ::
  T_IsSurjection_162 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_172 v0
  = case coe v0 of
      C_IsSurjection'46'constructor_6455 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsSurjection._.cong
d_cong_176 ::
  T_IsSurjection_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_176 v0 = coe d_cong_32 (coe d_isCongruent_170 (coe v0))
-- Function.Structures.IsSurjection._.isEquivalence₁
d_isEquivalence'8321'_178 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8321'_178 v0
  = coe d_isEquivalence'8321'_34 (coe d_isCongruent_170 (coe v0))
-- Function.Structures.IsSurjection._.isEquivalence₂
d_isEquivalence'8322'_180 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8322'_180 v0
  = coe d_isEquivalence'8322'_36 (coe d_isCongruent_170 (coe v0))
-- Function.Structures.IsSurjection._.Eq₁._≈_
d__'8776'__184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 -> AgdaAny -> AgdaAny -> ()
d__'8776'__184 = erased
-- Function.Structures.IsSurjection._.Eq₁._≉_
d__'8777'__186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 -> AgdaAny -> AgdaAny -> ()
d__'8777'__186 = erased
-- Function.Structures.IsSurjection._.Eq₁.Carrier
d_Carrier_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsSurjection_162 -> ()
d_Carrier_188 = erased
-- Function.Structures.IsSurjection._.Eq₁.isEquivalence
d_isEquivalence_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_190 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isEquivalence_190 v9
du_isEquivalence_190 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_190 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe d_isEquivalence'8321'_34 (coe v1)
-- Function.Structures.IsSurjection._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_192 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_192 v9
du_isPartialEquivalence_192 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_192 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsSurjection._.Eq₁.partialSetoid
d_partialSetoid_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_194 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_partialSetoid_194 v9
du_partialSetoid_194 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_194 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_40 (coe v1))
-- Function.Structures.IsSurjection._.Eq₁.refl
d_refl_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsSurjection_162 -> AgdaAny -> AgdaAny
d_refl_196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_refl_196 v9
du_refl_196 :: T_IsSurjection_162 -> AgdaAny -> AgdaAny
du_refl_196 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsSurjection._.Eq₁.reflexive
d_reflexive_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_198 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_198 v9
du_reflexive_198 ::
  T_IsSurjection_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_198 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsSurjection._.Eq₁.setoid
d_setoid_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_200 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_200 v9
du_setoid_200 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_200 v0
  = coe du_setoid_40 (coe d_isCongruent_170 (coe v0))
-- Function.Structures.IsSurjection._.Eq₁.sym
d_sym_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_202 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_sym_202 v9
du_sym_202 ::
  T_IsSurjection_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_202 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsSurjection._.Eq₁.trans
d_trans_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_204 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_trans_204 v9
du_trans_204 ::
  T_IsSurjection_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_204 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsSurjection._.Eq₂._≈_
d__'8776'__208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 -> AgdaAny -> AgdaAny -> ()
d__'8776'__208 = erased
-- Function.Structures.IsSurjection._.Eq₂._≉_
d__'8777'__210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 -> AgdaAny -> AgdaAny -> ()
d__'8777'__210 = erased
-- Function.Structures.IsSurjection._.Eq₂.Carrier
d_Carrier_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsSurjection_162 -> ()
d_Carrier_212 = erased
-- Function.Structures.IsSurjection._.Eq₂.isEquivalence
d_isEquivalence_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_214 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isEquivalence_214 v9
du_isEquivalence_214 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_214 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe d_isEquivalence'8322'_36 (coe v1)
-- Function.Structures.IsSurjection._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_216 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_216 v9
du_isPartialEquivalence_216 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_216 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsSurjection._.Eq₂.partialSetoid
d_partialSetoid_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_218 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_partialSetoid_218 v9
du_partialSetoid_218 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_218 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_66 (coe v1))
-- Function.Structures.IsSurjection._.Eq₂.refl
d_refl_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsSurjection_162 -> AgdaAny -> AgdaAny
d_refl_220 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_refl_220 v9
du_refl_220 :: T_IsSurjection_162 -> AgdaAny -> AgdaAny
du_refl_220 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsSurjection._.Eq₂.reflexive
d_reflexive_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_222 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_222 v9
du_reflexive_222 ::
  T_IsSurjection_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_222 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsSurjection._.Eq₂.setoid
d_setoid_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_224 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_224 v9
du_setoid_224 ::
  T_IsSurjection_162 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_224 v0
  = coe du_setoid_66 (coe d_isCongruent_170 (coe v0))
-- Function.Structures.IsSurjection._.Eq₂.sym
d_sym_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_226 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_sym_226 v9
du_sym_226 ::
  T_IsSurjection_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_226 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsSurjection._.Eq₂.trans
d_trans_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsSurjection_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_228 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_trans_228 v9
du_trans_228 ::
  T_IsSurjection_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_228 v0
  = let v1 = d_isCongruent_170 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsBijection
d_IsBijection_232 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsBijection_232
  = C_IsBijection'46'constructor_8915 T_IsInjection_92
                                      (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Function.Structures.IsBijection.isInjection
d_isInjection_240 :: T_IsBijection_232 -> T_IsInjection_92
d_isInjection_240 v0
  = case coe v0 of
      C_IsBijection'46'constructor_8915 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBijection.surjective
d_surjective_242 ::
  T_IsBijection_232 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_242 v0
  = case coe v0 of
      C_IsBijection'46'constructor_8915 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBijection._.cong
d_cong_246 ::
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_246 v0
  = coe
      d_cong_32 (coe d_isCongruent_100 (coe d_isInjection_240 (coe v0)))
-- Function.Structures.IsBijection._.injective
d_injective_248 ::
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_248 v0
  = coe d_injective_102 (coe d_isInjection_240 (coe v0))
-- Function.Structures.IsBijection._.isCongruent
d_isCongruent_250 :: T_IsBijection_232 -> T_IsCongruent_22
d_isCongruent_250 v0
  = coe d_isCongruent_100 (coe d_isInjection_240 (coe v0))
-- Function.Structures.IsBijection._.isEquivalence₁
d_isEquivalence'8321'_252 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8321'_252 v0
  = coe
      d_isEquivalence'8321'_34
      (coe d_isCongruent_100 (coe d_isInjection_240 (coe v0)))
-- Function.Structures.IsBijection._.isEquivalence₂
d_isEquivalence'8322'_254 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8322'_254 v0
  = coe
      d_isEquivalence'8322'_36
      (coe d_isCongruent_100 (coe d_isInjection_240 (coe v0)))
-- Function.Structures.IsBijection._.Eq₁._≈_
d__'8776'__258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> ()
d__'8776'__258 = erased
-- Function.Structures.IsBijection._.Eq₁._≉_
d__'8777'__260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> ()
d__'8777'__260 = erased
-- Function.Structures.IsBijection._.Eq₁.Carrier
d_Carrier_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsBijection_232 -> ()
d_Carrier_262 = erased
-- Function.Structures.IsBijection._.Eq₁.isEquivalence
d_isEquivalence_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_264 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isEquivalence_264 v9
du_isEquivalence_264 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_264 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe d_isEquivalence'8321'_34 (coe v2)
-- Function.Structures.IsBijection._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_266 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_266 v9
du_isPartialEquivalence_266 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_266 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    let v3 = coe du_setoid_40 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
-- Function.Structures.IsBijection._.Eq₁.partialSetoid
d_partialSetoid_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_268 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_partialSetoid_268 v9
du_partialSetoid_268 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_268 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_40 (coe v2))
-- Function.Structures.IsBijection._.Eq₁.refl
d_refl_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsBijection_232 -> AgdaAny -> AgdaAny
d_refl_270 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_refl_270 v9
du_refl_270 :: T_IsBijection_232 -> AgdaAny -> AgdaAny
du_refl_270 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8321'_34 (coe v2))
-- Function.Structures.IsBijection._.Eq₁.reflexive
d_reflexive_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_272 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_272 v9
du_reflexive_272 ::
  T_IsBijection_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_272 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    let v3 = coe du_setoid_40 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
        v4
-- Function.Structures.IsBijection._.Eq₁.setoid
d_setoid_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_274 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_274 v9
du_setoid_274 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_274 v0
  = let v1 = d_isInjection_240 (coe v0) in
    coe du_setoid_40 (coe d_isCongruent_100 (coe v1))
-- Function.Structures.IsBijection._.Eq₁.sym
d_sym_276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_276 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_sym_276 v9
du_sym_276 ::
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_276 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8321'_34 (coe v2))
-- Function.Structures.IsBijection._.Eq₁.trans
d_trans_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_278 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_trans_278 v9
du_trans_278 ::
  T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_278 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8321'_34 (coe v2))
-- Function.Structures.IsBijection._.Eq₂._≈_
d__'8776'__282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> ()
d__'8776'__282 = erased
-- Function.Structures.IsBijection._.Eq₂._≉_
d__'8777'__284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> ()
d__'8777'__284 = erased
-- Function.Structures.IsBijection._.Eq₂.Carrier
d_Carrier_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsBijection_232 -> ()
d_Carrier_286 = erased
-- Function.Structures.IsBijection._.Eq₂.isEquivalence
d_isEquivalence_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_288 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isEquivalence_288 v9
du_isEquivalence_288 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_288 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe d_isEquivalence'8322'_36 (coe v2)
-- Function.Structures.IsBijection._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_290 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_290 v9
du_isPartialEquivalence_290 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_290 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    let v3 = coe du_setoid_66 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
-- Function.Structures.IsBijection._.Eq₂.partialSetoid
d_partialSetoid_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_292 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_partialSetoid_292 v9
du_partialSetoid_292 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_292 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_66 (coe v2))
-- Function.Structures.IsBijection._.Eq₂.refl
d_refl_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsBijection_232 -> AgdaAny -> AgdaAny
d_refl_294 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_refl_294 v9
du_refl_294 :: T_IsBijection_232 -> AgdaAny -> AgdaAny
du_refl_294 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8322'_36 (coe v2))
-- Function.Structures.IsBijection._.Eq₂.reflexive
d_reflexive_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_296 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_296 v9
du_reflexive_296 ::
  T_IsBijection_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_296 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    let v3 = coe du_setoid_66 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
        v4
-- Function.Structures.IsBijection._.Eq₂.setoid
d_setoid_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_298 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_298 v9
du_setoid_298 ::
  T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_298 v0
  = let v1 = d_isInjection_240 (coe v0) in
    coe du_setoid_66 (coe d_isCongruent_100 (coe v1))
-- Function.Structures.IsBijection._.Eq₂.sym
d_sym_300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_300 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 = du_sym_300 v9
du_sym_300 ::
  T_IsBijection_232 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_300 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8322'_36 (coe v2))
-- Function.Structures.IsBijection._.Eq₂.trans
d_trans_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_302 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_trans_302 v9
du_trans_302 ::
  T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_302 v0
  = let v1 = d_isInjection_240 (coe v0) in
    let v2 = d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8322'_36 (coe v2))
-- Function.Structures.IsBijection.bijective
d_bijective_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBijection_232 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_bijective_304 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_bijective_304 v9
du_bijective_304 ::
  T_IsBijection_232 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_bijective_304 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe d_injective_102 (coe d_isInjection_240 (coe v0)))
      (coe d_surjective_242 (coe v0))
-- Function.Structures.IsBijection.isSurjection
d_isSurjection_306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_IsBijection_232 -> T_IsSurjection_162
d_isSurjection_306 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isSurjection_306 v9
du_isSurjection_306 :: T_IsBijection_232 -> T_IsSurjection_162
du_isSurjection_306 v0
  = coe
      C_IsSurjection'46'constructor_6455
      (coe d_isCongruent_100 (coe d_isInjection_240 (coe v0)))
      (coe d_surjective_242 (coe v0))
-- Function.Structures.IsLeftInverse
d_IsLeftInverse_312 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 = ()
data T_IsLeftInverse_312
  = C_IsLeftInverse'46'constructor_13035 T_IsCongruent_22
                                         (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                         (AgdaAny -> AgdaAny)
-- Function.Structures.IsLeftInverse.isCongruent
d_isCongruent_324 :: T_IsLeftInverse_312 -> T_IsCongruent_22
d_isCongruent_324 v0
  = case coe v0 of
      C_IsLeftInverse'46'constructor_13035 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsLeftInverse.from-cong
d_from'45'cong_326 ::
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'45'cong_326 v0
  = case coe v0 of
      C_IsLeftInverse'46'constructor_13035 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsLeftInverse.inverseˡ
d_inverse'737'_328 :: T_IsLeftInverse_312 -> AgdaAny -> AgdaAny
d_inverse'737'_328 v0
  = case coe v0 of
      C_IsLeftInverse'46'constructor_13035 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsLeftInverse._.isEquivalence₁
d_isEquivalence'8321'_332 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8321'_332 v0
  = coe d_isEquivalence'8321'_34 (coe d_isCongruent_324 (coe v0))
-- Function.Structures.IsLeftInverse._.isEquivalence₂
d_isEquivalence'8322'_334 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8322'_334 v0
  = coe d_isEquivalence'8322'_36 (coe d_isCongruent_324 (coe v0))
-- Function.Structures.IsLeftInverse._.cong
d_cong_336 ::
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_336 v0 = coe d_cong_32 (coe d_isCongruent_324 (coe v0))
-- Function.Structures.IsLeftInverse._.Eq₁._≈_
d__'8776'__340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> ()
d__'8776'__340 = erased
-- Function.Structures.IsLeftInverse._.Eq₁._≉_
d__'8777'__342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> ()
d__'8777'__342 = erased
-- Function.Structures.IsLeftInverse._.Eq₁.Carrier
d_Carrier_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsLeftInverse_312 -> ()
d_Carrier_344 = erased
-- Function.Structures.IsLeftInverse._.Eq₁.isEquivalence
d_isEquivalence_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_346 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isEquivalence_346 v10
du_isEquivalence_346 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_346 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe d_isEquivalence'8321'_34 (coe v1)
-- Function.Structures.IsLeftInverse._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_348 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           v10
  = du_isPartialEquivalence_348 v10
du_isPartialEquivalence_348 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_348 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsLeftInverse._.Eq₁.partialSetoid
d_partialSetoid_350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_350 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_partialSetoid_350 v10
du_partialSetoid_350 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_350 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_40 (coe v1))
-- Function.Structures.IsLeftInverse._.Eq₁.refl
d_refl_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsLeftInverse_312 -> AgdaAny -> AgdaAny
d_refl_352 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_refl_352 v10
du_refl_352 :: T_IsLeftInverse_312 -> AgdaAny -> AgdaAny
du_refl_352 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsLeftInverse._.Eq₁.reflexive
d_reflexive_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_354 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_reflexive_354 v10
du_reflexive_354 ::
  T_IsLeftInverse_312 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_354 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsLeftInverse._.Eq₁.setoid
d_setoid_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_356 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_setoid_356 v10
du_setoid_356 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_356 v0
  = coe du_setoid_40 (coe d_isCongruent_324 (coe v0))
-- Function.Structures.IsLeftInverse._.Eq₁.sym
d_sym_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_358 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_sym_358 v10
du_sym_358 ::
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_358 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsLeftInverse._.Eq₁.trans
d_trans_360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_360 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_trans_360 v10
du_trans_360 ::
  T_IsLeftInverse_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_360 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsLeftInverse._.Eq₂._≈_
d__'8776'__364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> ()
d__'8776'__364 = erased
-- Function.Structures.IsLeftInverse._.Eq₂._≉_
d__'8777'__366 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> ()
d__'8777'__366 = erased
-- Function.Structures.IsLeftInverse._.Eq₂.Carrier
d_Carrier_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsLeftInverse_312 -> ()
d_Carrier_368 = erased
-- Function.Structures.IsLeftInverse._.Eq₂.isEquivalence
d_isEquivalence_370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_370 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isEquivalence_370 v10
du_isEquivalence_370 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_370 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe d_isEquivalence'8322'_36 (coe v1)
-- Function.Structures.IsLeftInverse._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_372 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           v10
  = du_isPartialEquivalence_372 v10
du_isPartialEquivalence_372 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_372 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsLeftInverse._.Eq₂.partialSetoid
d_partialSetoid_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_374 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_partialSetoid_374 v10
du_partialSetoid_374 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_374 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_66 (coe v1))
-- Function.Structures.IsLeftInverse._.Eq₂.refl
d_refl_376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsLeftInverse_312 -> AgdaAny -> AgdaAny
d_refl_376 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_refl_376 v10
du_refl_376 :: T_IsLeftInverse_312 -> AgdaAny -> AgdaAny
du_refl_376 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsLeftInverse._.Eq₂.reflexive
d_reflexive_378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_378 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_reflexive_378 v10
du_reflexive_378 ::
  T_IsLeftInverse_312 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_378 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsLeftInverse._.Eq₂.setoid
d_setoid_380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_380 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_setoid_380 v10
du_setoid_380 ::
  T_IsLeftInverse_312 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_380 v0
  = coe du_setoid_66 (coe d_isCongruent_324 (coe v0))
-- Function.Structures.IsLeftInverse._.Eq₂.sym
d_sym_382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_382 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_sym_382 v10
du_sym_382 ::
  T_IsLeftInverse_312 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_382 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsLeftInverse._.Eq₂.trans
d_trans_384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsLeftInverse_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_384 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_trans_384 v10
du_trans_384 ::
  T_IsLeftInverse_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_384 v0
  = let v1 = d_isCongruent_324 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsRightInverse
d_IsRightInverse_390 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 = ()
data T_IsRightInverse_390
  = C_IsRightInverse'46'constructor_16307 T_IsCongruent_22
                                          (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny)
-- Function.Structures.IsRightInverse.isCongruent
d_isCongruent_402 :: T_IsRightInverse_390 -> T_IsCongruent_22
d_isCongruent_402 v0
  = case coe v0 of
      C_IsRightInverse'46'constructor_16307 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsRightInverse.from-cong
d_from'45'cong_404 ::
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'45'cong_404 v0
  = case coe v0 of
      C_IsRightInverse'46'constructor_16307 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsRightInverse.inverseʳ
d_inverse'691'_406 :: T_IsRightInverse_390 -> AgdaAny -> AgdaAny
d_inverse'691'_406 v0
  = case coe v0 of
      C_IsRightInverse'46'constructor_16307 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsRightInverse._.cong
d_cong_410 ::
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_410 v0 = coe d_cong_32 (coe d_isCongruent_402 (coe v0))
-- Function.Structures.IsRightInverse._.isEquivalence₁
d_isEquivalence'8321'_412 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8321'_412 v0
  = coe d_isEquivalence'8321'_34 (coe d_isCongruent_402 (coe v0))
-- Function.Structures.IsRightInverse._.isEquivalence₂
d_isEquivalence'8322'_414 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8322'_414 v0
  = coe d_isEquivalence'8322'_36 (coe d_isCongruent_402 (coe v0))
-- Function.Structures.IsRightInverse._.Eq₁._≈_
d__'8776'__418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> ()
d__'8776'__418 = erased
-- Function.Structures.IsRightInverse._.Eq₁._≉_
d__'8777'__420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> ()
d__'8777'__420 = erased
-- Function.Structures.IsRightInverse._.Eq₁.Carrier
d_Carrier_422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsRightInverse_390 -> ()
d_Carrier_422 = erased
-- Function.Structures.IsRightInverse._.Eq₁.isEquivalence
d_isEquivalence_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_424 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isEquivalence_424 v10
du_isEquivalence_424 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_424 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe d_isEquivalence'8321'_34 (coe v1)
-- Function.Structures.IsRightInverse._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_426 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           v10
  = du_isPartialEquivalence_426 v10
du_isPartialEquivalence_426 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_426 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsRightInverse._.Eq₁.partialSetoid
d_partialSetoid_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_428 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_partialSetoid_428 v10
du_partialSetoid_428 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_428 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_40 (coe v1))
-- Function.Structures.IsRightInverse._.Eq₁.refl
d_refl_430 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsRightInverse_390 -> AgdaAny -> AgdaAny
d_refl_430 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_refl_430 v10
du_refl_430 :: T_IsRightInverse_390 -> AgdaAny -> AgdaAny
du_refl_430 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsRightInverse._.Eq₁.reflexive
d_reflexive_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_432 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_reflexive_432 v10
du_reflexive_432 ::
  T_IsRightInverse_390 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_432 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsRightInverse._.Eq₁.setoid
d_setoid_434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_434 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_setoid_434 v10
du_setoid_434 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_434 v0
  = coe du_setoid_40 (coe d_isCongruent_402 (coe v0))
-- Function.Structures.IsRightInverse._.Eq₁.sym
d_sym_436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_436 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_sym_436 v10
du_sym_436 ::
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_436 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsRightInverse._.Eq₁.trans
d_trans_438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_438 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_trans_438 v10
du_trans_438 ::
  T_IsRightInverse_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_438 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsRightInverse._.Eq₂._≈_
d__'8776'__442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> ()
d__'8776'__442 = erased
-- Function.Structures.IsRightInverse._.Eq₂._≉_
d__'8777'__444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> ()
d__'8777'__444 = erased
-- Function.Structures.IsRightInverse._.Eq₂.Carrier
d_Carrier_446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsRightInverse_390 -> ()
d_Carrier_446 = erased
-- Function.Structures.IsRightInverse._.Eq₂.isEquivalence
d_isEquivalence_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_448 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isEquivalence_448 v10
du_isEquivalence_448 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_448 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe d_isEquivalence'8322'_36 (coe v1)
-- Function.Structures.IsRightInverse._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_450 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           v10
  = du_isPartialEquivalence_450 v10
du_isPartialEquivalence_450 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_450 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsRightInverse._.Eq₂.partialSetoid
d_partialSetoid_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_452 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_partialSetoid_452 v10
du_partialSetoid_452 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_452 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_66 (coe v1))
-- Function.Structures.IsRightInverse._.Eq₂.refl
d_refl_454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsRightInverse_390 -> AgdaAny -> AgdaAny
d_refl_454 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_refl_454 v10
du_refl_454 :: T_IsRightInverse_390 -> AgdaAny -> AgdaAny
du_refl_454 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsRightInverse._.Eq₂.reflexive
d_reflexive_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_456 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_reflexive_456 v10
du_reflexive_456 ::
  T_IsRightInverse_390 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_456 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsRightInverse._.Eq₂.setoid
d_setoid_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_458 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_setoid_458 v10
du_setoid_458 ::
  T_IsRightInverse_390 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_458 v0
  = coe du_setoid_66 (coe d_isCongruent_402 (coe v0))
-- Function.Structures.IsRightInverse._.Eq₂.sym
d_sym_460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_460 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_sym_460 v10
du_sym_460 ::
  T_IsRightInverse_390 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_460 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsRightInverse._.Eq₂.trans
d_trans_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsRightInverse_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_462 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_trans_462 v10
du_trans_462 ::
  T_IsRightInverse_390 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_462 v0
  = let v1 = d_isCongruent_402 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsInverse
d_IsInverse_468 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 = ()
data T_IsInverse_468
  = C_IsInverse'46'constructor_19111 T_IsLeftInverse_312
                                     (AgdaAny -> AgdaAny)
-- Function.Structures.IsInverse.isLeftInverse
d_isLeftInverse_478 :: T_IsInverse_468 -> T_IsLeftInverse_312
d_isLeftInverse_478 v0
  = case coe v0 of
      C_IsInverse'46'constructor_19111 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsInverse.inverseʳ
d_inverse'691'_480 :: T_IsInverse_468 -> AgdaAny -> AgdaAny
d_inverse'691'_480 v0
  = case coe v0 of
      C_IsInverse'46'constructor_19111 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsInverse._.from-cong
d_from'45'cong_484 ::
  T_IsInverse_468 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'45'cong_484 v0
  = coe d_from'45'cong_326 (coe d_isLeftInverse_478 (coe v0))
-- Function.Structures.IsInverse._.inverseˡ
d_inverse'737'_486 :: T_IsInverse_468 -> AgdaAny -> AgdaAny
d_inverse'737'_486 v0
  = coe d_inverse'737'_328 (coe d_isLeftInverse_478 (coe v0))
-- Function.Structures.IsInverse._.isCongruent
d_isCongruent_488 :: T_IsInverse_468 -> T_IsCongruent_22
d_isCongruent_488 v0
  = coe d_isCongruent_324 (coe d_isLeftInverse_478 (coe v0))
-- Function.Structures.IsInverse._.isEquivalence₁
d_isEquivalence'8321'_490 ::
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8321'_490 v0
  = coe
      d_isEquivalence'8321'_34
      (coe d_isCongruent_324 (coe d_isLeftInverse_478 (coe v0)))
-- Function.Structures.IsInverse._.isEquivalence₂
d_isEquivalence'8322'_492 ::
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8322'_492 v0
  = coe
      d_isEquivalence'8322'_36
      (coe d_isCongruent_324 (coe d_isLeftInverse_478 (coe v0)))
-- Function.Structures.IsInverse._.cong
d_cong_494 ::
  T_IsInverse_468 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_494 v0
  = coe
      d_cong_32
      (coe d_isCongruent_324 (coe d_isLeftInverse_478 (coe v0)))
-- Function.Structures.IsInverse._.Eq₁._≈_
d__'8776'__498 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsInverse_468 -> AgdaAny -> AgdaAny -> ()
d__'8776'__498 = erased
-- Function.Structures.IsInverse._.Eq₁._≉_
d__'8777'__500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsInverse_468 -> AgdaAny -> AgdaAny -> ()
d__'8777'__500 = erased
-- Function.Structures.IsInverse._.Eq₁.Carrier
d_Carrier_502 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsInverse_468 -> ()
d_Carrier_502 = erased
-- Function.Structures.IsInverse._.Eq₁.isEquivalence
d_isEquivalence_504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_504 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isEquivalence_504 v10
du_isEquivalence_504 ::
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_504 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe d_isEquivalence'8321'_34 (coe v2)
-- Function.Structures.IsInverse._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_506 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           v10
  = du_isPartialEquivalence_506 v10
du_isPartialEquivalence_506 ::
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_506 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    let v3 = coe du_setoid_40 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
-- Function.Structures.IsInverse._.Eq₁.partialSetoid
d_partialSetoid_508 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_508 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_partialSetoid_508 v10
du_partialSetoid_508 ::
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_508 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_40 (coe v2))
-- Function.Structures.IsInverse._.Eq₁.refl
d_refl_510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsInverse_468 -> AgdaAny -> AgdaAny
d_refl_510 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_refl_510 v10
du_refl_510 :: T_IsInverse_468 -> AgdaAny -> AgdaAny
du_refl_510 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8321'_34 (coe v2))
-- Function.Structures.IsInverse._.Eq₁.reflexive
d_reflexive_512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_512 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_reflexive_512 v10
du_reflexive_512 ::
  T_IsInverse_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_512 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    let v3 = coe du_setoid_40 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
        v4
-- Function.Structures.IsInverse._.Eq₁.setoid
d_setoid_514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_514 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_setoid_514 v10
du_setoid_514 ::
  T_IsInverse_468 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_514 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    coe du_setoid_40 (coe d_isCongruent_324 (coe v1))
-- Function.Structures.IsInverse._.Eq₁.sym
d_sym_516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_516 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_sym_516 v10
du_sym_516 ::
  T_IsInverse_468 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_516 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8321'_34 (coe v2))
-- Function.Structures.IsInverse._.Eq₁.trans
d_trans_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_518 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_trans_518 v10
du_trans_518 ::
  T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_518 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8321'_34 (coe v2))
-- Function.Structures.IsInverse._.Eq₂._≈_
d__'8776'__522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsInverse_468 -> AgdaAny -> AgdaAny -> ()
d__'8776'__522 = erased
-- Function.Structures.IsInverse._.Eq₂._≉_
d__'8777'__524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsInverse_468 -> AgdaAny -> AgdaAny -> ()
d__'8777'__524 = erased
-- Function.Structures.IsInverse._.Eq₂.Carrier
d_Carrier_526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsInverse_468 -> ()
d_Carrier_526 = erased
-- Function.Structures.IsInverse._.Eq₂.isEquivalence
d_isEquivalence_528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_528 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isEquivalence_528 v10
du_isEquivalence_528 ::
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_528 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe d_isEquivalence'8322'_36 (coe v2)
-- Function.Structures.IsInverse._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_530 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           v10
  = du_isPartialEquivalence_530 v10
du_isPartialEquivalence_530 ::
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_530 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    let v3 = coe du_setoid_66 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
-- Function.Structures.IsInverse._.Eq₂.partialSetoid
d_partialSetoid_532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_532 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_partialSetoid_532 v10
du_partialSetoid_532 ::
  T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_532 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_66 (coe v2))
-- Function.Structures.IsInverse._.Eq₂.refl
d_refl_534 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsInverse_468 -> AgdaAny -> AgdaAny
d_refl_534 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_refl_534 v10
du_refl_534 :: T_IsInverse_468 -> AgdaAny -> AgdaAny
du_refl_534 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8322'_36 (coe v2))
-- Function.Structures.IsInverse._.Eq₂.reflexive
d_reflexive_536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_536 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_reflexive_536 v10
du_reflexive_536 ::
  T_IsInverse_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_536 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    let v3 = coe du_setoid_66 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
        v4
-- Function.Structures.IsInverse._.Eq₂.setoid
d_setoid_538 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_538 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_setoid_538 v10
du_setoid_538 ::
  T_IsInverse_468 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_538 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    coe du_setoid_66 (coe d_isCongruent_324 (coe v1))
-- Function.Structures.IsInverse._.Eq₂.sym
d_sym_540 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_540 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_sym_540 v10
du_sym_540 ::
  T_IsInverse_468 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_540 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8322'_36 (coe v2))
-- Function.Structures.IsInverse._.Eq₂.trans
d_trans_542 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_542 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_trans_542 v10
du_trans_542 ::
  T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_542 v0
  = let v1 = d_isLeftInverse_478 (coe v0) in
    let v2 = d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8322'_36 (coe v2))
-- Function.Structures.IsInverse.isRightInverse
d_isRightInverse_544 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsInverse_468 -> T_IsRightInverse_390
d_isRightInverse_544 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_isRightInverse_544 v10
du_isRightInverse_544 :: T_IsInverse_468 -> T_IsRightInverse_390
du_isRightInverse_544 v0
  = coe
      C_IsRightInverse'46'constructor_16307
      (coe d_isCongruent_324 (coe d_isLeftInverse_478 (coe v0)))
      (coe d_from'45'cong_326 (coe d_isLeftInverse_478 (coe v0)))
      (coe d_inverse'691'_480 (coe v0))
-- Function.Structures.IsInverse.inverse
d_inverse_546 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsInverse_468 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_546 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
  = du_inverse_546 v10
du_inverse_546 ::
  T_IsInverse_468 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_inverse_546 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe d_inverse'737'_328 (coe d_isLeftInverse_478 (coe v0)))
      (coe d_inverse'691'_480 (coe v0))
-- Function.Structures.IsBiEquivalence
d_IsBiEquivalence_554 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
data T_IsBiEquivalence_554
  = C_IsBiEquivalence'46'constructor_23801 T_IsCongruent_22
                                           (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                           (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Structures.IsBiEquivalence.to-isCongruent
d_to'45'isCongruent_568 ::
  T_IsBiEquivalence_554 -> T_IsCongruent_22
d_to'45'isCongruent_568 v0
  = case coe v0 of
      C_IsBiEquivalence'46'constructor_23801 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBiEquivalence.from₁-cong
d_from'8321''45'cong_570 ::
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'8321''45'cong_570 v0
  = case coe v0 of
      C_IsBiEquivalence'46'constructor_23801 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBiEquivalence.from₂-cong
d_from'8322''45'cong_572 ::
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'8322''45'cong_572 v0
  = case coe v0 of
      C_IsBiEquivalence'46'constructor_23801 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBiEquivalence._.isEquivalence₁
d_isEquivalence'8321'_576 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8321'_576 v0
  = coe
      d_isEquivalence'8321'_34 (coe d_to'45'isCongruent_568 (coe v0))
-- Function.Structures.IsBiEquivalence._.isEquivalence₂
d_isEquivalence'8322'_578 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8322'_578 v0
  = coe
      d_isEquivalence'8322'_36 (coe d_to'45'isCongruent_568 (coe v0))
-- Function.Structures.IsBiEquivalence._.cong
d_cong_580 ::
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_580 v0
  = coe d_cong_32 (coe d_to'45'isCongruent_568 (coe v0))
-- Function.Structures.IsBiEquivalence._.Eq₁._≈_
d__'8776'__584 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> ()
d__'8776'__584 = erased
-- Function.Structures.IsBiEquivalence._.Eq₁._≉_
d__'8777'__586 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> ()
d__'8777'__586 = erased
-- Function.Structures.IsBiEquivalence._.Eq₁.Carrier
d_Carrier_588 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsBiEquivalence_554 -> ()
d_Carrier_588 = erased
-- Function.Structures.IsBiEquivalence._.Eq₁.isEquivalence
d_isEquivalence_590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_590 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    v11
  = du_isEquivalence_590 v11
du_isEquivalence_590 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_590 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe d_isEquivalence'8321'_34 (coe v1)
-- Function.Structures.IsBiEquivalence._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_592 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_592 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 v11
  = du_isPartialEquivalence_592 v11
du_isPartialEquivalence_592 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_592 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsBiEquivalence._.Eq₁.partialSetoid
d_partialSetoid_594 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_594 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    v11
  = du_partialSetoid_594 v11
du_partialSetoid_594 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_594 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_40 (coe v1))
-- Function.Structures.IsBiEquivalence._.Eq₁.refl
d_refl_596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny
d_refl_596 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_refl_596 v11
du_refl_596 :: T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny
du_refl_596 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsBiEquivalence._.Eq₁.reflexive
d_reflexive_598 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_598 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_reflexive_598 v11
du_reflexive_598 ::
  T_IsBiEquivalence_554 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_598 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsBiEquivalence._.Eq₁.setoid
d_setoid_600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_600 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_setoid_600 v11
du_setoid_600 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_600 v0
  = coe du_setoid_40 (coe d_to'45'isCongruent_568 (coe v0))
-- Function.Structures.IsBiEquivalence._.Eq₁.sym
d_sym_602 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_602 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_sym_602 v11
du_sym_602 ::
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_602 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsBiEquivalence._.Eq₁.trans
d_trans_604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_604 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_trans_604 v11
du_trans_604 ::
  T_IsBiEquivalence_554 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_604 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsBiEquivalence._.Eq₂._≈_
d__'8776'__608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> ()
d__'8776'__608 = erased
-- Function.Structures.IsBiEquivalence._.Eq₂._≉_
d__'8777'__610 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> ()
d__'8777'__610 = erased
-- Function.Structures.IsBiEquivalence._.Eq₂.Carrier
d_Carrier_612 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsBiEquivalence_554 -> ()
d_Carrier_612 = erased
-- Function.Structures.IsBiEquivalence._.Eq₂.isEquivalence
d_isEquivalence_614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_614 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    v11
  = du_isEquivalence_614 v11
du_isEquivalence_614 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_614 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe d_isEquivalence'8322'_36 (coe v1)
-- Function.Structures.IsBiEquivalence._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_616 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 v11
  = du_isPartialEquivalence_616 v11
du_isPartialEquivalence_616 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_616 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsBiEquivalence._.Eq₂.partialSetoid
d_partialSetoid_618 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_618 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    v11
  = du_partialSetoid_618 v11
du_partialSetoid_618 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_618 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_66 (coe v1))
-- Function.Structures.IsBiEquivalence._.Eq₂.refl
d_refl_620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny
d_refl_620 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_refl_620 v11
du_refl_620 :: T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny
du_refl_620 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsBiEquivalence._.Eq₂.reflexive
d_reflexive_622 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_622 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_reflexive_622 v11
du_reflexive_622 ::
  T_IsBiEquivalence_554 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_622 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsBiEquivalence._.Eq₂.setoid
d_setoid_624 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_624 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_setoid_624 v11
du_setoid_624 ::
  T_IsBiEquivalence_554 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_624 v0
  = coe du_setoid_66 (coe d_to'45'isCongruent_568 (coe v0))
-- Function.Structures.IsBiEquivalence._.Eq₂.sym
d_sym_626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_626 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_sym_626 v11
du_sym_626 ::
  T_IsBiEquivalence_554 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_626 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsBiEquivalence._.Eq₂.trans
d_trans_628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiEquivalence_554 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_628 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_trans_628 v11
du_trans_628 ::
  T_IsBiEquivalence_554 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_628 v0
  = let v1 = d_to'45'isCongruent_568 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsBiInverse
d_IsBiInverse_636 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
data T_IsBiInverse_636
  = C_IsBiInverse'46'constructor_28517 T_IsCongruent_22
                                       (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
-- Function.Structures.IsBiInverse.to-isCongruent
d_to'45'isCongruent_654 :: T_IsBiInverse_636 -> T_IsCongruent_22
d_to'45'isCongruent_654 v0
  = case coe v0 of
      C_IsBiInverse'46'constructor_28517 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBiInverse.from₁-cong
d_from'8321''45'cong_656 ::
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'8321''45'cong_656 v0
  = case coe v0 of
      C_IsBiInverse'46'constructor_28517 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBiInverse.from₂-cong
d_from'8322''45'cong_658 ::
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'8322''45'cong_658 v0
  = case coe v0 of
      C_IsBiInverse'46'constructor_28517 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBiInverse.inverseˡ
d_inverse'737'_660 :: T_IsBiInverse_636 -> AgdaAny -> AgdaAny
d_inverse'737'_660 v0
  = case coe v0 of
      C_IsBiInverse'46'constructor_28517 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBiInverse.inverseʳ
d_inverse'691'_662 :: T_IsBiInverse_636 -> AgdaAny -> AgdaAny
d_inverse'691'_662 v0
  = case coe v0 of
      C_IsBiInverse'46'constructor_28517 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Structures.IsBiInverse._.isEquivalence₁
d_isEquivalence'8321'_666 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8321'_666 v0
  = coe
      d_isEquivalence'8321'_34 (coe d_to'45'isCongruent_654 (coe v0))
-- Function.Structures.IsBiInverse._.isEquivalence₂
d_isEquivalence'8322'_668 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence'8322'_668 v0
  = coe
      d_isEquivalence'8322'_36 (coe d_to'45'isCongruent_654 (coe v0))
-- Function.Structures.IsBiInverse._.cong
d_cong_670 ::
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_670 v0
  = coe d_cong_32 (coe d_to'45'isCongruent_654 (coe v0))
-- Function.Structures.IsBiInverse._.Eq₁._≈_
d__'8776'__674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> ()
d__'8776'__674 = erased
-- Function.Structures.IsBiInverse._.Eq₁._≉_
d__'8777'__676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> ()
d__'8777'__676 = erased
-- Function.Structures.IsBiInverse._.Eq₁.Carrier
d_Carrier_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsBiInverse_636 -> ()
d_Carrier_678 = erased
-- Function.Structures.IsBiInverse._.Eq₁.isEquivalence
d_isEquivalence_680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_680 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    v11
  = du_isEquivalence_680 v11
du_isEquivalence_680 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_680 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe d_isEquivalence'8321'_34 (coe v1)
-- Function.Structures.IsBiInverse._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_682 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 v11
  = du_isPartialEquivalence_682 v11
du_isPartialEquivalence_682 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_682 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsBiInverse._.Eq₁.partialSetoid
d_partialSetoid_684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_684 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    v11
  = du_partialSetoid_684 v11
du_partialSetoid_684 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_684 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_40 (coe v1))
-- Function.Structures.IsBiInverse._.Eq₁.refl
d_refl_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsBiInverse_636 -> AgdaAny -> AgdaAny
d_refl_686 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_refl_686 v11
du_refl_686 :: T_IsBiInverse_636 -> AgdaAny -> AgdaAny
du_refl_686 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsBiInverse._.Eq₁.reflexive
d_reflexive_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_688 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_reflexive_688 v11
du_reflexive_688 ::
  T_IsBiInverse_636 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_688 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    let v2 = coe du_setoid_40 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsBiInverse._.Eq₁.setoid
d_setoid_690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_690 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_setoid_690 v11
du_setoid_690 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_690 v0
  = coe du_setoid_40 (coe d_to'45'isCongruent_654 (coe v0))
-- Function.Structures.IsBiInverse._.Eq₁.sym
d_sym_692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_692 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_sym_692 v11
du_sym_692 ::
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_692 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsBiInverse._.Eq₁.trans
d_trans_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_694 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_trans_694 v11
du_trans_694 ::
  T_IsBiInverse_636 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_694 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8321'_34 (coe v1))
-- Function.Structures.IsBiInverse._.Eq₂._≈_
d__'8776'__698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> ()
d__'8776'__698 = erased
-- Function.Structures.IsBiInverse._.Eq₂._≉_
d__'8777'__700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> ()
d__'8777'__700 = erased
-- Function.Structures.IsBiInverse._.Eq₂.Carrier
d_Carrier_702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsBiInverse_636 -> ()
d_Carrier_702 = erased
-- Function.Structures.IsBiInverse._.Eq₂.isEquivalence
d_isEquivalence_704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_704 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    v11
  = du_isEquivalence_704 v11
du_isEquivalence_704 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_704 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe d_isEquivalence'8322'_36 (coe v1)
-- Function.Structures.IsBiInverse._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_706 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_706 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 v11
  = du_isPartialEquivalence_706 v11
du_isPartialEquivalence_706 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_706 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
-- Function.Structures.IsBiInverse._.Eq₂.partialSetoid
d_partialSetoid_708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_708 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    v11
  = du_partialSetoid_708 v11
du_partialSetoid_708 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_708 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe du_setoid_66 (coe v1))
-- Function.Structures.IsBiInverse._.Eq₂.refl
d_refl_710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_IsBiInverse_636 -> AgdaAny -> AgdaAny
d_refl_710 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_refl_710 v11
du_refl_710 :: T_IsBiInverse_636 -> AgdaAny -> AgdaAny
du_refl_710 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsBiInverse._.Eq₂.reflexive
d_reflexive_712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_712 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_reflexive_712 v11
du_reflexive_712 ::
  T_IsBiInverse_636 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_712 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    let v2 = coe du_setoid_66 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v2))
        v3
-- Function.Structures.IsBiInverse._.Eq₂.setoid
d_setoid_714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_714 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_setoid_714 v11
du_setoid_714 ::
  T_IsBiInverse_636 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_714 v0
  = coe du_setoid_66 (coe d_to'45'isCongruent_654 (coe v0))
-- Function.Structures.IsBiInverse._.Eq₂.sym
d_sym_716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_716 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_sym_716 v11
du_sym_716 ::
  T_IsBiInverse_636 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_716 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence'8322'_36 (coe v1))
-- Function.Structures.IsBiInverse._.Eq₂.trans
d_trans_718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  T_IsBiInverse_636 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_718 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 v11
  = du_trans_718 v11
du_trans_718 ::
  T_IsBiInverse_636 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_718 v0
  = let v1 = d_to'45'isCongruent_654 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence'8322'_36 (coe v1))
