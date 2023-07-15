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

module MAlonzo.Code.Function.Bundles where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Bundles._._._≈_
d__'8776'__30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__30 = erased
-- Function.Bundles._._.Carrier
d_Carrier_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 -> ()
d_Carrier_32 = erased
-- Function.Bundles._._.Bijective
d_Bijective_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) -> ()
d_Bijective_42 = erased
-- Function.Bundles._._.Injective
d_Injective_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_46 = erased
-- Function.Bundles._._.Inverseʳ
d_Inverse'691'_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Inverse'691'_48 = erased
-- Function.Bundles._._.Inverseˡ
d_Inverse'737'_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Inverse'737'_50 = erased
-- Function.Bundles._._.Inverseᵇ
d_Inverse'7495'_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Inverse'7495'_52 = erased
-- Function.Bundles._._.Surjective
d_Surjective_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_54 = erased
-- Function.Bundles._._.IsBiInverse
d_IsBiInverse_60 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
-- Function.Bundles._._.IsBijection
d_IsBijection_62 a0 a1 a2 a3 a4 a5 a6 = ()
-- Function.Bundles._._.IsCongruent
d_IsCongruent_64 a0 a1 a2 a3 a4 a5 a6 = ()
-- Function.Bundles._._.IsInjection
d_IsInjection_66 a0 a1 a2 a3 a4 a5 a6 = ()
-- Function.Bundles._._.IsInverse
d_IsInverse_68 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Function.Bundles._._.IsLeftInverse
d_IsLeftInverse_70 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Function.Bundles._._.IsRightInverse
d_IsRightInverse_72 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Function.Bundles._._.IsSurjection
d_IsSurjection_74 a0 a1 a2 a3 a4 a5 a6 = ()
-- Function.Bundles._._.IsBijection.Eq₁._≈_
d__'8776'__226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__226 = erased
-- Function.Bundles._._.IsBijection.Eq₁._≉_
d__'8777'__228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> ()
d__'8777'__228 = erased
-- Function.Bundles._._.IsBijection.Eq₁.Carrier
d_Carrier_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 -> ()
d_Carrier_230 = erased
-- Function.Bundles._._.IsBijection.Eq₁.isEquivalence
d_isEquivalence_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_232 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isEquivalence_232 v7
du_isEquivalence_232 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_232 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v2)
-- Function.Bundles._._.IsBijection.Eq₁.isPartialEquivalence
d_isPartialEquivalence_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_234 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_234 v7
du_isPartialEquivalence_234 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_234 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    let v3
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
-- Function.Bundles._._.IsBijection.Eq₁.partialSetoid
d_partialSetoid_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_236 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_partialSetoid_236 v7
du_partialSetoid_236 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_236 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v2))
-- Function.Bundles._._.IsBijection.Eq₁.refl
d_refl_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny
d_refl_238 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_refl_238 v7
du_refl_238 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny
du_refl_238 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v2))
-- Function.Bundles._._.IsBijection.Eq₁.reflexive
d_reflexive_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_240 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_240 v7
du_reflexive_240 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_240 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    let v3
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
        v4
-- Function.Bundles._._.IsBijection.Eq₁.setoid
d_setoid_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_242 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_setoid_242 v7
du_setoid_242 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_242 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_40
      (coe MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1))
-- Function.Bundles._._.IsBijection.Eq₁.sym
d_sym_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_244 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_sym_244 v7
du_sym_244 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_244 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v2))
-- Function.Bundles._._.IsBijection.Eq₁.trans
d_trans_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_246 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_trans_246 v7
du_trans_246 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_246 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v2))
-- Function.Bundles._._.IsBijection.Eq₂._≈_
d__'8776'__250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__250 = erased
-- Function.Bundles._._.IsBijection.Eq₂._≉_
d__'8777'__252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> ()
d__'8777'__252 = erased
-- Function.Bundles._._.IsBijection.Eq₂.Carrier
d_Carrier_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 -> ()
d_Carrier_254 = erased
-- Function.Bundles._._.IsBijection.Eq₂.isEquivalence
d_isEquivalence_256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_256 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isEquivalence_256 v7
du_isEquivalence_256 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_256 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v2)
-- Function.Bundles._._.IsBijection.Eq₂.isPartialEquivalence
d_isPartialEquivalence_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_258 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_258 v7
du_isPartialEquivalence_258 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_258 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    let v3
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
-- Function.Bundles._._.IsBijection.Eq₂.partialSetoid
d_partialSetoid_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_260 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_partialSetoid_260 v7
du_partialSetoid_260 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_260 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v2))
-- Function.Bundles._._.IsBijection.Eq₂.refl
d_refl_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny
d_refl_262 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_refl_262 v7
du_refl_262 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny
du_refl_262 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v2))
-- Function.Bundles._._.IsBijection.Eq₂.reflexive
d_reflexive_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_264 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_264 v7
du_reflexive_264 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_264 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    let v3
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
        v4
-- Function.Bundles._._.IsBijection.Eq₂.setoid
d_setoid_266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_266 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_setoid_266 v7
du_setoid_266 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_266 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_66
      (coe MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1))
-- Function.Bundles._._.IsBijection.Eq₂.sym
d_sym_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_268 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_sym_268 v7
du_sym_268 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_268 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v2))
-- Function.Bundles._._.IsBijection.Eq₂.trans
d_trans_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_270 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_trans_270 v7
du_trans_270 ::
  MAlonzo.Code.Function.Structures.T_IsBijection_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_270 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v2))
-- Function.Bundles._._.IsCongruent.Eq₁._≈_
d__'8776'__282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__282 = erased
-- Function.Bundles._._.IsCongruent.Eq₁._≉_
d__'8777'__284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> ()
d__'8777'__284 = erased
-- Function.Bundles._._.IsCongruent.Eq₁.Carrier
d_Carrier_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 -> ()
d_Carrier_286 = erased
-- Function.Bundles._._.IsCongruent.Eq₁.isEquivalence
d_isEquivalence_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_288 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isEquivalence_288 v7
du_isEquivalence_288 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_288 v0
  = coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v0)
-- Function.Bundles._._.IsCongruent.Eq₁.isPartialEquivalence
d_isPartialEquivalence_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_290 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_290 v7
du_isPartialEquivalence_290 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_290 v0
  = let v1
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
-- Function.Bundles._._.IsCongruent.Eq₁.partialSetoid
d_partialSetoid_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_292 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_partialSetoid_292 v7
du_partialSetoid_292 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_292 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v0))
-- Function.Bundles._._.IsCongruent.Eq₁.refl
d_refl_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny
d_refl_294 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_refl_294 v7
du_refl_294 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny
du_refl_294 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v0))
-- Function.Bundles._._.IsCongruent.Eq₁.reflexive
d_reflexive_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_296 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_296 v7
du_reflexive_296 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_296 v0
  = let v1
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
        v2
-- Function.Bundles._._.IsCongruent.Eq₁.setoid
d_setoid_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_298 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 = du_setoid_298
du_setoid_298 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_298 v0 v1
  = coe MAlonzo.Code.Function.Structures.du_setoid_40 v1
-- Function.Bundles._._.IsCongruent.Eq₁.sym
d_sym_300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_300 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_sym_300 v7
du_sym_300 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_300 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v0))
-- Function.Bundles._._.IsCongruent.Eq₁.trans
d_trans_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_302 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_trans_302 v7
du_trans_302 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_302 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v0))
-- Function.Bundles._._.IsCongruent.Eq₂._≈_
d__'8776'__306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__306 = erased
-- Function.Bundles._._.IsCongruent.Eq₂._≉_
d__'8777'__308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> ()
d__'8777'__308 = erased
-- Function.Bundles._._.IsCongruent.Eq₂.Carrier
d_Carrier_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 -> ()
d_Carrier_310 = erased
-- Function.Bundles._._.IsCongruent.Eq₂.isEquivalence
d_isEquivalence_312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_312 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isEquivalence_312 v7
du_isEquivalence_312 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_312 v0
  = coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v0)
-- Function.Bundles._._.IsCongruent.Eq₂.isPartialEquivalence
d_isPartialEquivalence_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_314 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_314 v7
du_isPartialEquivalence_314 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_314 v0
  = let v1
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
-- Function.Bundles._._.IsCongruent.Eq₂.partialSetoid
d_partialSetoid_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_316 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_partialSetoid_316 v7
du_partialSetoid_316 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_316 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v0))
-- Function.Bundles._._.IsCongruent.Eq₂.refl
d_refl_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny
d_refl_318 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_refl_318 v7
du_refl_318 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny
du_refl_318 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v0))
-- Function.Bundles._._.IsCongruent.Eq₂.reflexive
d_reflexive_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_320 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_320 v7
du_reflexive_320 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_320 v0
  = let v1
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
        v2
-- Function.Bundles._._.IsCongruent.Eq₂.setoid
d_setoid_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_322 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 = du_setoid_322
du_setoid_322 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_322 v0 v1
  = coe MAlonzo.Code.Function.Structures.du_setoid_66 v1
-- Function.Bundles._._.IsCongruent.Eq₂.sym
d_sym_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_324 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_sym_324 v7
du_sym_324 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_324 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v0))
-- Function.Bundles._._.IsCongruent.Eq₂.trans
d_trans_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_326 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_trans_326 v7
du_trans_326 ::
  MAlonzo.Code.Function.Structures.T_IsCongruent_22 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_326 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v0))
-- Function.Bundles._._.IsInverse.Eq₁._≈_
d__'8776'__412 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__412 = erased
-- Function.Bundles._._.IsInverse.Eq₁._≉_
d__'8777'__414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> ()
d__'8777'__414 = erased
-- Function.Bundles._._.IsInverse.Eq₁.Carrier
d_Carrier_416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 -> ()
d_Carrier_416 = erased
-- Function.Bundles._._.IsInverse.Eq₁.isEquivalence
d_isEquivalence_418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_418 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isEquivalence_418 v8
du_isEquivalence_418 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_418 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v2)
-- Function.Bundles._._.IsInverse.Eq₁.isPartialEquivalence
d_isPartialEquivalence_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_420 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_420 v8
du_isPartialEquivalence_420 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_420 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    let v3
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
-- Function.Bundles._._.IsInverse.Eq₁.partialSetoid
d_partialSetoid_422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_422 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_partialSetoid_422 v8
du_partialSetoid_422 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_422 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v2))
-- Function.Bundles._._.IsInverse.Eq₁.refl
d_refl_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny
d_refl_424 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_refl_424 v8
du_refl_424 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny
du_refl_424 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v2))
-- Function.Bundles._._.IsInverse.Eq₁.reflexive
d_reflexive_426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_426 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_426 v8
du_reflexive_426 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_426 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    let v3
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
        v4
-- Function.Bundles._._.IsInverse.Eq₁.setoid
d_setoid_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_428 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_setoid_428 v8
du_setoid_428 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_428 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_40
      (coe MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1))
-- Function.Bundles._._.IsInverse.Eq₁.sym
d_sym_430 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_430 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_sym_430 v8
du_sym_430 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_430 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v2))
-- Function.Bundles._._.IsInverse.Eq₁.trans
d_trans_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_432 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_trans_432 v8
du_trans_432 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_432 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v2))
-- Function.Bundles._._.IsInverse.Eq₂._≈_
d__'8776'__436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__436 = erased
-- Function.Bundles._._.IsInverse.Eq₂._≉_
d__'8777'__438 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> ()
d__'8777'__438 = erased
-- Function.Bundles._._.IsInverse.Eq₂.Carrier
d_Carrier_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 -> ()
d_Carrier_440 = erased
-- Function.Bundles._._.IsInverse.Eq₂.isEquivalence
d_isEquivalence_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_442 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isEquivalence_442 v8
du_isEquivalence_442 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_442 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v2)
-- Function.Bundles._._.IsInverse.Eq₂.isPartialEquivalence
d_isPartialEquivalence_444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_444 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_444 v8
du_isPartialEquivalence_444 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_444 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    let v3
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
-- Function.Bundles._._.IsInverse.Eq₂.partialSetoid
d_partialSetoid_446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_446 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_partialSetoid_446 v8
du_partialSetoid_446 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_446 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v2))
-- Function.Bundles._._.IsInverse.Eq₂.refl
d_refl_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny
d_refl_448 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_refl_448 v8
du_refl_448 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny
du_refl_448 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v2))
-- Function.Bundles._._.IsInverse.Eq₂.reflexive
d_reflexive_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_450 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_450 v8
du_reflexive_450 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_450 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    let v3
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v3))
        v4
-- Function.Bundles._._.IsInverse.Eq₂.setoid
d_setoid_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_452 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_setoid_452 v8
du_setoid_452 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_452 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_66
      (coe MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1))
-- Function.Bundles._._.IsInverse.Eq₂.sym
d_sym_454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_454 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_sym_454 v8
du_sym_454 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_454 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v2))
-- Function.Bundles._._.IsInverse.Eq₂.trans
d_trans_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_456 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_trans_456 v8
du_trans_456 ::
  MAlonzo.Code.Function.Structures.T_IsInverse_468 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_456 v0
  = let v1
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v0) in
    let v2
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v2))
-- Function.Bundles._.Func
d_Func_642 a0 a1 a2 a3 a4 a5 = ()
data T_Func_642
  = C_Func'46'constructor_5935 (AgdaAny -> AgdaAny)
                               (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Bundles._.Func.to
d_to_648 :: T_Func_642 -> AgdaAny -> AgdaAny
d_to_648 v0
  = case coe v0 of
      C_Func'46'constructor_5935 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Func.cong
d_cong_650 ::
  T_Func_642 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_650 v0
  = case coe v0 of
      C_Func'46'constructor_5935 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Func.isCongruent
d_isCongruent_652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> MAlonzo.Code.Function.Structures.T_IsCongruent_22
d_isCongruent_652 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isCongruent_652 v4 v5 v6
du_isCongruent_652 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> MAlonzo.Code.Function.Structures.T_IsCongruent_22
du_isCongruent_652 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsCongruent'46'constructor_985
      (coe d_cong_650 (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
-- Function.Bundles._.Func._.Eq₁._≈_
d__'8776'__658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny -> ()
d__'8776'__658 = erased
-- Function.Bundles._.Func._.Eq₁._≉_
d__'8777'__660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny -> ()
d__'8777'__660 = erased
-- Function.Bundles._.Func._.Eq₁.Carrier
d_Carrier_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> ()
d_Carrier_662 = erased
-- Function.Bundles._.Func._.Eq₁.isEquivalence
d_isEquivalence_664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_664 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_664 v4 v5 v6
du_isEquivalence_664 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_664 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3)
-- Function.Bundles._.Func._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_666 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_666 v4 v5 v6
du_isPartialEquivalence_666 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_666 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
-- Function.Bundles._.Func._.Eq₁.partialSetoid
d_partialSetoid_668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_668 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_668 v4 v5 v6
du_partialSetoid_668 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_668 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v3))
-- Function.Bundles._.Func._.Eq₁.refl
d_refl_670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny
d_refl_670 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_670 v4 v5 v6
du_refl_670 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny
du_refl_670 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3))
-- Function.Bundles._.Func._.Eq₁.reflexive
d_reflexive_672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_672 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_672 v4 v5 v6
du_reflexive_672 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_672 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
        v5
-- Function.Bundles._.Func._.Eq₁.setoid
d_setoid_674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_674 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_674 v4 v5 v6
du_setoid_674 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_674 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.du_setoid_40
      (coe du_isCongruent_652 (coe v0) (coe v1) (coe v2))
-- Function.Bundles._.Func._.Eq₁.sym
d_sym_676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_676 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_676 v4 v5 v6
du_sym_676 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_676 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3))
-- Function.Bundles._.Func._.Eq₁.trans
d_trans_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_678 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_678 v4 v5 v6
du_trans_678 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_678 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3))
-- Function.Bundles._.Func._.Eq₂._≈_
d__'8776'__682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny -> ()
d__'8776'__682 = erased
-- Function.Bundles._.Func._.Eq₂._≉_
d__'8777'__684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny -> ()
d__'8777'__684 = erased
-- Function.Bundles._.Func._.Eq₂.Carrier
d_Carrier_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> ()
d_Carrier_686 = erased
-- Function.Bundles._.Func._.Eq₂.isEquivalence
d_isEquivalence_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_688 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_688 v4 v5 v6
du_isEquivalence_688 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_688 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3)
-- Function.Bundles._.Func._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_690 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_690 v4 v5 v6
du_isPartialEquivalence_690 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_690 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
-- Function.Bundles._.Func._.Eq₂.partialSetoid
d_partialSetoid_692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_692 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_692 v4 v5 v6
du_partialSetoid_692 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_692 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v3))
-- Function.Bundles._.Func._.Eq₂.refl
d_refl_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny
d_refl_694 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_694 v4 v5 v6
du_refl_694 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny
du_refl_694 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3))
-- Function.Bundles._.Func._.Eq₂.reflexive
d_reflexive_696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_696 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_696 v4 v5 v6
du_reflexive_696 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_696 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
        v5
-- Function.Bundles._.Func._.Eq₂.setoid
d_setoid_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_698 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_698 v4 v5 v6
du_setoid_698 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_698 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.du_setoid_66
      (coe du_isCongruent_652 (coe v0) (coe v1) (coe v2))
-- Function.Bundles._.Func._.Eq₂.sym
d_sym_700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_700 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_700 v4 v5 v6
du_sym_700 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_700 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3))
-- Function.Bundles._.Func._.Eq₂.trans
d_trans_702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_702 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_702 v4 v5 v6
du_trans_702 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Func_642 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_702 v0 v1 v2
  = let v3 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3))
-- Function.Bundles._.Injection
d_Injection_704 a0 a1 a2 a3 a4 a5 = ()
data T_Injection_704
  = C_Injection'46'constructor_7999 (AgdaAny -> AgdaAny)
                                    (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                    (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Bundles._.Injection.to
d_to_712 :: T_Injection_704 -> AgdaAny -> AgdaAny
d_to_712 v0
  = case coe v0 of
      C_Injection'46'constructor_7999 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Injection.cong
d_cong_714 ::
  T_Injection_704 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_714 v0
  = case coe v0 of
      C_Injection'46'constructor_7999 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Injection.injective
d_injective_716 ::
  T_Injection_704 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_716 v0
  = case coe v0 of
      C_Injection'46'constructor_7999 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Injection.function
d_function_718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> T_Func_642
d_function_718 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_function_718 v6
du_function_718 :: T_Injection_704 -> T_Func_642
du_function_718 v0
  = coe
      C_Func'46'constructor_5935 (coe d_to_712 (coe v0))
      (coe d_cong_714 (coe v0))
-- Function.Bundles._.Injection._.isCongruent
d_isCongruent_722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
d_isCongruent_722 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isCongruent_722 v4 v5 v6
du_isCongruent_722 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
du_isCongruent_722 v0 v1 v2
  = coe
      du_isCongruent_652 (coe v0) (coe v1) (coe du_function_718 (coe v2))
-- Function.Bundles._.Injection._.Eq₁._≈_
d__'8776'__726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny -> ()
d__'8776'__726 = erased
-- Function.Bundles._.Injection._.Eq₁._≉_
d__'8777'__728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny -> ()
d__'8777'__728 = erased
-- Function.Bundles._.Injection._.Eq₁.Carrier
d_Carrier_730 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> ()
d_Carrier_730 = erased
-- Function.Bundles._.Injection._.Eq₁.isEquivalence
d_isEquivalence_732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_732 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_732 v4 v5 v6
du_isEquivalence_732 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_732 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v4)
-- Function.Bundles._.Injection._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_734 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_734 v4 v5 v6
du_isPartialEquivalence_734 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_734 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    let v5
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v5))
-- Function.Bundles._.Injection._.Eq₁.partialSetoid
d_partialSetoid_736 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_736 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_736 v4 v5 v6
du_partialSetoid_736 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_736 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v4))
-- Function.Bundles._.Injection._.Eq₁.refl
d_refl_738 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny
d_refl_738 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_738 v4 v5 v6
du_refl_738 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny
du_refl_738 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v4))
-- Function.Bundles._.Injection._.Eq₁.reflexive
d_reflexive_740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_740 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_740 v4 v5 v6
du_reflexive_740 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_740 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    let v5
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v5))
        v6
-- Function.Bundles._.Injection._.Eq₁.setoid
d_setoid_742 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_742 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_742 v4 v5 v6
du_setoid_742 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_742 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_40
      (coe du_isCongruent_652 (coe v0) (coe v1) (coe v3))
-- Function.Bundles._.Injection._.Eq₁.sym
d_sym_744 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_744 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_744 v4 v5 v6
du_sym_744 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_744 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v4))
-- Function.Bundles._.Injection._.Eq₁.trans
d_trans_746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_746 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_746 v4 v5 v6
du_trans_746 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_746 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v4))
-- Function.Bundles._.Injection._.Eq₂._≈_
d__'8776'__750 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny -> ()
d__'8776'__750 = erased
-- Function.Bundles._.Injection._.Eq₂._≉_
d__'8777'__752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny -> ()
d__'8777'__752 = erased
-- Function.Bundles._.Injection._.Eq₂.Carrier
d_Carrier_754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> ()
d_Carrier_754 = erased
-- Function.Bundles._.Injection._.Eq₂.isEquivalence
d_isEquivalence_756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_756 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_756 v4 v5 v6
du_isEquivalence_756 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_756 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v4)
-- Function.Bundles._.Injection._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_758 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_758 v4 v5 v6
du_isPartialEquivalence_758 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_758 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    let v5
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v5))
-- Function.Bundles._.Injection._.Eq₂.partialSetoid
d_partialSetoid_760 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_760 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_760 v4 v5 v6
du_partialSetoid_760 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_760 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v4))
-- Function.Bundles._.Injection._.Eq₂.refl
d_refl_762 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny
d_refl_762 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_762 v4 v5 v6
du_refl_762 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny
du_refl_762 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v4))
-- Function.Bundles._.Injection._.Eq₂.reflexive
d_reflexive_764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_764 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_764 v4 v5 v6
du_reflexive_764 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_764 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    let v5
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v5))
        v6
-- Function.Bundles._.Injection._.Eq₂.setoid
d_setoid_766 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_766 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_766 v4 v5 v6
du_setoid_766 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_766 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_66
      (coe du_isCongruent_652 (coe v0) (coe v1) (coe v3))
-- Function.Bundles._.Injection._.Eq₂.sym
d_sym_768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_768 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_768 v4 v5 v6
du_sym_768 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_768 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v4))
-- Function.Bundles._.Injection._.Eq₂.trans
d_trans_770 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_770 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_770 v4 v5 v6
du_trans_770 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_770 v0 v1 v2
  = let v3 = coe du_function_718 (coe v2) in
    let v4 = coe du_isCongruent_652 (coe v0) (coe v1) (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v4))
-- Function.Bundles._.Injection.isInjection
d_isInjection_772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92
d_isInjection_772 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isInjection_772 v4 v5 v6
du_isInjection_772 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_704 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92
du_isInjection_772 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsInjection'46'constructor_3991
      (coe
         du_isCongruent_652 (coe v0) (coe v1)
         (coe du_function_718 (coe v2)))
      (coe d_injective_716 (coe v2))
-- Function.Bundles._.Surjection
d_Surjection_774 a0 a1 a2 a3 a4 a5 = ()
data T_Surjection_774
  = C_Surjection'46'constructor_10213 (AgdaAny -> AgdaAny)
                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                      (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Function.Bundles._.Surjection.to
d_to_782 :: T_Surjection_774 -> AgdaAny -> AgdaAny
d_to_782 v0
  = case coe v0 of
      C_Surjection'46'constructor_10213 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Surjection.cong
d_cong_784 ::
  T_Surjection_774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_784 v0
  = case coe v0 of
      C_Surjection'46'constructor_10213 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Surjection.surjective
d_surjective_786 ::
  T_Surjection_774 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_786 v0
  = case coe v0 of
      C_Surjection'46'constructor_10213 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Surjection.to⁻
d_to'8315'_788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny
d_to'8315'_788 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_to'8315'_788 v6 v7
du_to'8315'_788 :: T_Surjection_774 -> AgdaAny -> AgdaAny
du_to'8315'_788 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_surjective_786 v0 v1)
-- Function.Bundles._.Surjection.isCongruent
d_isCongruent_790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
d_isCongruent_790 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isCongruent_790 v4 v5 v6
du_isCongruent_790 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
du_isCongruent_790 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsCongruent'46'constructor_985
      (coe d_cong_784 (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
-- Function.Bundles._.Surjection._.Eq₁._≈_
d__'8776'__796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny -> ()
d__'8776'__796 = erased
-- Function.Bundles._.Surjection._.Eq₁._≉_
d__'8777'__798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny -> ()
d__'8777'__798 = erased
-- Function.Bundles._.Surjection._.Eq₁.Carrier
d_Carrier_800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> ()
d_Carrier_800 = erased
-- Function.Bundles._.Surjection._.Eq₁.isEquivalence
d_isEquivalence_802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_802 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_802 v4 v5 v6
du_isEquivalence_802 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_802 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3)
-- Function.Bundles._.Surjection._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_804 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_804 v4 v5 v6
du_isPartialEquivalence_804 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_804 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
-- Function.Bundles._.Surjection._.Eq₁.partialSetoid
d_partialSetoid_806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_806 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_806 v4 v5 v6
du_partialSetoid_806 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_806 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v3))
-- Function.Bundles._.Surjection._.Eq₁.refl
d_refl_808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny
d_refl_808 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_808 v4 v5 v6
du_refl_808 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny
du_refl_808 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3))
-- Function.Bundles._.Surjection._.Eq₁.reflexive
d_reflexive_810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_810 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_810 v4 v5 v6
du_reflexive_810 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_810 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
        v5
-- Function.Bundles._.Surjection._.Eq₁.setoid
d_setoid_812 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_812 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_812 v4 v5 v6
du_setoid_812 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_812 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.du_setoid_40
      (coe du_isCongruent_790 (coe v0) (coe v1) (coe v2))
-- Function.Bundles._.Surjection._.Eq₁.sym
d_sym_814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_814 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_814 v4 v5 v6
du_sym_814 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_814 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3))
-- Function.Bundles._.Surjection._.Eq₁.trans
d_trans_816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_816 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_816 v4 v5 v6
du_trans_816 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_816 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3))
-- Function.Bundles._.Surjection._.Eq₂._≈_
d__'8776'__820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny -> ()
d__'8776'__820 = erased
-- Function.Bundles._.Surjection._.Eq₂._≉_
d__'8777'__822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny -> ()
d__'8777'__822 = erased
-- Function.Bundles._.Surjection._.Eq₂.Carrier
d_Carrier_824 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> ()
d_Carrier_824 = erased
-- Function.Bundles._.Surjection._.Eq₂.isEquivalence
d_isEquivalence_826 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_826 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_826 v4 v5 v6
du_isEquivalence_826 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_826 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3)
-- Function.Bundles._.Surjection._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_828 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_828 v4 v5 v6
du_isPartialEquivalence_828 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_828 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
-- Function.Bundles._.Surjection._.Eq₂.partialSetoid
d_partialSetoid_830 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_830 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_830 v4 v5 v6
du_partialSetoid_830 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_830 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v3))
-- Function.Bundles._.Surjection._.Eq₂.refl
d_refl_832 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny
d_refl_832 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_832 v4 v5 v6
du_refl_832 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny
du_refl_832 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3))
-- Function.Bundles._.Surjection._.Eq₂.reflexive
d_reflexive_834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_834 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_834 v4 v5 v6
du_reflexive_834 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_834 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
        v5
-- Function.Bundles._.Surjection._.Eq₂.setoid
d_setoid_836 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_836 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_836 v4 v5 v6
du_setoid_836 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_836 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.du_setoid_66
      (coe du_isCongruent_790 (coe v0) (coe v1) (coe v2))
-- Function.Bundles._.Surjection._.Eq₂.sym
d_sym_838 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_838 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_838 v4 v5 v6
du_sym_838 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_838 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3))
-- Function.Bundles._.Surjection._.Eq₂.trans
d_trans_840 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_840 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_840 v4 v5 v6
du_trans_840 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_840 v0 v1 v2
  = let v3 = coe du_isCongruent_790 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3))
-- Function.Bundles._.Surjection.isSurjection
d_isSurjection_842 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162
d_isSurjection_842 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isSurjection_842 v4 v5 v6
du_isSurjection_842 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_774 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162
du_isSurjection_842 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsSurjection'46'constructor_6455
      (coe du_isCongruent_790 (coe v0) (coe v1) (coe v2))
      (coe d_surjective_786 (coe v2))
-- Function.Bundles._.Bijection
d_Bijection_844 a0 a1 a2 a3 a4 a5 = ()
data T_Bijection_844
  = C_Bijection'46'constructor_13257 (AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                     MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Function.Bundles._.Bijection.to
d_to_852 :: T_Bijection_844 -> AgdaAny -> AgdaAny
d_to_852 v0
  = case coe v0 of
      C_Bijection'46'constructor_13257 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Bijection.cong
d_cong_854 ::
  T_Bijection_844 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_854 v0
  = case coe v0 of
      C_Bijection'46'constructor_13257 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Bijection.bijective
d_bijective_856 ::
  T_Bijection_844 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_bijective_856 v0
  = case coe v0 of
      C_Bijection'46'constructor_13257 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Bijection.injective
d_injective_858 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_858 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_injective_858 v6 v7 v8
du_injective_858 ::
  T_Bijection_844 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_injective_858 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (d_bijective_856 (coe v0))
      v1 v2
-- Function.Bundles._.Bijection.surjective
d_surjective_860 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_860 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_surjective_860 v6
du_surjective_860 ::
  T_Bijection_844 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_surjective_860 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_bijective_856 (coe v0))
-- Function.Bundles._.Bijection.injection
d_injection_862 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> T_Injection_704
d_injection_862 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_injection_862 v6
du_injection_862 :: T_Bijection_844 -> T_Injection_704
du_injection_862 v0
  = coe
      C_Injection'46'constructor_7999 (coe d_to_852 (coe v0))
      (coe d_cong_854 (coe v0)) (coe du_injective_858 (coe v0))
-- Function.Bundles._.Bijection.surjection
d_surjection_864 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> T_Surjection_774
d_surjection_864 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_surjection_864 v6
du_surjection_864 :: T_Bijection_844 -> T_Surjection_774
du_surjection_864 v0
  = coe
      C_Surjection'46'constructor_10213 (coe d_to_852 (coe v0))
      (coe d_cong_854 (coe v0)) (coe du_surjective_860 (coe v0))
-- Function.Bundles._.Bijection._.isInjection
d_isInjection_868 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92
d_isInjection_868 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isInjection_868 v4 v5 v6
du_isInjection_868 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92
du_isInjection_868 v0 v1 v2
  = coe
      du_isInjection_772 (coe v0) (coe v1)
      (coe du_injection_862 (coe v2))
-- Function.Bundles._.Bijection._.isSurjection
d_isSurjection_872 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162
d_isSurjection_872 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isSurjection_872 v4 v5 v6
du_isSurjection_872 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162
du_isSurjection_872 v0 v1 v2
  = coe
      du_isSurjection_842 (coe v0) (coe v1)
      (coe du_surjection_864 (coe v2))
-- Function.Bundles._.Bijection._.to⁻
d_to'8315'_874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny
d_to'8315'_874 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_to'8315'_874 v6
du_to'8315'_874 :: T_Bijection_844 -> AgdaAny -> AgdaAny
du_to'8315'_874 v0
  = coe du_to'8315'_788 (coe du_surjection_864 (coe v0))
-- Function.Bundles._.Bijection.isBijection
d_isBijection_876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
d_isBijection_876 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isBijection_876 v4 v5 v6
du_isBijection_876 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
du_isBijection_876 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsBijection'46'constructor_8915
      (coe
         du_isInjection_772 (coe v0) (coe v1)
         (coe du_injection_862 (coe v2)))
      (coe du_surjective_860 (coe v2))
-- Function.Bundles._.Bijection._.Eq₁._≈_
d__'8776'__882 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny -> ()
d__'8776'__882 = erased
-- Function.Bundles._.Bijection._.Eq₁._≉_
d__'8777'__884 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny -> ()
d__'8777'__884 = erased
-- Function.Bundles._.Bijection._.Eq₁.Carrier
d_Carrier_886 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> ()
d_Carrier_886 = erased
-- Function.Bundles._.Bijection._.Eq₁.isEquivalence
d_isEquivalence_888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_888 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_888 v4 v5 v6
du_isEquivalence_888 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_888 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v5)
-- Function.Bundles._.Bijection._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_890 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_890 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_890 v4 v5 v6
du_isPartialEquivalence_890 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_890 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    let v6
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6))
-- Function.Bundles._.Bijection._.Eq₁.partialSetoid
d_partialSetoid_892 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_892 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_892 v4 v5 v6
du_partialSetoid_892 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_892 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v5))
-- Function.Bundles._.Bijection._.Eq₁.refl
d_refl_894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny
d_refl_894 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_894 v4 v5 v6
du_refl_894 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny
du_refl_894 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v5))
-- Function.Bundles._.Bijection._.Eq₁.reflexive
d_reflexive_896 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_896 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_896 v4 v5 v6
du_reflexive_896 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_896 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    let v6
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6))
        v7
-- Function.Bundles._.Bijection._.Eq₁.setoid
d_setoid_898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_898 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_898 v4 v5 v6
du_setoid_898 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_898 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_40
      (coe MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4))
-- Function.Bundles._.Bijection._.Eq₁.sym
d_sym_900 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_900 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_900 v4 v5 v6
du_sym_900 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_900 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v5))
-- Function.Bundles._.Bijection._.Eq₁.trans
d_trans_902 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_902 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_902 v4 v5 v6
du_trans_902 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_902 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v5))
-- Function.Bundles._.Bijection._.Eq₂._≈_
d__'8776'__906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny -> ()
d__'8776'__906 = erased
-- Function.Bundles._.Bijection._.Eq₂._≉_
d__'8777'__908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny -> ()
d__'8777'__908 = erased
-- Function.Bundles._.Bijection._.Eq₂.Carrier
d_Carrier_910 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> ()
d_Carrier_910 = erased
-- Function.Bundles._.Bijection._.Eq₂.isEquivalence
d_isEquivalence_912 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_912 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_912 v4 v5 v6
du_isEquivalence_912 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_912 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v5)
-- Function.Bundles._.Bijection._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_914 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_914 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_914 v4 v5 v6
du_isPartialEquivalence_914 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_914 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    let v6
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6))
-- Function.Bundles._.Bijection._.Eq₂.partialSetoid
d_partialSetoid_916 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_916 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_916 v4 v5 v6
du_partialSetoid_916 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_916 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v5))
-- Function.Bundles._.Bijection._.Eq₂.refl
d_refl_918 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny
d_refl_918 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_918 v4 v5 v6
du_refl_918 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny
du_refl_918 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v5))
-- Function.Bundles._.Bijection._.Eq₂.reflexive
d_reflexive_920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_920 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_920 v4 v5 v6
du_reflexive_920 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_920 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    let v6
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6))
        v7
-- Function.Bundles._.Bijection._.Eq₂.setoid
d_setoid_922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_922 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_922 v4 v5 v6
du_setoid_922 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_922 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_66
      (coe MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4))
-- Function.Bundles._.Bijection._.Eq₂.sym
d_sym_924 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_924 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_924 v4 v5 v6
du_sym_924 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_924 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v5))
-- Function.Bundles._.Bijection._.Eq₂.trans
d_trans_926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_926 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_926 v4 v5 v6
du_trans_926 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_844 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_926 v0 v1 v2
  = let v3 = coe du_isBijection_876 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isInjection_240 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_100 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v5))
-- Function.Bundles._.Equivalence
d_Equivalence_928 a0 a1 a2 a3 a4 a5 = ()
data T_Equivalence_928
  = C_Equivalence'46'constructor_17233 (AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Bundles._.Equivalence.to
d_to_938 :: T_Equivalence_928 -> AgdaAny -> AgdaAny
d_to_938 v0
  = case coe v0 of
      C_Equivalence'46'constructor_17233 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Equivalence.from
d_from_940 :: T_Equivalence_928 -> AgdaAny -> AgdaAny
d_from_940 v0
  = case coe v0 of
      C_Equivalence'46'constructor_17233 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Equivalence.to-cong
d_to'45'cong_942 ::
  T_Equivalence_928 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'cong_942 v0
  = case coe v0 of
      C_Equivalence'46'constructor_17233 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Equivalence.from-cong
d_from'45'cong_944 ::
  T_Equivalence_928 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'45'cong_944 v0
  = case coe v0 of
      C_Equivalence'46'constructor_17233 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.LeftInverse
d_LeftInverse_946 a0 a1 a2 a3 a4 a5 = ()
data T_LeftInverse_946
  = C_LeftInverse'46'constructor_18307 (AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny)
-- Function.Bundles._.LeftInverse.to
d_to_958 :: T_LeftInverse_946 -> AgdaAny -> AgdaAny
d_to_958 v0
  = case coe v0 of
      C_LeftInverse'46'constructor_18307 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.LeftInverse.from
d_from_960 :: T_LeftInverse_946 -> AgdaAny -> AgdaAny
d_from_960 v0
  = case coe v0 of
      C_LeftInverse'46'constructor_18307 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.LeftInverse.to-cong
d_to'45'cong_962 ::
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'cong_962 v0
  = case coe v0 of
      C_LeftInverse'46'constructor_18307 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.LeftInverse.from-cong
d_from'45'cong_964 ::
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'45'cong_964 v0
  = case coe v0 of
      C_LeftInverse'46'constructor_18307 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.LeftInverse.inverseˡ
d_inverse'737'_966 :: T_LeftInverse_946 -> AgdaAny -> AgdaAny
d_inverse'737'_966 v0
  = case coe v0 of
      C_LeftInverse'46'constructor_18307 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.LeftInverse.isCongruent
d_isCongruent_968 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
d_isCongruent_968 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isCongruent_968 v4 v5 v6
du_isCongruent_968 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
du_isCongruent_968 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsCongruent'46'constructor_985
      (coe d_to'45'cong_962 (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
-- Function.Bundles._.LeftInverse._.Eq₁._≈_
d__'8776'__974 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> ()
d__'8776'__974 = erased
-- Function.Bundles._.LeftInverse._.Eq₁._≉_
d__'8777'__976 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> ()
d__'8777'__976 = erased
-- Function.Bundles._.LeftInverse._.Eq₁.Carrier
d_Carrier_978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> ()
d_Carrier_978 = erased
-- Function.Bundles._.LeftInverse._.Eq₁.isEquivalence
d_isEquivalence_980 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_980 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_980 v4 v5 v6
du_isEquivalence_980 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_980 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3)
-- Function.Bundles._.LeftInverse._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_982 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_982 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_982 v4 v5 v6
du_isPartialEquivalence_982 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_982 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
-- Function.Bundles._.LeftInverse._.Eq₁.partialSetoid
d_partialSetoid_984 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_984 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_984 v4 v5 v6
du_partialSetoid_984 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_984 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v3))
-- Function.Bundles._.LeftInverse._.Eq₁.refl
d_refl_986 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny
d_refl_986 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_986 v4 v5 v6
du_refl_986 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny
du_refl_986 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3))
-- Function.Bundles._.LeftInverse._.Eq₁.reflexive
d_reflexive_988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_988 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_988 v4 v5 v6
du_reflexive_988 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_988 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
        v5
-- Function.Bundles._.LeftInverse._.Eq₁.setoid
d_setoid_990 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_990 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_990 v4 v5 v6
du_setoid_990 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_990 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.du_setoid_40
      (coe du_isCongruent_968 (coe v0) (coe v1) (coe v2))
-- Function.Bundles._.LeftInverse._.Eq₁.sym
d_sym_992 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_992 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_992 v4 v5 v6
du_sym_992 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_992 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3))
-- Function.Bundles._.LeftInverse._.Eq₁.trans
d_trans_994 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_994 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_994 v4 v5 v6
du_trans_994 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_994 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v3))
-- Function.Bundles._.LeftInverse._.Eq₂._≈_
d__'8776'__998 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> ()
d__'8776'__998 = erased
-- Function.Bundles._.LeftInverse._.Eq₂._≉_
d__'8777'__1000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1000 = erased
-- Function.Bundles._.LeftInverse._.Eq₂.Carrier
d_Carrier_1002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> ()
d_Carrier_1002 = erased
-- Function.Bundles._.LeftInverse._.Eq₂.isEquivalence
d_isEquivalence_1004 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1004 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_1004 v4 v5 v6
du_isEquivalence_1004 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_1004 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3)
-- Function.Bundles._.LeftInverse._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_1006 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1006 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_1006 v4 v5 v6
du_isPartialEquivalence_1006 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1006 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
-- Function.Bundles._.LeftInverse._.Eq₂.partialSetoid
d_partialSetoid_1008 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_1008 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_1008 v4 v5 v6
du_partialSetoid_1008 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_1008 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v3))
-- Function.Bundles._.LeftInverse._.Eq₂.refl
d_refl_1010 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny
d_refl_1010 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_1010 v4 v5 v6
du_refl_1010 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny
du_refl_1010 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3))
-- Function.Bundles._.LeftInverse._.Eq₂.reflexive
d_reflexive_1012 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1012 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_1012 v4 v5 v6
du_reflexive_1012 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1012 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    let v4
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4))
        v5
-- Function.Bundles._.LeftInverse._.Eq₂.setoid
d_setoid_1014 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1014 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_1014 v4 v5 v6
du_setoid_1014 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1014 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.du_setoid_66
      (coe du_isCongruent_968 (coe v0) (coe v1) (coe v2))
-- Function.Bundles._.LeftInverse._.Eq₂.sym
d_sym_1016 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1016 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_1016 v4 v5 v6
du_sym_1016 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_1016 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3))
-- Function.Bundles._.LeftInverse._.Eq₂.trans
d_trans_1018 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1018 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_1018 v4 v5 v6
du_trans_1018 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_1018 v0 v1 v2
  = let v3 = coe du_isCongruent_968 (coe v0) (coe v1) (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v3))
-- Function.Bundles._.LeftInverse.isLeftInverse
d_isLeftInverse_1020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
d_isLeftInverse_1020 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isLeftInverse_1020 v4 v5 v6
du_isLeftInverse_1020 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
du_isLeftInverse_1020 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsLeftInverse'46'constructor_13035
      (coe du_isCongruent_968 (coe v0) (coe v1) (coe v2))
      (coe d_from'45'cong_964 (coe v2)) (coe d_inverse'737'_966 (coe v2))
-- Function.Bundles._.LeftInverse.equivalence
d_equivalence_1022 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_946 -> T_Equivalence_928
d_equivalence_1022 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_equivalence_1022 v6
du_equivalence_1022 :: T_LeftInverse_946 -> T_Equivalence_928
du_equivalence_1022 v0
  = coe
      C_Equivalence'46'constructor_17233 (coe d_to_958 (coe v0))
      (coe d_from_960 (coe v0)) (coe d_to'45'cong_962 (coe v0))
      (coe d_from'45'cong_964 (coe v0))
-- Function.Bundles._.RightInverse
d_RightInverse_1024 a0 a1 a2 a3 a4 a5 = ()
data T_RightInverse_1024
  = C_RightInverse'46'constructor_21741 (AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny)
-- Function.Bundles._.RightInverse.to
d_to_1036 :: T_RightInverse_1024 -> AgdaAny -> AgdaAny
d_to_1036 v0
  = case coe v0 of
      C_RightInverse'46'constructor_21741 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.RightInverse.from
d_from_1038 :: T_RightInverse_1024 -> AgdaAny -> AgdaAny
d_from_1038 v0
  = case coe v0 of
      C_RightInverse'46'constructor_21741 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.RightInverse.to-cong
d_to'45'cong_1040 ::
  T_RightInverse_1024 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'cong_1040 v0
  = case coe v0 of
      C_RightInverse'46'constructor_21741 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.RightInverse.from-cong
d_from'45'cong_1042 ::
  T_RightInverse_1024 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'45'cong_1042 v0
  = case coe v0 of
      C_RightInverse'46'constructor_21741 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.RightInverse.inverseʳ
d_inverse'691'_1044 :: T_RightInverse_1024 -> AgdaAny -> AgdaAny
d_inverse'691'_1044 v0
  = case coe v0 of
      C_RightInverse'46'constructor_21741 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.RightInverse.isCongruent
d_isCongruent_1046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_RightInverse_1024 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
d_isCongruent_1046 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isCongruent_1046 v4 v5 v6
du_isCongruent_1046 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_RightInverse_1024 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
du_isCongruent_1046 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsCongruent'46'constructor_985
      (coe d_to'45'cong_1040 (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
-- Function.Bundles._.RightInverse.isRightInverse
d_isRightInverse_1048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_RightInverse_1024 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
d_isRightInverse_1048 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isRightInverse_1048 v4 v5 v6
du_isRightInverse_1048 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_RightInverse_1024 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
du_isRightInverse_1048 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsRightInverse'46'constructor_16307
      (coe du_isCongruent_1046 (coe v0) (coe v1) (coe v2))
      (coe d_from'45'cong_1042 (coe v2))
      (coe d_inverse'691'_1044 (coe v2))
-- Function.Bundles._.RightInverse.equivalence
d_equivalence_1050 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_RightInverse_1024 -> T_Equivalence_928
d_equivalence_1050 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_equivalence_1050 v6
du_equivalence_1050 :: T_RightInverse_1024 -> T_Equivalence_928
du_equivalence_1050 v0
  = coe
      C_Equivalence'46'constructor_17233 (coe d_to_1036 (coe v0))
      (coe d_from_1038 (coe v0)) (coe d_to'45'cong_1040 (coe v0))
      (coe d_from'45'cong_1042 (coe v0))
-- Function.Bundles._.Inverse
d_Inverse_1052 a0 a1 a2 a3 a4 a5 = ()
data T_Inverse_1052
  = C_Inverse'46'constructor_24077 (AgdaAny -> AgdaAny)
                                   (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                   (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Function.Bundles._.Inverse.to
d_to_1064 :: T_Inverse_1052 -> AgdaAny -> AgdaAny
d_to_1064 v0
  = case coe v0 of
      C_Inverse'46'constructor_24077 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Inverse.from
d_from_1066 :: T_Inverse_1052 -> AgdaAny -> AgdaAny
d_from_1066 v0
  = case coe v0 of
      C_Inverse'46'constructor_24077 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Inverse.to-cong
d_to'45'cong_1068 ::
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'cong_1068 v0
  = case coe v0 of
      C_Inverse'46'constructor_24077 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Inverse.from-cong
d_from'45'cong_1070 ::
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'45'cong_1070 v0
  = case coe v0 of
      C_Inverse'46'constructor_24077 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Inverse.inverse
d_inverse_1072 ::
  T_Inverse_1052 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1072 v0
  = case coe v0 of
      C_Inverse'46'constructor_24077 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.Inverse.inverseˡ
d_inverse'737'_1074 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny
d_inverse'737'_1074 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_inverse'737'_1074 v6
du_inverse'737'_1074 :: T_Inverse_1052 -> AgdaAny -> AgdaAny
du_inverse'737'_1074 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_inverse_1072 (coe v0))
-- Function.Bundles._.Inverse.inverseʳ
d_inverse'691'_1076 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny
d_inverse'691'_1076 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_inverse'691'_1076 v6
du_inverse'691'_1076 :: T_Inverse_1052 -> AgdaAny -> AgdaAny
du_inverse'691'_1076 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_inverse_1072 (coe v0))
-- Function.Bundles._.Inverse.leftInverse
d_leftInverse_1078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> T_LeftInverse_946
d_leftInverse_1078 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_leftInverse_1078 v6
du_leftInverse_1078 :: T_Inverse_1052 -> T_LeftInverse_946
du_leftInverse_1078 v0
  = coe
      C_LeftInverse'46'constructor_18307 (coe d_to_1064 (coe v0))
      (coe d_from_1066 (coe v0)) (coe d_to'45'cong_1068 (coe v0))
      (coe d_from'45'cong_1070 (coe v0))
      (coe du_inverse'737'_1074 (coe v0))
-- Function.Bundles._.Inverse.rightInverse
d_rightInverse_1080 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> T_RightInverse_1024
d_rightInverse_1080 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_rightInverse_1080 v6
du_rightInverse_1080 :: T_Inverse_1052 -> T_RightInverse_1024
du_rightInverse_1080 v0
  = coe
      C_RightInverse'46'constructor_21741 (coe d_to_1064 (coe v0))
      (coe d_from_1066 (coe v0)) (coe d_to'45'cong_1068 (coe v0))
      (coe d_from'45'cong_1070 (coe v0))
      (coe du_inverse'691'_1076 (coe v0))
-- Function.Bundles._.Inverse._.isLeftInverse
d_isLeftInverse_1084 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
d_isLeftInverse_1084 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isLeftInverse_1084 v4 v5 v6
du_isLeftInverse_1084 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
du_isLeftInverse_1084 v0 v1 v2
  = coe
      du_isLeftInverse_1020 (coe v0) (coe v1)
      (coe du_leftInverse_1078 (coe v2))
-- Function.Bundles._.Inverse._.isRightInverse
d_isRightInverse_1088 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
d_isRightInverse_1088 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isRightInverse_1088 v4 v5 v6
du_isRightInverse_1088 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
du_isRightInverse_1088 v0 v1 v2
  = coe
      du_isRightInverse_1048 (coe v0) (coe v1)
      (coe du_rightInverse_1080 (coe v2))
-- Function.Bundles._.Inverse.isInverse
d_isInverse_1090 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> MAlonzo.Code.Function.Structures.T_IsInverse_468
d_isInverse_1090 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isInverse_1090 v4 v5 v6
du_isInverse_1090 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> MAlonzo.Code.Function.Structures.T_IsInverse_468
du_isInverse_1090 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsInverse'46'constructor_19111
      (coe
         du_isLeftInverse_1020 (coe v0) (coe v1)
         (coe du_leftInverse_1078 (coe v2)))
      (coe du_inverse'691'_1076 (coe v2))
-- Function.Bundles._.Inverse._.Eq₁._≈_
d__'8776'__1096 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1096 = erased
-- Function.Bundles._.Inverse._.Eq₁._≉_
d__'8777'__1098 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1098 = erased
-- Function.Bundles._.Inverse._.Eq₁.Carrier
d_Carrier_1100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> ()
d_Carrier_1100 = erased
-- Function.Bundles._.Inverse._.Eq₁.isEquivalence
d_isEquivalence_1102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1102 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_1102 v4 v5 v6
du_isEquivalence_1102 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_1102 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v5)
-- Function.Bundles._.Inverse._.Eq₁.isPartialEquivalence
d_isPartialEquivalence_1104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1104 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_1104 v4 v5 v6
du_isPartialEquivalence_1104 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1104 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    let v6
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6))
-- Function.Bundles._.Inverse._.Eq₁.partialSetoid
d_partialSetoid_1106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_1106 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_1106 v4 v5 v6
du_partialSetoid_1106 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_1106 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v5))
-- Function.Bundles._.Inverse._.Eq₁.refl
d_refl_1108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny
d_refl_1108 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_1108 v4 v5 v6
du_refl_1108 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny
du_refl_1108 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v5))
-- Function.Bundles._.Inverse._.Eq₁.reflexive
d_reflexive_1110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1110 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_1110 v4 v5 v6
du_reflexive_1110 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1110 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    let v6
          = coe MAlonzo.Code.Function.Structures.du_setoid_40 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6))
        v7
-- Function.Bundles._.Inverse._.Eq₁.setoid
d_setoid_1112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1112 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_1112 v4 v5 v6
du_setoid_1112 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1112 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_40
      (coe MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4))
-- Function.Bundles._.Inverse._.Eq₁.sym
d_sym_1114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1114 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_1114 v4 v5 v6
du_sym_1114 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_1114 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v5))
-- Function.Bundles._.Inverse._.Eq₁.trans
d_trans_1116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1116 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_1116 v4 v5 v6
du_trans_1116 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_1116 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8321'_34 (coe v5))
-- Function.Bundles._.Inverse._.Eq₂._≈_
d__'8776'__1120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> ()
d__'8776'__1120 = erased
-- Function.Bundles._.Inverse._.Eq₂._≉_
d__'8777'__1122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> ()
d__'8777'__1122 = erased
-- Function.Bundles._.Inverse._.Eq₂.Carrier
d_Carrier_1124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> ()
d_Carrier_1124 = erased
-- Function.Bundles._.Inverse._.Eq₂.isEquivalence
d_isEquivalence_1126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1126 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isEquivalence_1126 v4 v5 v6
du_isEquivalence_1126 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_1126 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v5)
-- Function.Bundles._.Inverse._.Eq₂.isPartialEquivalence
d_isPartialEquivalence_1128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1128 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isPartialEquivalence_1128 v4 v5 v6
du_isPartialEquivalence_1128 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1128 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    let v6
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6))
-- Function.Bundles._.Inverse._.Eq₂.partialSetoid
d_partialSetoid_1130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
d_partialSetoid_1130 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_partialSetoid_1130 v4 v5 v6
du_partialSetoid_1130 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_PartialSetoid_10
du_partialSetoid_1130 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Bundles.du_partialSetoid_74
      (coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v5))
-- Function.Bundles._.Inverse._.Eq₂.refl
d_refl_1132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny
d_refl_1132 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_refl_1132 v4 v5 v6
du_refl_1132 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny
du_refl_1132 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v5))
-- Function.Bundles._.Inverse._.Eq₂.reflexive
d_reflexive_1134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1134 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_reflexive_1134 v4 v5 v6
du_reflexive_1134 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1134 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    let v6
          = coe MAlonzo.Code.Function.Structures.du_setoid_66 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v6))
        v7
-- Function.Bundles._.Inverse._.Eq₂.setoid
d_setoid_1136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1136 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_setoid_1136 v4 v5 v6
du_setoid_1136 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1136 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    coe
      MAlonzo.Code.Function.Structures.du_setoid_66
      (coe MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4))
-- Function.Bundles._.Inverse._.Eq₂.sym
d_sym_1138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1138 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_1138 v4 v5 v6
du_sym_1138 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_1138 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v5))
-- Function.Bundles._.Inverse._.Eq₂.trans
d_trans_1140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1140 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_trans_1140 v4 v5 v6
du_trans_1140 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Inverse_1052 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_1140 v0 v1 v2
  = let v3 = coe du_isInverse_1090 (coe v0) (coe v1) (coe v2) in
    let v4
          = MAlonzo.Code.Function.Structures.d_isLeftInverse_478 (coe v3) in
    let v5
          = MAlonzo.Code.Function.Structures.d_isCongruent_324 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Function.Structures.d_isEquivalence'8322'_36 (coe v5))
-- Function.Bundles._.BiEquivalence
d_BiEquivalence_1142 a0 a1 a2 a3 a4 a5 = ()
data T_BiEquivalence_1142
  = C_BiEquivalence'46'constructor_28745 (AgdaAny -> AgdaAny)
                                         (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                         (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                         (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                         (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Bundles._.BiEquivalence.to
d_to_1156 :: T_BiEquivalence_1142 -> AgdaAny -> AgdaAny
d_to_1156 v0
  = case coe v0 of
      C_BiEquivalence'46'constructor_28745 v1 v2 v3 v4 v5 v6 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiEquivalence.from₁
d_from'8321'_1158 :: T_BiEquivalence_1142 -> AgdaAny -> AgdaAny
d_from'8321'_1158 v0
  = case coe v0 of
      C_BiEquivalence'46'constructor_28745 v1 v2 v3 v4 v5 v6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiEquivalence.from₂
d_from'8322'_1160 :: T_BiEquivalence_1142 -> AgdaAny -> AgdaAny
d_from'8322'_1160 v0
  = case coe v0 of
      C_BiEquivalence'46'constructor_28745 v1 v2 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiEquivalence.to-cong
d_to'45'cong_1162 ::
  T_BiEquivalence_1142 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'cong_1162 v0
  = case coe v0 of
      C_BiEquivalence'46'constructor_28745 v1 v2 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiEquivalence.from₁-cong
d_from'8321''45'cong_1164 ::
  T_BiEquivalence_1142 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'8321''45'cong_1164 v0
  = case coe v0 of
      C_BiEquivalence'46'constructor_28745 v1 v2 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiEquivalence.from₂-cong
d_from'8322''45'cong_1166 ::
  T_BiEquivalence_1142 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'8322''45'cong_1166 v0
  = case coe v0 of
      C_BiEquivalence'46'constructor_28745 v1 v2 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiInverse
d_BiInverse_1168 a0 a1 a2 a3 a4 a5 = ()
data T_BiInverse_1168
  = C_BiInverse'46'constructor_30531 (AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                     (AgdaAny -> AgdaAny)
-- Function.Bundles._.BiInverse.to
d_to_1186 :: T_BiInverse_1168 -> AgdaAny -> AgdaAny
d_to_1186 v0
  = case coe v0 of
      C_BiInverse'46'constructor_30531 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiInverse.from₁
d_from'8321'_1188 :: T_BiInverse_1168 -> AgdaAny -> AgdaAny
d_from'8321'_1188 v0
  = case coe v0 of
      C_BiInverse'46'constructor_30531 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiInverse.from₂
d_from'8322'_1190 :: T_BiInverse_1168 -> AgdaAny -> AgdaAny
d_from'8322'_1190 v0
  = case coe v0 of
      C_BiInverse'46'constructor_30531 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiInverse.to-cong
d_to'45'cong_1192 ::
  T_BiInverse_1168 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'cong_1192 v0
  = case coe v0 of
      C_BiInverse'46'constructor_30531 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiInverse.from₁-cong
d_from'8321''45'cong_1194 ::
  T_BiInverse_1168 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'8321''45'cong_1194 v0
  = case coe v0 of
      C_BiInverse'46'constructor_30531 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiInverse.from₂-cong
d_from'8322''45'cong_1196 ::
  T_BiInverse_1168 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_from'8322''45'cong_1196 v0
  = case coe v0 of
      C_BiInverse'46'constructor_30531 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiInverse.inverseˡ
d_inverse'737'_1198 :: T_BiInverse_1168 -> AgdaAny -> AgdaAny
d_inverse'737'_1198 v0
  = case coe v0 of
      C_BiInverse'46'constructor_30531 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiInverse.inverseʳ
d_inverse'691'_1200 :: T_BiInverse_1168 -> AgdaAny -> AgdaAny
d_inverse'691'_1200 v0
  = case coe v0 of
      C_BiInverse'46'constructor_30531 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bundles._.BiInverse.to-isCongruent
d_to'45'isCongruent_1202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_BiInverse_1168 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
d_to'45'isCongruent_1202 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_to'45'isCongruent_1202 v4 v5 v6
du_to'45'isCongruent_1202 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_BiInverse_1168 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
du_to'45'isCongruent_1202 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsCongruent'46'constructor_985
      (coe d_to'45'cong_1192 (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
-- Function.Bundles._.BiInverse.isBiInverse
d_isBiInverse_1204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_BiInverse_1168 ->
  MAlonzo.Code.Function.Structures.T_IsBiInverse_636
d_isBiInverse_1204 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_isBiInverse_1204 v4 v5 v6
du_isBiInverse_1204 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_BiInverse_1168 ->
  MAlonzo.Code.Function.Structures.T_IsBiInverse_636
du_isBiInverse_1204 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Structures.C_IsBiInverse'46'constructor_28517
      (coe du_to'45'isCongruent_1202 (coe v0) (coe v1) (coe v2))
      (coe d_from'8321''45'cong_1194 (coe v2))
      (coe d_from'8322''45'cong_1196 (coe v2))
      (coe d_inverse'737'_1198 (coe v2))
      (coe d_inverse'691'_1200 (coe v2))
-- Function.Bundles._.BiInverse.biEquivalence
d_biEquivalence_1206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_BiInverse_1168 -> T_BiEquivalence_1142
d_biEquivalence_1206 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_biEquivalence_1206 v6
du_biEquivalence_1206 :: T_BiInverse_1168 -> T_BiEquivalence_1142
du_biEquivalence_1206 v0
  = coe
      C_BiEquivalence'46'constructor_28745 (coe d_to_1186 (coe v0))
      (coe d_from'8321'_1188 (coe v0)) (coe d_from'8322'_1190 (coe v0))
      (coe d_to'45'cong_1192 (coe v0))
      (coe d_from'8321''45'cong_1194 (coe v0))
      (coe d_from'8322''45'cong_1196 (coe v0))
-- Function.Bundles._⟶_
d__'10230'__1208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'10230'__1208 = erased
-- Function.Bundles._↣_
d__'8611'__1214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8611'__1214 = erased
-- Function.Bundles._↠_
d__'8608'__1220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8608'__1220 = erased
-- Function.Bundles._⤖_
d__'10518'__1226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'10518'__1226 = erased
-- Function.Bundles._⇔_
d__'8660'__1232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8660'__1232 = erased
-- Function.Bundles._↩_
d__'8617'__1238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8617'__1238 = erased
-- Function.Bundles._↪_
d__'8618'__1244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8618'__1244 = erased
-- Function.Bundles._↩↪_
d__'8617''8618'__1250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8617''8618'__1250 = erased
-- Function.Bundles._↔_
d__'8596'__1256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8596'__1256 = erased
-- Function.Bundles._._.Bijective
d_Bijective_1276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d_Bijective_1276 = erased
-- Function.Bundles._._.Injective
d_Injective_1280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d_Injective_1280 = erased
-- Function.Bundles._._.Inverseʳ
d_Inverse'691'_1282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Inverse'691'_1282 = erased
-- Function.Bundles._._.Inverseˡ
d_Inverse'737'_1284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Inverse'737'_1284 = erased
-- Function.Bundles._._.Inverseᵇ
d_Inverse'7495'_1286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Inverse'7495'_1286 = erased
-- Function.Bundles._._.Surjective
d_Surjective_1288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> ()
d_Surjective_1288 = erased
-- Function.Bundles._.mk⟶
d_mk'10230'_1290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> T_Func_642
d_mk'10230'_1290 ~v0 ~v1 ~v2 ~v3 v4 = du_mk'10230'_1290 v4
du_mk'10230'_1290 :: (AgdaAny -> AgdaAny) -> T_Func_642
du_mk'10230'_1290 v0
  = coe C_Func'46'constructor_5935 (coe v0) erased
-- Function.Bundles._.mk↣
d_mk'8611'_1296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Injection_704
d_mk'8611'_1296 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_mk'8611'_1296 v4 v5
du_mk'8611'_1296 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Injection_704
du_mk'8611'_1296 v0 v1
  = coe C_Injection'46'constructor_7999 (coe v0) erased (coe v1)
-- Function.Bundles._.mk↠
d_mk'8608'_1304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  T_Surjection_774
d_mk'8608'_1304 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_mk'8608'_1304 v4 v5
du_mk'8608'_1304 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  T_Surjection_774
du_mk'8608'_1304 v0 v1
  = coe C_Surjection'46'constructor_10213 (coe v0) erased (coe v1)
-- Function.Bundles._.mk⤖
d_mk'10518'_1312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Bijection_844
d_mk'10518'_1312 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_mk'10518'_1312 v4 v5
du_mk'10518'_1312 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Bijection_844
du_mk'10518'_1312 v0 v1
  = coe C_Bijection'46'constructor_13257 (coe v0) erased (coe v1)
-- Function.Bundles._.mk⇔
d_mk'8660'_1322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> T_Equivalence_928
d_mk'8660'_1322 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_mk'8660'_1322 v4 v5
du_mk'8660'_1322 ::
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> T_Equivalence_928
du_mk'8660'_1322 v0 v1
  = coe
      C_Equivalence'46'constructor_17233 (coe v0) (coe v1) erased erased
-- Function.Bundles._.mk↩
d_mk'8617'_1332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_LeftInverse_946
d_mk'8617'_1332 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_mk'8617'_1332 v4 v5 v6
du_mk'8617'_1332 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_LeftInverse_946
du_mk'8617'_1332 v0 v1 v2
  = coe
      C_LeftInverse'46'constructor_18307 (coe v0) (coe v1) erased erased
      (coe v2)
-- Function.Bundles._.mk↪
d_mk'8618'_1344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_RightInverse_1024
d_mk'8618'_1344 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_mk'8618'_1344 v4 v5 v6
du_mk'8618'_1344 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_RightInverse_1024
du_mk'8618'_1344 v0 v1 v2
  = coe
      C_RightInverse'46'constructor_21741 (coe v0) (coe v1) erased erased
      (coe v2)
-- Function.Bundles._.mk↩↪
d_mk'8617''8618'_1358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_BiInverse_1168
d_mk'8617''8618'_1358 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_mk'8617''8618'_1358 v4 v5 v6 v7 v8
du_mk'8617''8618'_1358 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_BiInverse_1168
du_mk'8617''8618'_1358 v0 v1 v2 v3 v4
  = coe
      C_BiInverse'46'constructor_30531 (coe v0) (coe v1) (coe v2) erased
      erased erased (coe v3) (coe v4)
-- Function.Bundles._.mk↔
d_mk'8596'_1374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Inverse_1052
d_mk'8596'_1374 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_mk'8596'_1374 v4 v5 v6
du_mk'8596'_1374 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Inverse_1052
du_mk'8596'_1374 v0 v1 v2
  = coe
      C_Inverse'46'constructor_24077 (coe v0) (coe v1) erased erased
      (coe v2)
-- Function.Bundles._.mk↔′
d_mk'8596''8242'_1386 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Inverse_1052
d_mk'8596''8242'_1386 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_mk'8596''8242'_1386 v4 v5 v6 v7
du_mk'8596''8242'_1386 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Inverse_1052
du_mk'8596''8242'_1386 v0 v1 v2 v3
  = coe
      du_mk'8596'_1374 (coe v0) (coe v1)
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v3))
