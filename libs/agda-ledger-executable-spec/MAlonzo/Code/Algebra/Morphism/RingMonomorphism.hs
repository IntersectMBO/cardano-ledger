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

module MAlonzo.Code.Algebra.Morphism.RingMonomorphism where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Morphism.GroupMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Morphism.RingMonomorphism._._*_
d__'42'__66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__66 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du__'42'__66 v4
du__'42'__66 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'42'__66 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 (coe v0)
-- Algebra.Morphism.RingMonomorphism._._+_
d__'43'__68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43'__68 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du__'43'__68 v4
du__'43'__68 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'43'__68 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 (coe v0)
-- Algebra.Morphism.RingMonomorphism._._≈_
d__'8776'__70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__70 = erased
-- Algebra.Morphism.RingMonomorphism._.-_
d_'45'__84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny
d_'45'__84 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du_'45'__84 v4
du_'45'__84 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny
du_'45'__84 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 (coe v0)
-- Algebra.Morphism.RingMonomorphism._.0#
d_0'35'_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny
d_0'35'_86 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du_0'35'_86 v4
du_0'35'_86 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 -> AgdaAny
du_0'35'_86 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)
-- Algebra.Morphism.RingMonomorphism._.1#
d_1'35'_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny
d_1'35'_88 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du_1'35'_88 v4
du_1'35'_88 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 -> AgdaAny
du_1'35'_88 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274 (coe v0)
-- Algebra.Morphism.RingMonomorphism._._≈_
d__'8776'__96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__96 = erased
-- Algebra.Morphism.RingMonomorphism._._+_
d__'43'__100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43'__100 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'43'__100 v5
du__'43'__100 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'43'__100 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 (coe v0)
-- Algebra.Morphism.RingMonomorphism._._*_
d__'42'__102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__102 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'42'__102 v5
du__'42'__102 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'42'__102 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 (coe v0)
-- Algebra.Morphism.RingMonomorphism._.0#
d_0'35'_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny
d_0'35'_114 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du_0'35'_114 v5
du_0'35'_114 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 -> AgdaAny
du_0'35'_114 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)
-- Algebra.Morphism.RingMonomorphism._.1#
d_1'35'_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny
d_1'35'_116 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du_1'35'_116 v5
du_1'35'_116 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 -> AgdaAny
du_1'35'_116 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274 (coe v0)
-- Algebra.Morphism.RingMonomorphism._.-_
d_'45'__122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny
d_'45'__122 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du_'45'__122 v5
du_'45'__122 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny
du_'45'__122 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 (coe v0)
-- Algebra.Morphism.RingMonomorphism._.assoc
d_assoc_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_126 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_assoc_126 v4 v5 v6 v7
du_assoc_126 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_126 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_assoc_140
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.cancel
d_cancel_128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cancel_128 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel_128 v4 v5 v6 v7
du_cancel_128 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_cancel_128 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel_216
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.cancelʳ
d_cancel'691'_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'691'_130 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'691'_130 v4 v5 v6 v7
du_cancel'691'_130 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'691'_130 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'691'_204
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.cancelˡ
d_cancel'737'_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'737'_132 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'737'_132 v4 v5 v6 v7
du_cancel'737'_132 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'737'_132 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'737'_192
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.comm
d_comm_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_134 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_comm_134 v4 v5 v6 v7
du_comm_134 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_134 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.cong
d_cong_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_136 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_cong_136 v4 v5 v6 v7
du_cong_136 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cong_136 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cong_126
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.idem
d_idem_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_idem_138 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_idem_138 v4 v5 v6 v7
du_idem_138 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_idem_138 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_idem_158
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.identity
d_identity_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_140 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_identity_140 v4 v5 v6 v7
du_identity_140 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_identity_140 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity_160
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.identityʳ
d_identity'691'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_identity'691'_142 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_identity'691'_142 v4 v5 v6 v7
du_identity'691'_142 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_identity'691'_142 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity'691'_154
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.identityˡ
d_identity'737'_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_identity'737'_144 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_identity'737'_144 v4 v5 v6 v7
du_identity'737'_144 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_identity'737'_144 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity'737'_148
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.isBand
d_isBand_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_146 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isBand_146 v4 v5 v6 v7
du_isBand_146 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_146 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isBand_282
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.isCommutativeMonoid
d_isCommutativeMonoid_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_148 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isCommutativeMonoid_148 v4 v5 v6 v7
du_isCommutativeMonoid_148 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_148 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isCommutativeMonoid_220
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.isMagma
d_isMagma_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_150 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isMagma_150 v4 v5 v6 v7
du_isMagma_150 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_150 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isMagma_218
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.isMonoid
d_isMonoid_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_152 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isMonoid_152 v4 v5 v6 v7
du_isMonoid_152 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_isMonoid_152 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isMonoid_176
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.isSelectiveMagma
d_isSelectiveMagma_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_isSelectiveMagma_154 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isSelectiveMagma_154 v4 v5 v6 v7
du_isSelectiveMagma_154 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
du_isSelectiveMagma_154 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSelectiveMagma_320
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.isSemigroup
d_isSemigroup_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_156 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isSemigroup_156 v4 v5 v6 v7
du_isSemigroup_156 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_156 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.sel
d_sel_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_158 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_sel_158 v4 v5 v6 v7
du_sel_158 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sel_158 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    let v7
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4) in
    let v8
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5) in
    let v9
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v6) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_sel_164
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v7))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v8))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v9))
-- Algebra.Morphism.RingMonomorphism._.zero
d_zero_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_160 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_zero_160 v4 v5 v6 v7
du_zero_160 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_160 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_zero_174
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.zeroʳ
d_zero'691'_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'691'_162 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_zero'691'_162 v4 v5 v6 v7
du_zero'691'_162 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'691'_162 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_zero'691'_168
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.zeroˡ
d_zero'737'_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'737'_164 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_zero'737'_164 v4 v5 v6 v7
du_zero'737'_164 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'737'_164 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v0) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290
              (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_zero'737'_162
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.inverse
d_inverse_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_166 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_inverse_166 v4 v5 v6 v7
du_inverse_166 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_inverse_166 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_inverse_190
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.inverseʳ
d_inverse'691'_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'691'_168 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_inverse'691'_168 v4 v5 v6 v7
du_inverse'691'_168 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'691'_168 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_inverse'691'_184
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.inverseˡ
d_inverse'737'_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'737'_170 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_inverse'737'_170 v4 v5 v6 v7
du_inverse'737'_170 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'737'_170 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_inverse'737'_178
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.isAbelianGroup
d_isAbelianGroup_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_isAbelianGroup_172 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isAbelianGroup_172 v4 v5 v6 v7
du_isAbelianGroup_172 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
du_isAbelianGroup_172 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_isAbelianGroup_382
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.isGroup
d_isGroup_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_174 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isGroup_174 v4 v5 v6 v7
du_isGroup_174 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
du_isGroup_174 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_isGroup_318
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.⁻¹-cong
d_'8315''185''45'cong_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_176 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_'8315''185''45'cong_176 v4 v5 v6 v7
du_'8315''185''45'cong_176 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8315''185''45'cong_176 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_'8315''185''45'cong_196
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.⁻¹-distrib-∙
d_'8315''185''45'distrib'45''8729'_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'distrib'45''8729'_178 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_'8315''185''45'distrib'45''8729'_178 v4 v5 v6 v7
du_'8315''185''45'distrib'45''8729'_178 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8315''185''45'distrib'45''8729'_178 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_'8315''185''45'distrib'45''8729'_310
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.assoc
d_assoc_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_182 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_assoc_182 v4 v5 v6 v7
du_assoc_182 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_182 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_assoc_140
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.cancel
d_cancel_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cancel_184 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel_184 v4 v5 v6 v7
du_cancel_184 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_cancel_184 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel_216
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.cancelʳ
d_cancel'691'_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'691'_186 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'691'_186 v4 v5 v6 v7
du_cancel'691'_186 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'691'_186 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'691'_204
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.cancelˡ
d_cancel'737'_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'737'_188 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'737'_188 v4 v5 v6 v7
du_cancel'737'_188 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'737'_188 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'737'_192
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.comm
d_comm_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_190 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_comm_190 v4 v5 v6 v7
du_comm_190 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_190 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.cong
d_cong_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_192 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_cong_192 v4 v5 v6 v7
du_cong_192 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cong_192 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cong_126
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.idem
d_idem_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_idem_194 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_idem_194 v4 v5 v6 v7
du_idem_194 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_idem_194 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_idem_158
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.identity
d_identity_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_196 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_identity_196 v4 v5 v6 v7
du_identity_196 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_identity_196 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity_160
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.identityʳ
d_identity'691'_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_identity'691'_198 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_identity'691'_198 v4 v5 v6 v7
du_identity'691'_198 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_identity'691'_198 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity'691'_154
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.identityˡ
d_identity'737'_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_identity'737'_200 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_identity'737'_200 v4 v5 v6 v7
du_identity'737'_200 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_identity'737'_200 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity'737'_148
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.isBand
d_isBand_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_202 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isBand_202 v4 v5 v6 v7
du_isBand_202 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_202 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isBand_282
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.isCommutativeMonoid
d_isCommutativeMonoid_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_204 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isCommutativeMonoid_204 v4 v5 v6 v7
du_isCommutativeMonoid_204 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_204 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isCommutativeMonoid_220
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.isMagma
d_isMagma_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_206 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isMagma_206 v4 v5 v6 v7
du_isMagma_206 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_206 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isMagma_218
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.isMonoid
d_isMonoid_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_208 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isMonoid_208 v4 v5 v6 v7
du_isMonoid_208 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_isMonoid_208 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isMonoid_176
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.isSelectiveMagma
d_isSelectiveMagma_210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_isSelectiveMagma_210 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isSelectiveMagma_210 v4 v5 v6 v7
du_isSelectiveMagma_210 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
du_isSelectiveMagma_210 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSelectiveMagma_320
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.isSemigroup
d_isSemigroup_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_212 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isSemigroup_212 v4 v5 v6 v7
du_isSemigroup_212 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_212 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.sel
d_sel_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_214 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_sel_214 v4 v5 v6 v7
du_sel_214 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sel_214 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
              (coe
                 MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_sel_164
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.RingMonomorphism._.zero
d_zero_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_216 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_zero_216 v4 v5 v6 v7
du_zero_216 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_216 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_zero_174
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.zeroʳ
d_zero'691'_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'691'_218 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_zero'691'_218 v4 v5 v6 v7
du_zero'691'_218 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'691'_218 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_zero'691'_168
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.zeroˡ
d_zero'737'_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'737'_220 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_zero'737'_220 v4 v5 v6 v7
du_zero'737'_220 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'737'_220 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_zero'737'_162
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
         (coe v3))
-- Algebra.Morphism.RingMonomorphism._.distribˡ
d_distrib'737'_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_326 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
                   v13
  = du_distrib'737'_326 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
du_distrib'737'_326 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_326 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_2174 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9)))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9)))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1 (coe v2 v8)
                     (coe v2 v9)))
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1 (coe v2 v8)
                        (coe v2 v9)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                        (coe v2 v8))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                        (coe v2 v9)))
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                           (coe v2 v8))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                           (coe v2 v9)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1
                        (coe
                           v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8))
                        (coe
                           v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9)))
                     (coe
                        v2
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                        (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8))
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9)))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5)))
                           (coe
                              v2
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9))))
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                           (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_'43''45'isMonoidHomomorphism_872
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                                    (coe
                                       MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                       (coe
                                          MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                          (coe v3))))))
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v9)))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                        (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                              (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4))))
                        (coe
                           v2
                           (let v10
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                         (coe v0)) in
                            coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__130 v10 v7 v8))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                           (coe v2 v8))
                        (coe
                           v2
                           (let v10
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                         (coe v0)) in
                            coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__130 v10 v7 v9))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                           (coe v2 v9))
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
                           (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                    (coe v3))))
                           v7 v8)
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
                           (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                    (coe v3))))
                           v7 v9)))
                  (coe v6 (coe v2 v7) (coe v2 v8) (coe v2 v9)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v5 (coe v2 v7)
                  (coe v2 v7)
                  (coe
                     v2
                     (let v10
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
                                (coe
                                   MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134
                                   (coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                         (coe v0)))) in
                      coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v10 v8 v9))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1 (coe v2 v8)
                     (coe v2 v9))
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
                     (coe v2 v7))
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_'43''45'isMonoidHomomorphism_872
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                    (coe v3))))))
                     v8 v9)))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                        (coe v3))))
               v7 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9))))
-- Algebra.Morphism.RingMonomorphism._.distribʳ
d_distrib'691'_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_336 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
                   v13
  = du_distrib'691'_336 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
du_distrib'691'_336 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_336 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_2174 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9) v7)
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7)
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9) v7))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9))
               (coe v2 v7))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7)
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9))
                  (coe v2 v7))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1 (coe v2 v8)
                     (coe v2 v9))
                  (coe v2 v7))
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7)
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1 (coe v2 v8)
                        (coe v2 v9))
                     (coe v2 v7))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v8)
                        (coe v2 v7))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v9)
                        (coe v2 v7)))
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7)
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v8)
                           (coe v2 v7))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v9)
                           (coe v2 v7)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1
                        (coe
                           v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7))
                        (coe
                           v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7)))
                     (coe
                        v2
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7)
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                        (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7))
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7)
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7)
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7)))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5)))
                           (coe
                              v2
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7)
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7))))
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                           (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_'43''45'isMonoidHomomorphism_872
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                                    (coe
                                       MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                       (coe
                                          MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                          (coe v3))))))
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v8 v7)
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v9 v7)))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                        (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                              (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4))))
                        (coe
                           v2
                           (let v10
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                         (coe v0)) in
                            coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__130 v10 v8 v7))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v8)
                           (coe v2 v7))
                        (coe
                           v2
                           (let v10
                                  = coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                         (coe v0)) in
                            coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__130 v10 v9 v7))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v9)
                           (coe v2 v7))
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
                           (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                    (coe v3))))
                           v8 v7)
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
                           (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                    (coe v3))))
                           v9 v7)))
                  (coe v6 (coe v2 v7) (coe v2 v8) (coe v2 v9)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v5
                  (coe
                     v2
                     (let v10
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
                                (coe
                                   MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134
                                   (coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                      (coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                         (coe v0)))) in
                      coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v10 v8 v9))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v1 (coe v2 v8)
                     (coe v2 v9))
                  (coe v2 v7) (coe v2 v7)
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_'43''45'isMonoidHomomorphism_872
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                    (coe v3))))))
                     v8 v9)
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
                     (coe v2 v7))))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                        (coe v3))))
               (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v0 v8 v9) v7)))
-- Algebra.Morphism.RingMonomorphism._.distrib
d_distrib_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_346 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_distrib_346 v4 v5 v6 v7 v8 v9 v10
du_distrib_346 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_distrib_346 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_distrib'737'_326 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe v5) (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v6)))
      (coe
         du_distrib'691'_336 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe v5) (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v6)))
-- Algebra.Morphism.RingMonomorphism._.zeroˡ
d_zero'737'_350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'737'_350 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 ~v8 v9 v10 v11
  = du_zero'737'_350 v4 v5 v6 v7 v9 v10 v11
du_zero'737'_350 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'737'_350 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_2174 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
         (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)) v6)
      (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)) v6))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
               (coe v2 v6))
            (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
                  (coe v2 v6))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1))
                  (coe v2 v6))
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                     (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1))
                     (coe v2 v6))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1))
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                        (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_'43''45'isMonoidHomomorphism_872
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                    (coe v3)))))))
                  (coe v5 (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4
                  (coe
                     v2
                     (let v7
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134
                                (coe
                                   MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                   (coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                      (coe v0))) in
                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v7)))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1)) (coe v2 v6)
                  (coe v2 v6)
                  (MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_'43''45'isMonoidHomomorphism_872
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                 (coe v3))))))
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                     (coe v2 v6))))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                        (coe v3))))
               (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)) v6)))
-- Algebra.Morphism.RingMonomorphism._.zeroʳ
d_zero'691'_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'691'_356 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 ~v8 v9 v10 v11
  = du_zero'691'_356 v4 v5 v6 v7 v9 v10 v11
du_zero'691'_356 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'691'_356 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_2174 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v6
         (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
      (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v6
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0))))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v6)
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0))))
            (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v6)
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0))))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v6)
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1)))
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v6)
                     (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1)))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1))
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                        (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_'43''45'isMonoidHomomorphism_872
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                    (coe v3)))))))
                  (coe v5 (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4 (coe v2 v6)
                  (coe v2 v6)
                  (coe
                     v2
                     (let v7
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawMonoid_134
                                (coe
                                   MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                   (coe
                                      MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                      (coe v0))) in
                      MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v7)))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v1))
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                     (coe v2 v6))
                  (MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_'43''45'isMonoidHomomorphism_872
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                 (coe v3))))))))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                        (coe v3))))
               v6 (MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0)))))
-- Algebra.Morphism.RingMonomorphism._.zero
d_zero_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_362 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 ~v8 v9 v10
  = du_zero_362 v4 v5 v6 v7 v9 v10
du_zero_362 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_362 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_zero'737'_350 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)))
      (coe
         du_zero'691'_356 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
-- Algebra.Morphism.RingMonomorphism._.neg-distribˡ-*
d_neg'45'distrib'737''45''42'_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_neg'45'distrib'737''45''42'_374 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
                                  v10 v11 v12
  = du_neg'45'distrib'737''45''42'_374 v4 v5 v6 v7 v8 v9 v10 v11 v12
du_neg'45'distrib'737''45''42'_374 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_neg'45'distrib'737''45''42'_374 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_2174 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8)
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                     (coe v2 v8)))
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                        (coe v2 v8)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1 (coe v2 v7))
                     (coe v2 v8))
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1 (coe v2 v7))
                        (coe v2 v8))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                        (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7))
                        (coe v2 v8))
                     (coe
                        v2
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                           (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7))
                           (coe v2 v8))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5)))
                           (coe
                              v2
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8)))
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                           (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
                           (coe
                              v2
                              (let v9
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                         (coe
                                            MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                            (coe v0)) in
                               coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__130 v9
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1
                              (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7))
                              (coe v2 v8))
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
                              (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                    (coe
                                       MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                       (coe v3))))
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7) v8)))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v5
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1 (coe v2 v7))
                        (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7))
                        (coe v2 v8) (coe v2 v8)
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                           (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
                           (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v7))
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1 (coe v2 v7))
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_'45''8255'homo_2134
                              (MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                 (coe v3))
                              v7))
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
                           (coe v2 v8))))
                  (coe v6 (coe v2 v7) (coe v2 v8)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906 v4
                  (coe
                     v2
                     (let v9
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                (coe
                                   MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
                      coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__130 v9 v7 v8))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                     (coe v2 v8))
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                              (coe v3))))
                     v7 v8)))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_'45''8255'homo_2134
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                  (coe v3))
               (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8))))
-- Algebra.Morphism.RingMonomorphism._.neg-distribʳ-*
d_neg'45'distrib'691''45''42'_390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_neg'45'distrib'691''45''42'_390 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
                                  v10 v11 v12
  = du_neg'45'distrib'691''45''42'_390 v4 v5 v6 v7 v8 v9 v10 v11 v12
du_neg'45'distrib'691''45''42'_390 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_neg'45'distrib'691''45''42'_390 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_2174 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                     (coe v2 v8)))
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                        (coe v2 v8)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1 (coe v2 v8)))
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1 (coe v2 v8)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                        (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
                     (coe
                        v2
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v5))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                           (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5)))
                           (coe
                              v2
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8))))
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                           (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
                           (coe
                              v2
                              (let v9
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                         (coe
                                            MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276
                                            (coe v0)) in
                               coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__130 v9 v7
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                              (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8)))
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
                              (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                                    (coe
                                       MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                       (coe v3))))
                              v7 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8))))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v5 (coe v2 v7)
                        (coe v2 v7)
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1 (coe v2 v8))
                        (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8))
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
                           (coe v2 v7))
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                           (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
                           (coe v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v0 v8))
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v1 (coe v2 v8))
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_'45''8255'homo_2134
                              (MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                                 (coe v3))
                              v8))))
                  (coe v6 (coe v2 v7) (coe v2 v8)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906 v4
                  (coe
                     v2
                     (let v9
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.du_rawNearSemiring_178
                                (coe
                                   MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)) in
                      coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__130 v9 v7 v8))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v1 (coe v2 v7)
                     (coe v2 v8))
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_'42''45'homo_874
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isNearSemiringHomomorphism_1254
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isSemiringHomomorphism_2132
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                              (coe v3))))
                     v7 v8)))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_'45''8255'homo_2134
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isRingHomomorphism_2172
                  (coe v3))
               (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v0 v7 v8))))
-- Algebra.Morphism.RingMonomorphism.isRing
d_isRing_398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
d_isRing_398 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isRing_398 v4 v5 v6 v7 v8
du_isRing_398 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394 ->
  MAlonzo.Code.Algebra.Structures.T_IsRing_2394
du_isRing_398 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsRing'46'constructor_80853
      (coe
         MAlonzo.Code.Algebra.Morphism.GroupMonomorphism.du_isAbelianGroup_382
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v0))
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_'43''45'rawGroup_290 (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_'43''45'isGroupMonomorphism_2208
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
            (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cong_126
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0))))
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1))))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
               (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2496 (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_assoc_140
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0))))
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1))))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
               (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2496 (coe v4))
         (coe
            MAlonzo.Code.Algebra.Structures.d_'42''45'assoc_2422 (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity_160
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
            (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
            (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1)))
         v2
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2496 (coe v4))
         (MAlonzo.Code.Algebra.Structures.d_'42''45'identity_2424 (coe v4)))
      (coe
         du_distrib_346 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe
            MAlonzo.Code.Algebra.Structures.d_isGroup_988
            (coe
               MAlonzo.Code.Algebra.Structures.d_'43''45'isAbelianGroup_2418
               (coe v4)))
         (coe
            MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2496 (coe v4))
         (coe MAlonzo.Code.Algebra.Structures.d_distrib_2426 (coe v4)))
      (coe
         du_zero_362 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe
            MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2496 (coe v4))
         (coe MAlonzo.Code.Algebra.Structures.d_zero_2428 (coe v4)))
-- Algebra.Morphism.RingMonomorphism.isCommutativeRing
d_isCommutativeRing_514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
d_isCommutativeRing_514 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isCommutativeRing_514 v4 v5 v6 v7 v8
du_isCommutativeRing_514 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeRing_2540
du_isCommutativeRing_514 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeRing'46'constructor_87819
      (coe
         du_isRing_398 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v0))))
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.du_rawSemiring_276 (coe v1))))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.du_'42''45'isMonoidMonomorphism_2218
               (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Structures.du_'42''45'isMagma_2496
            (coe MAlonzo.Code.Algebra.Structures.d_isRing_2556 (coe v4)))
         (coe MAlonzo.Code.Algebra.Structures.d_'42''45'comm_2558 (coe v4)))
